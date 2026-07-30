open Cmarkit_

(** Djot attribute specifiers inside pipe-table cells, behind
    [inline_attributes].

    {v
| a | b |
|---|---|
| x{#i} | y |
    v}

    A cell reads like a paragraph: the specifier attaches to the inline it is
    adjacent to, else to the last word of the pending text, else it is dropped.

    Inline parsing is three passes and it is [last_pass] that folds a specifier
    into its target -- [first_pass] and [second_pass] carry [Attribute_spec]
    tokens through untouched. A table row never reaches [last_pass]
    ([parse_table_row] goes [first_pass] -> [second_pass_cells] ->
    [parse_cols]), so the row parser does the fold itself, in
    [attach_attribute]. Before it did not, and every specifier a row could not
    resolve during [first_pass] reached [parse_cols] as a token it had no arm
    for: [Assertion failed], a hard crash of [Doc.of_string]. *)

let html s =
  let doc = Doc.of_string ~strict:false ~inline_attributes:true s in
  print_string (Cmarkit_html.of_doc ~safe:false doc)

let commonmark s =
  let doc = Doc.of_string ~strict:false ~inline_attributes:true s in
  print_string (Cmarkit_commonmark.of_doc doc)

let%expect_test "adjacent specifier attaches to the preceding word" =
  html "| a | b |\n|---|---|\n| x{#i} | y |\n";
  [%expect {|
    <div role="region"><table>
    <tr>
    <th>a</th>
    <th>b</th>
    </tr>
    <tr>
    <td><span id="i">x</span></td>
    <td>y</td>
    </tr>
    </table></div>
    |}]

let%expect_test "the same specifier in a paragraph, for comparison" =
  html "x{#i}\n";
  [%expect {| <p><span id="i">x</span></p> |}]

let%expect_test "a header cell folds too" =
  html "| a{#h} | b |\n|---|---|\n| x | y |\n";
  [%expect {|
    <div role="region"><table>
    <tr>
    <th><span id="h">a</span></th>
    <th>b</th>
    </tr>
    <tr>
    <td>x</td>
    <td>y</td>
    </tr>
    </table></div>
    |}]

let%expect_test "attaches to the last word only" =
  html "| a | b |\n|---|---|\n| p q{#i} | y |\n";
  [%expect {|
    <div role="region"><table>
    <tr>
    <th>a</th>
    <th>b</th>
    </tr>
    <tr>
    <td>p <span id="i">q</span></td>
    <td>y</td>
    </tr>
    </table></div>
    |}]

let%expect_test "two specifiers in one cell, each on its own target" =
  html "| a | b |\n|---|---|\n| x{#i}, z{.c} | y |\n";
  [%expect {|
    <div role="region"><table>
    <tr>
    <th>a</th>
    <th>b</th>
    </tr>
    <tr>
    <td><span id="i">x</span>, <span class="c">z</span></td>
    <td>y</td>
    </tr>
    </table></div>
    |}]

let%expect_test "consecutive specifiers merge onto one target" =
  html "| a | b |\n|---|---|\n| x{#i}{.c} | y |\n";
  [%expect {|
    <div role="region"><table>
    <tr>
    <th>a</th>
    <th>b</th>
    </tr>
    <tr>
    <td><span id="i" class="c">x</span></td>
    <td>y</td>
    </tr>
    </table></div>
    |}]

let%expect_test "attaches to a preceding emphasis or code span" =
  html "| a | b |\n|---|---|\n| *x*{#i} | `y`{.c} |\n";
  [%expect {|
    <div role="region"><table>
    <tr>
    <th>a</th>
    <th>b</th>
    </tr>
    <tr>
    <td><em id="i">x</em></td>
    <td><code class="c">y</code></td>
    </tr>
    </table></div>
    |}]

let%expect_test "a bracketed span is resolved before the row is split" =
  html "| a | b |\n|---|---|\n| [x y]{#i} | z |\n";
  [%expect {|
    <div role="region"><table>
    <tr>
    <th>a</th>
    <th>b</th>
    </tr>
    <tr>
    <td><span id="i">x y</span></td>
    <td>z</td>
    </tr>
    </table></div>
    |}]

(* Unattached specifiers *)

let%expect_test "after a space it attaches to nothing and is dropped" =
  html "| a | b |\n|---|---|\n| x {#i} | y |\n";
  [%expect {|
    <div role="region"><table>
    <tr>
    <th>a</th>
    <th>b</th>
    </tr>
    <tr>
    <td>x </td>
    <td>y</td>
    </tr>
    </table></div>
    |}];
  html "x {#i}\n";
  [%expect {| <p>x </p> |}]

let%expect_test "at the start of a cell it is dropped, leaving it empty" =
  html "| a | b |\n|---|---|\n| {#i} | y |\n";
  [%expect {|
    <div role="region"><table>
    <tr>
    <th>a</th>
    <th>b</th>
    </tr>
    <tr>
    <td></td>
    <td>y</td>
    </tr>
    </table></div>
    |}]

let%expect_test "at the start of a cell with text after it" =
  html "| a | b |\n|---|---|\n|{#i}x|{.c}y|\n";
  [%expect {|
    <div role="region"><table>
    <tr>
    <th>a</th>
    <th>b</th>
    </tr>
    <tr>
    <td>x</td>
    <td>y</td>
    </tr>
    </table></div>
    |}]

let%expect_test "a comment-only specifier is dropped, target untouched" =
  html "| a | b |\n|---|---|\n| x{% c %} | y |\n";
  [%expect {|
    <div role="region"><table>
    <tr>
    <th>a</th>
    <th>b</th>
    </tr>
    <tr>
    <td>x</td>
    <td>y</td>
    </tr>
    </table></div>
    |}]

let%expect_test "a cell holding only a comment is empty" =
  html "| a | b |\n|---|---|\n| {% c %} | y |\n";
  [%expect {|
    <div role="region"><table>
    <tr>
    <th>a</th>
    <th>b</th>
    </tr>
    <tr>
    <td></td>
    <td>y</td>
    </tr>
    </table></div>
    |}]

(* Cell boundaries *)

let%expect_test "a [|] inside an attribute value is not a cell boundary" =
  html "| a | b |\n|---|---|\n| x{k=\"a|b\"} | y |\n";
  [%expect {|
    <div role="region"><table>
    <tr>
    <th>a</th>
    <th>b</th>
    </tr>
    <tr>
    <td><span k="a|b">x</span></td>
    <td>y</td>
    </tr>
    </table></div>
    |}]

let%expect_test "an escaped specifier stays literal" =
  html "| a | b |\n|---|---|\n| x\\{#i} | y |\n";
  [%expect {|
    <div role="region"><table>
    <tr>
    <th>a</th>
    <th>b</th>
    </tr>
    <tr>
    <td>x{#i}</td>
    <td>y</td>
    </tr>
    </table></div>
    |}]

let%expect_test "a header-only row with a specifier" =
  html "| x{#i} | y |\n";
  [%expect {|
    <div role="region"><table>
    <tr>
    <td><span id="i">x</span></td>
    <td>y</td>
    </tr>
    </table></div>
    |}]

(* Roundtrip: the renderer normalizes cell padding for every table, with
   specifiers or without, but the specifier itself survives. *)

let%expect_test "commonmark roundtrip" =
  commonmark "| a | b |\n|---|---|\n| x{#i} | y |\n";
  [%expect {|
    |a|b|
    |---|---|
    |x{#i}|y|
    |}]

let%expect_test "commonmark roundtrip drops an unattached specifier" =
  commonmark "| a | b |\n|---|---|\n| {#i} | y |\n";
  [%expect {|
    |a|b|
    |---|---|
    ||y|
    |}]
