open Cmarkit_

(** Djot table captions, behind [table_captions]: a [^ text] line after a
    pipe table, its continuation lines indented.

    {v
| a | b |
|---|---|
| 1 | 2 |
^ The caption.
    v}

    The caption is inline content attached to the table, not a row: it has no
    cells and is not part of the grid. In HTML it becomes the table's
    [<caption>], which HTML requires to be the table's first child. *)

let html ?table_captions s =
  let doc = Doc.of_string ~strict:false ?table_captions s in
  print_string (Cmarkit_html.of_doc ~safe:false doc)

let commonmark ?table_captions s =
  let doc = Doc.of_string ~strict:false ?table_captions s in
  print_string (Cmarkit_commonmark.of_doc doc)

let table = "| a | b |\n|---|---|\n| 1 | 2 |\n"

let%expect_test "off: a [^] line after a table is a paragraph" =
  html (table ^ "^ The caption.\n");
  [%expect {|
    <div role="region"><table>
    <tr>
    <th>a</th>
    <th>b</th>
    </tr>
    <tr>
    <td>1</td>
    <td>2</td>
    </tr>
    </table></div><p>^ The caption.</p>
    |}]

let%expect_test "on: the caption attaches to the table" =
  html ~table_captions:true (table ^ "^ The caption.\n");
  [%expect {|
    <div role="region"><table>
    <caption>The caption.</caption>
    <tr>
    <th>a</th>
    <th>b</th>
    </tr>
    <tr>
    <td>1</td>
    <td>2</td>
    </tr>
    </table></div>
    |}]

let%expect_test "on: the caption is inline content" =
  html ~table_captions:true (table ^ "^ A *emphatic* caption.\n");
  [%expect {|
    <div role="region"><table>
    <caption>A <em>emphatic</em> caption.</caption>
    <tr>
    <th>a</th>
    <th>b</th>
    </tr>
    <tr>
    <td>1</td>
    <td>2</td>
    </tr>
    </table></div>
    |}]

let%expect_test "on: indented lines continue the caption" =
  html ~table_captions:true
    (table ^ "^ The caption,\n  continued on another line.\n");
  [%expect {|
    <div role="region"><table>
    <caption>The caption,
    continued on another line.</caption>
    <tr>
    <th>a</th>
    <th>b</th>
    </tr>
    <tr>
    <td>1</td>
    <td>2</td>
    </tr>
    </table></div>
    |}]

let%expect_test "on: an unindented line ends the table" =
  html ~table_captions:true
    (table ^ "^ The caption.\nA new paragraph.\n");
  [%expect {|
    <div role="region"><table>
    <caption>The caption.
    A new paragraph.</caption>
    <tr>
    <th>a</th>
    <th>b</th>
    </tr>
    <tr>
    <td>1</td>
    <td>2</td>
    </tr>
    </table></div>
    |}]

let%expect_test "on: the marker needs a space or the end of the line" =
  html ~table_captions:true (table ^ "^not a caption\n");
  [%expect {|
    <div role="region"><table>
    <tr>
    <th>a</th>
    <th>b</th>
    </tr>
    <tr>
    <td>1</td>
    <td>2</td>
    </tr>
    </table></div><p>^not a caption</p>
    |}]

(* A caption may sit behind indent and after a blank line. *)
let%expect_test "on: a caption attaches across a blank line, even indented" =
  html ~table_captions:true "| 1 | 2 |\n\n ^ The caption.\n";
  [%expect {|
    <div role="region"><table>
    <caption>The caption.</caption>
    <tr>
    <td>1</td>
    <td>2</td>
    </tr>
    </table></div>
    |}]

(* From djot's [regression.test]: several [^] paragraphs after a table each
   supply a caption, and the last one wins. *)
let%expect_test "on: a later caption replaces an earlier one" =
  html ~table_captions:true "| 1 | 2 |\n\n ^ cap1\n\n ^ cap2\n";
  [%expect {|
    <div role="region"><table>
    <caption>cap2</caption>
    <tr>
    <td>1</td>
    <td>2</td>
    </tr>
    </table></div>
    |}]

let%expect_test "on: a table without a caption is unchanged" =
  html ~table_captions:true table;
  [%expect {|
    <div role="region"><table>
    <tr>
    <th>a</th>
    <th>b</th>
    </tr>
    <tr>
    <td>1</td>
    <td>2</td>
    </tr>
    </table></div>
    |}]

let%expect_test "on: the caption round-trips through CommonMark" =
  commonmark ~table_captions:true (table ^ "^ The caption.\n");
  [%expect {|
    |a|b|
    |---|---|
    |1|2|
    ^ The caption.
    |}]
