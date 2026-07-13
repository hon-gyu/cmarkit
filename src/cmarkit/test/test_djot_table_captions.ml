open Cmarkit_

(** Djot table captions, behind [djot_table_captions]: a [^ text] line after a
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

let html ?djot_table_captions s =
  let doc = Doc.of_string ~strict:false ?djot_table_captions s in
  print_string (Cmarkit_html.of_doc ~safe:false doc)

let commonmark ?djot_table_captions s =
  let doc = Doc.of_string ~strict:false ?djot_table_captions s in
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
  html ~djot_table_captions:true (table ^ "^ The caption.\n");
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
  html ~djot_table_captions:true (table ^ "^ A *emphatic* caption.\n");
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
  html ~djot_table_captions:true
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
  html ~djot_table_captions:true
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
  html ~djot_table_captions:true (table ^ "^not a caption\n");
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

let%expect_test "on: a table without a caption is unchanged" =
  html ~djot_table_captions:true table;
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
  commonmark ~djot_table_captions:true (table ^ "^ The caption.\n");
  [%expect {|
    |a|b|
    |---|---|
    |1|2|
    ^ The caption.
    |}]
