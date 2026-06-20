open Cmarkit_

(* Djot divs (gated behind [~div]). A div is a colon-fenced container of
   block-level content with an optional class; it closes on a colon fence at
   least as long as the opening, on a boundary, or at end of document. Divs
   nest, innermost-first. *)

let pp md =
  let doc = Doc.of_string ~div:true ~strict:false md in
  Format.printf "%a" Pp.pp_block (Doc.block doc)

let%expect_test "basic class" =
  pp "::: warning\nHere is a paragraph.\n\nAnd here is another.\n:::\n";
  [%expect {|
    Blocks
      Ext_div { class="warning" }
        Blocks
          Paragraph "Here is a paragraph."
          Blank_line
          Paragraph "And here is another."
      Blank_line
    |}]

let%expect_test "no class" =
  pp ":::\njust text\n:::\n";
  [%expect {|
    Blocks
      Ext_div { class="-" }
        Paragraph "just text"
      Blank_line
    |}]

let%expect_test "closed by end of document" =
  pp "::: note\nunclosed\n";
  [%expect {|
    Ext_div { class="note" }
      Blocks
        Paragraph "unclosed"
        Blank_line
    |}]

let%expect_test "nesting" =
  pp "::::: outer\n::: inner\ninner text\n:::\nback in outer\n:::::\n";
  [%expect {|
    Blocks
      Ext_div { class="outer" }
        Blocks
          Ext_div { class="inner" }
            Paragraph "inner text"
          Paragraph "back in outer"
      Blank_line
    |}]

let%expect_test "a longer fence closes the innermost div" =
  pp ":::: a\n::: b\ncontent\n::::\nstill in a\n";
  [%expect {|
    Ext_div { class="a" }
      Blocks
        Ext_div { class="b" }
          Paragraph "content"
        Paragraph "still in a"
        Blank_line
    |}]

let%expect_test "a too-short colon line is text, not a close" =
  pp "::: d\n::\nstill text\n:::\n";
  [%expect {|
    Blocks
      Ext_div { class="d" }
        Paragraph ":: still text"
      Blank_line
    |}]

let%expect_test "gated off: parsed as a paragraph" =
  let doc = Doc.of_string ~strict:false "::: warning\ntext\n:::\n" in
  Format.printf "%a" Pp.pp_block (Doc.block doc);
  [%expect {|
    Blocks
      Paragraph "::: warning text :::"
      Blank_line
    |}]
