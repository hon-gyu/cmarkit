open Cmarkit_

(** Djot ordered list numbering, behind [extended_ordered_list_styles]: lower/upper
    alpha ([a.]), lower/upper roman ([iv.]) and the fully parenthesized
    delimiter ([(a)]), on top of CommonMark's decimal.

    CommonMark's [1.] and [1)] keep producing [`Ordered], so a document parsed
    without the knob keeps exactly the AST it had; only what djot adds goes
    through [`Ext_ordered].

    Two djot rules are pinned here:

    - a style change starts a new list;
    - an ambiguous marker resolves by context. [i.] is alpha 9 or roman 1; an
      unresolved lone/repeated marker defaults to roman, while a following
      unambiguous alpha marker settles it as alpha. *)

let html ?extended_ordered_list_styles s =
  let doc = Doc.of_string ~strict:false ?extended_ordered_list_styles s in
  print_string (Cmarkit_html.of_doc ~safe:false doc)

let commonmark ?extended_ordered_list_styles s =
  let doc = Doc.of_string ~strict:false ?extended_ordered_list_styles s in
  print_string (Cmarkit_commonmark.of_doc doc)

let pp ?extended_ordered_list_styles s =
  let doc = Doc.of_string ~strict:false ?extended_ordered_list_styles s in
  Format.printf "%a@." Pp.pp_block (Doc.block doc)

(* Styles
   ====== *)

let%expect_test "off: an alpha marker is just a paragraph" =
  html "a. one\nb. two\n";
  [%expect {|
    <p>a. one
    b. two</p>
    |}]

let%expect_test "on: lower and upper alpha" =
  html ~extended_ordered_list_styles:true "a. one\nb. two\n";
  html ~extended_ordered_list_styles:true "A) one\nB) two\n";
  [%expect {|
    <ol type="a">
    <li>one</li>
    <li>two</li>
    </ol>
    <ol type="A">
    <li>one</li>
    <li>two</li>
    </ol>
    |}]

let%expect_test "on: roman needs more than one letter to be unambiguous" =
  html ~extended_ordered_list_styles:true "iv. four\nv. five\n";
  html ~extended_ordered_list_styles:true "IX. nine\n";
  [%expect {|
    <ol type="i" start="4">
    <li>four</li>
    <li>five</li>
    </ol>
    <ol type="I" start="9">
    <li>nine</li>
    </ol>
    |}]

let%expect_test "on: decimal still opens an ordinary CommonMark list" =
  html ~extended_ordered_list_styles:true "1. one\n2. two\n";
  [%expect {|
    <ol>
    <li>one</li>
    <li>two</li>
    </ol>
    |}]

let%expect_test "on: the parenthesized delimiter, in any style" =
  html ~extended_ordered_list_styles:true "(a) one\n(b) two\n";
  html ~extended_ordered_list_styles:true "(1) one\n(2) two\n";
  [%expect {|
    <ol type="a">
    <li>one</li>
    <li>two</li>
    </ol>
    <ol>
    <li>one</li>
    <li>two</li>
    </ol>
    |}]

let%expect_test "on: a word that is not a marker stays a paragraph" =
  html ~extended_ordered_list_styles:true "hello. not a list\n";
  html ~extended_ordered_list_styles:true "ab. two letters is not alpha\n";
  [%expect {|
    <p>hello. not a list</p>
    <p>ab. two letters is not alpha</p>
    |}]

(* A style change starts a new list
   ================================ *)

let%expect_test "on: alpha then roman are two lists" =
  html ~extended_ordered_list_styles:true "a. one\nb. two\niv. four\n";
  [%expect {|
    <ol type="a">
    <li>one</li>
    <li>two</li>
    </ol>
    <ol type="i" start="4">
    <li>four</li>
    </ol>
    |}]

let%expect_test "on: the delimiter is part of the style" =
  html ~extended_ordered_list_styles:true "a. one\nb) two\n";
  [%expect {|
    <ol type="a">
    <li>one</li>
    </ol>
    <ol type="a" start="2">
    <li>two</li>
    </ol>
    |}]

(* Ambiguous markers resolve by context
   ==================================== *)

let%expect_test "on: unresolved [i.] markers default to roman" =
  html ~extended_ordered_list_styles:true "i. item\n";
  html ~extended_ordered_list_styles:true "i. one\ni. two\n";
  [%expect {|
    <ol type="i">
    <li>item</li>
    </ol>
    <ol type="i">
    <li>one</li>
    <li>two</li>
    </ol>
    |}]

let%expect_test "on: [i.] continues a roman list as roman" =
  (* [iv.] settles the list as roman, so the following [v.] and [x.] are roman
     too rather than starting alpha lists. *)
  html ~extended_ordered_list_styles:true "iv. four\nv. five\nx. ten\n";
  [%expect {|
    <ol type="i" start="4">
    <li>four</li>
    <li>five</li>
    <li>ten</li>
    </ol>
    |}]

let%expect_test "on: [b.] continues an alpha list as alpha" =
  html ~extended_ordered_list_styles:true "a. one\nb. two\nc. three\n";
  [%expect {|
    <ol type="a">
    <li>one</li>
    <li>two</li>
    <li>three</li>
    </ol>
    |}]

(* AST and round-trip
   ================== *)

let%expect_test "on: the AST records style, delimiter and start" =
  pp ~extended_ordered_list_styles:true "(iv) four\n";
  [%expect {|
    Blocks
      List { ordered "(iv)"; tight }
        - item
          Paragraph "four"
      Blank_line
    |}]

let%expect_test "on: markers round-trip through CommonMark" =
  commonmark ~extended_ordered_list_styles:true "a. one\nb. two\n";
  commonmark ~extended_ordered_list_styles:true "(iv) four\n(v) five\n";
  [%expect {|
    a. one
    b. two
    (iv) four
    (v) five
    |}]
