open Cmarkit_

(** Djot heading semantics, behind [djot_headings]. Two changes:

    - a heading runs until a blank line, so the lines after the [#] line
      continue its inline content, whether or not they repeat the [#] (which is
      stripped when they do);
    - a heading is an implicit link reference target, so [ [Some Heading][] ]
      links to it. That one needs [heading_auto_ids], which is what computes the
      id the link points at.

    The reference may sit above the heading it points at, so heading labels are
    registered in a pass over the block structure before any inline content is
    parsed. An explicit link reference definition of the same label wins. *)

let html ?djot_headings ?(heading_auto_ids = true) s =
  let doc = Doc.of_string ~strict:false ~heading_auto_ids ?djot_headings s in
  print_string (Cmarkit_html.of_doc ~safe:false doc)

(* Continuation lines
   ================== *)

let%expect_test "commonmark: a heading is exactly one line" =
  html "# A heading that\ntakes up two lines\n";
  [%expect {|
    <h1 id="a-heading-that"><a class="anchor" aria-hidden="true" href="#a-heading-that"></a>A heading that</h1>
    <p>takes up two lines</p>
    |}]

let%expect_test "djot: the heading runs until a blank line" =
  html ~djot_headings:true "# A heading that\ntakes up two lines\n";
  [%expect {|
    <h1 id="a-heading-that-takes-up-two-lines"><a class="anchor" aria-hidden="true" href="#a-heading-that-takes-up-two-lines"></a>A heading that
    takes up two lines</h1>
    |}]

let%expect_test "djot: continuation lines may repeat the [#]" =
  html ~djot_headings:true "# A heading that\n# takes up two lines\n";
  [%expect {|
    <h1 id="a-heading-that-takes-up-two-lines"><a class="anchor" aria-hidden="true" href="#a-heading-that-takes-up-two-lines"></a>A heading that
    takes up two lines</h1>
    |}]

let%expect_test "djot: a blank line ends the heading" =
  html ~djot_headings:true "# A heading\n\nA paragraph.\n";
  [%expect {|
    <h1 id="a-heading"><a class="anchor" aria-hidden="true" href="#a-heading"></a>A heading</h1>
    <p>A paragraph.</p>
    |}]

(* Headings as link reference targets
   ================================== *)

let%expect_test "commonmark: a heading defines no label" =
  html "# Some Heading\n\nSee [Some Heading][].\n";
  [%expect {|
    <h1 id="some-heading"><a class="anchor" aria-hidden="true" href="#some-heading"></a>Some Heading</h1>
    <p>See [Some Heading][].</p>
    |}]

let%expect_test "djot: a heading is an implicit link target" =
  html ~djot_headings:true "# Some Heading\n\nSee [Some Heading][].\n";
  [%expect {|
    <h1 id="some-heading"><a class="anchor" aria-hidden="true" href="#some-heading"></a>Some Heading</h1>
    <p>See <a href="#some-heading">Some Heading</a>.</p>
    |}]

let%expect_test "djot: the reference may come before the heading" =
  html ~djot_headings:true "See [Some Heading][].\n\n# Some Heading\n";
  [%expect {|
    <p>See <a href="#some-heading">Some Heading</a>.</p>
    <h1 id="some-heading"><a class="anchor" aria-hidden="true" href="#some-heading"></a>Some Heading</h1>
    |}]

let%expect_test "djot: the label matches case-insensitively, like any label" =
  html ~djot_headings:true "# Some Heading\n\nSee [some heading][].\n";
  [%expect {|
    <h1 id="some-heading"><a class="anchor" aria-hidden="true" href="#some-heading"></a>Some Heading</h1>
    <p>See <a href="#some-heading">some heading</a>.</p>
    |}]

let%expect_test "djot: an explicit definition of the same label wins" =
  html ~djot_headings:true
    "[Some Heading]: https://example.org\n\n# Some Heading\n\nSee [Some Heading][].\n";
  [%expect {|
    <h1 id="some-heading"><a class="anchor" aria-hidden="true" href="#some-heading"></a>Some Heading</h1>
    <p>See <a href="https://example.org">Some Heading</a>.</p>
    |}]

let%expect_test "djot: an unrelated reference is still undefined" =
  html ~djot_headings:true "# Some Heading\n\nSee [Other][].\n";
  [%expect {|
    <h1 id="some-heading"><a class="anchor" aria-hidden="true" href="#some-heading"></a>Some Heading</h1>
    <p>See [Other][].</p>
    |}]
