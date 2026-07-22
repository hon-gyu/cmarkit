open Cmarkit_

(** Paragraph interruption, behind [blocks_interrupt_paragraph].

    In CommonMark most block starts interrupt an open paragraph: a [#] line under
    a paragraph is a heading, a [```] line opens a code block, and so on. In djot
    none of them do — only a blank line ends a paragraph — so those lines are
    just more of the paragraph's text.

    This is what makes djot's thematic breaks and headings need a blank line
    above them, and it is why an unclosed [```] under a paragraph is read as a
    verbatim span rather than as a code fence.

    The knob subsumes [list_marker_interrupts_paragraph], which stays available
    for forbidding only lists. Container markers are not block starts and are
    unaffected: a block quote's [>] and a div's [:::] are matched before the
    paragraph is reached, so they still close what they close. *)

let html ?blocks_interrupt_paragraph ?div s =
  let doc = Doc.of_string ~strict:false ?blocks_interrupt_paragraph ?div s in
  print_string (Cmarkit_html.of_doc ~safe:false doc)

let%expect_test "commonmark: a heading interrupts a paragraph" =
  html "text\n# heading\n";
  [%expect {|
    <p>text</p>
    <h1>heading</h1>
    |}]

let%expect_test "off: the heading is paragraph text" =
  html ~blocks_interrupt_paragraph:false "text\n# heading\n";
  [%expect {|
    <p>text
    # heading</p>
    |}]

let%expect_test "off: a blank line is what lets the heading be a heading" =
  html ~blocks_interrupt_paragraph:false "text\n\n# heading\n";
  [%expect {|
    <p>text</p>
    <h1>heading</h1>
    |}]

let%expect_test "off: fences, breaks and quotes do not interrupt either" =
  (* The fence lines are now inline content, and there the two backtick runs pair
     up as an ordinary code span — which is why djot reads an {e unclosed} [```]
     under a paragraph as a verbatim span running to the end of the block. *)
  html ~blocks_interrupt_paragraph:false "text\n```\ncode\n```\n";
  html ~blocks_interrupt_paragraph:false "text\n***\n";
  html ~blocks_interrupt_paragraph:false "text\n> quoted\n";
  [%expect {|
    <p>text
    <code>code</code></p>
    <p>text
    ***</p>
    <p>text
    &gt; quoted</p>
    |}]

let%expect_test "off: a list marker does not interrupt either" =
  html ~blocks_interrupt_paragraph:false "text\n- item\n";
  [%expect {|
    <p>text
    - item</p>
    |}]

let%expect_test "off: a blank line still ends the paragraph" =
  html ~blocks_interrupt_paragraph:false "one\n\ntwo\n";
  [%expect {|
    <p>one</p>
    <p>two</p>
    |}]

let%expect_test "off: a div's closing fence still closes the div" =
  (* [:::] is a container marker, matched before the paragraph is reached, so it
     is not subject to the rule. *)
  html ~blocks_interrupt_paragraph:false ~div:true ":::\ntext\n:::\nafter\n";
  [%expect {|
    <div>
    <p>text</p>
    </div>
    <p>after</p>
    |}]

let%expect_test "off: a block quote's paragraph is ended by leaving the quote" =
  html ~blocks_interrupt_paragraph:false "> quoted\n\nafter\n";
  [%expect {|
    <blockquote>
    <p>quoted</p>
    </blockquote>
    <p>after</p>
    |}]
