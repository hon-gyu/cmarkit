open Cmarkit_

(** Djot's rules for reading a verbatim span's content, behind [djot_verbatim].

    Three divergences, all decided at parse time because every renderer that
    reads the content must read it the same way.

    {b Padding.} CommonMark strips one padding space from {e each} end of a code
    span whenever both are present and the content is not all spaces. That rule
    exists so that a span can hold content starting or ending with a backtick
    ([ `` ` `` ]), but it also eats the spaces of [ ` a ` ]. Djot strips a space
    only where it is doing that work — only where the space is what lets the
    content start or end with a backtick — and the two sides are independent. So
    [ ` a ` ] keeps its spaces.

    {b Line endings.} CommonMark turns a line ending inside a span into a space;
    djot keeps the newline.

    {b No closing run.} In djot an opening backtick run always opens a span: with
    no closing run of the same length the span runs to the end of the block.
    CommonMark leaves such a run as literal text. *)

let html ?djot_verbatim s =
  let doc = Doc.of_string ~strict:false ?djot_verbatim s in
  print_string (Cmarkit_html.of_doc ~safe:false doc)

let%expect_test "commonmark: both padding spaces are stripped" =
  html "a ` x ` b";
  [%expect {| <p>a <code>x</code> b</p> |}]

let%expect_test "djot: padding spaces that do no work are kept" =
  html ~djot_verbatim:true "a ` x ` b";
  [%expect {| <p>a <code> x </code> b</p> |}]

let%expect_test "both: a space lets the content start with a backtick" =
  html "a `` ` `` b";
  html ~djot_verbatim:true "a `` ` `` b";
  [%expect {|
    <p>a <code>`</code> b</p>
    <p>a <code>`</code> b</p>
    |}]

let%expect_test "djot: the two sides are independent" =
  (* The leading space is stripped (it lets the content start with a backtick),
     the trailing one is not (nothing needs it). *)
  html ~djot_verbatim:true "a `` `x `` b";
  [%expect {| <p>a <code>`x </code> b</p> |}]

let%expect_test "both: no padding, nothing to strip" =
  html "a `x` b";
  html ~djot_verbatim:true "a `x` b";
  [%expect {|
    <p>a <code>x</code> b</p>
    <p>a <code>x</code> b</p>
    |}]

let%expect_test "both: an all-spaces span" =
  html "a `  ` b";
  html ~djot_verbatim:true "a `  ` b";
  [%expect {|
    <p>a <code>  </code> b</p>
    <p>a <code>  </code> b</p>
    |}]

let%expect_test "line ending: a space in CommonMark, a newline in djot" =
  html "Some `code\nwith a break`";
  html ~djot_verbatim:true "Some `code\nwith a break`";
  [%expect {|
    <p>Some <code>code with a break</code></p>
    <p>Some <code>code
    with a break</code></p>
    |}]

let%expect_test "no closing run: literal in CommonMark, runs to end in djot" =
  html "a `b c";
  html ~djot_verbatim:true "a `b c";
  [%expect {|
    <p>a `b c</p>
    <p>a <code>b c</code></p>
    |}]

let%expect_test "djot: an unclosed span runs to the end of the block, not the line" =
  html ~djot_verbatim:true "` a\nc";
  [%expect {|
    <p><code> a
    c</code></p>
    |}]

let%expect_test "djot: a trailing run opens a span with no content" =
  html ~djot_verbatim:true "a `";
  [%expect {| <p>a <code></code></p> |}]

let%expect_test "both: a blank line still ends the block, and so the span" =
  html ~djot_verbatim:true "a `b\n\nc";
  [%expect {|
    <p>a <code>b</code></p>
    <p>c</p>
    |}]
