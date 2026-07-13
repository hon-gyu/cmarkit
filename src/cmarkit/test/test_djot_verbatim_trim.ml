open Cmarkit_

(** Djot's verbatim trimming rule, behind [djot_verbatim_trim].

    CommonMark strips one padding space from {e each} end of a code span
    whenever both are present and the content is not all spaces. That rule
    exists so that a span can hold content starting or ending with a backtick
    ([ `` ` `` ]), but it also eats the spaces of [ ` a ` ].

    Djot strips a space only where it is doing that work — only where the space
    is what lets the content start or end with a backtick — and the two sides are
    independent. So [ ` a ` ] keeps its spaces. *)

let html ?djot_verbatim_trim s =
  let doc = Doc.of_string ~strict:false ?djot_verbatim_trim s in
  print_string (Cmarkit_html.of_doc ~safe:false doc)

let%expect_test "commonmark: both padding spaces are stripped" =
  html "a ` x ` b";
  [%expect {| <p>a <code>x</code> b</p> |}]

let%expect_test "djot: padding spaces that do no work are kept" =
  html ~djot_verbatim_trim:true "a ` x ` b";
  [%expect {| <p>a <code> x </code> b</p> |}]

let%expect_test "both: a space lets the content start with a backtick" =
  html "a `` ` `` b";
  html ~djot_verbatim_trim:true "a `` ` `` b";
  [%expect {|
    <p>a <code>`</code> b</p>
    <p>a <code>`</code> b</p>
    |}]

let%expect_test "djot: the two sides are independent" =
  (* The leading space is stripped (it lets the content start with a backtick),
     the trailing one is not (nothing needs it). *)
  html ~djot_verbatim_trim:true "a `` `x `` b";
  [%expect {| <p>a <code>`x </code> b</p> |}]

let%expect_test "both: no padding, nothing to strip" =
  html "a `x` b";
  html ~djot_verbatim_trim:true "a `x` b";
  [%expect {|
    <p>a <code>x</code> b</p>
    <p>a <code>x</code> b</p>
    |}]

let%expect_test "both: an all-spaces span" =
  html "a `  ` b";
  html ~djot_verbatim_trim:true "a `  ` b";
  [%expect {|
    <p>a <code>  </code> b</p>
    <p>a <code>  </code> b</p>
    |}]
