open Cmarkit_

(** Djot's math spelling, behind [djot_math]: a verbatim span prefixed with [$]
    for inline math and [$$] for display math.

    {v
Inline $`e=mc^2` and display $$`\int_0^1 x`.
    v}

    It produces the same {!Cmarkit.Inline.Ext_math_span} as the pandoc [$...$]
    spelling, which stays behind the math extension ([~strict:false]). The two
    are different surface syntax for one node and can coexist. *)

let html ?djot_math s =
  let doc = Doc.of_string ~strict:false ?djot_math s in
  print_string (Cmarkit_html.of_doc ~safe:false doc)

let commonmark ?djot_math s =
  let doc = Doc.of_string ~strict:false ?djot_math s in
  print_string (Cmarkit_commonmark.of_doc doc)

let%expect_test "off: a dollar before a verbatim span is text" =
  html "a $`e=mc^2` b";
  [%expect {| <p>a $<code>e=mc^2</code> b</p> |}]

let%expect_test "on: [$`x`] is inline math" =
  html ~djot_math:true "a $`e=mc^2` b";
  [%expect {| <p>a \(e=mc^2\) b</p> |}]

let%expect_test "on: [$$`x`] is display math" =
  html ~djot_math:true "a $$`e=mc^2` b";
  [%expect {| <p>a \[e=mc^2\] b</p> |}]

let%expect_test "on: a bare verbatim span is still a code span" =
  html ~djot_math:true "a `e=mc^2` b";
  [%expect {| <p>a <code>e=mc^2</code> b</p> |}]

let%expect_test "on: an escaped dollar is not a math prefix" =
  html ~djot_math:true "a \\$`e=mc^2` b";
  [%expect {| <p>a $<code>e=mc^2</code> b</p> |}]

let%expect_test "on: an even backslash run leaves a live math prefix" =
  html ~djot_math:true "\\\\$`e=mc^2`";
  [%expect {| <p>\\(e=mc^2\)</p> |}]

let%expect_test "on: an escaped dollar can precede a live math prefix" =
  html ~djot_math:true "\\$$`a`";
  [%expect {| <p>$\(a\)</p> |}]

let%expect_test "on: dollars inside djot math stay verbatim" =
  html ~djot_math:true "$`e=\\text{the number $\\pi$}`";
  [%expect {| <p>\(e=\text{the number $\pi$}\)</p> |}]

let%expect_test "on: the pandoc spelling still works alongside it" =
  html ~djot_math:true "pandoc $e=mc^2$ and djot $`e=mc^2`";
  [%expect {| <p>pandoc \(e=mc^2\) and djot \(e=mc^2\)</p> |}]

let%expect_test "on: math round-trips through CommonMark" =
  (* The renderer writes the pandoc spelling, which is the one it has always
     written for this node; both spellings parse back to the same AST. *)
  commonmark ~djot_math:true "a $`e=mc^2` b";
  [%expect {| a $e=mc^2$ b |}]
