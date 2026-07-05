open Cmarkit_
open Cmarkit_.Sexp
open Sexplib0

let%expect_test "jsx_expr: heading with {expr}" =
  let doc = Doc.of_string ~strict:false ~jsx_expr:true "# {2 + 2}" in
  Format.printf "%a@." Sexp.pp_hum ((make_sexp_of ()).doc doc);
  [%expect {| (Heading 1 (Jsx_expr "2 + 2")) |}]

let%expect_test "jsx_expr: inline among prose, nested braces, string with brace"
    =
  let p s =
    let doc = Doc.of_string ~strict:false ~jsx_expr:true s in
    Format.printf "%a@." Sexp.pp_hum ((make_sexp_of ()).doc doc)
  in
  p "a {f x} b";
  p "r {{a = 1}} s";
  p {|t {"}"} u|};
  [%expect
    {|
    (Paragraph (Inlines (Text "a ") (Jsx_expr "f x") (Text " b")))
    (Paragraph (Inlines (Text "r ") (Jsx_expr "{a = 1}") (Text " s")))
    (Paragraph (Inlines (Text "t ") (Jsx_expr "\"}\"") (Text " u")))
    |}]

let%expect_test "jsx_expr: off by default (stays literal/attribute-free)" =
  let doc = Doc.of_string ~strict:false "x {2 + 2} y" in
  Format.printf "%a@." Sexp.pp_hum ((make_sexp_of ()).doc doc);
  [%expect {| (Paragraph (Text "x {2 + 2} y")) |}]

let%expect_test "jsx_element: self-closing components" =
  let p s =
    let doc = Doc.of_string ~strict:false ~jsx_expr:true ~jsx_element:true s in
    Format.printf "%a@." Sexp.pp_hum ((make_sexp_of ()).doc doc)
  in
  p {|<Greeting name="World" />|};
  p {|hi <Badge count={1 + 2} /> there|};
  p {|<Box a="x" b={ {r = 1} } c="y>z" />|};
  [%expect
    {|
    (Paragraph (Jsx_element "<Greeting name=\"World\" />"))
    (Paragraph
     (Inlines (Text "hi ") (Jsx_element "<Badge count={1 + 2} />")
      (Text " there")))
    (Paragraph (Jsx_element "<Box a=\"x\" b={ {r = 1} } c=\"y>z\" />"))
    |}]

let%expect_test "jsx_element: case-independent, per JSX (host elements too)" =
  let p s =
    let doc = Doc.of_string ~strict:false ~jsx_expr:true ~jsx_element:true s in
    Format.printf "%a@." Sexp.pp_hum ((make_sexp_of ()).doc doc)
  in
  (* Lowercase self-closing tags and member/namespaced names are elements, just
     as in JSX; case only matters at downstream lowering, not here. *)
  p {|an <img src="x" /> here|};
  p {|use <motion.div a={fade} /> ok|};
  p {|ns <a:b c='y' /> end|};
  [%expect
    {|
    (Paragraph
     (Inlines (Text "an ") (Jsx_element "<img src=\"x\" />") (Text " here")))
    (Paragraph
     (Inlines (Text "use ") (Jsx_element "<motion.div a={fade} />") (Text " ok")))
    (Paragraph
     (Inlines (Text "ns ") (Jsx_element "<a:b c='y' />") (Text " end")))
    |}]

let%expect_test "jsx_element: invalid elements fall through to autolink/raw HTML" =
  let p s =
    let doc = Doc.of_string ~strict:false ~jsx_expr:true ~jsx_element:true s in
    Format.printf "%a@." Sexp.pp_hum ((make_sexp_of ()).doc doc)
  in
  (* Not valid JSX elements: an autolink (invalid tag name) and an unquoted
     attribute value. The JSX branch returns nothing and the autolink / raw-HTML
     branches take over untouched. Lowercase host containers are valid JSX in
     MDX-ish mode. *)
  p {|see <https://ocaml.org> ok|};
  p {|a <div>x</div> b|};
  p {|bad <Foo a=b /> tag|};
  [%expect
    {|
    (Paragraph (Inlines (Text "see ") (Autolink https://ocaml.org) (Text " ok")))
    (Paragraph (Inlines (Text "a ") (Jsx_element <div> (Text x)) (Text " b")))
    (Paragraph (Inlines (Text "bad ") (Raw_html "<Foo a=b />") (Text " tag")))
    |}]

let%expect_test "jsx_element: inline containers" =
  let p s =
    let doc = Doc.of_string ~strict:false ~jsx_expr:true ~jsx_element:true s in
    Format.printf "%a@." Sexp.pp_hum ((make_sexp_of ()).doc doc)
  in
  p {|text <Hi>a **b**</Hi> more|};
  p {|<A><B>x</B></A>|};
  p {|nested <A><A>x</A></A> end|};
  p {|frag <>a *b*</> end|};
  p {|attrs <Card title="x" n={1}>body</Card> z|};
  [%expect {|
    (Paragraph
     (Inlines (Text "text ")
      (Jsx_element <Hi> (Inlines (Text "a ") (Strong_emphasis (Text b))))
      (Text " more")))
    (Paragraph (Jsx_element <A> (Jsx_element <B> (Text x))))
    (Paragraph
     (Inlines (Text "nested ") (Jsx_element <A> (Jsx_element <A> (Text x)))
      (Text " end")))
    (Paragraph
     (Inlines (Text "frag ")
      (Jsx_element <> (Inlines (Text "a ") (Emphasis (Text b)))) (Text " end")))
    (Paragraph
     (Inlines (Text "attrs ")
      (Jsx_element "<Card title=\"x\" n={1}>" (Text body)) (Text " z")))
    |}]

let%expect_test "jsx_element: inline container fallbacks" =
  let p s =
    let doc = Doc.of_string ~strict:false ~jsx_expr:true ~jsx_element:true s in
    Format.printf "%a@." Sexp.pp_hum ((make_sexp_of ()).doc doc)
  in
  (* Unmatched opens and stray closes do not raise; they fall back to raw HTML.
     Lowercase host containers are valid JSX in MDX-ish mode. *)
  p {|open <Comp> no close|};
  p {|host <b>bold</b> tag|};
  p {|stray </Comp> close|};
  [%expect {|
    (Paragraph (Inlines (Text "open ") (Raw_html <Comp>) (Text " no close")))
    (Paragraph
     (Inlines (Text "host ") (Jsx_element <b> (Text bold)) (Text " tag")))
    (Paragraph (Inlines (Text "stray ") (Raw_html </Comp>) (Text " close")))
    |}]

let%expect_test "jsx_element: commonmark roundtrip of inline containers" =
  let p s =
    let doc = Doc.of_string ~strict:false ~jsx_expr:true ~jsx_element:true s in
    print_string (Cmarkit_commonmark.of_doc doc)
  in
  p {|text <Hi>a **b**</Hi> more|};
  p {|<A><B>x</B></A>|};
  p {|frag <>a *b*</> end|};
  [%expect {| text <Hi>a **b**</Hi> more<A><B>x</B></A>frag <>a *b*</> end |}]

let%expect_test "jsx_block: basic block container" =
  let p s =
    let doc = Doc.of_string ~strict:false ~jsx_expr:true ~jsx_element:true s in
    Format.printf "%a@." Sexp.pp_hum ((make_sexp_of ()).doc doc)
  in
  p "<Card title=\"x\">\n\n# H\n\npara\n\n</Card>";
  [%expect {|
    (Jsx_block "<Card title=\"x\">"
     (Blocks Blank_line (Heading 1 (Text H)) Blank_line (Paragraph (Text para))
      Blank_line)
     </Card>)
    |}]

let%expect_test "jsx_block: nested and fragment" =
  let p s =
    let doc = Doc.of_string ~strict:false ~jsx_expr:true ~jsx_element:true s in
    Format.printf "%a@." Sexp.pp_hum ((make_sexp_of ()).doc doc)
  in
  p "<Outer>\n\n<Inner>\n\nx\n\n</Inner>\n\n</Outer>";
  p "<>\n\ntext\n\n</>";
  [%expect {|
    (Jsx_block <Outer>
     (Blocks Blank_line
      (Jsx_block <Inner> (Blocks Blank_line (Paragraph (Text x)) Blank_line)
       </Inner>)
      Blank_line)
     </Outer>)
    (Jsx_block <> (Blocks Blank_line (Paragraph (Text text)) Blank_line) </>)
    |}]

let%expect_test "jsx_block: host tag and unterminated block" =
  let p s =
    let doc = Doc.of_string ~strict:false ~jsx_expr:true ~jsx_element:true s in
    Format.printf "%a@." Sexp.pp_hum ((make_sexp_of ()).doc doc)
  in
  p "<div>\n\nx\n\n</div>";
  p "<Card>\n\nunclosed";
  [%expect {|
    (Jsx_block <div> (Blocks Blank_line (Paragraph (Text x)) Blank_line) </div>)
    (Jsx_block <Card> (Blocks Blank_line (Paragraph (Text unclosed))))
    |}]

let%expect_test "jsx_block: commonmark roundtrip" =
  let p s =
    let doc = Doc.of_string ~strict:false ~jsx_expr:true ~jsx_element:true s in
    print_string "----\n"; print_string (Cmarkit_commonmark.of_doc doc)
  in
  p "<Card title=\"x\">\n\n# H\n\npara\n\n</Card>";
  p "<Outer>\n\n<Inner>\n\nx\n\n</Inner>\n\n</Outer>";
  [%expect {|
    ----
    <Card title="x">

    # H

    para

    </Card>----
    <Outer>

    <Inner>

    x

    </Inner>

    </Outer>
    |}]

let%expect_test "jsx: html rendering" =
  let p s =
    let doc = Doc.of_string ~strict:false ~jsx_expr:true ~jsx_element:true s in
    print_string (Cmarkit_html.of_doc ~safe:false doc)
  in
  p {|text <Hi>a **b**</Hi> more|};
  print_string "====\n";
  p "<Card title=\"x\">\n\n# H\n\n</Card>";
  [%expect {|
    <p>text <Hi>a <strong>b</strong></Hi> more</p>
    ====
    <Card title="x">
    <h1>H</h1>
    </Card>
    |}]

let%expect_test "jsx: latex rendering" =
  let p s =
    let doc = Doc.of_string ~strict:false ~jsx_expr:true ~jsx_element:true s in
    print_string (Cmarkit_latex.of_doc doc)
  in
  p {|text <Hi>a **b**</Hi> more|};
  print_string "====\n";
  p "<Card>\n\n# H\n\n</Card>";
  [%expect {|
    text a \textbf{b} more
    ====
    \section{H}
    |}]

let%expect_test "jsx_block: edge cases" =
  let p s =
    let doc = Doc.of_string ~strict:false ~jsx_expr:true ~jsx_element:true s in
    Format.printf "%a@." Sexp.pp_hum ((make_sexp_of ()).doc doc)
  in
  (* content without blank lines is still block-level (div-style) *)
  p "<Card>\n# H\n</Card>";
  (* a component open interrupts a paragraph *)
  p "para text\n<Card>\n\nx\n\n</Card>";
  (* mismatched close does not close (kept as child raw html), block unterminated *)
  p "<Card>\n\ny\n\n</Other>";
  [%expect {|
    (Jsx_block <Card> (Heading 1 (Text H)) </Card>)
    (Blocks (Paragraph (Text "para text"))
     (Jsx_block <Card> (Blocks Blank_line (Paragraph (Text x)) Blank_line)
      </Card>))
    (Jsx_block <Card>
     (Blocks Blank_line (Paragraph (Text y)) Blank_line (Html_block </Other>)))
    |}]
