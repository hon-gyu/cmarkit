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

let%expect_test "jsx_element: lowercase and autolink are left alone" =
  let p s =
    let doc = Doc.of_string ~strict:false ~jsx_expr:true ~jsx_element:true s in
    Format.printf "%a@." Sexp.pp_hum ((make_sexp_of ()).doc doc)
  in
  p {|see <https://ocaml.org> ok|};
  p {|a <div>x</div> b|};
  [%expect
    {|
    (Paragraph (Inlines (Text "see ") (Autolink https://ocaml.org) (Text " ok")))
    (Paragraph
     (Inlines (Text "a ") (Raw_html <div>) (Text x) (Raw_html </div>)
      (Text " b")))
    |}]
