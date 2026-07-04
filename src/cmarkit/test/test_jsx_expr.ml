open Cmarkit_
open Cmarkit_.Sexp
open Sexplib0

let%expect_test "jsx_expr: heading with {expr}" =
  let doc = Doc.of_string ~strict:false ~jsx_expr:true "# {2 + 2}" in
  Format.printf "%a@." Sexp.pp_hum ((make_sexp_of ()).doc doc);
  [%expect {| (Heading 1 (Jsx_expr "2 + 2")) |}]
;;

let%expect_test "jsx_expr: inline among prose, nested braces, string with brace" =
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
;;

let%expect_test "jsx_expr: off by default (stays literal/attribute-free)" =
  let doc = Doc.of_string ~strict:false "x {2 + 2} y" in
  Format.printf "%a@." Sexp.pp_hum ((make_sexp_of ()).doc doc);
  [%expect {| (Paragraph (Text "x {2 + 2} y")) |}]
;;
