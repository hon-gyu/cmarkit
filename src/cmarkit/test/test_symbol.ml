open Cmarkit_

(** Djot symbols: [:name:], where [name] is a non-empty run of ASCII
    alphanumerics, '_', '+' or '-'.

    A symbol is opaque: no inline children, never spans a line djot renders it
    literally, leaving any meaning (an emoji table, say) to a downstream filter.
    The renderers here do the same, so the interesting cases are all about what
    does not become a symbol. *)

let html s =
  let doc = Doc.of_string ~strict:false ~colon_symbols:true s in
  print_string (Cmarkit_html.of_doc ~safe:false doc)

let sexp s =
  let doc = Doc.of_string ~strict:false ~colon_symbols:true s in
  Format.printf "%a@." Sexplib0.Sexp.pp_hum ((Sexp.make_sexp_of ()).doc doc)

(* [commonmark] renders back to source: symbols must roundtrip. *)
let commonmark s =
  let doc = Doc.of_string ~strict:false ~colon_symbols:true s in
  print_string (Cmarkit_commonmark.of_doc doc)

(* What is a symbol
   ================ *)

let%expect_test "djot reference example" =
  html "My reaction is :+1: :smiley:.";
  [%expect {| <p>My reaction is :+1: :smiley:.</p> |}]

let%expect_test "the name may hold alphanumerics, _, + and -" =
  sexp ":a_b: :c-d: :e+1: :f9:";
  [%expect
    {|
    (Paragraph
     (Inlines (Symbol a_b) (Text " ") (Symbol c-d) (Text " ") (Symbol e+1)
      (Text " ") (Symbol f9)))
    |}]

(* djot's symb.test: the scan is greedy and stops at the first closing colon,
   so [:ice:scream:] is the symbol [ice] then literal [scream:], not [ice:scream]. *)
let%expect_test "the scan stops at the first closing colon" =
  sexp ":ice:scream:";
  [%expect {| (Paragraph (Inlines (Symbol ice) (Text scream:))) |}]

let%expect_test "off by default: no knob, no symbol" =
  let doc = Doc.of_string ~strict:false ":smile:" in
  print_string (Cmarkit_html.of_doc ~safe:false doc);
  [%expect {| <p>:smile:</p> |}]

(* What is not a symbol
   ==================== *)

let%expect_test "an unterminated run stays text" =
  sexp ":smile";
  [%expect {| (Paragraph (Text :smile)) |}]

let%expect_test "an empty name is not a symbol" =
  sexp "::";
  [%expect {| (Paragraph (Text ::)) |}]

let%expect_test "a space in the name stops the scan" =
  sexp ":not a symbol:";
  [%expect {| (Paragraph (Text ":not a symbol:")) |}]

let%expect_test "a symbol may not span lines" =
  sexp ":sm\nile:";
  [%expect {| (Paragraph (Inlines (Text :sm) (Break soft) (Text ile:))) |}]

let%expect_test "an escaped colon does not open a symbol" =
  html "\\:smile:";
  [%expect {| <p>:smile:</p> |}]

(* Interaction with the rest of the grammar
   ======================================== *)

let%expect_test "symbols are opaque: no emphasis inside" =
  sexp ":a_b_c:";
  [%expect {| (Paragraph (Symbol a_b_c)) |}]

let%expect_test "a symbol nests inside emphasis" =
  html "*look :+1:*";
  [%expect {| <p><em>look :+1:</em></p> |}]

let%expect_test "no symbols inside a code span" =
  html "`:smile:`";
  [%expect {| <p><code>:smile:</code></p> |}]

let%expect_test "the colon of a link destination is not a symbol" =
  html "[x](https://example.org)";
  [%expect {| <p><a href="https://example.org">x</a></p> |}]

(* A destination, a title and a reference definition are read from source spans,
   not from tokens, so a symbol-shaped run inside one stays literal rather than
   becoming a node and corrupting the link. *)
let%expect_test "a symbol-shaped destination, title or ref def stays literal" =
  html "[x](:a:)";
  html "[x](u \":a:\")";
  html "[ref]: :a:\n\n[ref]";
  [%expect
    {|
    <p><a href=":a:">x</a></p>
    <p><a href="u" title=":a:">x</a></p>
    <p><a href=":a:">ref</a></p>
    |}]

let%expect_test "a symbol in link text and in image alt text" =
  html "[:a:](u)";
  html "![alt :a:](u)";
  [%expect
    {|
    <p><a href="u">:a:</a></p>
    <p><img src="u" alt="alt :a:" ></p>
    |}]

let%expect_test "html escapes the literal rendering" =
  html ":a<b:";
  [%expect {| <p>:a&lt;b:</p> |}]

(* Roundtrip
   ========= *)

let%expect_test "commonmark renders a symbol back to its source" =
  commonmark "My reaction is :+1: :smiley:.";
  [%expect {| My reaction is :+1: :smiley:. |}]
