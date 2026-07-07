open Cmarkit_
open Cmarkit_.Sexp
open Sexplib0

(* Djot-style generic span: [ [text]{attr} ]. A bracketed run that CommonMark
   cannot resolve as a link/image, immediately followed by an attribute, becomes
   an [Ext_attributes] wrapping the parsed bracket contents (brackets consumed).
   It is a *fallback*: link/image resolution keeps CommonMark precedence. *)

let html s =
  let doc = Doc.of_string ~strict:false ~djot_inline_attributes:true s in
  print_string (Cmarkit_html.of_doc ~safe:false doc)

let sexp s =
  let doc = Doc.of_string ~strict:false ~djot_inline_attributes:true s in
  Format.printf "%a@." Sexp.pp_hum ((make_sexp_of ()).doc doc)

let%expect_test "span: djot reference example" =
  html "It can be helpful to [read the manual]{.big .red}.";
  [%expect
    {| <p>It can be helpful to <span class="big red">read the manual</span>.</p> |}]

let%expect_test "span: brackets consumed, inner inlines parsed" =
  html "A plain [word]{.cls}.";
  print_string "\n";
  html "Nested [read *the* manual]{#id .a}.";
  [%expect
    {|
    <p>A plain <span class="cls">word</span>.</p>

    <p>Nested <span id="id" class="a">read <em>the</em> manual</span>.</p>
    |}]

let%expect_test "span: AST shape" =
  sexp "[word]{.cls}";
  [%expect {| (Paragraph (Attributes .cls (Text word))) |}]

let%expect_test "span: fallback -- CommonMark link resolution wins" =
  (* Undefined reference -> span. A definition elsewhere makes [foo] a shortcut
     reference link instead, and the attribute attaches to the link. This
     non-locality is inherited from CommonMark and accepted; the attribute is
     never lost. *)
  html "[foo]{.c}";
  print_string "\n";
  html "[foo]{.c}\n\n[foo]: /url";
  [%expect
    {|
    <p><span class="c">foo</span></p>

    <p><span class="c"><a href="/url">foo</a></span></p>
    |}]

let%expect_test "span: attributes attach to real links/images too" =
  html "See [this link](http://x){.big}.";
  print_string "\n";
  html "[foo][bar]{.c}\n\n[bar]: /b";
  [%expect
    {|
    <p>See <span class="big"><a href="http://x">this link</a></span>.</p>

    <p><span class="c"><a href="/b">foo</a></span></p>
    |}]

let%expect_test "span: not a span (no adjacent non-empty attribute)" =
  (* A space before '{' breaks adjacency; an empty or comment-only specifier is
     dropped and forms no span, leaving the brackets literal. *)
  html "[word] {.cls}";
  print_string "\n";
  html "[word]{}";
  print_string "\n";
  html "[word]{% just a comment %}";
  [%expect
    {|
    <p>[word] {.cls}</p>

    <p>[word]</p>

    <p>[word]</p>
    |}]

let%expect_test "span: adjacent specifiers merge" =
  html "[word]{.a}{#i}";
  [%expect {| <p><span id="i" class="a">word</span></p> |}]
