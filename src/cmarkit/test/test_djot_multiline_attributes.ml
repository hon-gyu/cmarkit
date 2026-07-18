open Cmarkit_

(** Multi-line djot attributes.

    Djot lets an attribute specifier span lines, comments included. Both
    scanners — the inline one ([{...}] after inline content) and the block one
    (an attribute line above a block) — already feed multi-line sources through,
    so a spec broken across lines produces the same attributes as the same spec
    on one line. There is no knob and no code for this.

    These tests exist to pin that down: it costs nothing and it is the kind of
    property a later change to either scanner could quietly break. *)

let html ?inline_attributes ?block_attributes s =
  let doc =
    Doc.of_string ~strict:false ?inline_attributes ?block_attributes s
  in
  print_string (Cmarkit_html.of_doc ~safe:false doc)

let inline_attrs = html ~inline_attributes:true
let block_attrs = html ~block_attributes:true

(* Inline attributes
   ================= *)

let%expect_test "inline: one line" =
  inline_attrs "a *word*{#id .cls key=value} b";
  [%expect {| <p>a <em id="id" class="cls" key="value">word</em> b</p> |}]

let%expect_test "inline: the same spec split over lines" =
  inline_attrs "a *word*{#id\n .cls\n key=value} b";
  [%expect {| <p>a <em id="id" class="cls" key="value">word</em> b</p> |}]

let%expect_test "inline: a comment inside a multi-line spec" =
  inline_attrs "a *word*{#id\n % a comment %\n .cls} b";
  [%expect {| <p>a <em id="id" class="cls">word</em> b</p> |}]

let%expect_test "inline: a comment may run to the closing brace" =
  inline_attrs "word{#id % a comment}";
  [%expect {| <p><span id="id">word</span></p> |}]

(* Block attributes
   ================ *)

let%expect_test "block: one line" =
  block_attrs "{#id .cls key=value}\nA paragraph.\n";
  [%expect {| <p id="id" class="cls" key="value">A paragraph.</p> |}]

let%expect_test "block: the same spec split over lines" =
  block_attrs "{#id\n .cls\n key=value}\nA paragraph.\n";
  [%expect {| <p id="id" class="cls" key="value">A paragraph.</p> |}]

let%expect_test "block: a comment inside a multi-line spec" =
  block_attrs "{#id\n % a comment %\n .cls}\nA paragraph.\n";
  [%expect {| <p id="id" class="cls">A paragraph.</p> |}]

(* Multi-line values (from djot's [attributes.test] AST-dump cases)
   =============================================================== *)

(* A quoted value that itself spans lines: the line breaks fold to single
   spaces, so the value is one string. djot's dump reads
   [attr="long value spanning multiple lines"]. *)
let%expect_test "block: a quoted value spanning lines folds to spaces" =
  block_attrs
    "{\n attr=\"long\n value\n spanning\n multiple\n lines\"\n }\n> a\n";
  [%expect {|
    <blockquote attr="long value spanning multiple lines">
    <p>a</p>
    </blockquote>
    |}]

(* Inside a block quote: the value carries an escape ([\$] -> [$]) and folds
   its own line break. djot's dump reads [key="bar a$bim"]. *)
let%expect_test "block: a multi-line value with an escape, inside a quote" =
  block_attrs "> {key=\"bar\n>    a\\$bim\"}\n> ou\n";
  [%expect {|
    <blockquote>
    <p key="bar a$bim">ou</p>
    </blockquote>
    |}]
