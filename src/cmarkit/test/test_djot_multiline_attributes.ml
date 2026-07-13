open Cmarkit_

(** Multi-line djot attributes.

    Djot lets an attribute specifier span lines, comments included. Both
    scanners — the inline one ([{...}] after inline content) and the block one
    (an attribute line above a block) — already feed multi-line sources through,
    so a spec broken across lines produces the same attributes as the same spec
    on one line. There is no knob and no code for this.

    These tests exist to pin that down: it costs nothing and it is the kind of
    property a later change to either scanner could quietly break. *)

let html ?djot_inline_attributes ?djot_block_attributes s =
  let doc =
    Doc.of_string ~strict:false ?djot_inline_attributes ?djot_block_attributes s
  in
  print_string (Cmarkit_html.of_doc ~safe:false doc)

let inline_attrs = html ~djot_inline_attributes:true
let block_attrs = html ~djot_block_attributes:true

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
