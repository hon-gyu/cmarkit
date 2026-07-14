(* CR: why can't this file be integrated with existing test_inline_config.ml file?
   djot extentions should not be nothing special

   the same applies to ../run/djot_inline_roundtrip
*)
(** Tests for the djot inline typing rules.

    Each rule is asserted in both directions, which is what makes it a rule
    rather than a guess:
    - with every rule on, the roundtrip property {b holds};
    - with any one rule off, it {b fails} — expressed as a [~negative] test, so
      this file goes red if a rule ever stops being necessary (the parser grew a
      witness, say) just as loudly as if one stops being sufficient.

    Paragraphs rather than whole blocks: the block generator has an open
    counterexample of its own (see [run/render_roundtrip.expected] — two
    paragraphs merging across a blank line inside nested lists) that has nothing
    to do with these nodes and would mask them.

    The rules, and the counterexample each one rules out:
    - [no_bare_smart_quotes]: a quote is a delimiter, not a character. The
      parser pairs openers with closers, and a bare quote node is only what is
      left when a quote fails to pair — so bare quotes are not compositionally
      placeable. Two in a paragraph pair up and come back as one [Ext_quoted],
      and a lone [Right_double_quote] renders a straight quote with nothing to
      pair with, which falls back to a {e left} one. Quoted spans are generated
      as [Ext_quoted] containers instead.
    - [no_adjacent_smart_dashes]: a hyphen run is divided by its total length
      alone, so the split the AST intended is unrecoverable. [En_dash] then
      [Em_dash] renders five hyphens, which divide back em-first: the order
      flips. Uniform runs are no safer — three [En_dash] render six hyphens,
      which come back as two [Em_dash].
    - [no_thematic_break_shaped_paragraph]: [Paragraph (Em_dash)] renders [---]
      and comes back a [Thematic_break]. Block structure wins over inline, and
      there is no escape.

    Symbols need no rule: [:a:] flush against [:b:] renders [:a::b:], and the
    scanner recovers both.

    The counts are above the default 500 because two dash leaves have to land
    flush before [no_adjacent_smart_dashes] is exercised at all, and that is
    rare. *)

module P = Cmarkit_generator.Property
module G = Cmarkit_generator.Gen
module Iconfig = Cmarkit_generator.Gen_inline.Iconfig

let count = 5000

let roundtrip =
  P.roundtrip_with ~emphasis_delims:[ '_' ] ~strong_emphasis_delims:[ '*' ]
    ~marked_emphasis_delims:true ~djot_symbols:true ~smart_punctuation:true ()

let test ~negative name (config : G.Bconfig.t) =
  let gen = G.gen_paragraph config in
  P.qcheck_test_of_t ~gen ~count ~negative () { roundtrip with P.name }

let with_inline (config : G.Bconfig.t) f =
  { config with G.Bconfig.inline = f config.G.Bconfig.inline }

let () =
  let base = G.Bconfig.typed_djot_md in
  let rand = Random.State.make [| 0 |] in
  ignore
  @@ QCheck_base_runner.run_tests ~colors:false ~rand
       [
         test ~negative:false "all rules on: roundtrip holds" base;
         test ~negative:true "no_bare_smart_quotes off: roundtrip breaks"
           (with_inline base (fun ic ->
                { ic with Iconfig.no_bare_smart_quotes = false }));
         test ~negative:true "no_adjacent_smart_dashes off: roundtrip breaks"
           (with_inline base (fun ic ->
                { ic with Iconfig.no_adjacent_smart_dashes = false }));
         test ~negative:true
           "no_thematic_break_shaped_paragraph off: roundtrip breaks"
           { base with G.Bconfig.no_thematic_break_shaped_paragraph = false };
       ]
