open Common_

(* Roundtrip for the djot inline extensions (symbols, smart punctuation).

   Paragraphs rather than whole blocks: the block-level generator still has an
   open counterexample of its own (see [render_roundtrip.expected], where two
   paragraphs inside nested lists merge across a blank line), which has nothing
   to do with these nodes and would only drown them out.

   Two inline typing rules earn their keep here, both of them cases where the
   parser cannot recover the split the AST intended:

   - [marked_smart_quotes]. A quote's direction is inferred from its neighbours,
     so a bare [Right_double_quote] alone in a paragraph renders a lone straight
     quote with nothing to its left, and comes back as a *left* quote. The brace
     markers state the direction outright.
   - [no_adjacent_smart_dashes]. A hyphen run is divided by its total length
     alone: [En_dash] then [Em_dash] renders five hyphens, which divide back
     em-first, flipping the order. Uniform runs are no safer — three [En_dash]
     render six hyphens, which come back as two [Em_dash].

   Symbols need no rule: [:a:] flush against [:b:] renders [:a::b:], and the
   scanner recovers both. *)

let () =
  let rand = Random.State.make [| 0 |] in
  let gen = G.gen_paragraph G.Bconfig.typed_djot_md in
  ignore
  @@ QCheck_base_runner.run_tests ~colors:false ~rand
       [
         (* Above the default 500: two dash leaves have to land flush before
            [no_adjacent_smart_dashes] is exercised at all, and that is rare. *)
         P.qcheck_test_of_t ~gen ~count:5000 ()
           (P.roundtrip_with ~emphasis_delims:[ '_' ]
              ~strong_emphasis_delims:[ '*' ] ~marked_emphasis_delims:true
              ~djot_symbols:true ~smart_punctuation:true ());
       ]
