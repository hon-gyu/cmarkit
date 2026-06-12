(* Test that a block config knob does what it says both in positive and
   negative cases:
   - positive: with the knob enabled, the generator only emits AST that
     satisfies the corresponding typing rule;
   - negative: with the knob disabled, the generator can violate it (so the
     knob is doing real work and the rule is genuinely falsifiable).

   Block typing rules are [Property.t] values, so we drive these through
   {!Property.qcheck_test_of_t} (which also renders counterexamples). *)

module P = Cmarkit_generator.Property
module T = Cmarkit_generator.Typing
module G = Cmarkit_generator.Gen

let count = 1000
let gen cfg = G.mk_gen_block ~config:cfg ()
let enable f = f G.Bconfig.default

let run test =
  let rand = Random.State.make [| 0 |] in
  ignore
  @@ QCheck_base_runner.run_tests ~long:true ~colors:false ~rand [ test ]

(* no_empty_paragraph
   ------------------ *)

let () =
  run
  @@ P.qcheck_test_of_t ~count
       ~gen:(gen (enable (fun c -> { c with no_empty_paragraph = true })))
       () T.no_empty_paragraph

let () =
  run
  @@ P.qcheck_test_of_t ~count ~negative:true
       ~gen:(gen (enable (fun c -> { c with no_empty_paragraph = false })))
       () T.no_empty_paragraph

(* no_empty_blocks
   --------------- *)

let () =
  run
  @@ P.qcheck_test_of_t ~count
       ~gen:(gen (enable (fun c -> { c with no_empty_blocks = true })))
       () T.no_empty_blocks

let () =
  run
  @@ P.qcheck_test_of_t ~count ~negative:true
       ~gen:(gen (enable (fun c -> { c with no_empty_blocks = false })))
       () T.no_empty_blocks

(* no_trailing_blank_line_in_blocks
   -------------------------------- *)

let () =
  run
  @@ P.qcheck_test_of_t ~count
       ~gen:
         (gen
            (enable (fun c -> { c with no_trailing_blank_line_in_blocks = true })))
       () T.no_trailing_blank_line_in_blocks

let () =
  run
  @@ P.qcheck_test_of_t ~count ~negative:true
       ~gen:
         (gen
            (enable (fun c ->
                 { c with no_trailing_blank_line_in_blocks = false })))
       () T.no_trailing_blank_line_in_blocks

(* no_html_block_starting_paragraph
   -------------------------------- *)

let () =
  run
  @@ P.qcheck_test_of_t ~count
       ~gen:
         (gen
            (enable (fun c ->
                 { c with no_html_block_starting_paragraph = true })))
       () T.no_html_block_starting_paragraph

let () =
  run
  @@ P.qcheck_test_of_t ~count ~negative:true
       ~gen:
         (gen
            (enable (fun c ->
                 { c with no_html_block_starting_paragraph = false })))
       () T.no_html_block_starting_paragraph

(* no_ambiguous_indented_code_after_list
   ------------------------------------- *)

let () =
  run
  @@ P.qcheck_test_of_t ~count
       ~gen:
         (gen
            (enable (fun c ->
                 { c with no_ambiguous_indented_code_after_list = true })))
       () T.no_ambiguous_indented_code_after_list

let () =
  run
  @@ P.qcheck_test_of_t ~count ~negative:true
       ~gen:
         (gen
            (enable (fun c ->
                 { c with no_ambiguous_indented_code_after_list = false })))
       () T.no_ambiguous_indented_code_after_list

(* no_adjacent_block_quotes
   ------------------------ *)

let () =
  run
  @@ P.qcheck_test_of_t ~count
       ~gen:
         (gen (enable (fun c -> { c with no_adjacent_block_quotes = true })))
       () T.no_adjacent_block_quotes

let () =
  run
  @@ P.qcheck_test_of_t ~count ~negative:true
       ~gen:
         (gen (enable (fun c -> { c with no_adjacent_block_quotes = false })))
       () T.no_adjacent_block_quotes
