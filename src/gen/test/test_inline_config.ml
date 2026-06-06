(* Test that a config knob does what it says both in positive and negative
   cases:
   - positive: with the knob enabled, the generator only emits AST that
     satisfies the corresponding property;
   - negative: with the knob disabled, the generator can violate it (so the
     knob is doing real work and the property is genuinely falsifiable). *)

module GI = Cmarkit_generator.Gen_inline
module TI = Cmarkit_generator.Typing_inline
module C = Cmarkit_generator.Common_
open Cmarkit_

let count = 1000

(* Print an inline by wrapping it in a paragraph, reusing the block printer. *)
let pp_inline (i : Inline.t) : string =
  let b = Block.Paragraph (Block.Paragraph.make i, Meta.none) in
  Format.asprintf "%a" C.pp_block b

let gen cfg = GI.gen_inline cfg

let run test =
  let rand = Random.State.make [| 0 |] in
  ignore
  @@ QCheck_base_runner.run_tests ~long:true ~colors:false ~rand [ test ]

(* no_nested_link
   -------------- *)

let () =
  run
  @@ QCheck2.Test.make ~name:"no_nested_link enabled ==> no link nested in a link"
       ~count ~print:pp_inline
       (gen { GI.Iconfig.default with no_nested_link = true })
       TI.no_nested_link

let () =
  run
  @@ QCheck2.Test.make_neg
       ~name:"no_nested_link disabled ==> nested links occur" ~count
       ~print:pp_inline
       (gen { GI.Iconfig.default with no_nested_link = false })
       TI.no_nested_link

(* no_empty_inlines
   ---------------- *)

let () =
  run
  @@ QCheck2.Test.make ~name:"no_empty_inlines enabled ==> inline is non-empty"
       ~count ~print:pp_inline
       (gen { GI.Iconfig.default with no_empty_inlines = true })
       (fun i -> not (Inline.is_empty i))

let () =
  run
  @@ QCheck2.Test.make_neg
       ~name:"no_empty_inlines disabled ==> empty inlines occur" ~count
       ~print:pp_inline
       (gen { GI.Iconfig.default with no_empty_inlines = false })
       (fun i -> not (Inline.is_empty i))

(* no_empty_emphasis
   ----------------- *)

let () =
  run
  @@ QCheck2.Test.make
       ~name:"no_empty_emphasis enabled ==> emphasis payloads are non-empty"
       ~count ~print:pp_inline
       (gen { GI.Iconfig.default with no_empty_emphasis = true })
       TI.no_empty_emphasis

let () =
  run
  @@ QCheck2.Test.make_neg
       ~name:"no_empty_emphasis disabled ==> empty emphasis occurs" ~count
       ~print:pp_inline
       (gen { GI.Iconfig.default with no_empty_emphasis = false })
       TI.no_empty_emphasis

(* no_adjacent_code_spans
   ---------------------- *)

(* Code spans are sparse under the default weights, so bias the distribution
   towards them to give adjacency a chance to appear (positive case) and to be
   reliably falsified (negative case). *)
let code_span_heavy = { GI.Iconfig.default with w_code_span = 8; w_inlines = 4 }

let () =
  run
  @@ QCheck2.Test.make
       ~name:"no_adjacent_code_spans enabled ==> no two code spans are adjacent"
       ~count ~print:pp_inline
       (gen { code_span_heavy with no_adjacent_code_spans = true })
       TI.no_adjacent_code_spans

let () =
  run
  @@ QCheck2.Test.make_neg
       ~name:"no_adjacent_code_spans disabled ==> adjacent code spans occur"
       ~count ~print:pp_inline
       (gen { code_span_heavy with no_adjacent_code_spans = false })
       TI.no_adjacent_code_spans
