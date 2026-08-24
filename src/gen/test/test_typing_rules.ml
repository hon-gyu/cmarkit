(** Tests for typing rules

    To make sure that our typing rule does not introduce unrealistic restrictive
    behaviors, we should make sure that it does not forbid any valid input,
    i.e., we need PBT that generates raw string and parse it to AST, and our
    rule should not reject it. If it's rejected, either there's a bug in our
    rule or the parser is buggy.

    Here we will use gen.ml with config that turns off typing rule constraints
    to generate AST and re-parse to ensure we get AST that is emitted by the
    parser *)

module P = Cmarkit_generator.Property
module R = Cmarkit_generator.Rules
module G = Cmarkit_generator.Gen
open Cmarkit_generator.Common_

let gen =
  let open QCheck2.Gen in
  let+ b = G.mk_gen_block ~config:G.Bconfig.default () in
  reparse ~marked_emphasis_delims:true b

let () =
  let rand = Random.State.make [| 0 |] in
  ignore
  @@ QCheck_base_runner.run_tests ~long:true ~colors:false ~rand
       [ P.qcheck_test_of_t ~gen () R.typed ]
