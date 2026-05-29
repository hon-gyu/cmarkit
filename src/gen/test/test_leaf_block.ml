module P = Oymarkit_generator.Property
module T = Oymarkit_generator.Typing
module G = Oymarkit_generator.Gen

let () =
  let rand = Random.State.make [| 0 |] in
  let gen = G.gen_leaf_block_ () in
  ignore
  @@ QCheck_base_runner.run_tests ~long:true ~colors:false ~rand
       [ P.qcheck_test_of_t ~gen () P.normalize_idempotent ];
