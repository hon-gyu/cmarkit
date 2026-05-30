open Common_

let () =
  let rand = Random.State.make [| 0 |] in
  let gen = G.(mk_gen_block ~config:Bconfig.typed_md ()) in
  ignore
  @@ QCheck_base_runner.run_tests ~colors:false ~rand
       [ P.qcheck_test_of_t ~gen () P.normalize_idempotent ];
