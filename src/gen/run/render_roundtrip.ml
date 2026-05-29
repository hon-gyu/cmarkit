open Common_

let () =
  let rand = Random.State.make [| 0 |] in
  ignore
  @@ QCheck_base_runner.run_tests ~colors:false ~rand
       [ P.qcheck_test_of_t ~config:G.Config.typed_md () P.roundtrip ]
