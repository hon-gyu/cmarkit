open Oymarkit_generator.Property

let () =
  let rand = Random.State.make [| 0 |] in
  ignore
  @@ QCheck_base_runner.run_tests ~colors:false ~rand
       [ qcheck_test_of_t () normalize_idempotent ];
