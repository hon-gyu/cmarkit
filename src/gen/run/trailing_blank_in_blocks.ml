open Oymarkit_generator.Property
open Oymarkit_generator.Typing

let () =
  let rand = Random.State.make [| 0 |] in
  ignore
  @@ QCheck_base_runner.run_tests ~colors:false ~rand
       [ qcheck_test_of_t () no_trailing_blank_line_in_blocks ];
