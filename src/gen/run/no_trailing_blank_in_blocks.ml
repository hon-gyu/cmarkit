open Common_

let gen = G.(mk_gen_block ~config:Config.typed_md ())

let () =
  let rand = Random.State.make [| 0 |] in
  ignore
  @@ QCheck_base_runner.run_tests ~colors:false ~rand
       [
         P.qcheck_test_of_t
           ~gen
           () T.no_trailing_blank_line_in_blocks;
       ]

let () = Printf.printf "\n\n"

let () =
  let rand = Random.State.make [| 0 |] in
  ignore
  @@ QCheck_base_runner.run_tests ~colors:false ~rand
       [
         P.qcheck_test_of_t
           ~gen
           ~negative:true
           () T.no_trailing_blank_line_in_blocks;
       ]
