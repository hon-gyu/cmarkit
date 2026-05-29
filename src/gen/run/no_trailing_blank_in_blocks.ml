open Common_

let () =
  let rand = Random.State.make [| 0 |] in
  ignore
  @@ QCheck_base_runner.run_tests ~colors:false ~rand
       [
         P.qcheck_test_of_t
           ?config:
             (Some
                G.
                  {
                    Config.typed_md with
                    no_trailing_blank_line_in_blocks = true;
                  })
           () T.no_trailing_blank_line_in_blocks;
       ]

let () = Printf.printf "\n\n"

let () =
  let rand = Random.State.make [| 0 |] in
  ignore
  @@ QCheck_base_runner.run_tests ~colors:false ~rand
       [
         P.qcheck_test_of_t
           ~negative:true
           ?config:
             (Some
                G.
                  {
                    Config.typed_md with
                    no_trailing_blank_line_in_blocks = false;
                  })
           () T.no_trailing_blank_line_in_blocks;
       ]
