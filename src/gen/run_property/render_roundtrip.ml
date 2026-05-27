open Oymarkit_generator.Property

let%expect_test _ =
  let rand = Random.State.make [| 0 |] in
  ignore
  @@ QCheck_base_runner.run_tests ~colors:false ~rand
       [ qcheck_test_of_t roundtrip ];
  [%expect
    {|
    --- Failure --------------------------------------------------------------------

    Test roundtrip failed (3 shrink steps):

    { block: Blocks
               Html_block { lines=1 }
               Blank_line
    ; metadata: { cm: ┌─────┐
                      │<div>│
                      └─────┘
                ; cm': ┌─────┐
                       │<div>│
                       └─────┘
                ; expect: Html_block { lines=1 }
                }
    }
    ================================================================================
    failure (1 tests failed, 0 tests errored, ran 1 tests)
    |}]
