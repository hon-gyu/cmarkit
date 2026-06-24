open Common_

(* Property-based tests for the Struct pass over keyable (colonized) blocks:
   idempotence and commutation with container blocks. *)
let () =
  let rand = Random.State.make [| 0 |] in
  let gen = P.struct_gen ~config:G.Bconfig.typed_md () in
  ignore
  @@ QCheck_base_runner.run_tests ~colors:false ~rand
       (List.map (fun p -> P.qcheck_test_of_t ~gen () p) P.struct_props)
