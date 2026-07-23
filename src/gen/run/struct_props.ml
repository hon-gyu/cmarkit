open Common_
module P_ext = Cmarkit_generator.Property_ext

(* Property-based tests for the Struct pass over keyable (colonized) blocks:
   idempotence and commutation with container blocks. *)
let () =
  let rand = Random.State.make [| 0 |] in
  let gen = P_ext.struct_gen ~config:G.Bconfig.typed_md () in
  ignore
  @@ QCheck_base_runner.run_tests ~colors:false ~rand
       (List.map (fun p -> P.qcheck_test_of_t ~gen () p) P_ext.struct_props)
