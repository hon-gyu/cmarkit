(* Minimum requirement for a leaf generator is that
   it should maintain render roundtrip.
*)
module P = Cmarkit_generator.Property
module T = Cmarkit_generator.Typing
module G = Cmarkit_generator.Gen

let () =
  let rand = Random.State.make [| 0 |] in
  let gen = G.gen_leaf_block_ () in
  ignore
  @@ QCheck_base_runner.run_tests ~long:true ~colors:false ~rand
       [
         P.qcheck_test_of_t ~gen ()
           (P.roundtrip_with ~emphasis_delims:[ '_' ]
              ~strong_emphasis_delims:[ '*' ] ());
       ];
