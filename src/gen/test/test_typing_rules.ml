(** Tests for typing rules

    To make sure that our typing rule does not introduce unrealistic restrictive
    behaviors, we should make sure that it does not forbid any valid input,
    i.e., we need PBT that generates raw string and parse it to AST, and our
    rule should not reject it. If it's rejected, either there's a bug in our
    rule or the parser is buggy.

    Here we will use gen.ml with config that turns off typing rule constraints
    *)

module P = Cmarkit_generator.Property
module T = Cmarkit_generator.Typing
module G = Cmarkit_generator.Gen

let gen = G.mk_gen_block ~config:G.Bconfig.empty ()


(* let property : P.t =
  let name = "Typing rule should not reject valid input" in
  let check = (fun b ->
    P.to_commonmark
  ) *)
