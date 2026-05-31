(* Minimum requirement for a leaf generator is that it should maintain render
   roundtrip: there must exist a CommonMark witness whose parse recovers the
   inline (modulo normalization).
 *)
module P = Cmarkit_generator.Property
module G = Cmarkit_generator.Gen_inline
open Cmarkit_generator.Common_
module Inline = Cmarkit_.Inline
module Block = Cmarkit_.Block
module Meta = Cmarkit_.Meta
module Sexp = Cmarkit_.Sexp

let sexp_of_inline : Inline.t -> Sexplib0.Sexp.t = (Sexp.make_sexp_of ()).inline

let canonical (i : Inline.t) =
  Format.asprintf "%a" Sexplib0.Sexp.pp_hum
    (sexp_of_inline (Inline.normalize i))

let inline_equal a b = String.equal (canonical a) (canonical b)

(* Render an inline by wrapping it in a paragraph (there is no inline-only renderer). *)
let render i =
  Block.Paragraph (Block.Paragraph.make i, Meta.none) |> to_commonmark

(* Parse back at the inline level, bypassing block-structure parsing. *)
let roundtrip i = Cmarkit_.Inline_parse_api.of_string (render i)

let test_print i : string =
  let i' = roundtrip i in
  let metadata : metadata =
    [
      ("i", String (canonical i));
      ("cm", Md (render i));
      ("i'", String (canonical i'));
    ]
  in
  Fmt.str "%a" pp_metadata metadata

let () =
  let rand = Random.State.make [| 0 |] in
  let gen = G.gen_leaf G.Iconfig.default in
  let test =
    QCheck2.Test.make ~name:"leaf inline roundtrip" ~print:test_print gen
      (fun i -> inline_equal i (roundtrip i))
  in
  ignore @@ QCheck_base_runner.run_tests ~long:true ~colors:false ~rand [ test ]
