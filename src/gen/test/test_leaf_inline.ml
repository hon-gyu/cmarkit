(* Minimum requirement for a leaf generator is that it should maintain render
   roundtrip: there must exist a CommonMark witness whose parse recovers the
   inline (modulo normalization).
 *)
module P = Oymarkit_generator.Property
module G = Oymarkit_generator.Gen_inline
module Inline = Oymarkit_.Inline
module Block = Oymarkit_.Block
module Meta = Oymarkit_.Meta
module Sexp = Oymarkit_.Sexp
module Inline_parse = Oymarkit_.Inline_parse

let sexp_of_inline : Inline.t -> Sexplib0.Sexp.t =
  (Sexp.make_sexp_of ()).inline

let canonical (i : Inline.t) =
  Format.asprintf "%a" Sexplib0.Sexp.pp_hum
    (sexp_of_inline (Inline.normalize i))

let inline_equal a b = String.equal (canonical a) (canonical b)

(* Render an inline by wrapping it in a paragraph (there is no inline-only renderer). *)
let render i =
  Block.Paragraph (Block.Paragraph.make i, Meta.none) |> P.to_commonmark

(* Parse back at the inline level, bypassing block-structure parsing. *)
let roundtrip i = Inline_parse.of_string (render i)

let test_print i : string =
  let i' = roundtrip i in
  let metadata : P.metadata =
    [
      ("i", P.String (canonical i));
      ("cm", P.Md (render i));
      ("i'", P.String (canonical i'));
    ]
  in
  Fmt.str "%a" P.pp_metadata metadata

let () =
  let rand = Random.State.make [| 0 |] in
  let gen = G.gen_leaf G.Iconfig.default in
  let test =
    QCheck2.Test.make ~name:"leaf inline roundtrip" ~print:test_print gen
      (fun i -> inline_equal i (roundtrip i))
  in
  ignore @@ QCheck_base_runner.run_tests ~long:true ~colors:false ~rand [ test ]
