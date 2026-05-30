(* Minimum requirement for a leaf generator is that it should maintain render
   roundtrip: there must exist a CommonMark witness whose parse recovers the
   inline (modulo normalization).

   There is no inline-level parser, so we roundtrip an inline by wrapping it in
   a paragraph, rendering the block to CommonMark, reparsing, and pulling the
   inline back out. *)
module P = Oymarkit_generator.Property
module G = Oymarkit_generator.Gen_inline
module Inline = Oymarkit_.Inline
module Block = Oymarkit_.Block
module Meta = Oymarkit_.Meta
module Sexp_ = Oymarkit_.Sexp_

let sexp_of_inline = (Sexp_.make_sexp_of ()).inline

let canonical i =
  Format.asprintf "%a" Sexplib0.Sexp.pp_hum
    (sexp_of_inline (Inline.normalize i))

let inline_equal a b = String.equal (canonical a) (canonical b)

(* Wrap an inline in a paragraph: the only way to get it through the parser. *)
let wrap i = Block.Paragraph (Block.Paragraph.make i, Meta.none)

(* Pull the inline back out of a (reparsed) block. The parser yields the
   paragraph either bare or inside a singleton [Blocks]. *)
let unwrap = function
  | Block.Blocks ([ Block.Paragraph (p, _) ], _)
  | Block.Paragraph (p, _) ->
      Some (Block.Paragraph.inline p)
  | _ -> None

let roundtrip i = unwrap (P.reparse (wrap i))

let test_print i : string =
  let sexp_i = canonical i in
  let cm = wrap i |> P.to_commonmark in
  let i' = roundtrip i in
  let sexp_i' = Option.map canonical i' in
  let cm' =Option.map (fun i -> wrap i |> P.to_commonmark) i' in
  let metadata : P.metadata =
    P.
      [
        ("i", String sexp_i);
        ("cm", Md cm);
        ("i'", String (Option.value ~default:"None" sexp_i'));
        ("cm'", Md (Option.value ~default:"None" cm'));
      ]
  in
  Fmt.str "%a" P.pp_metadata metadata

let () =
  let rand = Random.State.make [| 0 |] in
  let gen = G.gen_leaf G.Iconfig.default in
  let test =
    QCheck2.Test.make ~name:"leaf inline roundtrip" ~print:test_print gen
      (fun i ->
        match roundtrip i with
        | Some i' -> inline_equal i i'
        | None -> false)
  in
  ignore @@ QCheck_base_runner.run_tests ~long:true ~colors:false ~rand [ test ]
