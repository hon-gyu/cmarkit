(* js_of_ocaml entry point: expose oymarkit's markdown -> mdast-JSON parse as a
   global JS function [oymarkitParse], so a unified pipeline can use it as a
   parser replacement (see the Astro remark-oymarkit plugin).

   Importing the generated oyparse.bc.js for its side effect installs
   [globalThis.oymarkitParse : (md: string) => string] (the string is mdast
   JSON, a `root` node). *)

open Js_of_ocaml

let callout =
  Cmarkit.Block.Callout.Config.make ~kinds:Cmarkit.Block.Callout.Config.Any ()

(* Extra inline containers, all curly-required ([{==x==}], [{^x^}], ...). Bare
   Obsidian [==highlight==] would need [~highlight:Curly_optional], but that
   currently mis-parses at the parser level (empty [<mark></mark>] pairs), so we
   keep curly braces compulsory until that is sorted out in the parser. *)
let extra_inline_containers =
  Cmarkit.Inline.Extra_inline_container.Config.explicit

let parse (md : string) : string =
  let doc =
    Cmarkit.Doc.of_string ~strict:false ~locs:true ~wikilink:true ~div:true
      ~inline_attributes:true ~block_attributes:true ~block_id:true
      ~callout ~extra_inline_containers md
  in
  (* Keep the [^id] block-id marker (dimmed via CSS) rather than stripping it. *)
  Cmarkit_mdast.of_doc ~strip_block_id:false doc

let () =
  Js.Unsafe.set Js.Unsafe.global
    (Js.string "oymarkitParse")
    (Js.wrap_callback (fun (md : Js.js_string Js.t) ->
         Js.string (parse (Js.to_string md))))
