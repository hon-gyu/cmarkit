(** {0 Non-standard properties for extended syntax} *)
module Block = Cmarkit_.Block
module Inline = Cmarkit_.Inline
module Mapper = Cmarkit_.Mapper
module Meta = Cmarkit_.Meta
module Doc = Cmarkit_.Doc
module Struct = Cmarkit_.Struct
open Common_
open Property

(* Struct properties
   -----------------

   The {!Cmarkit_.Struct} pass is a post-parse [Doc.t] rewrite. Its defining
   property is content-invisibility: a keyed node keeps its label's ":" as real
   content, so {!Cmarkit_.Struct.unkey} flattens it back to the original block
   modulo normalisation (see [struct_content_invisible]). Enabling Struct thus
   changes only the tree's grouping, never its content -- which is also why both
   renderers can simply [unkey] keyed nodes and why CommonMark now round-trips
   them exactly.

   We also test idempotence and that the rewrite commutes with the container
   blocks (the general form of the struct x div / struct x attributes
   interaction tests). *)

let struct_rewrite ?paragraph_inline_value (b : Block.t) : Block.t =
  Doc.block (Struct.rewrite_doc ?paragraph_inline_value (Doc.make b))

(** Make paragraphs keyable so the rewrite is actually exercised: a non-empty
    text leaf ["foo bar"] becomes ["foo bar:"] (trailing-colon) or ["foo: bar"]
    (inline-value), chosen deterministically by length parity so the transform
    shrinks with the generated block. *)
let colonize (b : Block.t) : Block.t =
  let inline _ = function
    | Inline.Text (s, m) ->
        let t = String.trim s in
        if String.equal t "" then Mapper.default
        else if String.length t mod 2 = 0 then
          Mapper.ret (Inline.Text (t ^ ":", m))
        else Mapper.ret (Inline.Text (t ^ ": v", m))
    | _ -> Mapper.default
  in
  let mapper = Mapper.make ~inline () in
  Mapper.map_block mapper b |> Option.value ~default:Block.empty

(** A generator of keyable blocks: the base block generator with {!colonize}
    applied, so keying paths are reached. *)
let struct_gen ?config () : Block.t QCheck2.Gen.t =
  QCheck2.Gen.map colonize (Gen.mk_gen_block ?config ())

(** [rewrite (rewrite b) = rewrite b] structurally. *)
let struct_idempotent =
  let check b =
    check_eq ~expect:(struct_rewrite b) (struct_rewrite (struct_rewrite b))
  in
  { name = "struct/idempotent"; check }

(** Content-invisibility: [unkey] inverts [rewrite] modulo normalisation. A
    keyed node keeps its label's ":" as content, so flattening it reproduces the
    original block -- enabling Struct changes only the tree's grouping
    ("edges"), never its content. *)
let struct_content_invisible =
  let check b = check_eq ~expect:b (Struct.unkey (struct_rewrite b)) in
  { name = "struct/content_invisible"; check }

(* [rewrite] commutes with a single-block container: rewriting [wrap b] equals
   wrapping [rewrite b]. Generalizes the struct x other-syntax interaction
   tests -- the container survives and its content is rewritten consistently. *)
let struct_commutes_with ~name ~wrap =
  let check b =
    check_eq ~expect:(wrap (struct_rewrite b)) (struct_rewrite (wrap b))
  in
  { name; check }

let struct_commutes_block_quote =
  struct_commutes_with ~name:"struct/commutes/block_quote"
    ~wrap:wrap_block_quote

let struct_commutes_div =
  let wrap b = Block.Ext_div (Block.Div.make b, Meta.none) in
  struct_commutes_with ~name:"struct/commutes/div" ~wrap

let struct_commutes_attributes =
  let wrap b =
    Block.Ext_attributes (Block.Attributes.make ~specs:[] b, Meta.none)
  in
  struct_commutes_with ~name:"struct/commutes/attributes" ~wrap

let (struct_props : t list) =
  [
    struct_idempotent;
    struct_content_invisible;
    struct_commutes_block_quote;
    struct_commutes_div;
    struct_commutes_attributes;
  ]
