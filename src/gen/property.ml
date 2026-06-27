(** {0 Block AST Properties}
    - Property types
    - Counterexample printer
    - QCheck.Test bridge *)

(* TOOD:
  - make calculation of metadata lazy
*)
module Block = Cmarkit_.Block
module Inline = Cmarkit_.Inline
module Mapper = Cmarkit_.Mapper
module Meta = Cmarkit_.Meta
module Doc = Cmarkit_.Doc
module Struct = Cmarkit_.Struct
open Common_

(** {1 Property Type} *)

type result = Pass | Fail of Block.t * metadata
type t = { name : string; check : Block.t -> result }

exception Failed_precondition

let imply (p : t) (q : t) : t =
  let name = Fmt.str "%s ==> %s" p.name q.name in
  {
    name;
    check =
      (fun b ->
        match p.check b with
        | Pass -> q.check b
        | Fail _ -> raise Failed_precondition);
  }

let ( ==> ) = imply
let none : t = { name = "none"; check = (fun _ -> Pass) }

let and_ (p : t) (q : t) : t =
  let name = Fmt.str "%s &@ %s" p.name q.name in
  {
    name;
    check =
      (fun b ->
        match p.check b with
        | Pass ->
            begin match q.check b with
            | Pass -> Pass
            | Fail (b, meta2) -> Fail (b, [ (q.name, Object meta2) ])
            end
        | Fail (b, meta1) -> (
            match q.check b with
            | Pass -> Fail (b, [ (p.name, Object meta1) ])
            | Fail (_, meta2) ->
                let meta =
                  metadata_concat
                    ?wrapped_name:(Some (p.name, q.name))
                    meta1 meta2
                in
                Fail (b, meta)));
  }

let ( & ) = and_

let fail ?(message : string option) ?(expect : Block.t option) ?(metadata = [])
    (actual : Block.t) =
  let extra_meta = ref [] in
  Option.iter
    (fun r -> extra_meta := ("message", String r) :: !extra_meta)
    message;
  Option.iter (fun e -> extra_meta := ("expect", Block e) :: !extra_meta) expect;
  Fail (actual, metadata @ !extra_meta)

let pp_fail t fmt b : unit =
  match t.check b with
  | Pass -> assert false
  | Fail (b, meta) ->
      if false then
        Fmt.pf fmt "@[<v>{ block: %a@,; cm: %a@,; %a@,}@]" pp_block b (pp_cm ())
          (to_commonmark b)
          (if List.length meta = 0 then fun _ _ -> ()
           else fun fmt m -> Fmt.pf fmt "metadata: %a" pp_metadata m)
          meta
      else
        Fmt.pf fmt "@[<v>{ block: %a@,; cm: %a@,; %a@,}@]" pp_block b (pp_cm ())
          (to_commonmark b)
          (let pp_pair fmt (k, v) =
             Fmt.pf fmt "@[<v>\"%s\":@ %a@]" k (pp_value ()) v
           in
           fun fmt m ->
             Fmt.pf fmt "%a" (Fmt.list ~sep:(Fmt.any "@,; ") pp_pair) m)
          meta

let qcheck_test_of_t ?(count = 500) ?(negative = false)
    ?(gen = Gen.mk_gen_block ()) () (t : t) : QCheck2.Test.t =
  let make_test =
    if not negative then QCheck2.Test.make ~name:t.name ~count
    else QCheck2.Test.make_neg ~name:t.name ~count
  in
  make_test
    ~print:(fun b -> Fmt.str "%a" (pp_fail t) b)
    gen
    (fun b ->
      try
        match t.check b with
        | Pass -> true
        | Fail _ -> false
      with
      | Failed_precondition -> QCheck2.assume_fail ())

(** {1 Helpers} *)

(* Helpers
=========== *)

let normalize_block_inlines (b : Block.t) : Block.t =
  let rec inline_fix i =
    let i' = Inline.normalize i in
    if Sexplib0.Sexp.equal (sexp_of_inline i) (sexp_of_inline i') then i'
    else inline_fix i'
  in
  let inline _ i = Mapper.ret (inline_fix i) in
  let mapper = Mapper.make ~inline () in
  Mapper.map_block mapper b |> Option.value ~default:Block.empty

(** Canonicalize terminal blank-line ownership at container boundaries.

    This is deliberately a property-comparison rule, not a typing rule. The
    parser can emit both shapes involved here, so the generator should not
    reject them as non-emittable ASTs.

    The ambiguity is specifically about a [Blank_line] at the terminal edge of
    nested block/list-item content. When such a blank is rendered, Markdown has
    no marker that says whether it belongs to the inner [Blocks] payload or to
    the enclosing container/document: the same physical blank line both closes
    the inner content and separates the outer context. On reparse, the parser
    may attach that blank one level higher.

    For roundtrip equality we pick one canonical owner: terminal blanks float
    outward from nested [Blocks] and from the last item of a [List], and the
    root keeps them as document-level siblings. Interior blanks and non-final
    list-item blanks are not moved; those can affect paragraph/list structure
    and must remain observable. *)
let canonicalize_terminal_blank_ownership (b : Block.t) : Block.t =
  let blank = function
    | Block.Blank_line _ -> true
    | _ -> false
  in
  let split_trailing_blanks blocks =
    let rec loop trailing = function
      | b :: rev when blank b -> loop (b :: trailing) rev
      | rev -> (List.rev rev, trailing)
    in
    loop [] (List.rev blocks)
  in
  let append_as_blocks block trailing =
    match trailing with
    | [] -> block
    | _ -> (
        match block with
        | Block.Blocks (blocks, meta) -> Block.Blocks (blocks @ trailing, meta)
        | _ -> Block.Blocks (block :: trailing, Meta.none))
  in
  let item_with_block item block =
    Block.List_item.make
      ~before_marker:(Block.List_item.before_marker item)
      ~marker:(Block.List_item.marker item)
      ~after_marker:(Block.List_item.after_marker item)
      ?ext_task_marker:(Block.List_item.ext_task_marker item)
      block
  in
  let rec canonicalize block =
    match block with
    | Block.Blocks (blocks, meta) ->
        let blocks =
          List.concat_map
            (fun block ->
              let block, trailing = canonicalize block in
              block :: trailing)
            blocks
        in
        let blocks, trailing = split_trailing_blanks blocks in
        (Block.Blocks (blocks, meta), trailing)
    | Block.List (list, meta) ->
        let canonicalize_non_final_item (item, item_meta) =
          let block, trailing = canonicalize (Block.List_item.block item) in
          (item_with_block item (append_as_blocks block trailing), item_meta)
        in
        let canonicalize_final_item (item, item_meta) =
          let block, trailing = canonicalize (Block.List_item.block item) in
          ((item_with_block item block, item_meta), trailing)
        in
        begin match List.rev (Block.List'.items list) with
        | [] -> (Block.List (list, meta), [])
        | last :: rev_prefix ->
            let prefix = List.rev_map canonicalize_non_final_item rev_prefix in
            let last, trailing = canonicalize_final_item last in
            let items = prefix @ [ last ] in
            let list =
              Block.List'.make ~tight:(Block.List'.tight list)
                (Block.List'.type' list) items
            in
            (Block.List (list, meta), trailing)
        end
    | block -> (block, [])
  in
  let block, trailing = canonicalize b in
  append_as_blocks block trailing

let canonical b : string =
  b |> Block.normalize |> canonicalize_terminal_blank_ownership
  |> normalize_block_inlines
  |> Format.asprintf "%a" pp_block

let block_equal a b = String.equal (canonical a) (canonical b)

let check_eq ?(message : string option) ?(metadata = []) ~(expect : Block.t)
    (actual : Block.t) =
  if block_equal expect actual then Pass
  else fail ?message ~metadata ~expect actual

(* Properties
  =========== *)

(** [normalize (normalize b) = normalize b] structurally. *)
let normalize_idempotent =
  let check =
   fun b ->
    let n1 = Block.normalize b in
    let n2 = Block.normalize n1 in
    check_eq ~expect:n1 n2
  in
  { name = "normalize_idempotent"; check }

(** [parse (to_commonmark b) ≡ b] modulo {!canonical}.

    This is the most important property for generators as it ensures that the
    block structure is valid, i.e. can be emitted by the parser from a piece of
    CommonMark. *)
let roundtrip_with ?emphasis_delims ?strong_emphasis_delims ?intraword_emphasis
    ?marked_emphasis_delims ?strong_emphasis_width ?extra_inline_containers
    ?block_id ?djot_inline_attributes ?djot_block_attributes () =
  let check =
   fun b ->
    let b' =
      reparse ?emphasis_delims ?strong_emphasis_delims ?intraword_emphasis
        ?marked_emphasis_delims ?strong_emphasis_width ?extra_inline_containers
        ?block_id ?djot_inline_attributes ?djot_block_attributes b
    in
    if block_equal b b' then Pass
    else
      Fail (b, [ ("reparse", Block b'); ("reparse_cm", Md (to_commonmark b')) ])
  in
  { name = "roundtrip"; check }

let roundtrip = roundtrip_with ()

(** [render b = to_commonmark (parse (to_commonmark b))] as strings. *)
let render_determinism =
  {
    name = "render_determinism";
    check =
      (fun b ->
        let s1 = to_commonmark b in
        let b' = s1 |> Doc.of_string |> Doc.block in
        let s2 = to_commonmark b' in
        if String.equal s1 s2 then Pass
        else Fail (b, [ ("cm", Md s1); ("b'", Block b'); ("cm'", Md s2) ]));
  }

(* Container uniformity
   --------------------- *)

let wrap_block_quote b = Block.Block_quote (Block.Block_quote.make b, Meta.none)

let wrap_list_item b =
  let item = (Block.List_item.make b, Meta.none) in
  let l = Block.List'.make (`Unordered '-') [ item ] in
  Block.List (l, Meta.none)

(* Unwrap to the inner block, or None if outer shape changed. *)
let unwrap_block_quote = function
  | Block.Block_quote (bq, _) -> Some (Block.Block_quote.block bq)
  | _ -> None

let unwrap_list_item = function
  | Block.List (l, _) -> (
      match Block.List'.items l with
      | [ (item, _) ] -> Some (Block.List_item.block item)
      | _ -> None)
  | _ -> None

(* After reparse the top-level block is typically [Blocks [single]]; peel it. *)
let peel_singleton_blocks = function
  | Block.Blocks ([ b ], _) -> b
  | b -> b

let uniformity_with ~name ~wrap ~unwrap =
  {
    name;
    check =
      (fun b ->
        let wrapped = wrap b in
        let parsed = reparse wrapped |> peel_singleton_blocks in
        match unwrap parsed with
        | None ->
            fail ~message:"container shape not preserved by round-trip"
              ~expect:wrapped parsed
        | Some inner ->
            check_eq ~message:"container content not preserved" ~expect:b inner);
  }

(** Wrapping [b] in a [Block_quote] then round-tripping yields a [Block_quote]
    whose inner block matches [b]. *)
let uniformity_block_quote =
  uniformity_with ~name:"uniformity/block_quote" ~wrap:wrap_block_quote
    ~unwrap:unwrap_block_quote

(** Wrapping [b] in a single-item unordered list then round-tripping yields a
    list whose first item's block matches [b]. *)
let uniformity_list_item =
  uniformity_with ~name:"uniformity/list_item" ~wrap:wrap_list_item
    ~unwrap:unwrap_list_item

let (all : t list) =
  [
    roundtrip;
    normalize_idempotent;
    render_determinism;
    uniformity_block_quote;
    uniformity_list_item;
  ]

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
      else if String.length t mod 2 = 0 then Mapper.ret (Inline.Text (t ^ ":", m))
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
  let check b = check_eq ~expect:(struct_rewrite b) (struct_rewrite (struct_rewrite b)) in
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
  let check b = check_eq ~expect:(wrap (struct_rewrite b)) (struct_rewrite (wrap b)) in
  { name; check }

let struct_commutes_block_quote =
  struct_commutes_with ~name:"struct/commutes/block_quote" ~wrap:wrap_block_quote

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
