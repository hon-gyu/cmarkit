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

let canonical b : string =
  b |> Block.normalize |> normalize_block_inlines
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
