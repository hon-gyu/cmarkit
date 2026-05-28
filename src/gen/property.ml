(* TOOD:
  - make calculation of metadata lazy
*)
open Oymarkit_

let use_sexp = true

let pp_block fmt b =
  if use_sexp then
    Format.fprintf fmt "%a" Sexplib0.Sexp.pp_hum
      ((Sexp_.make_sexp_of ()).block b)
  else Format.printf "%a" Pp.pp_block b

type value =
  | String of string
  | Int of int
  | Float of float
  | Bool of bool
  | Null
  | Block of Block.t
  | Md of string

type metadata = (string * value) list

let box_frame_default = true
let to_commonmark b = b |> Doc.make |> Cmarkit_commonmark.of_doc

let pp_cm ?(box_frame = box_frame_default) () fmt (cm : string) =
  if not box_frame then Format.fprintf fmt "%s" cm
  else
    let b = PrintBox.(frame @@ text cm) in
    Format.fprintf fmt "%a" PrintBox_text.pp b

let pp_value ?(box_frame = box_frame_default) () fmt = function
  | String s -> Format.fprintf fmt "%s" s
  | Int i -> Format.fprintf fmt "%d" i
  | Float f -> Format.fprintf fmt "%f" f
  | Bool b -> Format.fprintf fmt "%b" b
  | Null -> Format.fprintf fmt "null"
  | Block b -> Format.fprintf fmt "%a" pp_block b
  | Md s ->
      if not box_frame then Format.fprintf fmt "%s" s
      else
        let b = PrintBox.(frame @@ text s) in
        Format.fprintf fmt "%a" PrintBox_text.pp b

let pp_metadata fmt m =
  let pp_pair fmt (k, v) = Fmt.pf fmt "@[<h>%s:@ %a@]" k (pp_value ()) v in
  Fmt.pf fmt "@[<v>{ %a@,}@]" (Fmt.list ~sep:(Fmt.any "@,; ") pp_pair) m

type result = Pass | Fail of Block.t * metadata
type t = { name : string; check : Block.t -> result }

exception Failed_precondition

let imply p q : t =
  let name = Fmt.str "%s ==> %s" p.name q.name in
  {
    name;
    check =
      (fun b ->
        match p.check b with
        | Pass -> q.check b
        | Fail _ -> raise Failed_precondition);
  }

let ( ==> ) p q = imply p q

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
      if Int.equal (List.length meta) 0 then Fmt.pf fmt "%a" pp_block b
      else
        Fmt.pf fmt "@[<v>{ block: %a@,; cm: %a@,; metadata: %a@,}@]" pp_block b
          (pp_cm ()) (to_commonmark b) pp_metadata meta

let qcheck_test_of_t ?(count = 500) () (t : t) : QCheck2.Test.t =
  QCheck2.Test.make ~name:t.name ~count
    ~print:(fun b -> Fmt.str "%a" (pp_fail t) b)
    (Gen.gen_block ())
    (fun b ->
      try
        match t.check b with
        | Pass -> true
        | Fail _ -> false
      with
      | Failed_precondition -> QCheck2.assume_fail ())

(* Helpers
=========== *)

let canonical b : string = Format.asprintf "%a" pp_block (Block.normalize b)
let block_equal a b = String.equal (canonical a) (canonical b)

let check_eq ?(message : string option) ?(metadata = []) ~(expect : Block.t)
    (actual : Block.t) =
  if block_equal expect actual then Pass
  else fail ?message ~metadata ~expect actual

let reparse (b : Block.t) : Block.t =
  b |> to_commonmark |> Doc.of_string |> Doc.block

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
let roundtrip =
  let check =
   fun b ->
    let b' = reparse b in
    let metadata =
      [ ("cm", Md (to_commonmark b)); ("cm'", Md (to_commonmark b')) ]
    in
    check_eq ~expect:b ~metadata b'
  in
  { name = "roundtrip"; check }

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
