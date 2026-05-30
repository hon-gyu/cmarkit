module Block = Cmarkit_.Block
module Sexp = Cmarkit_.Sexp
module Pp = Cmarkit_.Pp
module Doc = Cmarkit_.Doc

let use_sexp = true

let pp_block fmt b =
  if use_sexp then
    Format.fprintf fmt "%a" Sexplib0.Sexp.pp_hum
      ((Sexp.make_sexp_of ()).block b)
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

let reparse (b : Block.t) : Block.t =
  b |> to_commonmark |> Doc.of_string |> Doc.block
