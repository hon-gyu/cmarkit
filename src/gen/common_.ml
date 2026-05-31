module Block = Cmarkit_.Block
module Sexp = Cmarkit_.Sexp
module Pp = Cmarkit_.Pp
module Doc = Cmarkit_.Doc

let use_sexp = true
let sexp_of_block = (Sexp.make_sexp_of ()).block
let sexp_of_inline = (Sexp.make_sexp_of ()).inline

let pp_block fmt b =
  if use_sexp then
    Format.fprintf fmt "%a" Sexplib0.Sexp.pp_hum (sexp_of_block b)
  else Format.printf "%a" Pp.pp_block b

type value =
  | String of string
  | Int of int
  | Float of float
  | Bool of bool
  | Null
  | Object of (string * value) list
  | Block of Block.t
  | Md of string

type metadata = (string * value) list

let metadata_concat ?(wrapped_name : (string * string) option) m1 m2 =
  match wrapped_name with
  | None -> m1 @ m2
  | Some (n1, n2) ->
      let m1' = (n1, Object m1) in
      let m2' = (n2, Object m2) in
      [ m1'; m2' ]

let box_frame_default = true
let to_commonmark b = b |> Doc.make |> Cmarkit_commonmark.of_doc

let visible cm =
  let b = Buffer.create (String.length cm) in
  String.iter
    (function
      | '\n' -> Buffer.add_string b "\xe2\x86\xb5\n" (* ↵ + real newline *)
      | ' ' -> Buffer.add_string b "\xc2\xb7" (* · *)
      | '\t' -> Buffer.add_string b "\xe2\x87\xa5" (* ⇥ *)
      | c -> Buffer.add_char b c)
    cm;
  Buffer.contents b

let pp_cm ?(box_frame = box_frame_default) ?(transform_visible = true) () fmt
    (cm : string) =
  if not box_frame then Format.fprintf fmt "%s" cm
  else
    let cm = if transform_visible then visible cm else cm in
    let b =
      PrintBox.(
        frame @@ text_with_style Style.(default |> set_preformatted true) cm)
    in
    Format.fprintf fmt "%a" PrintBox_text.pp b

let rec pp_value ?(box_frame = box_frame_default) () fmt = function
  | String s -> Format.fprintf fmt "%s" s
  | Int i -> Format.fprintf fmt "%d" i
  | Float f -> Format.fprintf fmt "%f" f
  | Bool b -> Format.fprintf fmt "%b" b
  | Null -> Format.fprintf fmt "null"
  | Object o ->
      let pp_pair fmt (k, v) = Fmt.pf fmt "%s: %a" k (pp_value ()) v in
      let pp_pairs = Fmt.list ~sep:(Fmt.any "@,; ") pp_pair in
      Fmt.pf fmt "@[<v>{ %a@,}@]" pp_pairs o
  | Block b -> Format.fprintf fmt "%a" pp_block b
  | Md s ->
      if not box_frame then Format.fprintf fmt "%s" s
      else
        let b = PrintBox.(frame @@ text s) in
        Format.fprintf fmt "%a" PrintBox_text.pp b

let pp_metadata fmt m =
  let pp_pair fmt (k, v) = Fmt.pf fmt "@[<v>\"%s\":@ %a@]" k (pp_value ()) v in
  Fmt.pf fmt "@[<v>{ %a@,}@]" (Fmt.list ~sep:(Fmt.any "@,; ") pp_pair) m

let reparse ?emphasis_delims ?strong_emphasis_delims (b : Block.t) : Block.t =
  b |> to_commonmark
  |> Doc.of_string ?emphasis_delims ?strong_emphasis_delims
  |> Doc.block
