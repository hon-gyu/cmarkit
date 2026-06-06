[@@@ocamlformat "disable"]

(*---------------------------------------------------------------------------
   Copyright (c) 2021 The cmarkit programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Common_

type t = ..

module Autolink = struct
  type t = { is_email : bool; link : string node; }
  let is_email a = a.is_email
  let link a = a.link
  let make link =
    let is_email =
      let l = String.concat "" ["<"; fst link; ">"] in
      match Match.autolink_email l ~last:(String.length l - 1) ~start:0 with
      | None -> false | Some _ -> true
    in
    { is_email; link }
end

module Break = struct
  type type' = [ `Hard | `Soft ]
  type t =
  { layout_before : Layout.blanks node;
    type' : type';
    layout_after : Layout.blanks node; }

  let make
      ?(layout_before = Layout.empty) ?(layout_after = Layout.empty) type'
    =
    { layout_before; type'; layout_after }

  let type' b = b.type'
  let layout_before b = b.layout_before
  let layout_after b = b.layout_after
end

module Code_span = struct
  type t =
    { backtick_count : Layout.count;
      code_layout : Block_line.tight list; }

  let make ~backtick_count code_layout = { backtick_count; code_layout }

  let min_backtick_count ~min counts =
    let rec loop min = function
    | c :: cs -> if min <> c then min else loop (c + 1) cs | [] -> min
    in
    loop min (List.sort Int.compare counts)

  let of_string ?(meta = Meta.none) = function
  | "" -> { backtick_count = 1 ; code_layout = ["", ("", meta)] }
  | s ->
      (* This finds out the needed backtick count, whether spaces are needed,
          and treats blanks after newline as layout *)
      let max = String.length s - 1 in
      let need_sp = s.[0] = '`' || s.[max] = '`' in
      let s = if need_sp then String.concat "" [" "; s; " "] else s in
      let backtick_counts, code_layout =
        let rec loop bt_counts acc max btc start k = match k > max with
        | true ->
            (* assert (btc = 0) because of [need_sp] *)
            bt_counts,
            if acc = [] then ["", (s, meta)] else
            List.rev (Block_line.flush_tight ~meta s start max acc)
        | false ->
            if s.[k] = '`'
            then loop bt_counts acc max (btc + 1) start (k + 1) else
            let bt_counts = if btc > 0 then btc :: bt_counts else bt_counts in
            if not (s.[k] = '\n' || s.[k] = '\r')
            then loop bt_counts acc max 0 start (k + 1) else
            let acc = Block_line.flush_tight ~meta s start (k - 1) acc in
            let start =
              if k + 1 <= max && s.[k] = '\r' && s.[k + 1] = '\n'
              then k + 2 else k + 1
            in
            loop bt_counts acc max 0 start start
        in
        loop [] [] max 0 0 0
      in
      let backtick_count = min_backtick_count ~min:1 backtick_counts in
      { backtick_count; code_layout }

  let backtick_count cs = cs.backtick_count
  let code_layout cs = cs.code_layout
  let code cs =
    (* Extract code, see https://spec.commonmark.org/0.31.2/#code-spans *)
    let sp c = Char.equal c ' ' in
    let s = List.map Block_line.tight_to_string cs.code_layout in
    let s = String.concat " " s in
    if s = "" then "" else
    if s.[0] = ' ' && s.[String.length s - 1] = ' ' &&
        not (String.for_all sp s)
    then String.sub s 1 (String.length s - 2) else s
end

module Emphasis = struct
  type inline = t
  type t =
    { delim : Layout.char;
      inline : inline;
      open_marker : bool;
      close_marker : bool }
  let make ?(delim = '*') ?(open_marker = false) ?(close_marker = false) inline
    = { delim; inline; open_marker; close_marker }
  let inline e = e.inline
  let delim e = e.delim
  let open_marker e = e.open_marker
  let close_marker e = e.close_marker
end

module Link = struct
  type inline = t

  type reference_layout = [ `Collapsed | `Full | `Shortcut ]
  type reference =
  [ `Inline of Link_definition.t node
  | `Ref of reference_layout * Label.t * Label.t ]

  type t = { text : inline; reference : reference; }

  let make text reference = { text; reference }
  let text l = l.text
  let reference l = l.reference
  let referenced_label l = match l.reference with
  | `Inline _ -> None | `Ref (_, _, k) -> Some k

  let reference_definition defs l = match l.reference with
  | `Inline ld -> Some (Link_definition.Def ld)
  | `Ref (_, _, def) -> Label.Map.find_opt (Label.key def) defs

  let is_unsafe l =
    let allowed_data_url l =
      let allowed = ["image/gif"; "image/png"; "image/jpeg"; "image/webp"] in
      (* Extract mediatype from data:[<mediatype>][;base64],<data> *)
      match String.index_from_opt l 4 ',' with
      | None -> false
      | Some j ->
          let k = match String.index_from_opt l 4 ';' with
          | None -> j | Some k -> k
          in
          let t = String.sub l 5 (min j k - 5) in
          List.mem t allowed
    in
    Ascii.caseless_starts_with ~prefix:"javascript:" l ||
    Ascii.caseless_starts_with ~prefix:"vbscript:" l ||
    Ascii.caseless_starts_with ~prefix:"file:" l ||
    (Ascii.caseless_starts_with ~prefix:"data:" l && not (allowed_data_url l))
end

module Raw_html = struct
  type t = Block_line.tight list
end

module Text = struct
  type t = string
end

type t +=
| Autolink of Autolink.t node
| Break of Break.t node
| Code_span of Code_span.t node
| Emphasis of Emphasis.t node
| Image of Link.t node
| Inlines of t list node
| Link of Link.t node
| Raw_html of Raw_html.t node
| Strong_emphasis of Emphasis.t node
| Text of Text.t node

let empty = Inlines ([], Meta.none)

let err_unknown = "Unknown Cmarkit.Inline.t type extension"

(* Extensions *)

module Strikethrough = struct
  type nonrec t = t
  let make = Fun.id
  let inline = Fun.id
end

module Extra_inline_container = struct
  type inline = t
  type kind = Highlight | Superscript | Subscript | Inserted | Deleted

  module Config = struct
    type syntax = Disabled | Curly_required | Curly_optional

    type t =
      { highlight : syntax;
        superscript : syntax;
        subscript : syntax;
        inserted : syntax;
        deleted : syntax }

    let make
        ?(highlight = Disabled) ?(superscript = Disabled)
        ?(subscript = Disabled) ?(inserted = Disabled) ?(deleted = Disabled) ()
      =
      { highlight; superscript; subscript; inserted; deleted }

    let disabled = make ()

    let explicit =
      make ~highlight:Curly_required ~superscript:Curly_required
        ~subscript:Curly_required ~inserted:Curly_required
        ~deleted:Curly_required ()

    let syntax t = function
    | Highlight -> t.highlight
    | Superscript -> t.superscript
    | Subscript -> t.subscript
    | Inserted -> t.inserted
    | Deleted -> t.deleted
  end

  type t = { kind : kind; inline : inline }
  let make kind inline = { kind; inline }
  let kind c = c.kind
  let inline c = c.inline
end

module Attributes = struct
  type inline = t
  type t =
    { inline : inline;
      attributes : Attribute.t;
      specs : Attribute.t list }

  let make ~specs inline =
    let attributes = List.fold_left Attribute.merge Attribute.empty specs in
    { inline; attributes; specs }
  let inline a = a.inline
  let attributes a = a.attributes
  let specs a = a.specs
end

module Math_span = struct
  type t = { display : bool; tex_layout : Block_line.tight list; }
  let make ~display tex_layout = { display; tex_layout }
  let display ms = ms.display
  let tex_layout ms = ms.tex_layout
  let tex ms =
    let s = List.map Block_line.tight_to_string ms.tex_layout in
    String.concat " "s
end

type t +=
| Ext_strikethrough of Strikethrough.t node
| Ext_extra_inline_container of Extra_inline_container.t node
| Ext_attributes of Attributes.t node
| Ext_math_span of Math_span.t node

(* Functions on inlines *)

let is_empty = function
| Text ("", _) | Inlines ([], _) -> true | _ -> false

let ext_none _ = invalid_arg err_unknown
let meta ?(ext = ext_none) = function
| Autolink (_, m) | Break (_, m) | Code_span (_, m) | Emphasis (_, m)
| Image (_, m) | Inlines (_, m) | Link (_, m) | Raw_html (_, m)
| Strong_emphasis (_, m)  | Text (_, m) -> m
| Ext_strikethrough (_, m) | Ext_extra_inline_container (_, m) -> m
| Ext_attributes (_, m) -> m
| Ext_math_span (_, m) -> m
| i -> ext i

let rec normalize ?(ext = ext_none) = function
| Autolink _ | Break _ | Code_span _ | Raw_html _ | Text _
| Inlines ([], _) | Ext_math_span _ as i -> i
| Image (l, m) -> Image ({ l with text = normalize ~ext l.text }, m)
| Link (l, m) -> Link ({ l with text = normalize ~ext l.text }, m)
| Inlines ([i], _) -> i
| Emphasis (e, m) ->
    Emphasis ({ e with inline = normalize ~ext e.inline}, m)
| Strong_emphasis (e, m) ->
    Strong_emphasis ({ e with inline = normalize ~ext e.inline}, m)
| Inlines (i :: is, m) ->
    let rec loop acc = function
    | Inlines (is', m) :: is -> loop acc (List.rev_append (List.rev is') is)
    | Text (t', m') as i' :: is ->
        begin match acc with
        | Text (t, m) :: acc ->
            let tl = Textloc.span (Meta.textloc m) (Meta.textloc m') in
            let i = Text (t ^ t', Meta.with_textloc ~keep_id:true m tl) in
            loop (i :: acc) is
        | _ -> loop (normalize ~ext i' :: acc) is
        end
    | i :: is -> loop (normalize ~ext i :: acc) is
    | [] -> List.rev acc
    in
    let is = loop [normalize ~ext i] is in
    (match is with [i] -> i | _ -> Inlines (is, m))
| Ext_strikethrough (i, m) -> Ext_strikethrough (normalize ~ext i, m)
| Ext_extra_inline_container (c, m) ->
    let inline = normalize ~ext (Extra_inline_container.inline c) in
    let c = Extra_inline_container.make (Extra_inline_container.kind c) inline in
    Ext_extra_inline_container (c, m)
| Ext_attributes (a, m) ->
    Ext_attributes (Attributes.make ~specs:a.specs (normalize ~ext a.inline), m)
| i -> ext i

let ext_none ~break_on_soft = ext_none
let to_plain_text ?(ext = ext_none) ~break_on_soft i =
  let push s acc = (s :: List.hd acc) :: List.tl acc in
  let newline acc = [] :: (List.rev (List.hd acc)) :: List.tl acc in
  let rec loop ~break_on_soft acc = function
  | Autolink (a, _) :: is ->
      let acc = push (String.concat "" ["<"; fst a.link; ">"]) acc in
      loop ~break_on_soft acc is
  | Break ({ type' = `Hard }, _) :: is ->
      loop ~break_on_soft (newline acc) is
  | Break ({ type' = `Soft }, _) :: is ->
      let acc = if break_on_soft then newline acc else (push " " acc) in
      loop ~break_on_soft acc is
  | Code_span (cs, _) :: is ->
      loop ~break_on_soft (push (Code_span.code cs) acc) is
  | Emphasis ({ inline }, _) :: is | Strong_emphasis ({ inline }, _) :: is ->
      loop ~break_on_soft acc (inline :: is)
  | Inlines (is', _) :: is ->
      loop ~break_on_soft acc (List.rev_append (List.rev is') is)
  | Link (l, _) :: is | Image (l, _) :: is ->
      loop ~break_on_soft acc (l.text :: is)
  | Raw_html _ :: is ->
      loop ~break_on_soft acc is
  | Text (t, _) :: is ->
      loop ~break_on_soft (push t acc) is
  | Ext_strikethrough (i, _) :: is ->
      loop ~break_on_soft acc (i :: is)
  | Ext_extra_inline_container (c, _) :: is ->
      loop ~break_on_soft acc (Extra_inline_container.inline c :: is)
  | Ext_attributes (a, _) :: is ->
      loop ~break_on_soft acc (Attributes.inline a :: is)
  | Ext_math_span (m, _) :: is ->
      loop ~break_on_soft (push (Math_span.tex m) acc) is
  | i :: is ->
      loop ~break_on_soft acc (ext ~break_on_soft i :: is)
  | [] ->
      List.rev ((List.rev (List.hd acc)) :: List.tl acc)
  in
  loop ~break_on_soft ([] :: []) [i]

let id ?buf ?ext i =
  let text = to_plain_text ?ext ~break_on_soft:false i in
  let s = String.concat "\n" (List.map (String.concat "") text) in
  let b = match buf with
  | Some b -> Buffer.reset b; b | None -> Buffer.create 256
  in
  let[@inline] collapse_blanks b ~prev_byte =
    (* Collapses non initial white *)
    if Ascii.is_blank prev_byte && Buffer.length b <> 0
    then Buffer.add_char b '-'
  in
  let rec loop b s max ~prev_byte k =
    if k > max then Buffer.contents b else
    match s.[k] with
    | ' ' | '\t' as prev_byte -> loop b s max ~prev_byte (k + 1)
    | '_' | '-' as c ->
        collapse_blanks b ~prev_byte;
        Buffer.add_char b c;
        loop b s max ~prev_byte:c (k + 1)
    | c ->
        let () = collapse_blanks b ~prev_byte in
        let d = String.get_utf_8_uchar s k in
        let u = Uchar.utf_decode_uchar d in
        let u = match Uchar.to_int u with 0x0000 -> Uchar.rep | _ -> u in
        let k' = k + Uchar.utf_decode_length d in
        if Cmarkit_data.is_unicode_punctuation u
        then loop b s max ~prev_byte:'\x00' k' else
        let () = match Cmarkit_data.unicode_case_fold u with
        | None -> Buffer.add_utf_8_uchar b u
        | Some fold -> Buffer.add_string b fold
        in
        let prev_byte = s.[k] in
        loop b s max ~prev_byte k'
  in
  loop b s (String.length s - 1) ~prev_byte:'\x00' 0
