(* Extended modules *)
module Pp = Pp
module Sexp = Sexp
module Struct = Struct_
module Inline_struct = Inline_struct
module Inline_parse_api = Inline_parse_api
module Parser_common = Parser_common_

[@@@ocamlformat "disable"]

(*---------------------------------------------------------------------------
   Copyright (c) 2021 The cmarkit programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  --------------------------------------------------------------------------- *)

module String_map = Map.Make (String)
module Ascii = Cmarkit_base.Ascii
module Text = Cmarkit_base.Text
module Match = Cmarkit_base
module Textloc = Cmarkit_base.Textloc
module Meta = Cmarkit_base.Meta
module Layout = Common_.Layout
module Attribute = Attribute

type byte_pos = Textloc.byte_pos
type line_span = Match.line_span =
  (* Substring on a single line, hereafter abbreviated to span *)
  { line_pos : Textloc.line_pos; first : byte_pos; last : byte_pos }

type 'a node = 'a * Meta.t

module Block_line = Common_.Block_line
module Label = Common_.Label
module Link_definition = Common_.Link_definition

module Inline = Inline

(* Blocks *)

module Block = Block

(* Parsing *)

include Parser

(* Documents *)

module Doc = Doc

(* Maps and folds *)

module Mapper = struct
  type 'a filter_map = 'a option
  type 'a result = [ `Default | `Map of 'a filter_map ]
  let default = `Default
  let delete = `Map None
  let ret v = `Map (Some v)

  type t =
    { inline_ext_default : Inline.t map;
      block_ext_default : Block.t map;
      inline : Inline.t mapper;
      block : Block.t mapper }
  and 'a map = t -> 'a -> 'a filter_map
  and 'a mapper = t -> 'a -> 'a result

  let none _ _ = `Default
  let ext_inline_none _ _ = invalid_arg Inline.err_unknown
  let ext_block_none _ _ = invalid_arg Block.err_unknown
  let make
      ?(inline_ext_default = ext_inline_none)
      ?(block_ext_default = ext_block_none)
      ?(inline = none) ?(block = none) ()
    =
    { inline_ext_default; block_ext_default; inline; block }

  let inline_mapper m = m.inline
  let block_mapper m = m.block
  let inline_ext_default m = m.inline_ext_default
  let block_ext_default m = m.block_ext_default

  let ( let* ) = Option.bind

  let rec map_inline m i = match m.inline m i with
  | `Map i -> i
  | `Default ->
      let open Inline in
      match i with
      | Autolink _ | Break _ | Code_span _ | Raw_html _
      | Text _ | Ext_math_span _ | Ext_raw_inline _ | Ext_jsx_expr _ as i ->
          Some i
      | Ext_jsx_element (e, meta) ->
          (match Inline.Jsx_element.children e with
          | None -> Some (Ext_jsx_element (e, meta))
          | Some child ->
              let children =
                Option.value ~default:Inline.empty (map_inline m child)
              in
              let e =
                Inline.Jsx_element.make_container
                  (Inline.Jsx_element.raw e) children
              in
              Some (Ext_jsx_element (e, meta)))
      | Image (l, meta) ->
          let text = Option.value ~default:Inline.empty (map_inline m l.text) in
          Some (Image ({ l with text }, meta))
      | Link (l, meta) ->
          let* text = map_inline m l.text in
          Some (Link ({ l with text }, meta))
      | Emphasis (e, meta) ->
          let* inline = map_inline m e.inline in
          Some (Emphasis ({ e with inline }, meta))
      | Strong_emphasis (e, meta) ->
          let* inline = map_inline m e.inline in
          Some (Strong_emphasis ({ e with inline}, meta))
      | Inlines (is, meta) ->
          (match List.filter_map (map_inline m) is with
          | [] -> None | is -> Some (Inlines (is, meta)))
      | Ext_strikethrough (s, meta) ->
          let* inline = map_inline m s in
          Some (Ext_strikethrough (inline, meta))
      | Ext_extra_inline_container (c, meta) ->
          let kind = Inline.Extra_inline_container.kind c in
          let* inline = map_inline m (Inline.Extra_inline_container.inline c) in
          Some (Ext_extra_inline_container (Inline.Extra_inline_container.make kind inline, meta))
      | Ext_attributes (a, meta) ->
          let* inline = map_inline m (Inline.Attributes.inline a) in
          Some (Ext_attributes (Inline.Attributes.make ~specs:(Inline.Attributes.specs a) inline, meta))
      | ext -> m.inline_ext_default m ext

  let rec map_block m b = match m.block m b with
  | `Map b -> b
  | `Default ->
      let open Block in
      match b with
      | Blank_line _ | Code_block _ | Html_block _
      | Link_reference_definition _ | Thematic_break _
      | Ext_math_block _ | Ext_raw_block _ as b -> Some b
      | Heading (h, meta) ->
          let inline = match map_inline m (Block.Heading.inline h) with
          | None -> (* Can be empty *) Inline.Inlines ([], Meta.none)
          | Some i -> i
          in
          Some (Heading ({ h with inline}, meta))
      | Block_quote (b, meta) ->
          let block = match map_block m b.block with
          | None -> (* Can be empty *) Blocks ([], Meta.none) | Some b -> b
          in
          Some (Block_quote ({ b with block}, meta))
      | Blocks (bs, meta) ->
          (match List.filter_map (map_block m) bs with
          | [] -> None | bs -> Some (Blocks (bs, meta)))
      | List (l, meta) ->
          let map_list_item m (i, meta) =
            let* block = map_block m (List_item.block i) in
            Some ({ i with block }, meta)
          in
          (match List.filter_map (map_list_item m) l.items with
          | [] -> None | items -> Some (List ({ l with items }, meta)))
      | Paragraph (p, meta) ->
          let* inline = map_inline m (Paragraph.inline p) in
          Some (Paragraph ({ p with inline }, meta))
      | Ext_table (t, meta) ->
          let map_col m (i, layout) = match map_inline m i with
          | None -> (Inline.empty, layout) | Some i -> (i, layout)
          in
          let map_row (((r, meta), blanks) as row) = match r with
          | `Header is -> (`Header (List.map (map_col m) is), meta), blanks
          | `Sep _ -> row
          | `Data is -> (`Data (List.map (map_col m) is), meta), blanks
          in
          let rows = List.map map_row t.rows in
          Some (Ext_table ({ t with Table.rows }, meta))
      | Ext_definition_list (d, meta) ->
          let item (i, imeta) =
            let term = match map_inline m (Block.Definition_list.item_term i) with
            | None -> Inline.Inlines ([], Meta.none)
            | Some term -> term
            in
            let definition =
              match map_block m (Block.Definition_list.item_definition i) with
              | None -> Block.empty
              | Some b -> b
            in
            { i with Block.Definition_list.term; definition }, imeta
          in
          let items = List.map item (Block.Definition_list.items d) in
          Some (Ext_definition_list ({ d with items }, meta))
      | Ext_footnote_definition (fn, meta) ->
          let block = match map_block m fn.block with
          | None -> (* Can be empty *) Blocks ([], Meta.none) | Some b -> b
          in
          Some (Ext_footnote_definition ({ fn with block}, meta))
      | Ext_div (d, meta) ->
          let block = match map_block m (Block.Div.block d) with
          | None -> (* Can be empty *) Blocks ([], Meta.none) | Some b -> b
          in
          Some (Ext_div ({ d with block }, meta))
      | Ext_jsx_block (j, meta) ->
          let block = match map_block m (Block.Jsx_block.block j) with
          | None -> (* Can be empty *) Blocks ([], Meta.none) | Some b -> b
          in
          Some (Ext_jsx_block ({ j with block }, meta))
      | Ext_attributes (a, meta) ->
          let* block = map_block m (Block.Attributes.block a) in
          Some (Ext_attributes (Block.Attributes.make ~specs:(Block.Attributes.specs a) block, meta))
      | Ext_keyed ((l, b), meta) ->
          let l = Option.value ~default:Inline.empty (map_inline m l) in
          let b = match map_block m b with None -> Blocks ([], Meta.none) | Some b -> b in
          Some (Ext_keyed ((l, b), meta))
      | ext -> m.block_ext_default m ext

  let map_doc m d =
    let map_block m b = Option.value ~default:Block.empty (map_block m b) in
    (* XXX something better for defs should be devised here. *)
    let map_def m = function
    | Block.Footnote.Def (fn, meta) ->
        let block = map_block m (Block.Footnote.block fn) in
        Block.Footnote.Def ({ fn with block }, meta)
    | def -> def
    in
    let block = map_block m (Doc.block d) in
    let defs = Label.Map.map (map_def m) (Doc.defs d) in
    { d with Doc.block; defs }
end

module Folder = struct
  type 'a result = [ `Default | `Fold of 'a ]
  let default = `Default
  let ret v = `Fold v

  type ('a, 'b) fold = 'b t -> 'b -> 'a -> 'b
  and ('a, 'b) folder = 'b t -> 'b -> 'a -> 'b result
  and 'a t =
    { inline_ext_default : (Inline.t, 'a) fold;
      block_ext_default : (Block.t, 'a) fold;
      inline : (Inline.t, 'a) folder;
      block : (Block.t, 'a) folder; }

  let none _ _ _ = `Default
  let ext_inline_none _ _ _ = invalid_arg Inline.err_unknown
  let ext_block_none _ _ _ = invalid_arg Block.err_unknown
  let make
      ?(inline_ext_default = ext_inline_none)
      ?(block_ext_default = ext_block_none)
      ?(inline = none) ?(block = none) ()
    =
    { inline_ext_default; block_ext_default; inline; block }

  let inline_folder f = f.inline
  let block_folder f = f.block
  let inline_ext_default f = f.inline_ext_default
  let block_ext_default f = f.block_ext_default

  let rec fold_inline f acc i = match f.inline f acc i with
  | `Fold acc -> acc
  | `Default ->
      let open Inline in
      match i with
      | Autolink _ | Break _ | Code_span _ | Raw_html _ | Text _
      | Ext_math_span _ | Ext_raw_inline _ | Ext_jsx_expr _ -> acc
      | Ext_jsx_element (e, _) ->
          (match Inline.Jsx_element.children e with
          | None -> acc | Some child -> fold_inline f acc child)
      | Image (l, _) | Link (l, _) -> fold_inline f acc l.text
      | Emphasis ({ inline }, _) -> fold_inline f acc inline
      | Strong_emphasis ({ inline }, _) -> fold_inline f acc inline
      | Inlines (is, _) -> List.fold_left (fold_inline f) acc is
      | Ext_strikethrough (inline, _) -> fold_inline f acc inline
      | Ext_extra_inline_container (c, _) ->
          fold_inline f acc (Inline.Extra_inline_container.inline c)
      | Ext_attributes (a, _) ->
          fold_inline f acc (Inline.Attributes.inline a)
  | ext -> f.inline_ext_default f acc ext

  let rec fold_block f acc b = match f.block f acc b with
  | `Fold acc -> acc
  | `Default ->
      let open Block in
      match b with
      | Blank_line _ | Code_block _ | Html_block _
      | Link_reference_definition _ | Thematic_break _ | Ext_math_block _
      | Ext_raw_block _ -> acc
      | Heading (h, _) -> fold_inline f acc (Block.Heading.inline h)
      | Block_quote (bq, _) -> fold_block f acc bq.block
      | Blocks (bs, _) -> List.fold_left (fold_block f) acc bs
      | List (l, _) ->
          let fold_list_item m acc (i, _) =
            fold_block m acc (Block.List_item.block i)
          in
          List.fold_left (fold_list_item f) acc l.items
      | Paragraph (p, _) -> fold_inline f acc (Block.Paragraph.inline p)
      | Ext_table (t, _) ->
          let fold_row acc ((r, _), _) = match r with
          | (`Header is | `Data is) ->
              List.fold_left (fun acc (i, _) -> fold_inline f acc i) acc is
          | `Sep _ -> acc
          in
          List.fold_left fold_row acc t.Table.rows
      | Ext_definition_list (d, _) ->
          let item acc (i, _) =
            let acc = fold_inline f acc (Block.Definition_list.item_term i) in
            fold_block f acc (Block.Definition_list.item_definition i)
          in
          List.fold_left item acc (Block.Definition_list.items d)
      | Ext_footnote_definition (fn, _) -> fold_block f acc fn.block
      | Ext_div (d, _) -> fold_block f acc (Block.Div.block d)
      | Ext_jsx_block (j, _) -> fold_block f acc (Block.Jsx_block.block j)
      | Ext_attributes (a, _) -> fold_block f acc (Block.Attributes.block a)
      | Ext_keyed ((l, b), _) ->
          fold_block f (fold_inline f acc l) b
      | ext -> f.block_ext_default f acc ext

  let fold_doc f acc d = fold_block f acc (Doc.block d)
end
