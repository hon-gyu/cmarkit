(*---------------------------------------------------------------------------
   Copyright (c) 2021 The cmarkit programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

[@@@ocamlformat "disable"]

open Common_

type t = ..

module Blank_line = struct
  type t = Layout.blanks
end

module Block_quote = struct
  type nonrec t = { indent : Layout.indent; block : t; }
  let make ?(indent = 0) block = { indent; block }
  let indent bq = bq.indent
  let block bq = bq.block
end

module Code_block = struct
  type fenced_layout =
    { indent : Layout.indent;
      opening_fence : Layout.string node;
      closing_fence : Layout.string node option; }

  let default_fenced_layout =
    { indent = 0;
      opening_fence = Layout.empty;
      closing_fence = Some Layout.empty }

  type layout = [ `Indented | `Fenced of fenced_layout ]
  type t =
    { layout : layout;
      info_string : string node option;
      code : string node list; }

  let make ?(layout = `Fenced default_fenced_layout) ?info_string code =
    let layout = match info_string, layout with
    | Some _, `Indented -> `Fenced default_fenced_layout
    | _, layout -> layout
    in
    { layout; info_string; code }

  let layout cb = cb.layout
  let info_string cb = cb.info_string
  let code cb = cb.code

  let make_fence cb =
    let rec loop char counts = function
    | [] -> counts
    | (c, _) :: cs ->
        let max = String.length c - 1 in
        let k = ref 0 in
        while (!k <= max && c.[!k] = char) do incr k done;
        loop char (if !k <> 0 then !k :: counts else counts) cs
    in
    let char = match cb.info_string with
    | Some (i, _) when String.exists (Char.equal '`') i -> '~'
    | None | Some _ -> '`'
    in
    let counts = loop char [] cb.code in
    char,
    Inline.Code_span.min_backtick_count (* not char specific *) ~min:3 counts

  let language_of_info_string s =
    let rec next_white s max i =
      if i > max || Ascii.is_white s.[i] then i else
      next_white s max (i + 1)
    in
    if s = "" then None else
    let max = String.length s - 1 in
    let white = next_white s max 0 in
    let rem_first = Match.first_non_blank s ~last:max ~start:white in
    let lang = String.sub s 0 white in
    if lang = "" then None else
    Some (lang, String.sub s rem_first (max - rem_first + 1))

  let is_math_block = function
  | None -> false | Some (i, _) -> match language_of_info_string i with
  | Some ("math", _) -> true
  | Some _ | None -> false
end

module Heading = struct
  type atx_layout =
    { indent : Layout.indent;
      after_opening : Layout.blanks;
      closing : Layout.string; }

  let default_atx_layout = { indent = 0; after_opening = ""; closing = "" }

  type setext_layout =
    { leading_indent : Layout.indent;
      trailing_blanks : Layout.blanks;
      underline_indent : Layout.indent;
      underline_count : Layout.count node;
      underline_blanks : Layout.blanks; }

  type layout = [ `Atx of atx_layout | `Setext of setext_layout ]
  type id = [ `Auto of string | `Id of string ]
  type t = { layout : layout; level : int; inline : Inline.t; id : id option }

  let make ?id ?(layout = `Atx default_atx_layout) ~level inline =
    let max = match layout with `Atx _ -> 6 | `Setext _ -> 2 in
    let level = Int.max 1 (Int.min level max) in
    {layout; level; inline; id}

  let layout h = h.layout
  let level h = h.level
  let inline h = h.inline
  let id h = h.id
end

module Html_block = struct
  type t = string node list
end

module List_item = struct
  type block = t
  type t =
    { before_marker : Layout.indent;
      marker : Layout.string node;
      after_marker : Layout.indent;
      block : block;
      ext_task_marker : Uchar.t node option }

  let make
      ?(before_marker = 0) ?(marker = Layout.empty) ?(after_marker = 1)
      ?ext_task_marker block
    =
    { before_marker; marker; after_marker; block; ext_task_marker }

  let block i = i.block
  let before_marker i = i.before_marker
  let marker i = i.marker
  let after_marker i = i.after_marker
  let ext_task_marker i = i.ext_task_marker
  let task_status_of_task_marker u = match Uchar.to_int u with
  | 0x0020 -> `Unchecked
  | 0x0078 (* x *) | 0x0058 (* X *) | 0x2713 (* ✓ *) | 0x2714 (* ✔ *)
  | 0x10102 (* 𐄂 *) | 0x1F5F8 (* 🗸*) -> `Checked
  | 0x007E (* ~ *) -> `Cancelled
  | _ -> `Other u
end

module List' = struct
  type type' = [ `Unordered of Layout.char | `Ordered of int * Layout.char ]
  type t =
    { type' : type';
      tight : bool;
      items : List_item.t node list; }

  let make ?(tight = true) type' items = { type'; tight; items }

  let type' l = l.type'
  let tight l = l.tight
  let items l = l.items
end

module Block_id = struct
  type t = { id : string; marker : Meta.t }

  let key : t Meta.key = Meta.key ()
  let id t = t.id
  let marker t = t.marker
  let find meta = Meta.find key meta
  let add t meta = Meta.add key t meta
end

module Paragraph = struct
  type t =
    { leading_indent : Layout.indent;
      inline : Inline.t;
      trailing_blanks : Layout.blanks; }

  let make ?(leading_indent = 0) ?(trailing_blanks = "") inline =
    { leading_indent; inline; trailing_blanks }

  let inline p = p.inline
  let leading_indent p = p.leading_indent
  let trailing_blanks p = p.trailing_blanks
end

module Attributes = struct
  type block = t
  type t =
    { block : block;
      attributes : Attribute.t;
      specs : Attribute.t list }

  let make ~specs block =
    let attributes = List.fold_left Attribute.merge Attribute.empty specs in
    { block; attributes; specs }
  let block a = a.block
  let attributes a = a.attributes
  let specs a = a.specs
end

module Thematic_break = struct
  type t = { indent : Layout.indent; layout : Layout.string }
  let make ?(indent = 0) ?(layout = "---") () =  { indent; layout }
  let indent t = t.indent
  let layout t = t.layout
end

type t +=
| Blank_line of Layout.blanks node
| Block_quote of Block_quote.t node
| Blocks of t list node
| Code_block of Code_block.t node
| Heading of Heading.t node
| Html_block of Html_block.t node
| Link_reference_definition of Link_definition.t node
| List of List'.t node
| Paragraph of Paragraph.t node
| Ext_attributes of Attributes.t node
| Thematic_break of Thematic_break.t node

let empty = Blocks ([], Meta.none)

(* Extensions *)

module Table = struct
  type align = [ `Left | `Center | `Right ]
  type sep = align option * Layout.count
  type cell_layout = Layout.blanks * Layout.blanks
  type row =
  [ `Header of (Inline.t * cell_layout) list
  | `Sep of sep node list
  | `Data of (Inline.t * cell_layout) list ]

  type t =
    { indent : Layout.indent;
      col_count : int;
      rows : (row node * Layout.blanks) list }

  let col_count rows =
    let rec loop c = function
    | (((`Header cols | `Data cols), _), _) :: rs ->
        loop (Int.max (List.length cols) c) rs
    | (((`Sep cols), _), _) :: rs ->
        loop (Int.max (List.length cols) c) rs
    | [] -> c
    in
    loop 0 rows

  let make ?(indent = 0) rows = { indent; col_count = col_count rows; rows }
  let indent t = t.indent
  let col_count t = t.col_count
  let rows t = t.rows

  let parse_sep_row cs =
    let rec loop acc = function
    | [] -> Some (List.rev acc)
    | (Inline.Text (s, meta), ("", "")) :: cs ->
        if s = "" then None else
        let max = String.length s - 1 in
        let first_colon = s.[0] = ':' and  last_colon = s.[max] = ':' in
        let first = if first_colon then 1 else 0 in
        let last = if last_colon then max - 1 else max in
        begin
          match
            for i = first to last do if s.[i] <> '-' then raise Exit; done
          with
          | exception Exit -> None
          | () ->
              let count = last - first + 1 in
              let sep = match first_colon, last_colon with
              | false, false -> None
              | true, true -> Some `Center
              | true, false -> Some `Left
              | false, true -> Some `Right
              in
              loop (((sep, count), meta) :: acc) cs
        end
    | _ -> None
    in
    loop [] cs
end

module Footnote = struct
  type nonrec t =
    { indent : Layout.indent;
      label : Label.t;
      defined_label : Label.t option;
      block : t }

  let make ?(indent = 0) ?defined_label:d label block =
    let defined_label = match d with None -> Some label | Some d -> d in
    { indent; label; defined_label; block }

  let indent fn = fn.indent
  let label fn = fn.label
  let defined_label fn = fn.defined_label
  let block fn = fn.block

  type Label.def += Def of t node
  let stub label defined_label =
    Def ({ indent = 0; label; defined_label; block = empty}, Meta.none)
end

type t +=
| Ext_math_block of Code_block.t node
| Ext_table of Table.t node
| Ext_footnote_definition of Footnote.t node

(* Functions on blocks *)

let err_unknown = "Unknown Cmarkit.Block.t type extension"

let ext_none _ = invalid_arg err_unknown
let meta ?(ext = ext_none) = function
| Blank_line (_, m) | Block_quote (_, m) | Blocks (_, m) | Code_block (_, m)
| Heading (_, m) | Html_block (_, m) | Link_reference_definition (_, m)
| List (_, m) | Paragraph (_, m) | Thematic_break (_, m)
| Ext_math_block (_, m) | Ext_table (_, m)
| Ext_attributes (_, m)
| Ext_footnote_definition (_, m) -> m
| b -> ext b
[@@@ocamlformat "enable"]
(* Oymarkit: merging adjacent same-kind lists in [normalize].

   Two sibling [List]s of the same "kind" (same bullet character, or same
   ordered delimiter -- the ordered start number is irrelevant) separated by
   nothing but [Blank_line]s have no syntactic witness as two lists: the parser
   always fuses them into one (the blank lines only make the result loose). A
   [List] is a pure grouping container for its items -- the syntax lives in the
   item markers, not the list -- exactly as [Blocks] is for blocks, so this is
   the same representational freedom [normalize] already removes for nested and
   singleton [Blocks]. See doc/same-content-principle.md.

   Gated behind the [OYMARKIT_DISABLE_MERGE_ADJACENT_LISTS] environment variable
   (set to [1]/[true]/[yes]/[on], case-insensitive) so it can be toggled without
   recompiling. When unset this is the identity and [normalize] keeps cmarkit's
   original behaviour. *)

let merge_adjacent_lists_env = "OYMARKIT_DISABLE_MERGE_ADJACENT_LISTS"

let merge_adjacent_lists_enabled () =
  match
    Option.map String.lowercase_ascii (Sys.getenv_opt merge_adjacent_lists_env)
  with
  | Some ("1" | "true" | "yes" | "on") -> false
  | _ -> true

(* The merge "kind": ordered lists merge on their delimiter, unordered on their
   bullet; the ordered start number does not affect merging. *)
let list_kind : List'.type' -> [ `U of char | `O of char ] = function
  | `Unordered c -> `U c
  | `Ordered (_, c) -> `O c

(* Append [blanks] (the [Blank_line]s that sat between two fused lists) into the
   last item's content, reproducing the loose-list shape the parser emits for
   blank-separated items. No-op when [blanks] is empty (a gap-free fuse stays
   tight). *)
let push_blanks_into_last_item items blanks =
  match blanks with
  | [] -> items
  | _ -> (
      match List.rev items with
      | [] -> items
      | (last, m) :: rev_init ->
          let block =
            match last.List_item.block with
            | Blocks (bs, bm) -> Blocks (bs @ blanks, bm)
            | b -> Blocks (b :: blanks, Meta.none)
          in
          List.rev (({ last with List_item.block }, m) :: rev_init))

(* Fuse every maximal run of same-kind [List]s separated only by [Blank_line]s.
   Operates on an already-flattened, normalised block list. *)
let rec merge_adjacent_lists = function
  | List (l, m) :: rest ->
      let l, rest = absorb_following_lists l rest in
      List (l, m) :: merge_adjacent_lists rest
  | b :: rest -> b :: merge_adjacent_lists rest
  | [] -> []

and absorb_following_lists l rest =
  let kind = list_kind l.List'.type' in
  let rec span_blanks acc = function
    | (Blank_line _ as bl) :: bs -> span_blanks (bl :: acc) bs
    | bs -> (List.rev acc, bs)
  in
  match span_blanks [] rest with
  | blanks, List (l2, _) :: rest when list_kind l2.List'.type' = kind ->
      let items =
        push_blanks_into_last_item l.List'.items blanks @ l2.List'.items
      in
      let tight = l.List'.tight && l2.List'.tight && blanks = [] in
      absorb_following_lists { l with List'.items; tight } rest
  | _ -> (l, rest)

[@@@ocamlformat "disable"]

let rec normalize ?(ext = ext_none) = function
| Blank_line _ | Code_block _ | Heading _ | Html_block _
| Link_reference_definition _ | Paragraph _ | Thematic_break _
| Blocks ([], _) | Ext_math_block _ | Ext_table _ as b -> b
| Block_quote (b, m) ->
    let b = { b with block = normalize ~ext b.block } in
    Block_quote (b, m)
| List (l, m) ->
    let item (i, meta) =
      let block = List_item.block i in
      { i with List_item.block = normalize ~ext block }, meta
    in
    List ({ l with items = List.map item l.items }, m)
| Blocks (b :: bs, m) ->
    let rec loop acc = function
    | Blocks (bs', m) :: bs -> loop acc (List.rev_append (List.rev bs') bs)
    | b :: bs -> loop (normalize ~ext b :: acc) bs
    | [] -> List.rev acc
    in
    (* Route the head through [loop] too: pre-normalizing it and seeding it
       whole leaves a multi-element nested [Blocks] at head position un-spliced,
       violating the "no [Blocks _] case" contract (e.g.
       [Blocks [Blocks [x; y]; z]] would keep its inner [Blocks]). *)
    let bs = loop [] (b :: bs) in
    let bs = if merge_adjacent_lists_enabled () then merge_adjacent_lists bs else bs in
    (match bs with [b] -> b | _ -> Blocks (bs, m))
| Ext_footnote_definition (fn, m) ->
    let fn = { fn with block = normalize ~ext fn.block } in
    Ext_footnote_definition (fn, m)
| Ext_attributes (a, m) ->
    Ext_attributes (Attributes.make ~specs:a.specs (normalize ~ext a.block), m)
| b -> ext b

let rec defs
    ?(ext = fun b defs -> invalid_arg err_unknown) ?(init = Label.Map.empty)
  = function
  | Blank_line _ | Code_block _ | Heading _ | Html_block _
  | Paragraph _ | Thematic_break _
  | Ext_math_block _ | Ext_table _ -> init
  | Ext_attributes (a, _) -> defs ~ext ~init a.block
  | Block_quote (b, _) -> defs ~ext ~init (Block_quote.block b)
  | Blocks (bs, _) -> List.fold_left (fun init b -> defs ~ext ~init b) init bs
  | List (l, _) ->
      let add init (i, _) = defs ~ext ~init (List_item.block i) in
      List.fold_left add init l.items
  | Link_reference_definition ld ->
      begin match Link_definition.defined_label (fst ld) with
      | None -> init
      | Some def ->
          Label.Map.add (Label.key def) (Link_definition.Def ld) init
      end
  | Ext_footnote_definition fn ->
      let init = match Footnote.defined_label (fst fn) with
      | None -> init
      | Some def -> Label.Map.add (Label.key def) (Footnote.Def fn) init
      in
      defs ~ext ~init (Footnote.block (fst fn))
  | b -> ext init b
