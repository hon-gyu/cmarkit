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

  (* Djot raw block: the whole info string is [=format], e.g. [ ```=html ]. The
     format is the info string's only word, so [=html extra] is not one — it is
     an ordinary code block with a funny language. *)
  let raw_format_of_info_string = function
  | None -> None
  | Some (i, _) ->
      match language_of_info_string i with
      | Some (word, "") when String.length word > 1 && word.[0] = '=' ->
          Some (String.sub word 1 (String.length word - 1))
      | Some _ | None -> None
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
  (* Djot ordered list styles. CommonMark only counts in decimal and only
     delimits with [.] or [)], which stays [`Ordered]; the djot-only styles and
     the fully parenthesized delimiter go through [`Ext_ordered] so a document
     parsed without the knob keeps exactly the AST it had. *)
  type ordered_style =
  [ `Decimal | `Alpha_lower | `Alpha_upper | `Roman_lower | `Roman_upper ]

  type ordered_delim = [ `Period (* [1.] *) | `Paren (* [1)] *)
                       | `Parens (* [(1)] *) ]

  type type' =
  [ `Unordered of Layout.char
  | `Ordered of int * Layout.char
  | `Ext_ordered of ordered_style * ordered_delim * int ]

  (* Number to marker text, e.g. [4] as [`Roman_lower] is ["iv"]. Alpha runs out
     at [z] and roman has no zero, so out-of-range numbers fall back to decimal
     rather than inventing a spelling. *)
  let alpha_of_int ~upper n =
    if n < 1 || n > 26 then Int.to_string n else
    let base = if upper then Char.code 'A' else Char.code 'a' in
    String.make 1 (Char.chr (base + n - 1))

  let roman_of_int ~upper n =
    if n < 1 || n > 3999 then Int.to_string n else
    let digits =
      [ 1000, "m"; 900, "cm"; 500, "d"; 400, "cd"; 100, "c"; 90, "xc";
        50, "l"; 40, "xl"; 10, "x"; 9, "ix"; 5, "v"; 4, "iv"; 1, "i" ]
    in
    let b = Buffer.create 8 in
    let rec loop n = function
    | [] -> ()
    | (v, s) :: ds when n >= v -> Buffer.add_string b s; loop (n - v) ((v, s) :: ds)
    | _ :: ds -> loop n ds
    in
    loop n digits;
    let s = Buffer.contents b in
    if upper then String.uppercase_ascii s else s

  let ordered_number style n = match style with
  | `Decimal -> Int.to_string n
  | `Alpha_lower -> alpha_of_int ~upper:false n
  | `Alpha_upper -> alpha_of_int ~upper:true n
  | `Roman_lower -> roman_of_int ~upper:false n
  | `Roman_upper -> roman_of_int ~upper:true n

  let ordered_marker style delim n =
    let n = ordered_number style n in
    match delim with
    | `Period -> n ^ "."
    | `Paren -> n ^ ")"
    | `Parens -> "(" ^ n ^ ")"

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

(* Alias for the open block type so the [Callout] module below can refer to it
   while shadowing [t] with the callout metadata type. *)
type block = t

module Callout = struct
  (* Obsidian callouts. A callout is a blockquote whose first line is a
     [\[!kind\](+|-)? title] header. We do not introduce a new block: the node
     stays a {!Block_quote} and a [t] is attached to its [Meta.t] (cf.
     {!Block_id}). The header line is kept in the body so CommonMark roundtrips
     verbatim; renderers strip it with {!strip_header}. *)

  type fold = Foldable_open | Foldable_closed

  (* [kind] and [fold] are the decoded interpretation of the header; the title
     is *not* stored — it lives in the block-quote body (the header line, kept
     for roundtrip) and is the single source of truth, derived on demand by
     {!title}. This keeps the title a true inline container that mappers
     traverse, with no duplicated copy to fall out of sync. *)
  type t = { kind : string; fold : fold option }

  module Config = struct
    type kinds = Any | Only of string list
    type t = { enabled : bool; kinds : kinds }
    let make ?(kinds = Any) () = { enabled = true; kinds }
    let disabled = { enabled = false; kinds = Any }
    let enabled c = c.enabled
    let kinds c = c.kinds
  end

  let key : t Meta.key = Meta.key ()
  let make ?fold kind = { kind; fold }
  let kind c = c.kind
  let fold c = c.fold
  let find meta = Meta.find key meta
  let add t meta = Meta.add key t meta

  let is_kind_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' -> true | _ -> false

  let kind_allowed (config : Config.t) kind = match Config.kinds config with
  | Config.Any -> true
  | Config.Only kinds -> List.mem kind kinds

  (* Parse a [\[!kind\](+|-)?] header from the leading text of a blockquote's
     first paragraph. Only [kind] (lowercased, validated) and [fold] are
     decoded here; the title is left in the body and recovered by {!title}. *)
  let parse_header (config : Config.t) (s : string) : t option =
    let len = String.length s in
    if len < 4 || s.[0] <> '[' || s.[1] <> '!' then None else
    match String.index_opt s ']' with
    | None -> None
    | Some close ->
        let kind = String.lowercase_ascii (String.sub s 2 (close - 2)) in
        if kind = "" || not (String.for_all is_kind_char kind) then None else
        if not (kind_allowed config kind) then None else
        let after = close + 1 in
        let fold =
          if after < len && s.[after] = '+' then Some Foldable_open
          else if after < len && s.[after] = '-' then Some Foldable_closed
          else None
        in
        Some { kind; fold }

  let rec leading_text (inline : Inline.t) : string option = match inline with
  | Inline.Text (s, _) -> Some s
  | Inline.Inlines (i :: _, _) -> leading_text i
  | _ -> None

  (* [detect config inner] is the callout described by the header on the first
     line of blockquote content [inner], if any. *)
  let detect (config : Config.t) (inner : block) : t option =
    if not (Config.enabled config) then None else
    let first_inline = match inner with
    | Paragraph (p, _) -> Some (Paragraph.inline p)
    | Blocks (Paragraph (p, _) :: _, _) -> Some (Paragraph.inline p)
    | _ -> None
    in
    match first_inline with
    | None -> None
    | Some inline ->
        match leading_text inline with
        | None -> None
        | Some text -> parse_header config text

  (* Drop everything in [inline] up to and including the first line [Break].
     [None] if there is no break (the whole inline is the header line) or
     nothing remains after it. Loc-free and does not re-parse. *)
  let strip_first_line (inline : Inline.t) : Inline.t option =
    let rec drop = function
    | Inline.Break _ :: rest -> Some rest
    | _ :: rest -> drop rest
    | [] -> None
    in
    match inline with
    | Inline.Inlines (is, m) ->
        (match drop is with
         | None | Some [] -> None
         | Some [i] -> Some i
         | Some is -> Some (Inline.Inlines (is, m)))
    | _ -> None (* single inline = the whole header line, nothing follows *)

  (* [strip_header inner] is the callout body: [inner] with its first line (the
     [\[!kind\]...] header) removed. Used by renderers. *)
  let strip_header (inner : block) : block =
    let strip_para p meta rest =
      match strip_first_line (Paragraph.inline p) with
      | Some inline ->
          let p' =
            Paragraph.make ~leading_indent:(Paragraph.leading_indent p)
              ~trailing_blanks:(Paragraph.trailing_blanks p) inline
          in
          (match rest with
           | [] -> Paragraph (p', meta)
           | _ -> Blocks (Paragraph (p', meta) :: rest, Meta.none))
      | None ->
          (match rest with
           | [] -> empty
           | [single] -> single
           | _ -> Blocks (rest, Meta.none))
    in
    match inner with
    | Paragraph (p, meta) -> strip_para p meta []
    | Blocks (Paragraph (p, meta) :: rest, _) -> strip_para p meta rest
    | other -> other

  (* Byte length of the [\[!kind\](+|-)?] prefix in the (cleaned) leading text
     node. [kind] preserves the source length under lowercasing, so this
     relocates the prefix structurally without a stored offset. *)
  let prefix_len (c : t) =
    2 (* "[!" *) + String.length c.kind + 1 (* "]" *)
    + (match c.fold with None -> 0 | Some _ -> 1)

  let first_para_inline (inner : block) : Inline.t option = match inner with
  | Paragraph (p, _) -> Some (Paragraph.inline p)
  | Blocks (Paragraph (p, _) :: _, _) -> Some (Paragraph.inline p)
  | _ -> None

  (* The header line: inlines up to the first line [Break]. *)
  let header_line (inline : Inline.t) : Inline.t list =
    let rec take acc = function
    | Inline.Break _ :: _ -> List.rev acc
    | i :: rest -> take (i :: acc) rest
    | [] -> List.rev acc
    in
    match inline with
    | Inline.Inlines (is, _) -> take [] is
    | i -> [i]

  (* [title c inner] is the callout title as inline content: the header line
     with the [\[!kind\](+|-)?] prefix and following blanks dropped. [None] for
     a title-only-default callout (e.g. [\[!tip\]]). Inline formatting, links
     and wikilinks in the title are preserved (they remain shared with the
     body, so mappers transform them in place). *)
  let title (c : t) (inner : block) : Inline.t option =
    match first_para_inline inner with
    | None -> None
    | Some inline ->
        match header_line inline with
        | Inline.Text (s, m) :: rest ->
            let len = String.length s in
            let k = ref (min (prefix_len c) len) in
            while !k < len && (s.[!k] = ' ' || s.[!k] = '\t') do incr k done;
            let head =
              if !k >= len then [] else [Inline.Text (String.sub s !k (len - !k), m)]
            in
            (match head @ rest with
             | [] -> None
             | [i] -> Some i
             | is -> Some (Inline.Inlines (is, Meta.none)))
        | _ -> None
end

module Table = struct
  type align = [ `Left | `Center | `Right ]
  type sep = align option * Layout.count
  type cell_layout = Layout.blanks * Layout.blanks
  type row =
  [ `Header of (Inline.t * cell_layout) list
  | `Sep of sep node list
  | `Data of (Inline.t * cell_layout) list ]

  (* Djot table caption: a [^ text] line after the table, its continuation lines
     indented. It is inline content attached to the table rather than a row: it
     is not part of the grid and has no cells. *)
  type caption = { caption_indent : Layout.indent; inline : Inline.t }

  type t =
    { indent : Layout.indent;
      col_count : int;
      rows : (row node * Layout.blanks) list;
      caption : caption node option }

  let col_count rows =
    let rec loop c = function
    | (((`Header cols | `Data cols), _), _) :: rs ->
        loop (Int.max (List.length cols) c) rs
    | (((`Sep cols), _), _) :: rs ->
        loop (Int.max (List.length cols) c) rs
    | [] -> c
    in
    loop 0 rows

  let make ?(indent = 0) ?caption rows =
    { indent; col_count = col_count rows; rows; caption }

  let make_caption ?(caption_indent = 0) inline = { caption_indent; inline }
  let caption_indent c = c.caption_indent
  let caption_inline c = c.inline

  let indent t = t.indent
  let col_count t = t.col_count
  let rows t = t.rows
  let caption t = t.caption

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

module Div = struct
  type nonrec t =
    { indent : Layout.indent;
      opening_fence : Layout.string node;
      class' : string node option;
      closing_fence : Layout.string node option;
      block : t; }

  let make
      ?(indent = 0) ?(opening_fence = Layout.empty) ?class'
      ?(closing_fence = Some Layout.empty) block
    =
    { indent; opening_fence; class'; closing_fence; block }

  let indent d = d.indent
  let opening_fence d = d.opening_fence
  let class' d = d.class'
  let closing_fence d = d.closing_fence
  let block d = d.block
end

module Jsx_block = struct
  (* A JSX container element at block level:
     [ <Card ...> ] alone on a line, Markdown blocks, [ </Card> ] on a line.
     [raw_open] is the opening tag ("<Card ...>" or the fragment "<>") verbatim;
     [block] is the parsed Markdown block children; [raw_close] is the closing
     tag ("</Card>" or "</>") when the container was properly closed, [None] for
     an unterminated container closed by a document/parent boundary. Attributes
     stay raw in [raw_open]; downstream re-parses them (positions recoverable by
     offset into the node's span). *)
  type nonrec t =
    { indent : Layout.indent;
      raw_open : string node;
      block : t;
      raw_close : string node option }

  let make ?(indent = 0) ~raw_open ?raw_close block =
    { indent; raw_open; block; raw_close }

  let indent j = j.indent
  let raw_open j = j.raw_open
  let block j = j.block
  let raw_close j = j.raw_close
end

module Definition_list = struct
  (* Djot definition list. An item is a [: term] line followed by the definition
     blocks, which are indented under it. Unlike a list item, the marker line
     carries content (the term), so the term is inline and the definition is the
     block child — they are not the same kind of thing and do not share a node. *)
  type block = t

  type item =
    { before_marker : Layout.indent;
      marker : Layout.string node;
      after_marker : Layout.indent;
      term : Inline.t;
      definition : block }

  type t = { tight : bool; items : item node list }

  let make_item ?(before_marker = 0) ?(marker = Layout.empty)
      ?(after_marker = 1) ~term definition
    =
    { before_marker; marker; after_marker; term; definition }

  let make ?(tight = true) items = { tight; items }

  let tight d = d.tight
  let items d = d.items
  let item_before_marker i = i.before_marker
  let item_marker i = i.marker
  let item_after_marker i = i.after_marker
  let item_term i = i.term
  let item_definition i = i.definition
end

module Raw_block = struct
  (* Djot raw block: a code fence whose info string is [=format]. The content is
     passed through verbatim by a renderer whose output format is [format] and
     dropped by every other one. The [Code_block.t] is kept whole, info string
     included, so layout and rendering back to djot need nothing special. *)
  type t = { format : string; code_block : Code_block.t }

  let make ~format code_block = { format; code_block }
  let format r = r.format
  let code_block r = r.code_block
end

type t +=
| Ext_math_block of Code_block.t node
| Ext_definition_list of Definition_list.t node
| Ext_raw_block of Raw_block.t node
| Ext_table of Table.t node
| Ext_footnote_definition of Footnote.t node
| Ext_div of Div.t node
| Ext_jsx_block of Jsx_block.t node
| Ext_keyed of (Inline.t * t) node

(* Functions on blocks *)

let err_unknown = "Unknown Cmarkit.Block.t type extension"

let ext_none _ = invalid_arg err_unknown
let meta ?(ext = ext_none) = function
| Blank_line (_, m) | Block_quote (_, m) | Blocks (_, m) | Code_block (_, m)
| Heading (_, m) | Html_block (_, m) | Link_reference_definition (_, m)
| List (_, m) | Paragraph (_, m) | Thematic_break (_, m)
| Ext_math_block (_, m) | Ext_raw_block (_, m) | Ext_table (_, m)
| Ext_definition_list (_, m)
| Ext_attributes (_, m)
| Ext_div (_, m)
| Ext_jsx_block (_, m)
| Ext_keyed (_, m)
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
let list_kind :
  List'.type' ->
  [ `U of char | `O of char
  | `Ext_o of List'.ordered_style * List'.ordered_delim ] = function
  | `Unordered c -> `U c
  | `Ordered (_, c) -> `O c
  (* A style change starts a new list in djot, so the style is part of the kind
     two lists must share to merge. *)
  | `Ext_ordered (style, delim, _) -> `Ext_o (style, delim)

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
[@@@ocamlformat "enable"]
(* Oymarkit: Indented code blocks are the only [Code_block]s the parser coalesces: two
   adjacent ones (separated only by blank lines) are a single block whose content
   includes those blanks as empty lines. Fenced blocks are self-delimiting and
   never fuse, so the merge only touches [`Indented] ones. Same
   representational-freedom family as adjacent-list merging (see
   doc/same-content-principle.md), gated by the parallel
   [OYMARKIT_DISABLE_MERGE_ADJACENT_CODE] environment variable. *)

let merge_adjacent_code_env = "OYMARKIT_DISABLE_MERGE_ADJACENT_CODE"

let merge_adjacent_indented_cb_enabled () =
  match
    Option.map String.lowercase_ascii (Sys.getenv_opt merge_adjacent_code_env)
  with
  | Some ("1" | "true" | "yes" | "on") -> false
  | _ -> true

let cb_is_indented cb =
  match cb.Code_block.layout with
  | `Indented -> true
  | `Fenced _ -> false

(* Each blank line between two fused indented code blocks becomes one empty
   content line, reproducing the parser's [[a; ""; b]] shape for [    a\n\n    b]. *)
let blank_code_lines blanks = List.map (fun _ -> ("", Meta.none)) blanks

(* Fuse every maximal run of indented [Code_block]s separated only by
   [Blank_line]s. Operates on an already-flattened, normalised block list. *)
let rec merge_adjacent_indented_cb = function
  | Code_block (cb, m) :: rest when cb_is_indented cb ->
      let cb, rest = absorb_following_code cb rest in
      Code_block (cb, m) :: merge_adjacent_indented_cb rest
  | b :: rest -> b :: merge_adjacent_indented_cb rest
  | [] -> []

and absorb_following_code cb rest =
  let rec span_blanks acc = function
    | (Blank_line _ as bl) :: bs -> span_blanks (bl :: acc) bs
    | bs -> (List.rev acc, bs)
  in
  match span_blanks [] rest with
  | blanks, Code_block (cb2, _) :: rest when cb_is_indented cb2 ->
      let code =
        cb.Code_block.code @ blank_code_lines blanks @ cb2.Code_block.code
      in
      absorb_following_code { cb with Code_block.code } rest
  | _ -> (cb, rest)

[@@@ocamlformat "disable"]

let rec normalize ?(ext = ext_none) = function
| Blank_line _ | Code_block _ | Heading _ | Html_block _
| Link_reference_definition _ | Paragraph _ | Thematic_break _
| Blocks ([], _) | Ext_math_block _ | Ext_raw_block _ | Ext_table _ as b -> b
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
    let bs = if merge_adjacent_indented_cb_enabled () then merge_adjacent_indented_cb bs else bs in
    (match bs with [b] -> b | _ -> Blocks (bs, m))
| Ext_definition_list (d, m) ->
    let item (i, meta) =
      { i with Definition_list.definition =
                 normalize ~ext i.Definition_list.definition }, meta
    in
    Ext_definition_list ({ d with items = List.map item d.items }, m)
| Ext_footnote_definition (fn, m) ->
    let fn = { fn with block = normalize ~ext fn.block } in
    Ext_footnote_definition (fn, m)
| Ext_div (d, m) ->
    Ext_div ({ d with block = normalize ~ext d.block }, m)
| Ext_jsx_block (j, m) ->
    Ext_jsx_block ({ j with block = normalize ~ext j.block }, m)
| Ext_attributes (a, m) ->
    Ext_attributes (Attributes.make ~specs:a.specs (normalize ~ext a.block), m)
| Ext_keyed ((l, b), m) ->
    Ext_keyed ((l, normalize ~ext b), m)
| b -> ext b

let rec defs
    ?(ext = fun b defs -> invalid_arg err_unknown) ?(init = Label.Map.empty)
  = function
  | Blank_line _ | Code_block _ | Heading _ | Html_block _
  | Paragraph _ | Thematic_break _
  | Ext_math_block _ | Ext_raw_block _ | Ext_table _ -> init
  | Ext_attributes (a, _) -> defs ~ext ~init a.block
  | Ext_definition_list (d, _) ->
      let item init (i, _) = defs ~ext ~init i.Definition_list.definition in
      List.fold_left item init d.items
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
  | Ext_div (d, _) -> defs ~ext ~init (Div.block d)
  | Ext_jsx_block (j, _) -> defs ~ext ~init (Jsx_block.block j)
  | Ext_keyed ((_, b), _) ->
      defs ~ext ~init b
  | b -> ext init b
