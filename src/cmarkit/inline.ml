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
  (* [djot] selects djot's rules for reading the content of a verbatim span: how
     its padding spaces are stripped and how its line endings are joined, see
     [code] below. It is a property of the span rather than a renderer option
     because it is decided by how the document was parsed, and every renderer
     that reads the content must read it the same way. *)
  type t =
    { backtick_count : Layout.count;
      code_layout : Block_line.tight list;
      djot : bool }

  let make ?(djot = false) ~backtick_count code_layout =
    { backtick_count; code_layout; djot }

  let min_backtick_count ~min counts =
    let rec loop min = function
    | c :: cs -> if min <> c then min else loop (c + 1) cs | [] -> min
    in
    loop min (List.sort Int.compare counts)

  let of_string ?(meta = Meta.none) ?(djot = false) = function
  | "" -> { backtick_count = 1 ; code_layout = ["", ("", meta)]; djot }
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
      { backtick_count; code_layout; djot }

  let backtick_count cs = cs.backtick_count
  let code_layout cs = cs.code_layout
  let djot cs = cs.djot

  (* Djot strips a padding space only where it is doing work, i.e. only where it
     is what lets the content start or end with a backtick. CommonMark strips one
     from both ends whenever both are there, which also eats the spaces of
     [ ` a ` ]; djot keeps those. The two sides are independent here. *)
  let djot_code s =
    let max = String.length s - 1 in
    let first =
      if max >= 1 && s.[0] = ' ' && s.[1] = '`' then 1 else 0
    in
    let last =
      if max - 1 >= first && s.[max] = ' ' && s.[max - 1] = '`' then max - 1
      else max
    in
    if first > last then "" else String.sub s first (last - first + 1)

  let code cs =
    (* Extract code, see https://spec.commonmark.org/0.31.2/#code-spans *)
    let sp c = Char.equal c ' ' in
    let s = List.map Block_line.tight_to_string cs.code_layout in
    (* CommonMark turns a line ending inside a code span into a space; djot keeps
       the newline. *)
    let s = String.concat (if cs.djot then "\n" else " ") s in
    if s = "" then "" else
    if cs.djot then djot_code s else
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

    (* Djot spells highlight, insert and delete with braces ([ {=x=} ], [ {+x+} ],
       [ {-x-} ]) because the bare delimiters are too common in prose to claim,
       but writes superscript and subscript bare ([ ^x^ ], [ ~x~ ]), braces
       optional. *)
    let djot =
      make ~highlight:Curly_required ~superscript:Curly_optional
        ~subscript:Curly_optional ~inserted:Curly_required
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
[@@@ocamlformat "enable"]

module Smart_punct = struct
  (* Djot smart punctuation. The node keeps enough to reproduce its source: the
     resolved character is a function of [kind], and [marker] records whether a
     quote was written with an explicit brace override ([{"] / ["}]). Without
     [marker] the CommonMark renderer would emit a bare quote whose direction
     could flip when re-parsed, since direction is inferred from context. This
     mirrors the [open_marker]/[close_marker] fields of {!Emphasis}. *)

  type kind =
    | Left_double_quote
    | Right_double_quote
    | Left_single_quote
    | Right_single_quote
    | Ellipsis
    | Em_dash
    | En_dash

  type t = { kind : kind; marker : bool }

  let make ?(marker = false) kind = { kind; marker }
  let kind sp = sp.kind
  let marker sp = sp.marker

  let is_opener = function
    | Left_double_quote
    | Left_single_quote ->
        true
    | _ -> false

  let to_source sp =
    let quote =
      match sp.kind with
      | Left_double_quote
      | Right_double_quote ->
          "\""
      | Left_single_quote
      | Right_single_quote ->
          "'"
      | Ellipsis -> "..."
      | Em_dash -> "---"
      | En_dash -> "--"
    in
    if not sp.marker then quote
    else if is_opener sp.kind then "{" ^ quote
    else quote ^ "}"

  let to_utf_8 sp =
    match sp.kind with
    | Left_double_quote -> "\xE2\x80\x9C" (* U+201C *)
    | Right_double_quote -> "\xE2\x80\x9D" (* U+201D *)
    | Left_single_quote -> "\xE2\x80\x98" (* U+2018 *)
    | Right_single_quote -> "\xE2\x80\x99" (* U+2019 *)
    | Ellipsis -> "\xE2\x80\xA6" (* U+2026 *)
    | Em_dash -> "\xE2\x80\x94" (* U+2014 *)
    | En_dash -> "\xE2\x80\x93" (* U+2013 *)

  (* Djot divides a run of hyphens into em- and en-dashes uniformly if it can,
     preferring em-dashes when either would be uniform: 4 hyphens are two
     en-dashes, 6 are two em-dashes. A lone hyphen is left alone. *)
  let divide_hyphens n =
    if n <= 1 then (0, 0)
    else if n mod 3 = 0 then (n / 3, 0)
    else if n mod 2 = 0 then (0, n / 2)
    else if n mod 3 = 2 then ((n - 2) / 3, 1)
    else ((n - 4) / 3, 2)
end

module Symbol = struct
  (* A djot symbol [ :name: ]. Opaque and self-contained: no inline children,
     never spans a line. Djot renders it literally and leaves any meaning
     (mapping [:smile:] to an emoji, say) to a downstream filter, so we keep
     only the name and no interpretation of it. *)
  type t = { name : string }

  let make name = { name }
  let name s = s.name
  let to_source s = String.concat "" [ ":"; s.name; ":" ]
end

module Jsx_expr = struct
  (* A JSX expression container [ {expr} ]. The braces delimit a span of
     embedded code that is opaque to Markdown: [expr] is kept verbatim (like a
     wikilink's content) and interpreted downstream by the consumer, not here.
     Single line, no inline children. *)
  type t = { expr : string }

  let make expr = { expr }
  let expr j = j.expr
end

module Jsx_element = struct
  (* A JSX element. Two shapes share this node:
     - self-closing [ <Tag attrs... /> ] -> [children = None].
     - inline container [ <Tag attrs...>inlines</Tag> ] -> [children = Some i],
       where [i] is the parsed Markdown inline content between the open and
       close tags (real AST nodes, not opaque text). Fragments [ <>...</> ] are
       containers with [raw = "<>"].
     In both cases [raw] holds the opening (or self-closing) tag source verbatim
     ('<' to its terminating '>' inclusive); the tag/attribute structure is
     parsed downstream by the consumer, not here. Single source line. *)
  type inline = t
  type t = { raw : string; children : inline option }

  let make raw = { raw; children = None }
  let make_container raw children = { raw; children = Some children }
  let raw e = e.raw
  let children e = e.children

  let name e =
    (* Tag name extracted from the opening tag in [raw] (e.g. "Card" from
       "<Card a=1>", "Foo.Bar" from "<Foo.Bar>"); "" for a fragment "<>". *)
    let raw = e.raw in
    let n = String.length raw in
    if n < 2 || raw.[1] = '>' then ""
    else
      let stop c = c = ' ' || c = '\t' || c = '>' || c = '/' in
      let i = ref 1 in
      while !i < n && not (stop raw.[!i]) do
        incr i
      done;
      String.sub raw 1 (!i - 1)

  let close_tag e =
    (* The matching closing tag reconstructed from [raw]'s name: "</Card>", or
       "</>" for a fragment. Used to roundtrip a container to CommonMark. *)
    String.concat "" [ "</"; name e; ">" ]
end

module Wikilink = struct
  (* Obsidian-style wikilinks: [ [[target#fragment|display]] ] and the embed
     form [ ![[...]] ]. Wikilinks have no inline children: the content between
     the brackets is opaque text, parsed into [target]/[fragment]/[display] for
     consumers but kept verbatim in [content] so rendering roundtrips exactly. *)

  type fragment =
    | Heading of string list (* "#h1#h2" -> [ ["h1"; "h2"] ] *)
    | Block_ref of string (* "#^id"   -> "id" *)

  type t = {
    content : string; (* raw text between the brackets, sans '!' *)
    target : string option;
    fragment : fragment option;
    display : string option;
    embed : bool;
  }

  let lsplit2 s ~on =
    match String.index_opt s on with
    | None -> None
    | Some i ->
        Some (String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1))

  let non_empty_hash_parts s =
    List.filter (fun s -> s <> "") (String.split_on_char '#' s)

  let parse_fragment frag =
    (* [frag] is the text after the first '#' *)
    if frag = "" then None
    else if frag.[0] = '^' then
      Some (Block_ref (String.sub frag 1 (String.length frag - 1)))
    else
      match non_empty_hash_parts frag with
      | [] -> None
      | parts -> Some (Heading parts)

  let parse_content content =
    let ref_part, display =
      match lsplit2 content ~on:'|' with
      | Some (r, d) -> (String.trim r, Some (String.trim d))
      | None -> (String.trim content, None)
    in
    let target, fragment =
      match lsplit2 ref_part ~on:'#' with
      | None -> ((if ref_part = "" then None else Some ref_part), None)
      | Some (t, frag) ->
          ((if t = "" then None else Some t), parse_fragment frag)
    in
    (target, fragment, display)

  let make ~embed content =
    let target, fragment, display = parse_content content in
    { content; target; fragment; display; embed }

  let content w = w.content
  let target w = w.target
  let fragment w = w.fragment
  let display w = w.display
  let embed w = w.embed

  let to_commonmark w =
    String.concat "" [ (if w.embed then "![[" else "[["); w.content; "]]" ]

  let to_plain_text w =
    match w.display with
    | Some d -> d
    | None -> (
        match w.target with
        | Some t -> t
        | None -> w.content)
end

module Raw_inline = struct
  (* Djot raw inline: a verbatim span followed by a [ {=format} ] specifier, e.g.
     [ `<a>`{=html} ]. Passed through verbatim by a renderer whose output format
     is [format], dropped by every other one. The [Code_span.t] is kept whole so
     the content and its backtick count survive a render back to djot. *)
  type t = { format : string; code_span : Code_span.t }

  let make ~format code_span = { format; code_span }
  let format r = r.format
  let code_span r = r.code_span
  let code r = Code_span.code r.code_span
end

[@@@ocamlformat "disable"]

type t +=
| Ext_raw_inline of Raw_inline.t node
| Ext_strikethrough of Strikethrough.t node
| Ext_extra_inline_container of Extra_inline_container.t node
| Ext_attributes of Attributes.t node
| Ext_math_span of Math_span.t node
| Ext_smart_punct of Smart_punct.t node
| Ext_symbol of Symbol.t node
| Ext_wikilink of Wikilink.t node
| Ext_jsx_expr of Jsx_expr.t node
| Ext_jsx_element of Jsx_element.t node

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
| Ext_raw_inline (_, m) -> m
| Ext_smart_punct (_, m) -> m
| Ext_symbol (_, m) -> m
| Ext_wikilink (_, m) -> m
| Ext_jsx_expr (_, m) -> m
| Ext_jsx_element (_, m) -> m
| i -> ext i

let rec normalize ?(ext = ext_none) = function
| Autolink _ | Break _ | Code_span _ | Raw_html _ | Text _
| Inlines ([], _) | Ext_math_span _ | Ext_raw_inline _ | Ext_smart_punct _ | Ext_symbol _
| Ext_wikilink _ | Ext_jsx_expr _ as i -> i
| Ext_jsx_element (e, m) ->
    (match Jsx_element.children e with
     | None -> Ext_jsx_element (e, m)
     | Some child ->
         let e = Jsx_element.make_container (Jsx_element.raw e)
                   (normalize ~ext child)
         in
         Ext_jsx_element (e, m))
| Image (l, m) -> Image ({ l with text = normalize ~ext l.text }, m)
| Link (l, m) -> Link ({ l with text = normalize ~ext l.text }, m)
| Emphasis (e, m) ->
    Emphasis ({ e with inline = normalize ~ext e.inline}, m)
| Strong_emphasis (e, m) ->
    Strong_emphasis ({ e with inline = normalize ~ext e.inline}, m)
(* OYMARKIT CHANGE:
   upstream returns the singleton element raw, [| Inlines ([i], _) -> i], which
   does not recurse. If [i] is itself an [Inlines] the result keeps a nested
   (and singleton) [Inlines], violating normalize's contract, and only one layer
   is peeled per pass so normalize is not even idempotent on e.g.
   [Inlines [Inlines [Inlines []]]]. Normalizing the unwrapped element fixes
   both. Reachable via [Link]/[Image] text and [Emphasis] content (the loop path
   splices sub-[Inlines] separately and is unaffected). *)
| Inlines ([i], _) -> normalize ~ext i
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
    (* OYMARKIT CHANGE:
       upstream seeds the accumulator with the pre-normalized head,
       [let is = loop [normalize ~ext i] is in], which bypasses the splice arm
       above. When the head normalizes to an [Inlines] it then survives as a
       nested case, violating normalize's documented contract ("[is] has no
       [Inlines _] case"). We instead push the head back into the work list so
       it flows through the same splice/merge logic as every other element.
       Behaviour is identical for all parser-produced ASTs, which never nest an
       [Inlines] directly inside an [Inlines]; only hand-built ASTs hit the
       difference. *)
    let is = loop [] (i :: is) in
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
  | Ext_raw_inline (r, _) :: is ->
      loop ~break_on_soft (push (Raw_inline.code r) acc) is
  | Ext_smart_punct (sp, _) :: is ->
      loop ~break_on_soft (push (Smart_punct.to_utf_8 sp) acc) is
  | Ext_symbol (s, _) :: is ->
      loop ~break_on_soft (push (Symbol.to_source s) acc) is
  | Ext_wikilink (wl, _) :: is ->
      loop ~break_on_soft (push (Wikilink.to_plain_text wl) acc) is
  | Ext_jsx_expr (j, _) :: is ->
      loop ~break_on_soft (push (Jsx_expr.expr j) acc) is
  | Ext_jsx_element (e, _) :: is ->
      let acc = push (Jsx_element.raw e) acc in
      (match Jsx_element.children e with
       | None -> loop ~break_on_soft acc is
       | Some child -> loop ~break_on_soft acc (child :: is))
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
