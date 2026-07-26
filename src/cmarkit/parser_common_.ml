open Common_

(** {1 Global flags for enabling/disabling Oymarkit-related features}
    We tend to gate our modifications behind this flag so that we can keep a
    clear distinction between original code and our features. *)

let enable_oymarkit_ = ref true
let is_oymarkit_enabled () = !enable_oymarkit_
let set_enable_oymarkit b = enable_oymarkit_ := b

(** {1 Cmarkit original code} *)

[@@@ocamlformat "disable"]

(*---------------------------------------------------------------------------
   Copyright (c) 2021 The cmarkit programmers. All rights reserved.
   Copyright (c) 2026 The oymarkit programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)


(* Closer indexes.

   They map closing delimiters to the position where they
   start. Shortcuts forward searches in inline parsing. See
   Inline_struct. *)

module Pos_set = Set.Make (Int) (* Sets of positions. *)
module Pos_map = Map.Make (Int) (* Maps keyed by positions. *)
module Id_set = Set.Make (String) (* Sets of identifiers. *)
module Closer = struct
  type t =
  | Backticks of int (* run length *)
  | Right_brack
  | Right_paren (* Only for ruling out pathological cases. *)
  | Emphasis_marks of char
  | Extra_inline_container_marks of char * bool
  | Quoted_marks of char * bool
  | Strikethrough_marks
  | Math_span_marks of int (* run length *)

  let compare = Stdlib.compare
end

module Closer_index = struct
  include Map.Make (Closer)
  type nonrec t = Pos_set.t t

  let add cl pos cidx =
    let add = function
    | None -> Some (Pos_set.singleton pos)
    | Some occs -> Some (Pos_set.add pos occs)
    in
    update cl add cidx

  let closer_pos cl ~after cidx = match find_opt cl cidx with
  | None -> None
  | Some occs -> Pos_set.find_first_opt (fun pos -> pos > after) occs

  let closer_exists cl ~after cidx = match closer_pos cl ~after cidx with
  | None -> false | Some _ -> true
end

(* Columns. That notion is needed to handle tab stops.
   See https://spec.commonmark.org/current/#tabs *)

type col = int
let[@inline] next_tab_stop col = (col + 4) land (lnot 3)
[@@@ocamlformat "enable"]

(** Module for centralizing Oymarkit-related modifications *)
module Oymarkit_mod = struct
  type delim_set = { star : bool; underscore : bool }
  type list_indent = [ `Content_column | `Marker_plus_one ]
  type list_tightness = [ `Any_blank | `Non_list_boundary_blank ]
  type verbatim_style = [ `Code_span | `Verbatim_span ]

  type t = {
    emphasis_delims : delim_set;
    strong_emphasis_delims : delim_set;
    intraword_emphasis : bool;
    marked_emphasis_delims : bool;
    strong_emphasis_width : int;
    extra_inline_containers : Inline.Extra_inline_container.Config.t;
    block_id : bool;
    inline_attributes : bool;
    block_attributes : bool;
    underscore_thematic_break : bool;
    colon_symbols : bool;
    backslash_space_nbsp : bool;
    hard_break_trailing_blanks : bool;
    two_space_hard_break : bool;
    format_raw_content : bool;
    extended_ordered_list_styles : bool;
    definition_lists : bool;
    backtick_math : bool;
    table_captions : bool;
    verbatim_style : verbatim_style;
    multiline_atx_headings : bool;
    atx_closing_sequence : bool;
    heading_implicit_targets : bool;
    djot_links : bool;
    case_sensitive_labels : bool;
    blocks_interrupt_paragraph : bool;
    list_marker_interrupts_paragraph : bool;
    list_indent : list_indent;
    list_tightness : list_tightness;
    simple_emphasis_flanking : bool;
    smart_punctuation : bool;
    indented_code : bool;
    setext_headings : bool;
    lazy_continuation : bool;
    raw_html : bool;
    entity_refs : bool;
    tilde_code_fences : bool;
    whitespace_free_info_string : bool;
    block_quote_marker_space : bool;
    div : bool;
    wikilink : bool;
    jsx_expr : bool;
    jsx_element : bool;
    callout : Block.Callout.Config.t;
  }

  type emphasis_role = Any | Opener_only | Closer_only
  type emphasis_match = { used : int; strong : bool }

  let parse_emph_delims (delims : char list) : (delim_set, string) result =
    let exception Early_return of string in
    if delims = [] then Error "delims is empty"
    else
      let rec loop star underscore = function
        | [] -> { star; underscore }
        | '*' :: cs -> loop true underscore cs
        | '_' :: cs -> loop star true cs
        | invalid_char :: _ ->
            raise
              (Early_return
                 (Printf.sprintf "must only contain '*' or '_', got '%c'"
                    invalid_char))
      in
      try Ok (loop false false delims) with
      | Early_return msg -> Error msg

  let make ~emphasis_delims ~strong_emphasis_delims ~intraword_emphasis
      ~marked_emphasis_delims ~strong_emphasis_width ~extra_inline_containers
      ~block_id ~inline_attributes ~block_attributes ~underscore_thematic_break
      ~colon_symbols ~backslash_space_nbsp ~hard_break_trailing_blanks
      ~two_space_hard_break ~format_raw_content ~extended_ordered_list_styles
      ~definition_lists ~backtick_math ~table_captions ~verbatim_style
      ~multiline_atx_headings ~atx_closing_sequence ~heading_implicit_targets
      ~djot_links ~case_sensitive_labels ~simple_emphasis_flanking
      ~blocks_interrupt_paragraph ~list_marker_interrupts_paragraph ~list_indent
      ~list_tightness ~smart_punctuation ~indented_code ~setext_headings
      ~lazy_continuation ~raw_html ~entity_refs ~tilde_code_fences
      ~whitespace_free_info_string ~block_quote_marker_space ~div ~wikilink
      ~jsx_expr ~jsx_element ~callout =
    let emphasis_delims =
      match parse_emph_delims emphasis_delims with
      | Ok delims -> delims
      | Error msg -> failwith (Printf.sprintf "emphasis_delims: %s" msg)
    in
    let strong_emphasis_delims =
      match parse_emph_delims strong_emphasis_delims with
      | Ok delims -> delims
      | Error msg -> failwith (Printf.sprintf "strong_emphasis_delims: %s" msg)
    in
    if strong_emphasis_width <> 1 && strong_emphasis_width <> 2 then
      failwith "strong_emphasis_width: must be 1 or 2";
    {
      emphasis_delims;
      strong_emphasis_delims;
      intraword_emphasis;
      marked_emphasis_delims;
      strong_emphasis_width;
      extra_inline_containers;
      block_id;
      inline_attributes;
      block_attributes;
      underscore_thematic_break;
      colon_symbols;
      backslash_space_nbsp;
      hard_break_trailing_blanks;
      two_space_hard_break;
      format_raw_content;
      extended_ordered_list_styles;
      definition_lists;
      backtick_math;
      table_captions;
      verbatim_style;
      multiline_atx_headings;
      atx_closing_sequence;
      heading_implicit_targets;
      djot_links;
      case_sensitive_labels;
      blocks_interrupt_paragraph;
      list_marker_interrupts_paragraph;
      list_indent;
      list_tightness;
      simple_emphasis_flanking;
      smart_punctuation;
      indented_code;
      setext_headings;
      lazy_continuation;
      raw_html;
      entity_refs;
      tilde_code_fences;
      whitespace_free_info_string;
      block_quote_marker_space;
      div;
      wikilink;
      jsx_expr;
      jsx_element;
      callout;
    }

  let delim_allowed delims = function
    | '*' -> delims.star
    | '_' -> delims.underscore
    | _ -> false

  (* Delimiter knobs restrict the number of characters that may be consumed
     from a matching delimiter run. If strong emphasis is disallowed for the
     character but emphasis is allowed, a run such as [__x__] falls back to
     consuming one delimiter on each side, leaving the remaining pair to parse
     as nested emphasis. *)
  let emphasis_match_used t ~char ~opener_count ~closer_count =
    if
      closer_count >= t.strong_emphasis_width
      && opener_count >= t.strong_emphasis_width
      && delim_allowed t.strong_emphasis_delims char
    then Some { used = t.strong_emphasis_width; strong = true }
    else if delim_allowed t.emphasis_delims char then
      Some { used = 1; strong = false }
    else None

  let emphasis_may_open_close t ~role ~char ~is_left_flanking ~is_right_flanking
      ~prev_white ~next_white ~prev_punct ~next_punct =
    (* Djot has no flanking classification at all: a delimiter may open if it is
       not followed by whitespace and may close if it is not preceded by
       whitespace, and [_] plays by the same rules as [*] (CommonMark gives [_]
       extra punctuation clauses to keep [snake_case] intact — djot leaves that
       to [intraword_emphasis], which stays orthogonal to this knob). A run may
       therefore both open and close; which it does is settled by matching. *)
    let may_open, may_close =
      if t.simple_emphasis_flanking then (not next_white, not prev_white)
      else
        let may_open =
          (char = '*' && is_left_flanking)
          || char = '_' && is_left_flanking
             && ((not is_right_flanking) || prev_punct)
        in
        let may_close =
          (char = '*' && is_right_flanking)
          || char = '_' && is_right_flanking
             && ((not is_left_flanking) || next_punct)
        in
        (may_open, may_close)
    in
    let may_open, may_close =
      if t.intraword_emphasis then (may_open, may_close)
      else
        let intraword =
          not (prev_white || next_white || prev_punct || next_punct)
        in
        if intraword then (false, false) else (may_open, may_close)
    in
    (* An explicit marker ([{_]/[_}]) declares the delimiter's role outright, so
       it forces [may_open]/[may_close] regardless of flanking and the intraword
       knob: those rules exist to disambiguate bare runs, and a marker has
       removed the ambiguity. This is the Djot model — the marker [is] the
       opener/closer rather than merely being permitted to act as one. *)
    match role with
    | Any -> (may_open, may_close)
    | Opener_only -> (true, false)
    | Closer_only -> (false, true)

  let marked_emphasis_delims t = t.marked_emphasis_delims

  let extra_inline_container_syntax t kind =
    Inline.Extra_inline_container.Config.syntax t.extra_inline_containers kind

  let block_id t = t.block_id
  let inline_attributes t = t.inline_attributes
  let block_attributes t = t.block_attributes

  (* Whether [_] joins [*] and [-] as a thematic-break delimiter. Arbitrary
     indentation follows independently from disabling [indented_code]. *)
  let underscore_thematic_break t = t.underscore_thematic_break
  let colon_symbols t = t.colon_symbols

  (* Backslash-space, blanks after a hard-break backslash, and two-space hard
     breaks are independent policies. *)
  let backslash_space_nbsp t = t.backslash_space_nbsp
  let hard_break_trailing_blanks t = t.hard_break_trailing_blanks
  let two_space_hard_break t = t.two_space_hard_break

  (* Djot raw content: a verbatim span with a [ {=format} ] specifier, and a code
     fence whose info string is [=format]. A renderer whose output format matches
     passes the content through verbatim; every other one drops it. *)
  let format_raw_content t = t.format_raw_content

  (* Djot numbers ordered lists in decimal, lower/upper alpha and lower/upper
     roman, and delimits with [1.], [1)] or [(1)]. A style change starts a new
     list. *)
  let extended_ordered_list_styles t = t.extended_ordered_list_styles

  (* Djot definition lists: a [: term] line followed by the indented blocks that
     define it. *)
  let definition_lists t = t.definition_lists

  (* Djot spells math as a dollar-prefixed verbatim span, [ $`x` ] inline and
     [ $$`x` ] display. It produces the same [Ext_math_span] as the pandoc
     [$...$] spelling behind the math extension; the two can coexist. *)
  let backtick_math t = t.backtick_math

  (* Djot table caption: a [^ text] line after a table, continuation lines
     indented. *)
  let table_captions t = t.table_captions

  (* Code spans require a matching closer and use symmetric padding. Verbatim
     spans may be open-ended and strip padding only where it protects a
     backtick. *)
  let verbatim_style t = t.verbatim_style

  (* A djot heading runs until a blank line. Implicit-target registration and
     Djot auto IDs are independently controlled by [heading_implicit_targets]. *)
  let multiline_atx_headings t = t.multiline_atx_headings
  let atx_closing_sequence t = t.atx_closing_sequence
  let heading_implicit_targets t = t.heading_implicit_targets

  (* Djot links have no titles: the whole of [ (url "title") ] is the
     destination, which may also be split over lines (the newlines are removed).
     Reference definitions likewise have no titles: the rest of the line is the
     destination. *)
  let djot_links t = t.djot_links
  let case_sensitive_labels t = t.case_sensitive_labels

  (* In djot no block start interrupts a paragraph at all: only a blank line ends
     one. A [# h] or [```] line under a paragraph is more of that paragraph's
     text. (djot.js block.ts: block starts are only tried when the last matched
     container takes block content, and an open paragraph takes inline content.)

     This subsumes [list_marker_interrupts_paragraph] when off, but the two are
     kept apart: forbidding only lists is a useful CommonMark-side knob on its
     own. A block interrupts a paragraph only if this is on and, for a list
     marker, that one is too. *)
  let blocks_interrupt_paragraph t = t.blocks_interrupt_paragraph

  (* In djot a list marker does not interrupt a paragraph: a [- x] line under a
     paragraph is more of that paragraph's text, not a list. *)
  let list_marker_interrupts_paragraph t = t.list_marker_interrupts_paragraph

  (* Djot's list item content is anything indented past the marker, where
     CommonMark's must reach the content column. *)
  let list_indent t = t.list_indent

  (* Djot's looseness rule: a blank line only loosens a list if what follows it
     is not a list boundary. A blank before a nested list, or before the next
     item, leaves the list tight; a blank between two paragraphs of one item does
     not. CommonMark loosens on any blank line between items. *)
  let list_tightness t = t.list_tightness
  let simple_emphasis_flanking t = t.simple_emphasis_flanking
  let smart_punctuation t = t.smart_punctuation
  let indented_code t = t.indented_code
  let setext_headings t = t.setext_headings

  (* CommonMark lets a paragraph inside a block quote or list item continue on a
     line that carries neither the [>] marker nor the item indentation. Djot has
     no such lazy lines: an unmarked line closes the container. *)
  let lazy_continuation t = t.lazy_continuation

  (* Djot parses no raw HTML, neither the inline form nor HTML blocks: [<div>]
     is text. Raw output is written with the [=html] raw syntax instead.
     Autolinks are unaffected — they are their own construct in both. *)
  let raw_html t = t.raw_html

  (* Djot leaves [&amp;] and [&#38;] literal: backslash is its only escape. *)
  let entity_refs t = t.entity_refs

  (* Djot fences with tildes as well as backticks, so the preset leaves this on;
     the knob stays because forbidding one of two spellings is useful on its
     own. *)
  let tilde_code_fences t = t.tilde_code_fences
  let whitespace_free_info_string t = t.whitespace_free_info_string

  (* Djot's block quote marker is [>] followed by a space or the end of the
     line, where CommonMark also quotes [>text]. *)
  let block_quote_marker_space t = t.block_quote_marker_space
  let div t = t.div
  let wikilink t = t.wikilink
  let jsx_expr t = t.jsx_expr
  let jsx_element t = t.jsx_element
  let callout t = t.callout
end

[@@@ocamlformat "disable"]

(* Parser abstraction *)

type parser =
  { file : Textloc.fpath (* input file name *);
    i : string (* input string *);
    buf : Buffer.t (* scratch buffer. *);
    exts : bool; (* parse extensions if [true]. *)
    nolocs : bool; (* do not compute locations if [true]. *)
    nolayout : bool; (* do not compute layout fields if [true]. *)
    heading_auto_ids : bool; (* compute heading ids. *)
    nested_links : bool;
    oymarkit_mod : Oymarkit_mod.t;
    (* Heading identifiers, assigned once by [assign_heading_ids] before the
       blocks are converted. [heading_ids] maps a heading's first text byte to
       the identifier it was given; [used_ids] is the set of identifiers already
       taken, against which derived ones are made unique. *)
    mutable heading_ids : Block.Heading.id Pos_map.t;
    mutable used_ids : Id_set.t;
    mutable defs : Label.defs;
    resolver : Label.resolver;
    mutable cidx : Closer_index.t; (* For inline parsing. *)
    (* Current line (only used during block parsing) *)
    mutable current_line_pos : Textloc.line_pos;
    mutable current_line_last_char :
      (* first char of line - 1 on empty lines *) Textloc.byte_pos;
    mutable current_char : Textloc.byte_pos;
    mutable current_char_col : col;
    mutable next_non_blank :
      (* current_line_last_char + 1 if none. *) Textloc.byte_pos;
    mutable next_non_blank_col : col;
    mutable tab_consumed_cols :
      (* number of cols consumed from the tab if i.[current_char] is '\t' *)
      col; }

let parser
    ?(defs = Label.Map.empty) ?(resolver = Label.default_resolver)
    ?(nested_links = false) ?(heading_auto_ids = false) ?(layout = false)
    ?(locs = false) ?(file = Textloc.file_none)
    (* Oymarkit begin *)
    ?(djot = false)
    ?emphasis_delims
    ?strong_emphasis_delims
    ?intraword_emphasis
    ?marked_emphasis_delims
    ?strong_emphasis_width
    ?extra_inline_containers
    ?(block_id = false)
    ?inline_attributes
    ?block_attributes
    ?underscore_thematic_break
    ?colon_symbols
    ?backslash_space_nbsp
    ?hard_break_trailing_blanks
    ?two_space_hard_break
    ?format_raw_content
    ?extended_ordered_list_styles
    ?definition_lists
    ?backtick_math
    ?table_captions
    ?verbatim_style
    ?multiline_atx_headings
    ?atx_closing_sequence
    ?heading_implicit_targets
    ?djot_links
    ?case_sensitive_labels
    ?simple_emphasis_flanking
    ?blocks_interrupt_paragraph
    ?list_marker_interrupts_paragraph
    ?list_indent
    ?list_tightness
    ?smart_punctuation
    ?indented_code
    ?setext_headings
    ?lazy_continuation
    ?raw_html
    ?entity_refs
    ?tilde_code_fences
    ?whitespace_free_info_string
    ?block_quote_marker_space
    ?div
    ?(wikilink = false)
    ?(jsx_expr = false)
    ?(jsx_element = false)
    ?(callout = Block.Callout.Config.disabled)
    (* Oymarkit end *)
    ~strict i
  =
  (* [djot] is a preset: a knob left unspecified takes its djot value instead of
     its CommonMark one. An explicitly passed knob always wins, so the preset can
     be used as a base and individual features dialed back. Every knob that has a
     djot meaning goes through [knob], which is what keeps the preset in sync as
     knobs are added: a new one is either given a djot value here or is left with
     no djot meaning at all. *)
  let knob ~cmark ~djot:djot_value = function
  | Some v -> v
  | None -> if djot then djot_value else cmark
  in
  (* Djot's delimiters are single characters with fixed roles: [_] is emphasis
     and [*] is strong. There is no doubling: [**x**] is strong emphasis of
     [*x*], which is what [strong_emphasis_width = 1] says. *)
  let emphasis_delims = knob ~cmark:[ '*'; '_' ] ~djot:[ '_' ] emphasis_delims in
  let strong_emphasis_delims =
    knob ~cmark:[ '*'; '_' ] ~djot:[ '*' ] strong_emphasis_delims
  in
  let strong_emphasis_width = knob ~cmark:2 ~djot:1 strong_emphasis_width in
  let intraword_emphasis = knob ~cmark:true ~djot:true intraword_emphasis in
  let marked_emphasis_delims =
    knob ~cmark:false ~djot:true marked_emphasis_delims
  in
  let extra_inline_containers =
    let cmark = Inline.Extra_inline_container.Config.disabled in
    let djot = Inline.Extra_inline_container.Config.djot in
    knob ~cmark ~djot extra_inline_containers
  in
  let inline_attributes =
    knob ~cmark:false ~djot:true inline_attributes
  in
  let block_attributes =
    knob ~cmark:false ~djot:true block_attributes
  in
  let underscore_thematic_break =
    knob ~cmark:true ~djot:false underscore_thematic_break
  in
  let colon_symbols = knob ~cmark:false ~djot:true colon_symbols in
  let backslash_space_nbsp = knob ~cmark:false ~djot:true backslash_space_nbsp in
  let hard_break_trailing_blanks =
    knob ~cmark:false ~djot:true hard_break_trailing_blanks
  in
  let two_space_hard_break =
    knob ~cmark:true ~djot:false two_space_hard_break
  in
  let format_raw_content = knob ~cmark:false ~djot:true format_raw_content in
  let extended_ordered_list_styles =
    knob ~cmark:false ~djot:true extended_ordered_list_styles
  in
  let definition_lists =
    knob ~cmark:false ~djot:true definition_lists
  in
  let backtick_math = knob ~cmark:false ~djot:true backtick_math in
  let table_captions =
    knob ~cmark:false ~djot:true table_captions
  in
  let verbatim_style =
    knob ~cmark:`Code_span ~djot:`Verbatim_span verbatim_style
  in
  let multiline_atx_headings = knob ~cmark:false ~djot:true multiline_atx_headings in
  let atx_closing_sequence =
    knob ~cmark:true ~djot:false atx_closing_sequence
  in
  let heading_implicit_targets =
    knob ~cmark:false ~djot:true heading_implicit_targets
  in
  let djot_links = knob ~cmark:false ~djot:true djot_links in
  let case_sensitive_labels =
    knob ~cmark:false ~djot:true case_sensitive_labels
  in
  let simple_emphasis_flanking = knob ~cmark:false ~djot:true simple_emphasis_flanking in
  let blocks_interrupt_paragraph =
    knob ~cmark:true ~djot:false blocks_interrupt_paragraph
  in
  let list_marker_interrupts_paragraph =
    knob ~cmark:true ~djot:false list_marker_interrupts_paragraph
  in
  let list_indent =
    knob ~cmark:`Content_column ~djot:`Marker_plus_one list_indent
  in
  let list_tightness =
    knob ~cmark:`Any_blank ~djot:`Non_list_boundary_blank list_tightness
  in
  let smart_punctuation = knob ~cmark:false ~djot:true smart_punctuation in
  let indented_code = knob ~cmark:true ~djot:false indented_code in
  let setext_headings = knob ~cmark:true ~djot:false setext_headings in
  (* Djot does have lazy paragraph continuation, contrary to what its prose
     suggests: [> Lazy\nblock quote.] is one quoted paragraph, and a list item's
     paragraph continues on an unindented line. The knob stays — it is useful on
     its own — but the preset leaves it on. *)
  let lazy_continuation = knob ~cmark:true ~djot:true lazy_continuation in
  let raw_html = knob ~cmark:true ~djot:false raw_html in
  let entity_refs = knob ~cmark:true ~djot:false entity_refs in
  let tilde_code_fences = knob ~cmark:true ~djot:true tilde_code_fences in
  let whitespace_free_info_string = knob ~cmark:false ~djot:true whitespace_free_info_string in
  let block_quote_marker_space =
    knob ~cmark:false ~djot:true block_quote_marker_space
  in
  let div = knob ~cmark:false ~djot:true div in
  let oymarkit_mod =
    Oymarkit_mod.make ~emphasis_delims ~strong_emphasis_delims
      ~intraword_emphasis ~marked_emphasis_delims ~strong_emphasis_width
      ~extra_inline_containers ~block_id ~inline_attributes
      ~block_attributes ~underscore_thematic_break ~colon_symbols
      ~backslash_space_nbsp ~hard_break_trailing_blanks
      ~two_space_hard_break
      ~format_raw_content ~extended_ordered_list_styles ~definition_lists ~backtick_math
      ~table_captions ~verbatim_style ~multiline_atx_headings
      ~atx_closing_sequence
      ~heading_implicit_targets ~djot_links ~case_sensitive_labels
      ~simple_emphasis_flanking ~blocks_interrupt_paragraph
      ~list_marker_interrupts_paragraph ~list_indent
      ~list_tightness ~smart_punctuation ~indented_code ~setext_headings ~lazy_continuation
      ~raw_html ~entity_refs ~tilde_code_fences ~whitespace_free_info_string ~block_quote_marker_space ~div
      ~wikilink ~jsx_expr ~jsx_element ~callout
  in
  let nolocs = not locs and nolayout = not layout and exts = not strict in
  { file; i; buf = Buffer.create 512; exts; nolocs; nolayout;
    heading_auto_ids; nested_links;
    oymarkit_mod;
    heading_ids = Pos_map.empty; used_ids = Id_set.empty;
    defs; resolver; cidx = Closer_index.empty;
    current_line_pos = 1, 0; current_line_last_char = -1; current_char = 0;
    current_char_col = 0; next_non_blank = 0; next_non_blank_col = 0;
    tab_consumed_cols = 0; }

let find_label_defining_key p key = match Label.Map.find_opt key p.defs with
| Some (Link_definition.Def ld) -> Link_definition.defined_label (fst ld)
| Some (Block.Footnote.Def fn) -> Block.Footnote.defined_label (fst fn)
| None -> None
| _ -> assert false

let set_label_def p l def = p.defs <- Label.Map.add (Label.key l) def p.defs
let def_label p l =
  p.resolver (`Def (find_label_defining_key p (Label.key l), l))

let find_def_for_ref ~image p ref =
  let kind = if image then `Image else `Link in
  let def = find_label_defining_key p (Label.key ref) in
  p.resolver (`Ref (kind, ref, def))

let debug_span p s = String.sub p.i s.first (s.last - s.first + 1)
let debug_line p =
  let first = snd p.current_line_pos and last = p.current_line_last_char in
  String.sub p.i first (last - first + 1)

let current_line_span p ~first ~last =
  { line_pos = p.current_line_pos; first; last }

(* Making metas and text locations. This is centralized here to be able
   to disable their creation which has a non-negligible impact on
   performance. *)

let meta p textloc = if p.nolocs then Meta.none else Meta.make ~textloc ()

let textloc_of_span p span =
  if p.nolocs then Textloc.none else
  let first_byte = span.first and last_byte = span.last in
  let first_line = span.line_pos and last_line = span.line_pos in
  Textloc.v ~file:p.file ~first_byte ~last_byte ~first_line ~last_line

let textloc_of_lines p ~first ~last ~first_line ~last_line =
  if p.nolocs then Textloc.none else
  let first_byte = first and first_line = first_line.line_pos in
  let last_byte = last and last_line = last_line.line_pos in
  Textloc.v ~file:p.file ~first_byte ~last_byte ~first_line ~last_line

let meta_of_spans p ~first:first_line ~last:last_line =
  if p.nolocs then Meta.none else
  let first = first_line.first and last = last_line.last in
  meta p (textloc_of_lines p ~first ~last ~first_line ~last_line)

let meta_of_metas p ~first ~last =
  if p.nolocs then Meta.none else
  meta p (Textloc.span (Meta.textloc first) (Meta.textloc last))

let clean_raw_span ?pad p span =
  Text.utf_8_clean_raw ?pad p.buf p.i ~first:span.first ~last:span.last,
  meta p (textloc_of_span p span)

let unref p = Oymarkit_mod.entity_refs p.oymarkit_mod
let backslash_space_nbsp p = Oymarkit_mod.backslash_space_nbsp p.oymarkit_mod

let clean_unref_span p span =
  Text.utf_8_clean_unref ~unref:(unref p) p.buf p.i ~first:span.first
    ~last:span.last,
  meta p (textloc_of_span p span)

let clean_unesc_unref_span p span =
  Text.utf_8_clean_unesc_unref ~unref:(unref p) ~backslash_space_nbsp:(backslash_space_nbsp p)
    p.buf p.i ~first:span.first ~last:span.last,
  meta p (textloc_of_span p span)

let layout_clean_raw_span ?pad p span =
  if p.nolayout then Layout.empty else clean_raw_span ?pad p span

let layout_clean_raw_span' ?pad p span =
  (* Like [layout_raw_span] but no meta *)
  if p.nolayout then "" else
  Text.utf_8_clean_raw ?pad p.buf p.i ~first:span.first ~last:span.last

let _tight_block_lines xxx_span p ~rev_spans =
  let rec loop p acc = function
  | [] -> acc
  | [_, fst_line] -> ("", xxx_span p fst_line) :: acc
  | (line_start, span) :: spans ->
      let acc =
        let layout =
          if p.nolayout || span.first <= line_start then "" else
          Text.utf_8_clean_raw p.buf p.i ~first:line_start
            ~last:(span.first - 1)
        in
        (layout, xxx_span p span) :: acc
      in
      loop p acc spans
  in
  loop p [] rev_spans

let tight_block_lines p ~rev_spans =
  _tight_block_lines clean_unesc_unref_span p ~rev_spans

let raw_tight_block_lines p ~rev_spans =
  _tight_block_lines clean_raw_span p ~rev_spans

let first_non_blank_in_span p s = Match.first_non_blank_in_span p.i s
let first_non_blank_over_nl ~next_line p lines line ~start =
  match Match.first_non_blank_over_nl ~next_line p.i lines ~line ~start with
  | `None -> None
  | `This_line non_blank ->
      let layout =
        if non_blank = start then [] else
        [clean_raw_span p { line with first = start ; last = non_blank - 1}]
      in
      Some (lines, line, layout, non_blank)
  | `Next_line (lines, newline, non_blank) ->
      let first_layout = clean_raw_span p { line with first = start } in
      let next_layout = clean_raw_span p { newline with last = non_blank -1 } in
      let layout = [first_layout; next_layout] in
      Some (lines, newline, layout, non_blank)
