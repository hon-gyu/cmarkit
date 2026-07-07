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
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)


(* Closer indexes.

   They map closing delimiters to the position where they
   start. Shortcuts forward searches in inline parsing. See
   Inline_struct. *)

module Pos_set = Set.Make (Int) (* Sets of positions. *)
module Closer = struct
  type t =
  | Backticks of int (* run length *)
  | Right_brack
  | Right_paren (* Only for ruling out pathological cases. *)
  | Emphasis_marks of char
  | Extra_inline_container_marks of char * bool
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

  type t = {
    emphasis_delims : delim_set;
    strong_emphasis_delims : delim_set;
    intraword_emphasis : bool;
    marked_emphasis_delims : bool;
    strong_emphasis_width : int;
    extra_inline_containers : Inline.Extra_inline_container.Config.t;
    block_id : bool;
    djot_inline_attributes : bool;
    djot_block_attributes : bool;
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
      ~block_id ~djot_inline_attributes ~djot_block_attributes ~div ~wikilink
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
      djot_inline_attributes;
      djot_block_attributes;
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
  let djot_inline_attributes t = t.djot_inline_attributes
  let djot_block_attributes t = t.djot_block_attributes
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
    ?(emphasis_delims = [ '*'; '_' ])
    ?(strong_emphasis_delims = [ '*'; '_' ])
    ?(intraword_emphasis = true)
    ?(marked_emphasis_delims = false)
    ?(strong_emphasis_width = 2)
    ?(extra_inline_containers = Inline.Extra_inline_container.Config.disabled)
    ?(block_id = false)
    ?(djot_inline_attributes = false)
    ?(djot_block_attributes = false)
    ?(div = false)
    ?(wikilink = false)
    ?(jsx_expr = false)
    ?(jsx_element = false)
    ?(callout = Block.Callout.Config.disabled)
    (* Oymarkit end *)
    ~strict i
  =
  let oymarkit_mod =
    Oymarkit_mod.make ~emphasis_delims ~strong_emphasis_delims
      ~intraword_emphasis ~marked_emphasis_delims ~strong_emphasis_width
      ~extra_inline_containers ~block_id ~djot_inline_attributes
      ~djot_block_attributes ~div ~wikilink ~jsx_expr ~jsx_element ~callout
  in
  let nolocs = not locs and nolayout = not layout and exts = not strict in
  { file; i; buf = Buffer.create 512; exts; nolocs; nolayout;
    heading_auto_ids; nested_links;
    oymarkit_mod;
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

let clean_unref_span p span =
  Text.utf_8_clean_unref p.buf p.i ~first:span.first ~last:span.last,
  meta p (textloc_of_span p span)

let clean_unesc_unref_span p span =
  Text.utf_8_clean_unesc_unref p.buf p.i ~first:span.first ~last:span.last,
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
