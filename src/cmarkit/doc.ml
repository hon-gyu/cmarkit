(*---------------------------------------------------------------------------
   Copyright (c) 2021 The cmarkit programmers. All rights reserved.
   Copyright (c) 2026 The oymarkit programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

[@@@ocamlformat "disable"]

module Meta = Cmarkit_base.Meta
module Label = Common_.Label
module Layout = Common_.Layout
open Parser_common_
open Parser

type t = { nl : Layout.string; block : Block.t; defs : Label.defs }
type list_indent = [ `Content_column | `Marker_plus_one ]
type list_tightness = [ `Any_blank | `Non_list_boundary_blank ]
type verbatim_style = [ `Code_span | `Verbatim_span ]
let make ?(nl = "\n") ?(defs = Label.Map.empty) block = { nl; block; defs }
let empty = make (Block.Blocks ([], Meta.none))
let nl d = d.nl
let block d = d.block
let defs d = d.defs
let of_string
    ?defs ?resolver ?nested_links ?heading_auto_ids ?layout ?locs ?file
    ?djot ?emphasis_delims ?strong_emphasis_delims ?intraword_emphasis
    ?marked_emphasis_delims ?strong_emphasis_width ?extra_inline_containers
    ?block_id ?inline_attributes ?block_attributes
    ?underscore_thematic_break ?colon_symbols ?backslash_space_nbsp
    ?hard_break_trailing_blanks ?two_space_hard_break
    ?format_raw_content
    ?extended_ordered_list_styles ?definition_lists ?backtick_math
    ?table_captions ?verbatim_style ?multiline_atx_headings
    ?atx_closing_sequence
    ?heading_implicit_targets ?djot_links ?case_sensitive_labels ?simple_emphasis_flanking
    ?blocks_interrupt_paragraph
    ?list_marker_interrupts_paragraph ?list_indent ?list_tightness ?smart_punctuation
    ?indented_code ?setext_headings ?lazy_continuation ?raw_html ?entity_refs
    ?tilde_code_fences ?whitespace_free_info_string ?block_quote_marker_space
    ?div ?wikilink ?jsx_expr ?jsx_element ?callout
    ?strict s
  =
  (* Djot's tables, footnotes, task lists and math are all cmarkit extensions,
     so the preset only makes sense non-strict; [strict] can still be forced. *)
  let strict = match strict with
  | Some strict -> strict
  | None -> not (djot = Some true)
  in
  let p =
    parser ?defs ?resolver ?nested_links ?heading_auto_ids ?layout ?locs
      ?djot ?emphasis_delims ?strong_emphasis_delims ?intraword_emphasis ?file
      ?marked_emphasis_delims ?strong_emphasis_width ?extra_inline_containers
      ?block_id ?inline_attributes ?block_attributes
      ?underscore_thematic_break ?colon_symbols ?backslash_space_nbsp
      ?hard_break_trailing_blanks ?two_space_hard_break
      ?format_raw_content
      ?extended_ordered_list_styles ?definition_lists ?backtick_math
      ?table_captions ?verbatim_style ?multiline_atx_headings
      ?atx_closing_sequence
      ?heading_implicit_targets ?djot_links ?case_sensitive_labels ?simple_emphasis_flanking
      ?blocks_interrupt_paragraph
    ?list_marker_interrupts_paragraph ?list_indent ?list_tightness ?smart_punctuation
      ?indented_code ?setext_headings ?lazy_continuation ?raw_html ?entity_refs
      ?tilde_code_fences ?whitespace_free_info_string ?block_quote_marker_space
      ?div ?wikilink ?jsx_expr ?jsx_element ?callout ~strict s
  in
  let nl, doc = Block_struct.parse p in
  (* Heading identifiers are settled before the blocks are converted: the
     conversion reads them, and djot's implicit heading targets must be in
     [defs] by then, since that is when inline parsing resolves references. *)
  assign_heading_ids p (fst doc);
  attach_ref_def_attributes p (fst doc);
  let block = block_struct_to_doc p doc in
  make ~nl block ~defs:p.defs

let unicode_version = Cmarkit_data.unicode_version
let commonmark_version = "0.31.2"
