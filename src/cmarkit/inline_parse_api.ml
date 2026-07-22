(** Inline-level parsing API.

    {@meta[
    ai-disclosure: "autonomous"
    ]}

    [Doc.of_string] parses a string as a document, running block-structure
    parsing before inline parsing. That promotes some inline-looking constructs
    to blocks — most notably a line that is just an HTML tag, which becomes an
    [Html_block] rather than an inline [Raw_html].

    The block parser internally delegates inline content to
    [Inline_struct.parse] (see [Parser]). This module exposes that pass
    directly: it parses a string as if it were already the inline payload of a
    block, skipping block-structure parsing entirely. It does not touch the
    original cmarkit modules. *)

open Common_
open Parser_common_
open Parser

(* Cut [s] into the line spans the inline parser expects: one span per line
   (the newline between two lines is implicit), in *reverse* order — the inline
   parser, like the block parser feeding it, takes the last line at the head. *)
let line_spans s =
  let len = String.length s in
  let rec loop lineno start k acc =
    if k >= len then
      { line_pos = (lineno, start); first = start; last = len - 1 } :: acc
    else if s.[k] = '\n' then
      let span = { line_pos = (lineno, start); first = start; last = k - 1 } in
      loop (lineno + 1) (k + 1) (k + 1) (span :: acc)
    else loop lineno start (k + 1) acc
  in
  loop 1 0 0 []

(* Inline-level analogue of {!Doc.of_string}. *)
let of_string ?defs ?resolver ?nested_links ?heading_auto_ids ?layout ?locs
    ?file ?emphasis_delims ?strong_emphasis_delims ?intraword_emphasis
    ?marked_emphasis_delims ?strong_emphasis_width ?extra_inline_containers
    ?inline_attributes ?wikilink ?jsx_expr ?jsx_element
    ?(strict = true) s =
  let p =
    parser ?defs ?resolver ?nested_links ?heading_auto_ids ?layout ?locs ?file
      ?emphasis_delims ?strong_emphasis_delims ?intraword_emphasis
      ?marked_emphasis_delims ?strong_emphasis_width ?extra_inline_containers
      ?inline_attributes ?wikilink ?jsx_expr ?jsx_element
      ~strict s
  in
  let _layout, inline = Inline_struct.parse p (line_spans s) in
  inline

(* Colon-keyed segmentation of [s] as a single paragraph's inline payload. See
   {!Inline_struct.parse_keyed}. A single-segment result means [s] had no
   structural colon. *)
let keyed_segments_of_string ?defs ?resolver ?nested_links ?heading_auto_ids
    ?layout ?locs ?file ?emphasis_delims ?strong_emphasis_delims
    ?intraword_emphasis ?marked_emphasis_delims ?strong_emphasis_width
    ?extra_inline_containers ?inline_attributes ?wikilink
    ?(strict = true) s =
  let p =
    parser ?defs ?resolver ?nested_links ?heading_auto_ids ?layout ?locs ?file
      ?emphasis_delims ?strong_emphasis_delims ?intraword_emphasis
      ?marked_emphasis_delims ?strong_emphasis_width ?extra_inline_containers
      ?inline_attributes ?wikilink
      ~strict s
  in
  Inline_struct.parse_keyed p (line_spans s)
