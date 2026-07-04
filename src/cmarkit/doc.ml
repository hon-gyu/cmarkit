(*---------------------------------------------------------------------------
   Copyright (c) 2021 The cmarkit programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

[@@@ocamlformat "disable"]

module Meta = Cmarkit_base.Meta
module Label = Common_.Label
module Layout = Common_.Layout
open Parser_common_
open Parser

type t = { nl : Layout.string; block : Block.t; defs : Label.defs }
let make ?(nl = "\n") ?(defs = Label.Map.empty) block = { nl; block; defs }
let empty = make (Block.Blocks ([], Meta.none))
let nl d = d.nl
let block d = d.block
let defs d = d.defs
let of_string
    ?defs ?resolver ?nested_links ?heading_auto_ids ?layout ?locs ?file
    ?emphasis_delims ?strong_emphasis_delims ?intraword_emphasis
    ?marked_emphasis_delims ?strong_emphasis_width ?extra_inline_containers
    ?block_id ?djot_inline_attributes ?djot_block_attributes ?div ?wikilink
    ?jsx_expr ?callout ?(strict = true) s
  =
  let p =
    parser ?defs ?resolver ?nested_links ?heading_auto_ids ?layout ?locs
      ?emphasis_delims ?strong_emphasis_delims ?intraword_emphasis ?file
      ?marked_emphasis_delims ?strong_emphasis_width ?extra_inline_containers
      ?block_id ?djot_inline_attributes ?djot_block_attributes ?div ?wikilink
      ?jsx_expr ?callout ~strict s
  in
  let nl, doc = Block_struct.parse p in
  let block = block_struct_to_doc p doc in
  make ~nl block ~defs:p.defs

let unicode_version = Cmarkit_data.unicode_version
let commonmark_version = "0.31.2"
