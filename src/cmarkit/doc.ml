(*---------------------------------------------------------------------------
   Copyright (c) 2021 The cmarkit programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Meta = Cmarkit_base.Meta
module Label = Common.Label
module Layout = Common.Layout
open Parser

type t = { nl : Layout.string; block : Block.t; defs : Label.defs }
let make ?(nl = "\n") ?(defs = Label.Map.empty) block = { nl; block; defs }
let empty = make (Block.Blocks ([], Meta.none))
let nl d = d.nl
let block d = d.block
let defs d = d.defs
let of_string
    ?defs ?resolver ?nested_links ?heading_auto_ids ?layout ?locs ?file
    ?(strict = true) s
  =
  let p =
    parser ?defs ?resolver ?nested_links ?heading_auto_ids ?layout ?locs
      ?file ~strict s
  in
  let nl, doc = Block_struct.parse p in
  let block = block_struct_to_doc p doc in
  make ~nl block ~defs:p.defs

let unicode_version = Cmarkit_data.unicode_version
let commonmark_version = "0.31.2"
