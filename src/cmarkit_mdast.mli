(*---------------------------------------------------------------------------
   Copyright (c) 2026 The oymarkit programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Render a {!Cmarkit.Doc.t} to an {{:https://github.com/syntax-tree/mdast}
    mdast} JSON string.

    The output is a single mdast [root] node serialized as JSON, suitable for
    handing to a [unified] pipeline as a parser replacement (see the [oyparse]
    js_of_ocaml entry point).

    CommonMark nodes map to their standard mdast counterparts. Oymarkit
    extensions that have no native mdast type are lowered to generic nodes
    carrying {{:https://github.com/syntax-tree/mdast-util-to-hast#fields-on-nodes}
    [data.hName] / [data.hProperties]} hints, so [mdast-util-to-hast]
    (i.e. [remark-rehype]) renders them to the intended HTML with no custom
    handler. *)

val of_doc : ?strip_block_id:bool -> Cmarkit.Doc.t -> string
(** [of_doc d] is the mdast [root] of [d] as a JSON string.

    [strip_block_id] (defaults to [true]) controls the Obsidian block-id [^id]
    marker on a paragraph. When [true] the marker is removed from the rendered
    text (the id is still emitted on the paragraph). When [false] the marker is
    kept and wrapped in a [span] with class [block-id], and the paragraph gains
    the class [has-block-id], so both can be styled. *)
