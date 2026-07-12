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

val of_doc : Cmarkit.Doc.t -> string
(** [of_doc d] is the mdast [root] of [d] as a JSON string. *)
