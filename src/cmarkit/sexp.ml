open Sexplib0
open Common

(** {1 Core compatible mocks} *)

module List = struct
  include List

  let map : 'a list -> f:('a -> 'b) -> 'b list = fun l ~f -> map f l

  let filter_map : 'a list -> f:('a -> 'b option) -> 'b list =
   fun l ~f -> filter_map f l
end

module String = struct
  include String

  let concat : ?sep:string -> string list -> string =
   fun ?(sep = "") l -> concat sep l
end

(** {1 oyster/pkg/oystermark/lib/parse/common.ml vendor start}
    ---------------------------------------------------------------------------------------------
*)

[@@@ocamlformat "disable"]

(** A sexp-converter for inlines. Returns [None] to fall through to the
    next converter in the composed chain. [recurse] is the fully-composed
    [sexp_of_inline] for recursing into children.

    Both core and extension converters share this type; composition is
    just list order with [None] as the fallthrough signal, analogous to
    [Cmarkit.Mapper]'s [`Default]. *)
type inline_sexp =
  (Inline.t -> Sexp.t) -> with_meta:(Meta.t -> Sexp.t -> Sexp.t) -> Inline.t -> Sexp.t option

(** A sexp-converter for blocks. Receives both [recurse_inline] and
    [recurse_block]. [with_meta] wraps a block sexp with its metadata
    sub-sexps — pass through to keep metadata in the output. *)
type block_sexp =
  recurse_inline:(Inline.t -> Sexp.t)
  -> recurse_block:(Block.t -> Sexp.t)
  -> with_meta:(Meta.t -> Sexp.t -> Sexp.t)
  -> Block.t
  -> Sexp.t option

(** A sexp-converter for a single metadata key. *)
type meta_sexp = Meta.t -> Sexp.t option

type sexp_of =
  { inline : Inline.t -> Sexp.t
  ; block : Block.t -> Sexp.t
  ; meta : Meta.t -> Sexp.t list
  ; doc : Doc.t -> Sexp.t
  }

(** Core inline converter. Always returns [Some] — unknown constructors
    emit [<unknown-inline>]. Placed last in the composed chain. *)
let sexp_of_inline_core : inline_sexp =
  fun recurse ~with_meta i ->
  let meta, body =
    match i with
    | Inline.Text (s, m) -> m, Sexp.List [ Atom "Text"; Atom s ]
    | Inline.Autolink (a, m) ->
      let link = fst (Inline.Autolink.link a) in
      m, Sexp.List [ Atom "Autolink"; Atom link ]
    | Inline.Break (b, m) ->
      let type_s =
        match Inline.Break.type' b with
        | `Hard -> "hard"
        | `Soft -> "soft"
      in
      m, Sexp.List [ Atom "Break"; Atom type_s ]
    | Inline.Code_span (cs, m) ->
      m, Sexp.List [ Atom "Code_span"; Atom (Inline.Code_span.code cs) ]
    | Inline.Emphasis (e, m) ->
      m, Sexp.List [ Atom "Emphasis"; recurse (Inline.Emphasis.inline e) ]
    | Inline.Strong_emphasis (e, m) ->
      m, Sexp.List [ Atom "Strong_emphasis"; recurse (Inline.Emphasis.inline e) ]
    | Inline.Link (l, m) -> m, Sexp.List [ Atom "Link"; recurse (Inline.Link.text l) ]
    | Inline.Image (l, m) -> m, Sexp.List [ Atom "Image"; recurse (Inline.Link.text l) ]
    | Inline.Raw_html (html, m) ->
      let s =
        List.map html ~f:(fun bl -> Block_line.tight_to_string bl)
        |> String.concat ~sep:""
      in
      m, Sexp.List [ Atom "Raw_html"; Atom s ]
    | Inline.Inlines (is, m) -> m, Sexp.List (Atom "Inlines" :: List.map is ~f:recurse)
    | Inline.Ext_strikethrough (s, m) ->
      m, Sexp.List [ Atom "Strikethrough"; recurse (Inline.Strikethrough.inline s) ]
    | Inline.Ext_math_span (ms, m) ->
      m, Sexp.List [ Atom "Math_span"; Atom (Inline.Math_span.tex ms) ]
    | _ -> Meta.none, Sexp.Atom "<unknown-inline>"
  in
  Some (with_meta meta body)
;;

(** Core block converter. Always returns [Some]. Placed last in the chain. *)
let sexp_of_block_core : block_sexp =
  fun ~recurse_inline ~recurse_block ~with_meta b ->
  let s =
    match b with
    | Block.Blank_line (_, meta) -> with_meta meta (Sexp.Atom "Blank_line")
    | Block.Paragraph (p, meta) ->
      with_meta
        meta
        (Sexp.List [ Atom "Paragraph"; recurse_inline (Block.Paragraph.inline p) ])
    | Block.Heading (h, meta) ->
      with_meta
        meta
        (Sexp.List
           [ Atom "Heading"
           ; Atom (Int.to_string (Block.Heading.level h))
           ; recurse_inline (Block.Heading.inline h)
           ])
    | Block.Code_block (cb, meta) ->
      let info =
        match Block.Code_block.info_string cb with
        | None -> Sexp.Atom "no-info"
        | Some (s, _) -> Sexp.Atom s
      in
      let code =
        List.map (Block.Code_block.code cb) ~f:(fun bl ->
          Sexp.Atom (Block_line.to_string bl))
      in
      with_meta meta (Sexp.List (Atom "Code_block" :: info :: code))
    | Block.Html_block (lines, meta) ->
      let s =
        List.map lines ~f:(fun bl -> Block_line.to_string bl) |> String.concat ~sep:"\n"
      in
      with_meta meta (Sexp.List [ Atom "Html_block"; Atom s ])
    | Block.Block_quote (bq, meta) ->
      with_meta
        meta
        (Sexp.List [ Atom "Block_quote"; recurse_block (Block.Block_quote.block bq) ])
    | Block.List (l, meta) ->
      let items =
        List.map (Block.List'.items l) ~f:(fun (item, _item_meta) ->
          recurse_block (Block.List_item.block item))
      in
      with_meta meta (Sexp.List (Atom "List" :: items))
    | Block.Blocks (bs, meta) ->
      with_meta meta (Sexp.List (Atom "Blocks" :: List.map bs ~f:recurse_block))
    | Block.Link_reference_definition _ -> Sexp.Atom "Link_reference_definition"
    | Block.Thematic_break (_, meta) -> with_meta meta (Sexp.Atom "Thematic_break")
    | _ -> Sexp.Atom "<unknown-block>"
  in
  Some s
;;

(** Compose a list of extension converters with the built-in core
    converters into a mutually-recursive triple. Extensions are tried
    in list order; the first to return [Some] wins. The core converters
    are always appended last — callers pass extensions only. *)
let make_sexp_of
      ?(inlines : inline_sexp list = [])
      ?(blocks : block_sexp list = [])
      ?(metas : meta_sexp list = [])
      ()
  : sexp_of
  =
  let inlines = inlines @ [ sexp_of_inline_core ] in
  let blocks = blocks @ [ sexp_of_block_core ] in
  let rec sexp_of_inline (i : Inline.t) : Sexp.t =
    let rec try_ = function
      | [] -> Sexp.Atom "<unknown-inline>"
      | f :: rest ->
        (match f sexp_of_inline ~with_meta i with
         | Some s -> s
         | None -> try_ rest)
    in
    try_ inlines
  and sexp_of_block (b : Block.t) : Sexp.t =
    let rec try_ = function
      | [] -> Sexp.Atom "<unknown-block>"
      | f :: rest ->
        (match
           f ~recurse_inline:sexp_of_inline ~recurse_block:sexp_of_block ~with_meta b
         with
         | Some s -> s
         | None -> try_ rest)
    in
    try_ blocks
  and sexp_of_meta (meta : Meta.t) : Sexp.t list =
    List.filter_map metas ~f:(fun ext -> ext meta)
  and with_meta (meta : Meta.t) (sexp : Sexp.t) : Sexp.t =
    match sexp_of_meta meta with
    | [] -> sexp
    | items -> Sexp.List [ sexp; Sexp.List (Atom "meta" :: items) ]
  in
  let sexp_of_doc (d : Doc.t) : Sexp.t = sexp_of_block (Doc.block d) in
  { inline = sexp_of_inline
  ; block = sexp_of_block
  ; meta = sexp_of_meta
  ; doc = sexp_of_doc
  }
;;
[@@@ocamlformat "enable"]

(** {1 oyster/pkg/oystermark/lib/parse/common.ml vendor end}
    ---------------------------------------------------------------------------------------------
*)
