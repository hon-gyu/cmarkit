(*---------------------------------------------------------------------------
   Copyright (c) 2026 The oymarkit programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Oymarkit: Struct -- colon-keyed tree restructuring.

   A dedicated, self-contained pass that rewrites an already-parsed [Doc.t] so
   that list items and paragraphs carrying a colon-delimited label/value
   relationship become {!Block.Ext_keyed} nodes (rendered as a list item or a
   free block depending on where the node sits).

   Detection of the structural colon happens during inline parsing -- only
   there is the source available to handle escaping and code-span/emphasis
   opacity (see {!Inline_struct}). The resulting label/value segments are
   carried on the paragraph's {!Meta.t} under {!Inline_struct.keyed_segments}.
   This pass reads those segments and does the {e tree} work only: classifying
   the keying form, validating labels, absorbing bodies, and building the keyed
   nodes.

   The keyed-node *constructors* live in {!Block} (so that the core traversals
   -- [normalize], [Mapper], [Folder] -- and the renderers can give them
   structural recursion, exactly like [Ext_div]).

   This pass is opt-in: it runs only via {!rewrite_doc}, and only paragraphs
   parsed with extensions enabled carry segments. *)

open Common_

(* Keying classification
   =====================

   The segments shipped on the meta are: zero or more chain labels followed by a
   final value segment (empty for the trailing-colon form). We only decide here
   whether they form a valid key and of which kind. *)

type decomposition =
  | Chain_trailing_colon of Inline.t list * Inline.t option
  | Chain_with_value of Inline.t list * Inline.t

(* The [Inline.t option] of [Chain_trailing_colon] is the block-id marker that
   stood in for the value, when the trailing form arose from an id-only value
   (see {!is_id_only_value}) rather than from a bare trailing ":". It is kept so
   {!unkey} can put the marker back and stay content-invisible. *)

(** Whether [s] is only a ":" separator (the colon plus optional blanks). Labels
    keep their raw separator (see {!Inline_struct}), so a non-[Text] key such as
    emphasis arrives as [Inlines [unit; Text sep]]. *)
let is_separator_text (s : string) : bool =
  s <> "" && String.contains s ':'
  && String.for_all (fun c -> c = ':' || c = ' ' || c = '\t') s

(** A single inline that can stand as a key: text, emphasis, strong emphasis,
    code span or autolink. *)
let is_key_unit : Inline.t -> bool = function
  | Inline.Text _ | Inline.Emphasis _ | Inline.Strong_emphasis _ | Inline.Code_span _
  | Inline.Autolink _ -> true
  | _ -> false

(** A label is valid iff its key is a single inline unit, optionally carrying a
    djot inline attribute ([key{.x}: ...]). The label carries its trailing ":"
    separator as content: a bare [Text] key keeps it inline (still one [Text]
    node); a non-[Text] key (emphasis, strong emphasis, code span, autolink) --
    or any attributed key -- arrives as the unit followed by a separator [Text],
    since the attribute / non-[Text] node cannot coalesce with the separator.
    Either way the returned label is the colon-inclusive one, so re-joining it to
    the value reproduces the source (see {!unkey}). *)
let as_label : Inline.t -> Inline.t option = function
  | (Inline.Text _ | Inline.Emphasis _ | Inline.Strong_emphasis _ | Inline.Code_span _
    | Inline.Autolink _) as i -> Some i
  | Inline.Inlines ([ unit; Inline.Text (sep, _) ], _) as i when is_separator_text sep ->
    (match unit with
     | Inline.Emphasis _ | Inline.Strong_emphasis _ | Inline.Code_span _
     | Inline.Autolink _ -> Some i
     | Inline.Ext_attributes (a, _) when is_key_unit (Inline.Attributes.inline a) -> Some i
     | _ -> None)
  | _ -> None

let all_labels (segments : Inline.t list) : Inline.t list option =
  let rec go acc = function
    | [] -> Some (List.rev acc)
    | seg :: rest -> (match as_label seg with Some l -> go (l :: acc) rest | None -> None)
  in
  go [] segments

let is_empty_segment : Inline.t -> bool = function
  | Inline.Inlines ([], _) | Inline.Text ("", _) -> true
  | _ -> false

(** A value beginning with a break sits on the next source line; the colon must
    be followed by its value on the same line. *)
let value_starts_with_break : Inline.t -> bool = function
  | Inline.Break _ | Inline.Inlines (Inline.Break _ :: _, _) -> true
  | _ -> false

(** The marker text ["^id"] of the block identifier carried on [meta], if any. *)
let block_id_marker_text (meta : Meta.t) : string option =
  match Block.Block_id.find meta with
  | None -> None
  | Some b -> Some ("^" ^ Block.Block_id.id b)

(** Whether [value] consists of nothing but the block-id [marker]. Such a value
    is {e empty}: the marker is consumed as the node's identifier instead of
    becoming its value, so the node keys as if a bare ":" had been written and
    goes on to claim the following content. A marker merely {e ending} a
    non-empty value ([key: v ^x]) identifies the node without emptying it, and a
    marker that yields no identifier at all (escaped, or not a terminal suffix)
    leaves the value alone -- both fall out of comparing the whole value. *)
let is_id_only_value ~(marker : string option) (value : Inline.t) : bool =
  match marker with
  | None -> false
  | Some marker ->
    let rec text : Inline.t -> string option = function
      | Inline.Text (s, _) -> Some s
      | Inline.Inlines ([ i ], _) -> text i
      | _ -> None
    in
    (match text value with Some s -> String.trim s = marker | None -> false)

(** Classify the carried colon [segments]: the last segment is the value (empty
    for the trailing form), the rest are chain labels. [marker] is the block-id
    marker of the enclosing paragraph, which can empty the value. [None] if not
    a valid key -- a label is not a single inline unit, or the value starts on
    the next line. *)
let classify ~(marker : string option) (segments : Inline.t list)
  : decomposition option
  =
  match List.rev segments with
  | [] | [ _ ] -> None
  | value :: rev_labels ->
    (match all_labels (List.rev rev_labels) with
     | None -> None
     | Some labels ->
       if is_empty_segment value
       then Some (Chain_trailing_colon (labels, None))
       else if is_id_only_value ~marker value
       then Some (Chain_trailing_colon (labels, Some value))
       else if value_starts_with_break value
       then None
       else Some (Chain_with_value (labels, value)))

(** Read the colon segments shipped on a paragraph's meta by the inline parser
    and classify them. *)
let decompose (meta : Meta.t) : decomposition option =
  match Meta.find Inline_struct.keyed_segments meta with
  | None -> None
  | Some segments -> classify ~marker:(block_id_marker_text meta) segments

(* Shared helpers
   ============== *)

let is_blank_line : Block.t -> bool = function
  | Block.Blank_line _ -> true
  | _ -> false

(** A trailing blank line inside a list item is loose-list layout, not content.
    It is also an {e empty continuation}: there is nothing for a trailing colon
    to claim, so the item is not keyed at all (Rule 2). *)
let ends_with_blank_line (blocks : Block.t list) : bool =
  match List.rev blocks with
  | b :: _ -> is_blank_line b
  | [] -> false

(** Channel carrying the block-id marker that an id-only value stood for, from
    the rewrite to {!unkey}. The marker is not part of the keyed node's body --
    that is the point of the id-only rule -- so {!unkey} would otherwise drop it
    and the rewrite would stop being content-invisible. *)
let id_only_marker : Inline.t Meta.key = Meta.key ()

(** Split [bs] at the first blank line. *)
let span_non_blank (bs : Block.t list) : Block.t list * Block.t list =
  let rec go acc = function
    | [] -> List.rev acc, []
    | b :: _ as rest when is_blank_line b -> List.rev acc, rest
    | b :: rest -> go (b :: acc) rest
  in
  go [] bs

let wrap_blocks : Block.t list -> Block.t = function
  | [] -> Block.Blocks ([], Meta.none)
  | [ single ] -> single
  | multiple -> Block.Blocks (multiple, Meta.none)

let mk_keyed label body = Block.Ext_keyed ((label, body), Meta.none)

(** Build nested keyed nodes from a list of label inlines (outermost first) and
    a body block. Whether the result renders as a list item or a free block is
    decided by where it is placed (an item's block vs a free block), not by the
    node itself. An empty [labels] is impossible from {!decompose}, but handled
    gracefully by returning the body unchanged.

    An id-only [marker] is recorded on the {e innermost} node: the marker sits at
    the end of the last label in the source, so that is where {!unkey} must put
    it back. The identifier it yields names the {e outermost} node instead (the
    block the paragraph became), which {!set_outer_meta} attaches -- the marker's
    two effects deliberately land on different nodes. *)
let build_nested_keyed
      ?(marker : Inline.t option)
      (labels : Inline.t list)
      (body : Block.t)
  : Block.t
  =
  match List.rev labels with
  | [] -> body
  | innermost :: outers ->
    let inner_meta =
      match marker with
      | None -> Meta.none
      | Some i -> Meta.add id_only_marker i Meta.none
    in
    let inner = Block.Ext_keyed ((innermost, body), inner_meta) in
    List.fold_left (fun acc label -> mk_keyed label acc) inner outers

(** Replace the meta of the outermost keyed node. Used to forward a transformed
    paragraph's meta (which may carry e.g. a {!Block.Block_id.t}) onto the keyed
    node that supplants it.

    Block attributes are {e not} carried this way: the fork represents them as
    an {!Block.Ext_attributes} wrapper around the target, which the sibling
    rewrite re-wraps around the produced keyed node.

    An {!id_only_marker} already on the node survives the replacement: on a
    one-label chain the innermost node {e is} the outermost one, and dropping the
    marker would lose it from {!unkey}. *)
let set_outer_meta (meta : Meta.t) (block : Block.t) : Block.t =
  match block with
  | Block.Ext_keyed ((l, b), old) ->
    let meta =
      match Meta.find id_only_marker old with
      | None -> meta
      | Some i -> Meta.add id_only_marker i meta
    in
    if meta == Meta.none then block else Block.Ext_keyed ((l, b), meta)
  | _ -> block

(** Rebuild a div, preserving layout, with a new body. *)
let div_with_body (d : Block.Div.t) (body : Block.t) : Block.Div.t =
  Block.Div.make
    ~indent:(Block.Div.indent d)
    ~opening_fence:(Block.Div.opening_fence d)
    ?class':(Block.Div.class' d)
    ~closing_fence:(Block.Div.closing_fence d)
    body

let value_paragraph (value : Inline.t) : Block.t =
  Block.Paragraph (Block.Paragraph.make value, Meta.none)

(** The leading paragraph's meta (carrying the keying segments) and the item's
    remaining sub-blocks. *)
let list_item_paragraph (item : Block.List_item.t)
  : (Meta.t * Block.t list) option
  =
  match Block.List_item.block item with
  | Block.Paragraph (_, m) -> Some (m, [])
  | Block.Blocks (Block.Paragraph (_, m) :: rest, _) -> Some (m, rest)
  | _ -> None

let rebuild_item (item : Block.List_item.t) (block : Block.t) : Block.List_item.t =
  Block.List_item.make
    ~before_marker:(Block.List_item.before_marker item)
    ~marker:(Block.List_item.marker item)
    ~after_marker:(Block.List_item.after_marker item)
    ?ext_task_marker:(Block.List_item.ext_task_marker item)
    block

let make_list
      (l : Block.List'.t)
      (list_meta : Meta.t)
      (items : Block.List_item.t node list)
  : Block.t
  =
  Block.List
    (Block.List'.make ~tight:(Block.List'.tight l) (Block.List'.type' l) items, list_meta)

(* Tree rewrite
   ============

   The whole rewrite is a family of mutually-recursive walkers closed over the
   [paragraph_inline_value] knob -- no global mutable state. *)

let rewrite_block ~(paragraph_inline_value : bool) (root : Block.t) : Block.t =
  let rec rewrite_block_list (blocks : Block.t list) : Block.t list =
    match blocks with
    | [] -> []
    | (Block.Paragraph (_, p_meta) as block) :: rest ->
      (match decompose p_meta with
       | None -> rewrite_within_block block :: rewrite_block_list rest
       | Some (Chain_trailing_colon (labels, marker)) ->
         absorb_paragraph_trailing ~original:block ~original_meta:p_meta ~labels ~marker rest
       | Some (Chain_with_value (labels, value)) ->
         if paragraph_inline_value
         then (
           let body = value_paragraph value in
           let keyed = build_nested_keyed labels body in
           set_outer_meta p_meta keyed :: rewrite_block_list rest)
         else rewrite_within_block block :: rewrite_block_list rest)
    | Block.List (l, list_meta) :: rest -> handle_list l list_meta rest
    | Block.Ext_attributes (a, attr_meta) :: rest ->
      (* A block attribute wraps its target. Look through the wrapper for a
         keyable paragraph; if keying happens, re-wrap the resulting keyed node
         so the attribute still applies. *)
      let specs = Block.Attributes.specs a in
      let rewrap (b : Block.t) : Block.t =
        Block.Ext_attributes (Block.Attributes.make ~specs b, attr_meta)
      in
      (match Block.Attributes.block a with
       | Block.Paragraph (_, inner_meta) as inner ->
         (match decompose inner_meta with
          | None -> rewrap (rewrite_within_block inner) :: rewrite_block_list rest
          | Some (Chain_trailing_colon (labels, marker)) ->
            (match absorb_trailing_core ~labels ~marker rest with
             | None -> rewrap inner :: rewrite_block_list rest
             | Some (keyed, after) ->
               rewrap (set_outer_meta inner_meta keyed) :: rewrite_block_list after)
          | Some (Chain_with_value (labels, value)) ->
            if paragraph_inline_value
            then (
              let body = value_paragraph value in
              let keyed = build_nested_keyed labels body in
              rewrap (set_outer_meta inner_meta keyed) :: rewrite_block_list rest)
            else rewrap (rewrite_within_block inner) :: rewrite_block_list rest)
       | inner -> rewrap (rewrite_within_block inner) :: rewrite_block_list rest)
    | block :: rest -> rewrite_within_block block :: rewrite_block_list rest

  and absorb_trailing_core ~labels ~marker rest : (Block.t * Block.t list) option =
    let children, after = span_non_blank rest in
    match children with
    | [] -> None
    | _ :: _ ->
      let body = wrap_blocks (rewrite_block_list children) in
      Some (build_nested_keyed ?marker labels body, after)

  and absorb_paragraph_trailing ~original ~original_meta ~labels ~marker rest =
    match absorb_trailing_core ~labels ~marker rest with
    | None -> original :: rewrite_block_list rest
    | Some (keyed, after) ->
      set_outer_meta original_meta keyed :: rewrite_block_list after

  and handle_list l list_meta rest =
    let items = Block.List'.items l in
    let rec loop items rest =
      let items, rest, absorbed = rewrite_list_items l items rest in
      if absorbed
      then (
        match rest with
        | Block.List (l', _) :: rest'
          when Block.List'.type' l = Block.List'.type' l' ->
          let more_items, rest' = loop (Block.List'.items l') rest' in
          items @ more_items, rest'
        | _ -> items, rest)
      else items, rest
    in
    let items, rest = loop items rest in
    make_list l list_meta items :: rewrite_block_list rest

  and rewrite_list_items
        (l : Block.List'.t)
        (items : Block.List_item.t node list)
        (following : Block.t list)
    : Block.List_item.t node list * Block.t list * bool
    =
    match items with
    | [] -> [], following, false
    | [ (item, meta) ] ->
      let item', following, absorbed = rewrite_last_item item following in
      [ item', meta ], following, absorbed
    | (item, meta) :: rest_items ->
      (match try_tag_non_last_item l item rest_items with
       | `Absorbed_rest new_block -> [ rebuild_item item new_block, meta ], following, false
       | `Tagged new_block ->
         let rest_items, following, absorbed = rewrite_list_items l rest_items following in
         (rebuild_item item new_block, meta) :: rest_items, following, absorbed
       | `Untouched ->
         let block = Block.List_item.block item in
         let block' = rewrite_within_block block in
         let item = if block == block' then item else rebuild_item item block' in
         let rest_items, following, absorbed = rewrite_list_items l rest_items following in
         (item, meta) :: rest_items, following, absorbed)

  and try_tag_non_last_item
        (l : Block.List'.t)
        (item : Block.List_item.t)
        (rest_items : Block.List_item.t node list)
    =
    match list_item_paragraph item with
    | None -> `Untouched
    | Some (meta, sub_blocks) ->
      (match decompose meta with
       | None -> `Untouched
       | Some (Chain_with_value (labels, value)) ->
         let body = wrap_blocks (value_paragraph value :: rewrite_block_list sub_blocks) in
         `Tagged (set_outer_meta meta (build_nested_keyed labels body))
       | Some (Chain_trailing_colon (labels, marker)) ->
         if ends_with_blank_line sub_blocks
         then `Untouched
         else if not (List.is_empty sub_blocks)
         then (
           let body = wrap_blocks (rewrite_block_list sub_blocks) in
           `Tagged (set_outer_meta meta (build_nested_keyed ?marker labels body)))
         else (
           (* Bare trailing-colon middle item absorbs remaining siblings as a
              nested list of the same type. *)
           let absorbed_items, _, _ = rewrite_list_items l rest_items [] in
           let nested_list = make_list l Meta.none absorbed_items in
           `Absorbed_rest
             (set_outer_meta meta (build_nested_keyed ?marker labels nested_list))))

  and rewrite_last_item (item : Block.List_item.t) (following : Block.t list)
    : Block.List_item.t * Block.t list * bool
    =
    let recurse_item () =
      let block = Block.List_item.block item in
      let block' = rewrite_within_block block in
      if block == block' then item else rebuild_item item block'
    in
    match list_item_paragraph item with
    | None -> recurse_item (), following, false
    | Some (meta, sub_blocks) ->
      (match decompose meta with
       | None -> recurse_item (), following, false
       | Some (Chain_with_value (labels, value)) ->
         let body = wrap_blocks (value_paragraph value :: rewrite_block_list sub_blocks) in
         ( rebuild_item item (set_outer_meta meta (build_nested_keyed labels body))
         , following
         , false )
       | Some (Chain_trailing_colon (labels, marker)) ->
         if ends_with_blank_line sub_blocks
         then recurse_item (), following, false
         else if not (List.is_empty sub_blocks)
         then (
           let body = wrap_blocks (rewrite_block_list sub_blocks) in
           ( rebuild_item item (set_outer_meta meta (build_nested_keyed ?marker labels body))
           , following
           , false ))
         else (
           let absorbed, remaining = span_non_blank following in
           if List.is_empty absorbed
           then item, following, false
           else (
             let body = wrap_blocks (rewrite_block_list absorbed) in
             let new_block =
               set_outer_meta meta (build_nested_keyed ?marker labels body)
             in
             rebuild_item item new_block, remaining, true)))

  and rewrite_within_block (block : Block.t) : Block.t =
    match block with
    | Block.Blocks (blocks, meta) -> Block.Blocks (rewrite_block_list blocks, meta)
    | Block.Block_quote (bq, meta) ->
      let inner = Block.Block_quote.block bq in
      Block.Block_quote (Block.Block_quote.make (rewrite_within_block inner), meta)
    | Block.List (l, list_meta) ->
      (match handle_list l list_meta [] with
       | [ single ] -> single
       | multiple -> Block.Blocks (multiple, Meta.none))
    | Block.Paragraph (_, p_meta) as block ->
      (match decompose p_meta with
       | Some (Chain_with_value (labels, value)) when paragraph_inline_value ->
         let body = value_paragraph value in
         set_outer_meta p_meta (build_nested_keyed labels body)
       | _ -> block)
    | Block.Ext_div (d, meta) ->
      Block.Ext_div (div_with_body d (rewrite_within_block (Block.Div.block d)), meta)
    | Block.Ext_attributes (a, meta) ->
      let specs = Block.Attributes.specs a in
      Block.Ext_attributes
        (Block.Attributes.make ~specs (rewrite_within_block (Block.Attributes.block a)), meta)
    | Block.Ext_keyed ((label, body), meta) ->
      Block.Ext_keyed ((label, rewrite_within_block body), meta)
    | _ -> block
  in
  rewrite_within_block root

let rewrite_doc ?(paragraph_inline_value = true) (doc : Doc.t) : Doc.t =
  let block = Doc.block doc in
  let block' = rewrite_block ~paragraph_inline_value block in
  if block == block'
  then doc
  else Doc.make ~nl:(Doc.nl doc) ~defs:(Doc.defs doc) block'

(* Content-preserving inverse
   ==========================

   Flatten keyed nodes back to the plain blocks they stand for. Because each
   label carries its ":" separator as real content (see {!Inline_struct}),
   re-joining a label to its value reproduces the source modulo
   {!Inline.normalize}; hence [unkey (rewrite_block ... b)] equals [b] under
   normalisation -- the Struct rewrite is content-invisible. Renderers without
   keyed-node support (e.g. HTML) can {!unkey} a node and render the result as
   ordinary CommonMark. *)

let rec unkey (block : Block.t) : Block.t =
  match block with
  | Block.Ext_keyed ((label, body), meta) ->
    (match unkeyed_blocks ~meta label body with
     | [ single ] -> single
     | blocks -> Block.Blocks (blocks, Meta.none))
  | Block.Blocks (bs, m) -> Block.Blocks (List.map unkey bs, m)
  | Block.Block_quote (bq, m) ->
    Block.Block_quote (Block.Block_quote.make (unkey (Block.Block_quote.block bq)), m)
  | Block.List (l, m) ->
    let items =
      List.map
        (fun (item, im) -> rebuild_item item (unkey (Block.List_item.block item)), im)
        (Block.List'.items l)
    in
    make_list l m items
  | Block.Ext_div (d, m) -> Block.Ext_div (div_with_body d (unkey (Block.Div.block d)), m)
  | Block.Ext_attributes (a, m) ->
    let specs = Block.Attributes.specs a in
    Block.Ext_attributes
      (Block.Attributes.make ~specs (unkey (Block.Attributes.block a)), m)
  | b -> b

(* The plain blocks a single keyed node stands for. The leading value paragraph
   (inline-value form) is re-joined to the label, which already carries the ":";
   a body opening with a non-paragraph (trailing-colon form, e.g. an absorbed
   sub-list) keeps a bare "label:" paragraph followed by the body. A nested
   [Blocks] is left for [Block.normalize] to splice into the parent.

   An id-only value is not in the body at all -- it was consumed as the node's
   identifier (see {!is_id_only_value}) -- so the marker is restored from
   {!id_only_marker} and re-joined to the label, exactly as a value would be. *)
and unkeyed_blocks ~(meta : Meta.t) (label : Inline.t) (body : Block.t) : Block.t list =
  let para inline = Block.Paragraph (Block.Paragraph.make inline, Meta.none) in
  let merge value = Inline.Inlines ([ label; value ], Meta.none) in
  match Meta.find id_only_marker meta with
  | Some marker ->
    let head = para (merge marker) in
    (match unkey body with
     | Block.Blocks (bs, _) -> head :: bs
     | other -> [ head; other ])
  | None ->
    (match unkey body with
     | Block.Paragraph (p, _) -> [ para (merge (Block.Paragraph.inline p)) ]
     | Block.Blocks (Block.Paragraph (p, _) :: rest, _) ->
       para (merge (Block.Paragraph.inline p)) :: rest
     | Block.Blocks (bs, _) -> para label :: bs
     | other -> [ para label; other ])

(* Bare key of a keyed label
   =========================

   The inverse of the raw-separator slicing: drop the trailing ":" separator a
   label carries (see {!Inline_struct} and {!unkey}). Unlike {!unkey} -- which
   keeps the separator so the rewrite stays content-invisible -- this recovers
   the displayed key for a *semantic* renderer (e.g. a <dl>/<dt>). *)

(** Drop a [Text] label's trailing separator: the blanks the splitter put after
    the colon, then the single separator colon itself. Blanks and colons that
    belong to the key (e.g. the [:] in [http://x.com]) are preserved -- a label
    segment never contains an internal structural [": "]. *)
let strip_separator_text (s : string) : string =
  let j = ref (String.length s) in
  while !j > 0 && (s.[!j - 1] = ' ' || s.[!j - 1] = '\t') do decr j done;
  if !j > 0 && s.[!j - 1] = ':' then decr j;
  String.sub s 0 !j

let label_key (label : Inline.t) : Inline.t =
  match label with
  | Inline.Inlines ([ unit; Inline.Text (sep, _) ], _) when is_separator_text sep -> unit
  | Inline.Text (s, meta) -> Inline.Text (strip_separator_text s, meta)
  | other -> other
