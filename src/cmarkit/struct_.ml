(* Oymarkit: Struct -- colon-keyed tree restructuring.

   A dedicated, self-contained pass that rewrites an already-parsed
   [Doc.t] so that list items and paragraphs carrying a colon-delimited
   label/value relationship become {!Block.Ext_keyed_list_item} /
   {!Block.Ext_keyed_block} nodes.

   The keyed-node *constructors* live in {!Block} (so that the core
   traversals — [normalize], [defs], [Mapper], [Folder] — and the
   renderers can give them structural recursion, exactly like
   [Ext_div]). Everything else — colon detection and the whole tree
   rewrite — is centralized here.

   This pass is opt-in: it is enabled simply by calling {!rewrite_doc};
   nothing produces keyed nodes unless this pass runs. *)

open Common_

(* String helpers
   ==============

   The core library is Stdlib-only (no Base/Core), so the few
   string operations struct needs are spelled out here. *)

let is_ws = function ' ' | '\t' | '\n' | '\r' | '\011' | '\012' -> true | _ -> false

let lstrip (s : string) : string =
  let n = String.length s in
  let i = ref 0 in
  while !i < n && is_ws s.[!i] do incr i done;
  if !i = 0 then s else String.sub s !i (n - !i)

let rstrip (s : string) : string =
  let n = String.length s in
  let j = ref n in
  while !j > 0 && is_ws s.[!j - 1] do decr j done;
  if !j = n then s else String.sub s 0 !j

let strip (s : string) : string = lstrip (rstrip s)
let is_blank (s : string) : bool = String.for_all is_ws s

(** Index of the first occurrence of [pat] in [s], if any. *)
let find_sub (s : string) (pat : string) : int option =
  let n = String.length s and m = String.length pat in
  if m = 0
  then Some 0
  else (
    let last = n - m in
    let matches i =
      let k = ref 0 in
      while !k < m && Char.equal s.[i + !k] pat.[!k] do incr k done;
      !k = m
    in
    let rec loop i = if i > last then None else if matches i then Some i else loop (i + 1) in
    loop 0)

(* Colon detection
   =============== *)

module Colon : sig
  type decomposition =
    | Chain_trailing_colon of Inline.t list
    | Chain_with_value of Inline.t list * Inline.t

  val decompose : Inline.t -> decomposition option
end = struct
  (** Count consecutive backslashes immediately before position [pos] in [s]. *)
  let count_preceding_backslashes (s : string) (pos : int) : int =
    let rec go i n = if i >= 0 && Char.equal s.[i] '\\' then go (i - 1) (n + 1) else n in
    go (pos - 1) 0

  (** Strip a trailing [:] from a raw text string. The original must end with
      [:] directly — any trailing whitespace means the colon was followed by a
      space, not end-of-line, and is not a structural trailing colon. A colon
      preceded by an odd number of backslashes is escaped. *)
  let strip_colon_from_text (s : string) : string option =
    let len = String.length s in
    if len = 0 || not (Char.equal s.[len - 1] ':')
    then None
    else if count_preceding_backslashes s (len - 1) mod 2 = 1
    then None
    else Some (rstrip (String.sub s 0 (len - 1)))

  (** Walk the inline tree rightward; if the rightmost [Text] leaf ends with an
      unescaped [:], return the tree with that colon removed. *)
  let rec strip_trailing_colon (inline : Inline.t) : Inline.t option =
    match inline with
    | Inline.Text (s, meta) ->
      (match strip_colon_from_text s with
       | None -> None
       | Some stripped -> Some (Inline.Text (stripped, meta)))
    | Inline.Inlines (inlines, meta) ->
      let rec go_last rev_prefix = function
        | [] -> None
        | [ last ] ->
          (match strip_trailing_colon last with
           | Some last' -> Some (Inline.Inlines (List.rev_append rev_prefix [ last' ], meta))
           | None -> None)
        | x :: rest -> go_last (x :: rev_prefix) rest
      in
      go_last [] inlines
    | _ -> None

  (** Unwrap [Inlines] to a flat child list; other nodes become a singleton. *)
  let unwrap_inline (inline : Inline.t) : Inline.t list =
    match inline with
    | Inline.Inlines (is, _) -> is
    | other -> [ other ]

  let non_blank_text = function
    | Inline.Text (s, _) -> not (is_blank s)
    | _ -> true

  (** A segment is a valid label if it consists of a single simple inline unit
      after filtering empty text nodes. *)
  let as_simple_label (segment : Inline.t list) : Inline.t option =
    match List.filter non_blank_text segment with
    | [] -> Some (Inline.Text ("", Meta.none))
    | [ Inline.Text (s, meta) ] -> Some (Inline.Text (strip s, meta))
    | [ (Inline.Emphasis _ as e) ]
    | [ (Inline.Strong_emphasis _ as e) ]
    | [ (Inline.Code_span _ as e) ]
    | [ (Inline.Autolink _ as e) ] -> Some e
    | _ -> None

  (** Split a flat list of inlines into segments at [": "] (colon-space)
      boundaries found inside [Text] nodes. Non-text inlines are never split. *)
  let split_at_colon_space (children : Inline.t list) : Inline.t list list =
    (* [current_rev]: inlines accumulated for the segment being built (reversed).
       [segments_rev]: completed segments (reversed). *)
    let flush current_rev segments_rev = List.rev current_rev :: segments_rev in
    let rec go current_rev segments_rev = function
      | [] -> List.rev (flush current_rev segments_rev)
      | Inline.Text (s, meta) :: rest -> split_text current_rev segments_rev s meta rest
      | other :: rest -> go (other :: current_rev) segments_rev rest
    and split_text current_rev segments_rev s meta rest =
      match find_sub s ": " with
      | None -> go (Inline.Text (s, meta) :: current_rev) segments_rev rest
      | Some i ->
        let before = String.sub s 0 i in
        let after = lstrip (String.sub s (i + 1) (String.length s - (i + 1))) in
        let current_rev =
          if is_blank before
          then current_rev
          else Inline.Text (rstrip before, meta) :: current_rev
        in
        let segments_rev = flush current_rev segments_rev in
        if is_blank after
        then go [] segments_rev rest
        else split_text [] segments_rev after meta rest
    in
    go [] [] children

  (** Reassemble a segment (inline list) into a single [Inline.t]. *)
  let rebuild_value_inline (segment : Inline.t list) : Inline.t =
    match segment with
    | [] -> Inline.Text ("", Meta.none)
    | [ single ] -> single
    | multiple -> Inline.Inlines (multiple, Meta.none)

  (** A value segment is valid iff — after removing empty text nodes — it is
      non-empty and does not begin with a soft/hard break. A leading break would
      mean the value is on the next source line; the colon must be followed by
      its value on the same line. *)
  let is_valid_value_segment (segment : Inline.t list) : bool =
    match List.filter non_blank_text segment with
    | [] -> false
    | Inline.Break _ :: _ -> false
    | _ -> true

  let validate_labels (segments : Inline.t list list) : Inline.t list option =
    let labels = List.filter_map as_simple_label segments in
    if List.length labels = List.length segments && labels <> [] then Some labels else None

  type decomposition =
    | Chain_trailing_colon of Inline.t list
    | Chain_with_value of Inline.t list * Inline.t

  (** Decompose an inline into a keying decomposition.

      {ul
      {- Trailing [:] (no space after) → all [": "]-separated segments are chain
         labels; body comes from sub-blocks or absorbed following content.}
      {- No trailing [:] but at least one [": "] split → the last segment is the
         inline value (unrestricted), preceding segments are chain labels.}
      {- Otherwise → no decomposition.}} *)
  let decompose (inline : Inline.t) : decomposition option =
    match strip_trailing_colon inline with
    | Some stripped ->
      let segments = split_at_colon_space (unwrap_inline stripped) in
      Option.map (fun labels -> Chain_trailing_colon labels) (validate_labels segments)
    | None ->
      let segments = split_at_colon_space (unwrap_inline inline) in
      (match List.rev segments with
       | [] | [ _ ] -> None
       | value_seg :: rev_label_segs ->
         if not (is_valid_value_segment value_seg)
         then None
         else (
           match validate_labels (List.rev rev_label_segs) with
           | None -> None
           | Some labels -> Some (Chain_with_value (labels, rebuild_value_inline value_seg))))

  let%test_module "strip_trailing_colon" =
    (module struct
      let text s = Inline.Text (s, Meta.none)

      let check s =
        match strip_trailing_colon (text s) with
        | Some (Inline.Text (s, _)) -> Some s
        | Some _ -> Some "<non-text>"
        | None -> None

      let%test "basic" = check "foo:" = Some "foo"
      let%test "no colon" = check "foo" = None
      let%test "trailing space prevents stripping" = check "foo: " = None
      let%test "bare colon" = check ":" = Some ""
      let%test "empty" = check "" = None
      let%test "escaped (odd backslash)" = check "foo\\:" = None
      let%test "double backslash (even) is not escaped" = check "foo\\\\:" = Some "foo\\\\"
      let%test "triple backslash (odd) is escaped" = check "foo\\\\\\:" = None

      let%test "inlines" =
        let inline = Inline.Inlines ([ text "hello "; text "world:" ], Meta.none) in
        Option.is_some (strip_trailing_colon inline)

      let%test "code span is not text" =
        let cs = Inline.Code_span (Inline.Code_span.of_string "foo:", Meta.none) in
        Option.is_none (strip_trailing_colon cs)

      let%test "emphasis is opaque" =
        let em = Inline.Emphasis (Inline.Emphasis.make (text "foo:"), Meta.none) in
        Option.is_none (strip_trailing_colon em)

      let%test "emphasis before trailing colon" =
        let inline =
          Inline.Inlines
            ( [ Inline.Emphasis (Inline.Emphasis.make (text "foo"), Meta.none); text " bar:" ]
            , Meta.none )
        in
        Option.is_some (strip_trailing_colon inline)
    end)
end

(* Shared helpers
   ============== *)

let is_blank_line : Block.t -> bool = function
  | Block.Blank_line _ -> true
  | _ -> false

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

let mk_keyed_block label body = Block.Ext_keyed_block ((label, body), Meta.none)
let mk_keyed_item label body = Block.Ext_keyed_list_item ((label, body), Meta.none)

(** Build nested keyed nodes from a list of label inlines (outermost first) and
    a body block. An empty [labels] is impossible from {!Colon.decompose}, but
    handled gracefully by returning the body unchanged. *)
let build_nested_keyed
      ~(make_node : Inline.t -> Block.t -> Block.t)
      (labels : Inline.t list)
      (body : Block.t)
  : Block.t
  =
  match List.rev labels with
  | [] -> body
  | innermost :: outers ->
    List.fold_left (fun acc label -> make_node label acc) (make_node innermost body) outers

(** Replace the meta of the outermost keyed node. Used to forward a transformed
    paragraph's meta (which may carry e.g. a {!Block.Block_id.t}) onto the keyed
    node that supplants it.

    Block attributes are {e not} carried this way: the fork represents them as
    an {!Block.Ext_attributes} wrapper around the target, which the sibling
    rewrite re-wraps around the produced keyed node. *)
let set_outer_meta (meta : Meta.t) (block : Block.t) : Block.t =
  if meta == Meta.none
  then block
  else (
    match block with
    | Block.Ext_keyed_block ((l, b), _) -> Block.Ext_keyed_block ((l, b), meta)
    | Block.Ext_keyed_list_item ((l, b), _) -> Block.Ext_keyed_list_item ((l, b), meta)
    | _ -> block)

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

let list_item_paragraph (item : Block.List_item.t)
  : (Block.Paragraph.t * Block.t list) option
  =
  match Block.List_item.block item with
  | Block.Paragraph (p, _) -> Some (p, [])
  | Block.Blocks (Block.Paragraph (p, _) :: rest, _) -> Some (p, rest)
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
    | (Block.Paragraph (p, p_meta) as block) :: rest ->
      (match Colon.decompose (Block.Paragraph.inline p) with
       | None -> rewrite_within_block block :: rewrite_block_list rest
       | Some (Colon.Chain_trailing_colon labels) ->
         absorb_paragraph_trailing ~original:block ~original_meta:p_meta ~labels rest
       | Some (Colon.Chain_with_value (labels, value)) ->
         if paragraph_inline_value
         then (
           let body = value_paragraph value in
           let keyed = build_nested_keyed ~make_node:mk_keyed_block labels body in
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
       | Block.Paragraph (p, _) as inner ->
         (match Colon.decompose (Block.Paragraph.inline p) with
          | None -> rewrap (rewrite_within_block inner) :: rewrite_block_list rest
          | Some (Colon.Chain_trailing_colon labels) ->
            (match absorb_trailing_core ~labels rest with
             | None -> rewrap inner :: rewrite_block_list rest
             | Some (keyed, after) -> rewrap keyed :: rewrite_block_list after)
          | Some (Colon.Chain_with_value (labels, value)) ->
            if paragraph_inline_value
            then (
              let body = value_paragraph value in
              let keyed = build_nested_keyed ~make_node:mk_keyed_block labels body in
              rewrap keyed :: rewrite_block_list rest)
            else rewrap (rewrite_within_block inner) :: rewrite_block_list rest)
       | inner -> rewrap (rewrite_within_block inner) :: rewrite_block_list rest)
    | block :: rest -> rewrite_within_block block :: rewrite_block_list rest

  and absorb_trailing_core ~labels rest : (Block.t * Block.t list) option =
    let children, after = span_non_blank rest in
    match children with
    | [] -> None
    | _ :: _ ->
      let body = wrap_blocks (rewrite_block_list children) in
      Some (build_nested_keyed ~make_node:mk_keyed_block labels body, after)

  and absorb_paragraph_trailing ~original ~original_meta ~labels rest =
    match absorb_trailing_core ~labels rest with
    | None -> original :: rewrite_block_list rest
    | Some (keyed, after) -> set_outer_meta original_meta keyed :: rewrite_block_list after

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
    | Some (p, sub_blocks) ->
      (match Colon.decompose (Block.Paragraph.inline p) with
       | None -> `Untouched
       | Some (Colon.Chain_with_value (labels, value)) ->
         let body = wrap_blocks (value_paragraph value :: rewrite_block_list sub_blocks) in
         `Tagged (build_nested_keyed ~make_node:mk_keyed_item labels body)
       | Some (Colon.Chain_trailing_colon labels) ->
         if not (List.is_empty sub_blocks)
         then (
           let body = wrap_blocks (rewrite_block_list sub_blocks) in
           `Tagged (build_nested_keyed ~make_node:mk_keyed_item labels body))
         else (
           (* Bare trailing-colon middle item absorbs remaining siblings as a
              nested list of the same type. *)
           let absorbed_items, _, _ = rewrite_list_items l rest_items [] in
           let nested_list = make_list l Meta.none absorbed_items in
           `Absorbed_rest (build_nested_keyed ~make_node:mk_keyed_item labels nested_list)))

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
    | Some (p, sub_blocks) ->
      (match Colon.decompose (Block.Paragraph.inline p) with
       | None -> recurse_item (), following, false
       | Some (Colon.Chain_with_value (labels, value)) ->
         let body = wrap_blocks (value_paragraph value :: rewrite_block_list sub_blocks) in
         rebuild_item item (build_nested_keyed ~make_node:mk_keyed_item labels body), following, false
       | Some (Colon.Chain_trailing_colon labels) ->
         if not (List.is_empty sub_blocks)
         then (
           let body = wrap_blocks (rewrite_block_list sub_blocks) in
           rebuild_item item (build_nested_keyed ~make_node:mk_keyed_item labels body), following, false)
         else (
           let absorbed, remaining = span_non_blank following in
           if List.is_empty absorbed
           then item, following, false
           else (
             let body = wrap_blocks (rewrite_block_list absorbed) in
             let new_block = build_nested_keyed ~make_node:mk_keyed_item labels body in
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
    | Block.Paragraph (p, p_meta) as block ->
      (match Colon.decompose (Block.Paragraph.inline p) with
       | Some (Colon.Chain_with_value (labels, value)) when paragraph_inline_value ->
         let body = value_paragraph value in
         set_outer_meta p_meta (build_nested_keyed ~make_node:mk_keyed_block labels body)
       | _ -> block)
    | Block.Ext_div (d, meta) ->
      Block.Ext_div (div_with_body d (rewrite_within_block (Block.Div.block d)), meta)
    | Block.Ext_attributes (a, meta) ->
      let specs = Block.Attributes.specs a in
      Block.Ext_attributes
        (Block.Attributes.make ~specs (rewrite_within_block (Block.Attributes.block a)), meta)
    | Block.Ext_keyed_list_item ((label, body), meta) ->
      Block.Ext_keyed_list_item ((label, rewrite_within_block body), meta)
    | Block.Ext_keyed_block ((label, body), meta) ->
      Block.Ext_keyed_block ((label, rewrite_within_block body), meta)
    | _ -> block
  in
  rewrite_within_block root

let rewrite_doc ?(paragraph_inline_value = true) (doc : Doc.t) : Doc.t =
  let block = Doc.block doc in
  let block' = rewrite_block ~paragraph_inline_value block in
  if block == block'
  then doc
  else Doc.make ~nl:(Doc.nl doc) ~defs:(Doc.defs doc) block'
