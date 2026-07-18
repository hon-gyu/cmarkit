(** _
    - {!Config} encodes {!mod:Typing} rules
    - when the typed config is enabled, we expect the generator to produce ASTs
      that satisfy the typing rules *)

open Cmarkit_
module G = QCheck2.Gen
open Gen_inline

let (gen_string : string G.t) = G.string_printable

let blank_line_egs : Block.t list =
  [ ""; "  "; "\t" ] |> List.map (fun bl -> Block.(Blank_line (bl, Meta.none)))

let gen_blank_line : Block.t G.t =
  (* according to cmarkit.mli:Layout.blanks, blank is only made of spaces and tabs *)
  let open G in
  let vocab = [ " "; "\t" ] in
  let (words_g : string list t) = list_size nat_small (oneof_list vocab) in
  map
    (fun words ->
      let bl = String.concat "" words in
      Block.(Blank_line (bl, Meta.none)))
    words_g

(* Generate a thematic break with a varied but valid layout: a run of >= 3
   matching [-], [*] or [_], each optionally followed by spaces/tabs, with an
   optional 0-3 indent. [exclude] removes characters from the candidate set,
   used to avoid the bullet-marker collision (see {!Typing.no_marker_colliding_
   thematic_break}): e.g. [- ---] is a uniform [-] run and reparses as a
   thematic break, not a list item. *)
let gen_thematic_break ?(exclude = []) () : Block.t G.t =
  let open G in
  let chars =
    List.filter (fun c -> not (List.mem c exclude)) [ '-'; '*'; '_' ]
  in
  let* c = oneof_list chars in
  let* k = int_range 3 5 in
  let* gaps =
    list_size (return k)
      (oneof_list_weighted [ (4, ""); (2, " "); (1, "  "); (1, "\t") ])
  in
  let layout =
    let b = Buffer.create 16 in
    List.iter
      (fun gap ->
        Buffer.add_char b c;
        Buffer.add_string b gap)
      gaps;
    Buffer.contents b
  in
  let* indent = oneof_list_weighted [ (4, 0); (1, 1); (1, 2); (1, 3) ] in
  return
    Block.(
      Thematic_break (Block.Thematic_break.make ~indent ~layout (), Meta.none))

let code_block_egs : Block.t list =
  Block.Code_block.
    [
      make [];
      make ~info_string:("ocaml", Meta.none) [ ("let x = 1", Meta.none) ];
      make ~layout:`Indented [ ("indented code", Meta.none) ];
    ]
  |> List.map (fun cb -> Block.(Code_block (cb, Meta.none)))

(* Render-order peeling for the html-block absorption rule
   ({!Typing.no_html_block_absorbing_successor}). Absorption only crosses
   [Blocks] siblings; a [Block_quote]/[List] boundary stops it, so we peel
   [Blocks] tails/heads but no further. *)
let rec trailing_absorbing : Block.t -> bool = function
  | Block.Blocks (bs, _) -> (
      match List.rev bs with
      | last :: _ -> trailing_absorbing last
      | [] -> false)
  | Block.Html_block (lines, _) -> Common_.html_block_absorbs lines
  | _ -> false

let rec leads_with_blank : Block.t -> bool = function
  | Block.Blocks (b0 :: _, _) -> leads_with_blank b0
  | Block.Blank_line _ -> true
  | _ -> false

(* Insert a [Blank_line] between any two consecutive siblings where the first's
   render-order trailing leaf is an absorbing html block and the second does not
   already start with a blank line. Never appends after the last element, so a
   genuinely-final html block keeps no trailing blank. *)
let separate_absorbing_html (bs : Block.t list) : Block.t list =
  let blank = Block.Blank_line ("", Meta.none) in
  let rec go = function
    | a :: (b :: _ as rest) ->
        if trailing_absorbing a && not (leads_with_blank b) then
          a :: blank :: go rest
        else a :: go rest
    | last -> last
  in
  go bs

(** Fence indented code blocks in the render-order context rejected by
    {!Typing.no_ambiguous_indented_code_after_list}.

    The traversal carries one bit of sibling state: whether the last non-blank
    block at the current container level was a list whose final item accepts a
    four-space continuation line. Blank lines and nested [Blocks] preserve this
    state because neither establishes a CommonMark container boundary.
    [Block_quote], [List], and footnote contents are rewritten independently, so
    their boundaries reset the outer state.

    When such a list is followed by an [`Indented] code block, only the code
    block's layout is changed to the default fenced layout. Its content,
    metadata, and surrounding block structure are preserved.

    An alternative is context-aware sibling generation in {!gen_blocks}: after
    generating a list, its successor could be generated with indented code
    disabled. That would avoid this repair traversal, but it would thread
    render-order state through the recursive [gen_block]/[gen_blocks] API,
    including nested [Blocks]. This post-generation rewrite keeps the core
    generator compositional while retaining the generated code block rather than
    filtering out the entire AST. *)
let fence_ambiguous_indented_code (block : Block.t) : Block.t =
  let rec rewrite after_list = function
    | Block.Blocks (bs, meta) ->
        let after_list, bs = rewrite_blocks after_list bs in
        (after_list, Block.Blocks (bs, meta))
    | Block.Blank_line _ as block -> (after_list, block)
    | Block.List (l, meta) ->
        let rewrite_item (item, item_meta) =
          let _, block = rewrite false (Block.List_item.block item) in
          let item =
            Block.List_item.make
              ~before_marker:(Block.List_item.before_marker item)
              ~marker:(Block.List_item.marker item)
              ~after_marker:(Block.List_item.after_marker item)
              ?ext_task_marker:(Block.List_item.ext_task_marker item)
              block
          in
          (item, item_meta)
        in
        let items = List.map rewrite_item (Block.List'.items l) in
        let l =
          Block.List'.make ~tight:(Block.List'.tight l) (Block.List'.type' l)
            items
        in
        let after_list =
          match Common_.list_last_item_continuation_indent l with
          | Some indent -> indent <= 4
          | None -> after_list
        in
        (after_list, Block.List (l, meta))
    | Block.Block_quote (bq, meta) ->
        let _, block = rewrite false (Block.Block_quote.block bq) in
        let bq =
          Block.Block_quote.make ~indent:(Block.Block_quote.indent bq) block
        in
        (false, Block.Block_quote (bq, meta))
    | Block.Ext_footnote_definition (fn, meta) ->
        let _, block = rewrite false (Block.Footnote.block fn) in
        let fn =
          Block.Footnote.make ~indent:(Block.Footnote.indent fn)
            ~defined_label:(Block.Footnote.defined_label fn)
            (Block.Footnote.label fn) block
        in
        (false, Block.Ext_footnote_definition (fn, meta))
    | Block.Code_block (cb, meta)
      when after_list && Block.Code_block.layout cb = `Indented ->
        let cb =
          Block.Code_block.make
            ?info_string:(Block.Code_block.info_string cb)
            (Block.Code_block.code cb)
        in
        (false, Block.Code_block (cb, meta))
    | block -> (false, block)
  and rewrite_blocks after_list = function
    | [] -> (after_list, [])
    | block :: blocks ->
        let after_list, block = rewrite after_list block in
        let after_list, blocks = rewrite_blocks after_list blocks in
        (after_list, block :: blocks)
  in
  snd (rewrite false block)

(** Insert an outside blank line between adjacent block-quote siblings.

    Nested [Blocks] are transparent in render order, so the traversal carries
    adjacency state through them. Actual containers are rewritten independently.
    This preserves the two quote nodes and their inner block boundaries.

    TODO: this is quite complicated and I am not entirely sure if this is the
    best way *)
let separate_adjacent_block_quotes (block : Block.t) : Block.t =
  let blank = Block.Blank_line ("", Meta.none) in
  let rec rewrite = function
    | Block.Blocks (bs, meta) ->
        let _, bs = rewrite_blocks false bs in
        Block.Blocks (bs, meta)
    | Block.Block_quote (bq, meta) ->
        let block = rewrite (Block.Block_quote.block bq) in
        let bq =
          Block.Block_quote.make ~indent:(Block.Block_quote.indent bq) block
        in
        Block.Block_quote (bq, meta)
    | Block.List (l, meta) ->
        let rewrite_item (item, item_meta) =
          let block = rewrite (Block.List_item.block item) in
          let item =
            Block.List_item.make
              ~before_marker:(Block.List_item.before_marker item)
              ~marker:(Block.List_item.marker item)
              ~after_marker:(Block.List_item.after_marker item)
              ?ext_task_marker:(Block.List_item.ext_task_marker item)
              block
          in
          (item, item_meta)
        in
        let items = List.map rewrite_item (Block.List'.items l) in
        let l =
          Block.List'.make ~tight:(Block.List'.tight l) (Block.List'.type' l)
            items
        in
        Block.List (l, meta)
    | Block.Ext_footnote_definition (fn, meta) ->
        let block = rewrite (Block.Footnote.block fn) in
        let fn =
          Block.Footnote.make ~indent:(Block.Footnote.indent fn)
            ~defined_label:(Block.Footnote.defined_label fn)
            (Block.Footnote.label fn) block
        in
        Block.Ext_footnote_definition (fn, meta)
    | block -> block
  and rewrite_blocks after_quote = function
    | [] -> (after_quote, [])
    | Block.Blocks (bs, meta) :: rest ->
        let after_quote, bs = rewrite_blocks after_quote bs in
        let block = Block.Blocks (bs, meta) in
        let after_quote, rest = rewrite_blocks after_quote rest in
        (after_quote, block :: rest)
    | (Block.Block_quote _ as quote) :: rest ->
        let quote = rewrite quote in
        let prefix = if after_quote then [ blank; quote ] else [ quote ] in
        let after_quote, rest = rewrite_blocks true rest in
        (after_quote, prefix @ rest)
    | block :: rest ->
        let block = rewrite block in
        let after_quote, rest = rewrite_blocks false rest in
        (after_quote, block :: rest)
  in
  rewrite block

let html_block_egs : Block.t list =
  [
    [ ("<div>", Meta.none) ];
    [ ("<p>hello</p>", Meta.none) ];
    [ ("<!-- comment -->", Meta.none) ];
  ]
  |> List.map (fun lines -> Block.(Html_block (lines, Meta.none)))

module Bconfig = struct
  type t = {
    (* Pure block rules
    -------------------- *)
    no_direct_blank_line : bool;
    no_trailing_blank_line_in_blocks : bool;
    no_empty_paragraph : bool;
    no_empty_blocks : bool;
    no_empty_list : bool;
        (** A [List] with zero items has no syntactic witness (no item marker),
            so the parser never emits one. *)
    no_list_item_leading_blank_prefix : bool;
        (** A list item with two or more leading blank lines before its first
            non-blank block closes before that content on reparse. Blank-only
            items are still allowed. *)
    no_marker_colliding_thematic_break : bool;
        (** A bullet list item whose leading block is a thematic break of the
            {e same} character as the marker collapses on reparse: [- ---] is a
            uniform [-] run, so it parses as a thematic break, not a list. *)
    no_html_block_absorbing_successor : bool;
        (** A type-6/7 (and any unclosed) HTML block stays open at its last line
            and swallows the block that renders right after it. Insert a
            [Blank_line] between them (only ever {e between} siblings, never
            after the last, so a final html block keeps no trailing blank). *)
    no_ambiguous_indented_code_after_list : bool;
        (** If a list's final continuation indent is at most four columns, an
            indented code block after it becomes item content. Use a fenced
            layout in that context. *)
    no_adjacent_block_quotes : bool;
        (** Adjacent quote-marker runs parse as one block quote. Insert an
            outside blank line to preserve two sibling quote containers. *)
    (* inline <-> block interaction rules
    -------------------- *)
    no_html_block_starting_paragraph : bool;
        (** A html tag at the start of a paragraph will be parsed to a HTML
            block. *)
    no_break_in_atx_heading : bool;
    no_thematic_break_shaped_paragraph : bool;
        (** A paragraph whose rendering is a thematic break line is not a
            paragraph on reparse: block structure wins. The only inline that can
            do this is a lone [Em_dash], which renders [---]; there is no
            witness for it and no escape, so the paragraph is repaired by
            prefixing a text leaf. Costs a reparse per paragraph, so it is only
            worth setting where smart-punctuation dashes are generated. *)
    (* Pure inline rules
    -------------------- *)
    inline : Iconfig.t;
  }

  let default =
    {
      no_direct_blank_line = false;
      no_trailing_blank_line_in_blocks = false;
      no_empty_paragraph = false;
      no_empty_blocks = false;
      no_empty_list = false;
      no_list_item_leading_blank_prefix = false;
      no_marker_colliding_thematic_break = false;
      no_html_block_absorbing_successor = false;
      no_ambiguous_indented_code_after_list = false;
      no_adjacent_block_quotes = false;
      no_html_block_starting_paragraph = true;
      no_break_in_atx_heading = false;
      no_thematic_break_shaped_paragraph = false;
      inline = Iconfig.typed;
    }

  (** This config indicates its generator will only construct
      AST that is valid w.r.t. some desirable properties.

      @requirement
      Each of these choice needs to be justified with a test
      against a desirable property in both positive and negative
      cases. I.e.:
      - what property holds with this knob enabled?
      - what property is violated with this knob disabled?
  *)
  let typed_md =
    {
      default with
      no_empty_paragraph = true;
      no_empty_blocks = true;
      no_empty_list = true;
      no_list_item_leading_blank_prefix = true;
      no_marker_colliding_thematic_break = true;
      no_html_block_absorbing_successor = true;
      no_ambiguous_indented_code_after_list = true;
      no_adjacent_block_quotes = true;
      no_html_block_starting_paragraph = true;
      no_break_in_atx_heading = true;
      inline = Iconfig.typed;
      (* *)
      no_trailing_blank_line_in_blocks = false;
    }

  (** {!typed_md} with the djot inline extensions switched on. Reparsing must
      supply [colon_symbols] and [smart_punctuation], or the rendered source
      comes back as plain text. *)
  let typed_djot_md =
    {
      typed_md with
      inline = Iconfig.typed_djot;
      no_thematic_break_shaped_paragraph = true;
    }
end

(* A paragraph whose rendering is a thematic break line loses to block structure
   on reparse: [Paragraph (Em_dash)] renders [---] and comes back a
   [Thematic_break]. There is no witness and no escape, so repair by prefixing a
   text leaf — [jia---] is a paragraph again, and the dash still roundtrips. The
   test is a reparse rather than a match on [Em_dash] so that it keeps holding if
   the leaf corpus grows another inline that can render a bare break line. *)
let paragraph_collapses_to_thematic_break (inline : Inline.t) : bool =
  let p = Block.(Paragraph (Block.Paragraph.make inline, Meta.none)) in
  let rec has_break = function
    | Block.Thematic_break _ -> true
    | Block.Blocks (bs, _) -> List.exists has_break bs
    | _ -> false
  in
  has_break (Common_.reparse ~smart_punctuation:true p)

let gen_paragraph (config : Bconfig.t) : Block.t G.t =
  let ic =
    {
      config.inline with
      no_empty_inlines = config.no_empty_paragraph;
      no_html_block_start = config.no_html_block_starting_paragraph;
    }
  in
  let repair inline =
    if
      config.no_thematic_break_shaped_paragraph
      && paragraph_collapses_to_thematic_break inline
    then Inline.Inlines ([ List.hd text_egs; inline ], Meta.none)
    else inline
  in
  G.map
    (fun inline ->
      let inline = repair inline in
      Block.(Paragraph (Block.Paragraph.make inline, Meta.none)))
    (gen_inline ic)

let gen_heading (config : Bconfig.t) : Block.t G.t =
  let w_break =
    if config.no_break_in_atx_heading then 0 else config.inline.w_break
  in
  let ic = { config.inline with w_break } in
  G.map
    (fun (level, inline) ->
      Block.(Heading (Block.Heading.make ~level inline, Meta.none)))
    G.(pair (int_range 1 6) (gen_inline ic))

type block_gen_state = { foo : int }

let init_state : block_gen_state = { foo = 0 }

(* [lead_exclude] are characters a thematic break may not use here, because this
   leaf sits at the leading (marker) line of a list item; see
   {!gen_thematic_break}. *)
let gen_leaf_block_ ?(config = Bconfig.default) ?(rule_lead_exclude_chars = [])
    ?(w_blank_line = 1) ?(w_thematic_break = 1) ?(w_code_block = 1)
    ?(w_html_block = 1) ?(w_paragraph = 1) ?(w_heading = 1) () : Block.t G.t =
  [
    (w_blank_line, G.oneof_list blank_line_egs);
    (w_thematic_break, gen_thematic_break ~exclude:rule_lead_exclude_chars ());
    (w_code_block, G.oneof_list code_block_egs);
    (w_html_block, G.oneof_list html_block_egs);
    (w_paragraph, gen_paragraph config);
    (w_heading, gen_heading config);
  ]
  |> List.filter (fun (w, _) -> w > 0)
  |> G.oneof_weighted

let gen_leaf_block ?(rule_lead_exclude_chars = []) (config : Bconfig.t) st =
  let w_blank_line = if config.no_direct_blank_line then Some 0 else None in
  gen_leaf_block_ ~config ~rule_lead_exclude_chars ?w_blank_line ()

let limit_list_item_leading_blank_prefix (block : Block.t) : Block.t =
  let blank = function
    | Block.Blank_line _ -> true
    | _ -> false
  in
  let rec leading_blanks acc = function
    | b :: bs when blank b -> leading_blanks (b :: acc) bs
    | rest -> (List.rev acc, rest)
  in
  match Block.normalize block with
  | Block.Blocks (bs, meta) -> (
      match leading_blanks [] bs with
      | _ :: _ :: _, (_ :: _ as rest) -> Block.Blocks (List.hd bs :: rest, meta)
      | _ -> block)
  | _ -> block

let rec gen_block ?(rule_lead_exclude_chars = []) config st n =
  let open G in
  match n with
  | 0 -> gen_leaf_block ~rule_lead_exclude_chars config st
  | n ->
      let block_quote_of_b b =
        Block.(Block_quote (Block.Block_quote.make b, Meta.none))
      in
      oneof_weighted
        [
          (2, gen_leaf_block ~rule_lead_exclude_chars config st);
          (* The first child of [Blocks] inherits the leading position. *)
          (1, gen_blocks ~rule_lead_exclude_chars config st (n / 2));
          (* A block quote's [>] absorbs the leading position, so the marker
             collision cannot reach inside; drop [lead_exclude]. *)
          (1, map block_quote_of_b (gen_block config st (n / 2)));
          (* A list is never a thematic break, so the leading position never
             reaches it; it manages its own items' leading position. *)
          (1, gen_list config st n);
        ]

and gen_list config st n : Block.t G.t =
  let open G in
  (* Start integer for ordered lists; the renderer only keeps the first item's
     value, so a small spread is enough. *)
  let gen_start =
    oneof_list_weighted [ (6, 1); (1, 0); (1, 2); (1, 9); (1, 42) ]
  in
  let* type' =
    oneof_weighted
      [
        (3, return (`Unordered '-'));
        (1, return (`Unordered '+'));
        (1, return (`Unordered '*'));
        (2, map (fun s -> `Ordered (s, '.')) gen_start);
        (1, map (fun s -> `Ordered (s, ')')) gen_start);
      ]
  in
  (* For a bullet list, the item's leading block sits on the marker line, so a
     thematic break there must not reuse the marker char (would collapse to a
     thematic break on reparse). Ordered markers never collide. *)
  let item_lead_exclude =
    match type' with
    | `Unordered c when config.no_marker_colliding_thematic_break -> [ c ]
    | _ -> []
  in
  let gen_item =
    map
      (fun block ->
        let block =
          if config.no_list_item_leading_blank_prefix then
            limit_list_item_leading_blank_prefix block
          else block
        in
        (Block.List_item.make block, Meta.none))
      (gen_block ~rule_lead_exclude_chars:item_lead_exclude config st (n / 2))
  in
  let gen_len =
    if config.no_empty_list then int_range 1 (max 1 (n / 2))
    else int_bound (n / 2)
  in
  map
    (fun items -> Block.(List (Block.List'.make type' items, Meta.none)))
    (list_size gen_len gen_item)

and gen_blocks ?(rule_lead_exclude_chars = []) config st n : Block.t G.t =
  let open G in
  let gen_len =
    if config.no_empty_blocks then int_range 1 (max 1 n) else int_bound n
  in
  let config' =
    if config.no_trailing_blank_line_in_blocks then
      { config with no_direct_blank_line = true }
    else config
  in
  (* Only the head sits at the leading position; the rest start fresh lines. *)
  let* len = gen_len in
  let* blocks =
    if len = 0 then return []
    else
      let* head = gen_block ~rule_lead_exclude_chars config' st n in
      let* tail = list_size (return (len - 1)) (gen_block config' st n) in
      return (head :: tail)
  in
  let blocks =
    if config.no_html_block_absorbing_successor then
      separate_absorbing_html blocks
    else blocks
  in
  return (Block.Blocks (blocks, Meta.none))

let mk_gen_block ?(config = Bconfig.default) () : Block.t G.t =
  let gen = G.(sized_size nat_small @@ gen_block config init_state) in
  let gen =
    if config.no_ambiguous_indented_code_after_list then
      G.map fence_ambiguous_indented_code gen
    else gen
  in
  if config.no_adjacent_block_quotes then
    G.map separate_adjacent_block_quotes gen
  else gen

let%expect_test "Default config should give a sensible distribution" =
  Pp_distr.pp_gen ~display:`Boxplot () Format.std_formatter (mk_gen_block ())
    Stat.block_stats;
  [%expect
    {|
                                            Boxplot
    ┌─────────────────────────┬────────────────────────────────────────────────────────────┐
    │n=1000                   │↓0                                                        8↓│
    ├─────────────────────────┼────────────────────────────────────────────────────────────┤
    │blank_line               │[------------------+------------------------]               │
    │p5=0.00|p95=6.00|mu=2.58 │                                                            │
    ├─────────────────────────┼────────────────────────────────────────────────────────────┤
    │block_quote              │[-----------------------------------------------------+----~│
    │p5=0.00|p95=15.00|mu=7.35│                                                            │
    ├─────────────────────────┼────────────────────────────────────────────────────────────┤
    │blocks                   │[----------------------------------------------------+-----~│
    │p5=0.00|p95=15.00|mu=7.30│                                                            │
    ├─────────────────────────┼────────────────────────────────────────────────────────────┤
    │code_block               │[------------------+------------------------]               │
    │p5=0.00|p95=6.00|mu=2.64 │                                                            │
    ├─────────────────────────┼────────────────────────────────────────────────────────────┤
    │heading                  │[-----------------+-----------------]                       │
    │p5=0.00|p95=5.00|mu=2.49 │                                                            │
    ├─────────────────────────┼────────────────────────────────────────────────────────────┤
    │html_block               │[-----------------+-------------------------]               │
    │p5=0.00|p95=6.00|mu=2.51 │                                                            │
    ├─────────────────────────┼────────────────────────────────────────────────────────────┤
    │list                     │[------------------------------------------------------+---~│
    │p5=0.00|p95=16.00|mu=7.51│                                                            │
    ├─────────────────────────┼────────────────────────────────────────────────────────────┤
    │paragraph                │[------------------+------------------------]               │
    │p5=0.00|p95=6.00|mu=2.60 │                                                            │
    ├─────────────────────────┼────────────────────────────────────────────────────────────┤
    │thematic_break           │[-----------------+-------------------------]               │
    │p5=0.00|p95=6.00|mu=2.56 │                                                            │
    └─────────────────────────┴────────────────────────────────────────────────────────────┘
    |}]
