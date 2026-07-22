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
   [Blocks] tails/heads but no further.

   TODO(migration): subsumed by {!summarize}, but not identical — these treat an
   empty [Blocks] as opaque where {!summarize} treats it as transparent, which is
   the correct reading and would move baselines. Delete each when the rule that
   uses it becomes a guard. *)
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
    -------------------------------------- *)
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

(* Generation context
   =================== *)

(** The generator threads {!Rule.ctx} — the attributes a rule may consult —
    alongside the two things a rule must {e not} see: which rules are switched
    on, and the weights. A rule says what it forbids; {!Bconfig} says whether it
    is asked. *)
type ctx = {
  attrs : Rule.ctx;
  config : Bconfig.t;
  rules : Rule.t list;  (** Enabled rules, resolved once from [config]. *)
}

(* CR: what does "two vocabularies" means here? Vague *)
(** Which rule each {!Bconfig} knob switches on. The single place the two
    vocabularies meet. *)
let enabled_rules (c : Bconfig.t) : Rule.t list =
  List.filter_map
    (fun (on, r) -> if on then Some r else None)
    [
      (c.no_trailing_blank_line_in_blocks, Rule.no_trailing_blank_line_in_blocks);
      (c.no_empty_paragraph, Rule.no_empty_paragraph);
      (c.no_empty_blocks, Rule.no_empty_blocks);
      (c.no_empty_list, Rule.no_empty_list);
      (c.no_list_item_leading_blank_prefix, Rule.no_list_item_leading_blank_prefix);
      (c.no_marker_colliding_thematic_break, Rule.no_marker_colliding_thematic_break);
      (c.no_html_block_absorbing_successor, Rule.no_html_block_absorbing_successor);
      (c.no_ambiguous_indented_code_after_list, Rule.no_ambiguous_indented_code_after_list);
      (c.no_adjacent_block_quotes, Rule.no_adjacent_block_quotes);
      (c.no_html_block_starting_paragraph, Rule.no_html_block_starting_paragraph);
    ]

let init_ctx ?(lead_exclude = []) (config : Bconfig.t) : ctx =
  {
    attrs = Rule.init_ctx ~lead_exclude ();
    config;
    rules = enabled_rules config;
  }

let enter_container (ctx : ctx) : ctx =
  { ctx with attrs = Rule.enter_container ctx.attrs }

(* Guards
   ------ *)

(** How often each rule removed each candidate, keyed by [(rule, choice)].

    Filtering a candidate reallocates its weight to the survivors, so a guard
    bends the distribution declared in {!Bconfig} exactly as a repair pass did.
    The difference is that a guard can say so, and this table is where it says
    it: without a per-rule count we would have traded one invisible distortion
    for another. {!Pp_distr} measures the distribution that came out; this
    measures which rule bent it. *)
let rejections : (string * string, int) Hashtbl.t = Hashtbl.create 16

let reset_rejections () = Hashtbl.reset rejections

let record_rejection ~(rule : string) ~(choice : Rule.choice) =
  let key = (rule, Rule.string_of_choice choice) in
  let n = Option.value ~default:0 (Hashtbl.find_opt rejections key) in
  Hashtbl.replace rejections key (n + 1)

let pp_rejections ppf () =
  let rows =
    Hashtbl.fold (fun (r, c) n acc -> (r, c, n) :: acc) rejections []
    |> List.sort compare
  in
  match rows with
  | [] -> Format.fprintf ppf "no candidate was rejected@."
  | rows ->
      List.iter
        (fun (r, c, n) -> Format.fprintf ppf "%-28s %-12s %6d@." r c n)
        rows

(** Drop the candidates the enabled rules forbid.

    [`Leaf] is the guaranteed candidate and no rule may forbid it: it is a
    single leaf block, so no adjacency or containment rule has anything to say
    about it. That is what keeps the list from going empty — repairs can never
    get stuck, guards can, and the fallback is the price of the trade. If a
    future rule does need to forbid a leaf, it belongs inside
    {!gen_leaf_block_}'s weights, where the per-constructor choice still leaves
    somewhere to go. *)
let keep_allowed (ctx : ctx) (cands : (int * Rule.choice) list) :
    (int * Rule.choice) list =
  let allowed (_, c) =
    match Rule.first_forbidding ctx.rules ctx.attrs c with
    | None -> true
    | Some r ->
        record_rejection ~rule:r.Rule.name ~choice:c;
        false
  in
  match List.filter allowed cands with
  | [] -> [ (1, `Leaf) ]
  | kept -> kept

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

let gen_leaf_block (ctx : ctx) : (Block.t * Rule.summary) G.t =
  let config = ctx.config in
  let w_blank_line = if config.no_direct_blank_line then Some 0 else None in
  G.map
    (fun b -> (b, Rule.summarize b))
    (gen_leaf_block_ ~config ~rule_lead_exclude_chars:ctx.attrs.Rule.lead_exclude
       ?w_blank_line ())

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

let rec gen_block (ctx : ctx) n : (Block.t * Rule.summary) G.t =
  let open G in
  match n with
  | 0 -> gen_leaf_block ctx
  | n ->
      let block_quote_of_b (b, _) =
        let bq = Block.(Block_quote (Block.Block_quote.make b, Meta.none)) in
        (bq, { Rule.summary_opaque with trailing_block_quote = true })
      in
      let gen_of : Rule.choice -> (Block.t * Rule.summary) G.t = function
        | `Leaf -> gen_leaf_block ctx
        (* The first child of [Blocks] inherits the leading position, and its
           predecessor in render order, so [ctx] passes through unchanged. *)
        | `Blocks -> gen_blocks ctx (n / 2)
        | `Block_quote ->
            map block_quote_of_b (gen_block (enter_container ctx) (n / 2))
        (* A list is never a thematic break, so the leading position never
           reaches it; it manages its own items' leading position. *)
        | `List -> gen_list (enter_container ctx) n
      in
      [ (2, `Leaf); (1, `Blocks); (1, `Block_quote); (1, `List) ]
      |> keep_allowed ctx
      |> List.map (fun (w, c) -> (w, gen_of c))
      |> oneof_weighted

and gen_list (ctx : ctx) n : (Block.t * Rule.summary) G.t =
  let config = ctx.config in
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
  let item_ctx =
    let c = enter_container ctx in
    { c with attrs = { c.attrs with Rule.lead_exclude = item_lead_exclude } }
  in
  let gen_item =
    map
      (fun (block, _) ->
        let block =
          if config.no_list_item_leading_blank_prefix then
            limit_list_item_leading_blank_prefix block
          else block
        in
        (Block.List_item.make block, Meta.none))
      (gen_block item_ctx (n / 2))
  in
  let gen_len =
    if config.no_empty_list then int_range 1 (max 1 (n / 2))
    else int_bound (n / 2)
  in
  map
    (fun items ->
      let l = Block.List'.make type' items in
      let summary =
        {
          Rule.summary_opaque with
          list_continuation_indent =
            Common_.list_last_item_continuation_indent l;
        }
      in
      (Block.(List (l, Meta.none)), summary))
    (list_size gen_len gen_item)

and gen_blocks (ctx : ctx) n : (Block.t * Rule.summary) G.t =
  let open G in
  let config = ctx.config in
  let gen_len =
    if config.no_empty_blocks then int_range 1 (max 1 n) else int_bound n
  in
  let ctx =
    if config.no_trailing_blank_line_in_blocks then
      { ctx with config = { config with no_direct_blank_line = true } }
    else ctx
  in
  let* len = gen_len in
  (* Left-to-right fold: each child is generated in a context that knows its
     predecessor's summary and whether it closes the sequence. This is the
     "across" half of the attribute grammar and the reason the adjacency rules
     can become guards; a [list_size] cannot express it, because its elements
     are independent.

     The per-child context comes from {!Rule.nth_child} and the accumulator
     from {!Rule.advance}, which is the same pair {!Rule.check} uses to walk a
     finished tree. Sharing them is what stops the generator and the checker
     from disagreeing about what "the previous sibling" means. *)
  let rec fold i prev acc =
    if i >= len then return (List.rev acc)
    else
      let child_ctx =
        { ctx with attrs = Rule.nth_child ctx.attrs ~i ~len ~prev }
      in
      let* b, s = gen_block child_ctx n in
      fold (i + 1) (Rule.advance prev s) (b :: acc)
  in
  let* blocks = fold 0 ctx.attrs.Rule.prev [] in
  let blocks =
    if config.no_html_block_absorbing_successor then
      separate_absorbing_html blocks
    else blocks
  in
  (* TODO(migration): recomputed rather than folded up from the children's
     summaries because [separate_absorbing_html] may have inserted blocks the
     fold never saw. Becomes [summary_seq (List.map snd children)] once that
     repair is a guard. *)
  let block = Block.Blocks (blocks, Meta.none) in
  return (block, Rule.summarize block)

let mk_gen_block ?(config = Bconfig.default) () : Block.t G.t =
  let gen =
    G.(sized_size nat_small @@ fun n -> map fst (gen_block (init_ctx config) n))
  in
  (* [no_adjacent_block_quotes] is absent here on purpose: it is a guard in
     {!gen_block} now, not a repair. *)
  if config.no_ambiguous_indented_code_after_list then
    G.map fence_ambiguous_indented_code gen
  else gen

let%expect_test "Default config should give a sensible distribution" =
  Pp_distr.pp_gen ~display:`Boxplot () Format.std_formatter (mk_gen_block ())
    Stat.block_stats;
  [%expect
    {|
                                            Boxplot
    ┌─────────────────────────┬────────────────────────────────────────────────────────────┐
    │n=1000                   │↓0                                                       10↓│
    ├─────────────────────────┼────────────────────────────────────────────────────────────┤
    │blank_line               │[-------------+--------------------------]                  │
    │p5=0.00|p95=7.00|mu=2.50 │                                                            │
    ├─────────────────────────┼────────────────────────────────────────────────────────────┤
    │block_quote              │[----------------------------------------+-----------------~│
    │p5=0.00|p95=19.00|mu=7.11│                                                            │
    ├─────────────────────────┼────────────────────────────────────────────────────────────┤
    │blocks                   │[----------------------------------------+-----------------~│
    │p5=0.00|p95=20.00|mu=7.11│                                                            │
    ├─────────────────────────┼────────────────────────────────────────────────────────────┤
    │code_block               │[--------------+-------------------]                        │
    │p5=0.00|p95=6.00|mu=2.59 │                                                            │
    ├─────────────────────────┼────────────────────────────────────────────────────────────┤
    │heading                  │[-------------+--------------------]                        │
    │p5=0.00|p95=6.00|mu=2.45 │                                                            │
    ├─────────────────────────┼────────────────────────────────────────────────────────────┤
    │html_block               │[-------------+--------------------------]                  │
    │p5=0.00|p95=7.00|mu=2.48 │                                                            │
    ├─────────────────────────┼────────────────────────────────────────────────────────────┤
    │list                     │[-----------------------------------------+----------------~│
    │p5=0.00|p95=19.00|mu=7.23│                                                            │
    ├─────────────────────────┼────────────────────────────────────────────────────────────┤
    │paragraph                │[--------------+-------------------------]                  │
    │p5=0.00|p95=7.00|mu=2.55 │                                                            │
    ├─────────────────────────┼────────────────────────────────────────────────────────────┤
    │thematic_break           │[-------------+--------------------]                        │
    │p5=0.00|p95=6.00|mu=2.48 │                                                            │
    └─────────────────────────┴────────────────────────────────────────────────────────────┘
    |}]

(* How much a guard bends the declared weights.

   [typed_md] enables [no_adjacent_block_quotes], so a `Block_quote` candidate
   is dropped whenever the previous sibling in render order is already a quote.
   The count below is that deviation, made legible: it is the number of times
   the generator wanted a quote and the rule said no. Under the old repair pass
   the same situation produced a silently-inserted [Blank_line] and no number at
   all. *)
let%expect_test "guards report how often they rejected a candidate" =
  let sample config =
    reset_rejections ();
    let gen = mk_gen_block ~config () in
    let rand = Random.State.make [| 42 |] in
    for _ = 1 to 1000 do
      ignore (QCheck2.Gen.generate1 ~rand gen)
    done;
    Format.printf "%a" pp_rejections ()
  in
  sample Bconfig.default;
  [%expect {| no candidate was rejected |}];
  sample Bconfig.typed_md;
  [%expect {| no adjacent block quotes     block_quote    2735 |}]
