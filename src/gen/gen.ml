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
    no_marker_colliding_thematic_break : bool;
        (** A bullet list item whose leading block is a thematic break of the
            {e same} character as the marker collapses on reparse: [- ---] is a
            uniform [-] run, so it parses as a thematic break, not a list. *)
    (* inline <-> block interaction rules
    -------------------- *)
    no_html_block_starting_paragraph : bool;
        (** A html tag at the start of a paragraph will be parsed to a HTML
            block. *)
    no_break_in_atx_heading : bool;
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
      no_marker_colliding_thematic_break = false;
      no_html_block_starting_paragraph = true;
      no_break_in_atx_heading = false;
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
      no_marker_colliding_thematic_break = true;
      no_html_block_starting_paragraph = true;
      no_break_in_atx_heading = true;
      inline = Iconfig.typed;
      (* *)
      no_trailing_blank_line_in_blocks = false;
    }
end

let gen_paragraph (config : Bconfig.t) : Block.t G.t =
  let ic =
    {
      config.inline with
      no_empty_inlines = config.no_empty_paragraph;
      no_html_block_start = config.no_html_block_starting_paragraph;
    }
  in
  G.map
    (fun inline -> Block.(Paragraph (Block.Paragraph.make inline, Meta.none)))
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

let rec gen_block ?(rule_lead_exclude_chars = []) config st n =
  let open G in
  match n with
  | 0 -> gen_leaf_block ~rule_lead_exclude_chars config st
  | n ->
      let block_quote_of_b b =
        Block.(Block_quote (Block.Block_quote.make b, Meta.none))
      in
      let gen_list_block =
        let marker = '-' in
        (* The item's leading block sits on the marker line, so a thematic break
           there must not reuse the marker char (would collapse to a thematic
           break on reparse). *)
        let item_lead_exclude =
          if config.no_marker_colliding_thematic_break then [ marker ] else []
        in
        let gen_item =
          map
            (fun block -> (Block.List_item.make block, Meta.none))
            (gen_block ~rule_lead_exclude_chars:item_lead_exclude config st
               (n / 2))
        in
        let gen_len =
          if config.no_empty_list then int_range 1 (max 1 (n / 2))
          else int_bound (n / 2)
        in
        map
          (fun items ->
            Block.(List (Block.List'.make (`Unordered marker) items, Meta.none)))
          (list_size gen_len gen_item)
      in
      oneof_weighted
        [
          (2, gen_leaf_block ~rule_lead_exclude_chars config st);
          (* The first child of [Blocks] inherits the leading position. *)
          (1, gen_blocks ~rule_lead_exclude_chars config st (n / 2));
          (* A block quote's [>] absorbs the leading position, so the marker
             collision cannot reach inside; drop [lead_exclude]. *)
          (1, map block_quote_of_b (gen_block config st (n / 2)));
          (1, gen_list_block);
        ]

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
  return (Block.Blocks (blocks, Meta.none))

let mk_gen_block ?(config = Bconfig.default) () : Block.t G.t =
  G.(sized_size nat_small @@ gen_block config init_state)

let%expect_test "Default config should give a sensible distribution" =
  Pp_distr.pp_gen ~display:`Boxplot () Format.std_formatter (mk_gen_block ())
    Stat.block_stats;
  [%expect
    {|
                                            Boxplot
    ┌─────────────────────────┬────────────────────────────────────────────────────────────┐
    │n=1000                   │↓0                                                       11↓│
    ├─────────────────────────┼────────────────────────────────────────────────────────────┤
    │blank_line               │[--------------+--------------------------]                 │
    │p5=0.00|p95=8.00|mu=2.93 │                                                            │
    ├─────────────────────────┼────────────────────────────────────────────────────────────┤
    │block_quote              │[-------------------------------------------+--------------~│
    │p5=0.00|p95=22.00|mu=8.30│                                                            │
    ├─────────────────────────┼────────────────────────────────────────────────────────────┤
    │blocks                   │[------------------------------------------+---------------~│
    │p5=0.00|p95=20.00|mu=8.19│                                                            │
    ├─────────────────────────┼────────────────────────────────────────────────────────────┤
    │code_block               │[--------------+--------------------------------]           │
    │p5=0.00|p95=9.00|mu=2.90 │                                                            │
    ├─────────────────────────┼────────────────────────────────────────────────────────────┤
    │heading                  │[--------------+----------------]                           │
    │p5=0.00|p95=6.00|mu=2.80 │                                                            │
    ├─────────────────────────┼────────────────────────────────────────────────────────────┤
    │html_block               │[--------------+---------------------]                      │
    │p5=0.00|p95=7.00|mu=2.90 │                                                            │
    ├─────────────────────────┼────────────────────────────────────────────────────────────┤
    │list                     │[--------------------------------------------+-------------~│
    │p5=0.00|p95=20.00|mu=8.53│                                                            │
    ├─────────────────────────┼────────────────────────────────────────────────────────────┤
    │paragraph                │[---------------+--------------------]                      │
    │p5=0.00|p95=7.00|mu=3.00 │                                                            │
    ├─────────────────────────┼────────────────────────────────────────────────────────────┤
    │thematic_break           │[--------------+--------------------------------]           │
    │p5=0.00|p95=9.00|mu=2.87 │                                                            │
    └─────────────────────────┴────────────────────────────────────────────────────────────┘
    |}]
