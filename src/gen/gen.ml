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

let thematic_break_egs : Block.t list =
  Block.Thematic_break.[ make (); make ~layout:"***" (); make ~layout:"___" () ]
  |> List.map (fun tb -> Block.(Thematic_break (tb, Meta.none)))

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
    no_direct_blank_line : bool;
    no_trailing_blank_line_in_blocks : bool;
    no_empty_paragraph : bool;
    no_empty_blocks : bool;
    no_break_in_atx_heading : bool;
    inline : Iconfig.t;
  }

  let empty =
    {
      no_direct_blank_line = false;
      no_trailing_blank_line_in_blocks = false;
      no_empty_paragraph = false;
      no_empty_blocks = false;
      no_break_in_atx_heading = false;
      inline = Iconfig.default;
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
      empty with
      no_trailing_blank_line_in_blocks = true;
      no_empty_paragraph = true;
      no_empty_blocks = true;
      no_break_in_atx_heading = true;
    }
end

let gen_paragraph (config : Bconfig.t) : Block.t G.t =
  let ic = { config.inline with no_empty_inlines = config.no_empty_paragraph } in
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

let gen_leaf_block_ ?(config = Bconfig.empty) ?(w_blank_line = 1)
    ?(w_thematic_break = 1) ?(w_code_block = 1) ?(w_html_block = 1)
    ?(w_paragraph = 1) ?(w_heading = 1) () : Block.t G.t =
  [
    (w_blank_line, G.oneof_list blank_line_egs);
    (w_thematic_break, G.oneof_list thematic_break_egs);
    (w_code_block, G.oneof_list code_block_egs);
    (w_html_block, G.oneof_list html_block_egs);
    (w_paragraph, gen_paragraph config);
    (w_heading, gen_heading config);
  ]
  |> List.filter (fun (w, _) -> w > 0)
  |> G.oneof_weighted

let gen_leaf_block (config : Bconfig.t) st =
  let w_blank_line = if config.no_direct_blank_line then Some 0 else None in
  gen_leaf_block_ ~config ?w_blank_line ()

let rec gen_block config st n =
  let open G in
  match n with
  | 0 -> gen_leaf_block config st
  | n ->
      let block_quote_of_b b =
        Block.(Block_quote (Block.Block_quote.make b, Meta.none))
      in
      let gen_list_block =
        let gen_item =
          map
            (fun block -> (Block.List_item.make block, Meta.none))
            (gen_block config st (n / 2))
        in
        map
          (fun items ->
            Block.(List (Block.List'.make (`Unordered '-') items, Meta.none)))
          (list_size (int_bound (n / 2)) gen_item)
      in
      oneof_weighted
        [
          (2, gen_leaf_block config st);
          (1, gen_blocks config st (n / 2));
          (1, map block_quote_of_b (gen_block config st (n / 2)));
          (1, gen_list_block);
        ]

and gen_blocks config st n : Block.t G.t =
  let open G in
  let gen_len =
    if config.no_empty_blocks then int_range 1 (max 1 n) else int_bound n
  in
  let (blocks : Block.t list t) =
    if config.no_trailing_blank_line_in_blocks then
      let config' = { config with no_direct_blank_line = true } in
      list_size gen_len (gen_block config' st n)
    else
      (* Note: here it's possible that blocks has length 0 or 1 *)
      list_size gen_len (gen_block config st n)
  in
  map (fun bs -> Block.Blocks (bs, Meta.none)) blocks

let mk_gen_block ?(config = Bconfig.empty) () : Block.t G.t =
  G.(sized_size nat_small @@ gen_block config init_state)

let%expect_test _ =
  Pp_distr.pp_gen ~display:`Boxplot () Format.std_formatter (mk_gen_block ())
    Stat.block_stats;
  [%expect
    {|
                                            Boxplot
    ┌─────────────────────────┬────────────────────────────────────────────────────────────┐
    │n=1000                   │↓0                                                       13↓│
    ├─────────────────────────┼────────────────────────────────────────────────────────────┤
    │blank_line               │[------------+--------------------------]                   │
    │p5=0.00|p95=9.00|mu=2.91 │                                                            │
    ├─────────────────────────┼────────────────────────────────────────────────────────────┤
    │block_quote              │[-------------------------------------+--------------------~│
    │p5=0.00|p95=28.00|mu=8.41│                                                            │
    ├─────────────────────────┼────────────────────────────────────────────────────────────┤
    │blocks                   │[-----------------------------------+----------------------~│
    │p5=0.00|p95=25.00|mu=8.06│                                                            │
    ├─────────────────────────┼────────────────────────────────────────────────────────────┤
    │code_block               │[------------+--------------------------]                   │
    │p5=0.00|p95=9.00|mu=2.95 │                                                            │
    ├─────────────────────────┼────────────────────────────────────────────────────────────┤
    │heading                  │[-----------+------------------]                            │
    │p5=0.00|p95=7.00|mu=2.77 │                                                            │
    ├─────────────────────────┼────────────────────────────────────────────────────────────┤
    │html_block               │[-----------+-----------------------]                       │
    │p5=0.00|p95=8.00|mu=2.83 │                                                            │
    ├─────────────────────────┼────────────────────────────────────────────────────────────┤
    │list                     │[------------------------------------+---------------------~│
    │p5=0.00|p95=27.00|mu=8.18│                                                            │
    ├─────────────────────────┼────────────────────────────────────────────────────────────┤
    │paragraph                │[------------+----------------------]                       │
    │p5=0.00|p95=8.00|mu=2.92 │                                                            │
    ├─────────────────────────┼────────────────────────────────────────────────────────────┤
    │thematic_break           │[-----------+-----------------------]                       │
    │p5=0.00|p95=8.00|mu=2.84 │                                                            │
    └─────────────────────────┴────────────────────────────────────────────────────────────┘
    |}]
