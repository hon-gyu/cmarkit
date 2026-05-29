open Oymarkit_
module G = QCheck2.Gen

let (gen_string : string G.t) = G.string_printable

(* Inline
   ====== *)

let text_egs : Inline.t list =
  [ "jia"; "yi"; "bing" ] |> List.map (fun pl -> Inline.(Text (pl, Meta.none)))

let (code_span_egs : Inline.t list) =
  Inline.Code_span.
    [
      of_string "";
      of_string "`add`";
      of_string "``sub``";
      of_string "`` `mul` ``";
    ]
  |> List.map (fun pl -> Inline.(Code_span (pl, Meta.none)))

let (autolink_egs : Inline.t list) =
  Inline.Autolink.
    [ make ("www.foo.com", Meta.none); make ("bar@gmail.com", Meta.none) ]
  |> List.map (fun pl -> Inline.(Autolink (pl, Meta.none)))

let (break_egs : Inline.t list) =
  Inline.Break.[ make `Hard; make `Soft ]
  |> List.map (fun pl -> Inline.(Break (pl, Meta.none)))

let mk_emph_egs i : Inline.t list =
  Inline.Emphasis.[ make ~delim:'*' i; make ~delim:'_' i ]
  |> List.map (fun pl -> Inline.(Emphasis (pl, Meta.none)))

let mk_strong_emph_egs i : Inline.t list =
  Inline.Emphasis.[ make ~delim:'*' i; make ~delim:'_' i ]
  |> List.map (fun pl -> Inline.(Strong_emphasis (pl, Meta.none)))

let mk_link i : Inline.t =
  let dest = ("https://example.com", Meta.none) in
  let ref_ = `Inline (Link_definition.make ~dest (), Meta.none) in
  Inline.(Link (Inline.Link.make i ref_, Meta.none))

let mk_image i : Inline.t =
  let dest = ("https://example.com/img.png", Meta.none) in
  let ref_ = `Inline (Link_definition.make ~dest (), Meta.none) in
  Inline.(Image (Inline.Link.make i ref_, Meta.none))

let (raw_html_egs : Inline.t list) =
  [ "<br />"; "<em>foo</em>"; "<!-- comment -->" ]
  |> List.map (fun s ->
      Inline.(Raw_html (Block_line.tight_list_of_string s, Meta.none)))

(* TODO: extension strikethrough and math_span *)

let gen_inline ?(w_break = 1) () : Inline.t G.t =
  let gen_leaf =
    [
      (1, G.oneof_list text_egs);
      (1, G.oneof_list code_span_egs);
      (1, G.oneof_list autolink_egs);
      (w_break, G.oneof_list break_egs);
      (1, G.oneof_list raw_html_egs);
    ]
    |> List.filter (fun (w, _) -> w > 0)
    |> G.oneof_weighted
  in
  G.(
    sized_size nat_small
    @@ fix (fun self (n : int) ->
        match n with
        | 0 -> gen_leaf
        | n ->
            let inlines_of_is is = Inline.Inlines (is, Meta.none) in
            let emph_gen =
              bind (self (n - 1)) (fun i -> oneof_list @@ mk_emph_egs i)
            in
            let strong_emph_gen =
              bind (self (n - 1)) (fun i -> oneof_list @@ mk_strong_emph_egs i)
            in
            oneof_weighted
              [
                (1, gen_leaf);
                ( 1,
                  map inlines_of_is
                    (list_size (int_bound (n / 2)) (self (n / 2))) );
                (1, emph_gen);
                (1, strong_emph_gen);
                (1, map mk_link (self (n - 1)));
                (1, map mk_image (self (n - 1)));
              ]))

(* Block
=================== *)

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

let gen_paragraph : Block.t G.t =
  G.map
    (fun inline -> Block.(Paragraph (Block.Paragraph.make inline, Meta.none)))
    (gen_inline ())

let gen_heading : Block.t G.t =
  G.map
    (fun (level, inline) ->
      Block.(Heading (Block.Heading.make ~level inline, Meta.none)))
    G.(pair (int_range 1 6) (gen_inline ~w_break:0 ()))

let html_block_egs : Block.t list =
  [
    [ ("<div>\n", Meta.none) ];
    [ ("<p>hello</p>\n", Meta.none) ];
    [ ("<!-- comment -->\n", Meta.none) ];
  ]
  |> List.map (fun lines -> Block.(Html_block (lines, Meta.none)))

type block_gen_config = {
  no_direct_blank_line : bool;
  no_trailing_blank_line_in_blocks : bool;
}

type block_gen_state = { foo : int }

let default_config : block_gen_config =
  { no_direct_blank_line = false; no_trailing_blank_line_in_blocks = false }

let init_state : block_gen_state = { foo = 0 }

let gen_leaf_block_ ?(w_blank_line = 1) ?(w_thematic_break = 1)
    ?(w_code_block = 1) ?(w_html_block = 1) ?(w_paragraph = 1) ?(w_heading = 1)
    () : Block.t G.t =
  G.oneof_weighted
    [
      (w_blank_line, G.oneof_list blank_line_egs);
      (w_thematic_break, G.oneof_list thematic_break_egs);
      (w_code_block, G.oneof_list code_block_egs);
      (w_html_block, G.oneof_list html_block_egs);
      (w_paragraph, gen_paragraph);
      (w_heading, gen_heading);
    ]

let gen_leaf_block config st =
  let w_blank_line = if config.no_direct_blank_line then Some 0 else None in
  gen_leaf_block_ ?w_blank_line ()

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
  let (blocks : Block.t list t) =
    if config.no_trailing_blank_line_in_blocks then
      let config' = { config with no_direct_blank_line = true } in
      list_size (int_bound n) (gen_block config' st n)
    else
      (* Note: here it's possible that blocks has length 0 or 1 *)
      list_size (int_bound n) (gen_block config st n)
  in
  map (fun bs -> Block.Blocks (bs, Meta.none)) blocks

let mk_gen_block ?(config = default_config) () : Block.t G.t =
  G.(sized_size nat_small @@ gen_block config init_state)

(* let gen_block ?(w_direct_blank_line = 1) ?(w_direct_thematic_break = 1)
    ?(w_direct_code_block = 1) ?(w_direct_html_block = 1)
    ?(w_direct_paragraph = 1) ?(w_direct_heading = 1) () : Block.t G.t =
  G.(
    sized_size nat_small
    @@ fix (fun self (n : int) ->
        match n with
        | 0 ->
            gen_block_leaf ~w_blank_line:w_direct_blank_line
              ~w_thematic_break:w_direct_thematic_break
              ~w_code_block:w_direct_code_block
              ~w_html_block:w_direct_html_block ~w_paragraph:w_direct_paragraph
              ~w_heading:w_direct_heading ()
        | n ->
            let blocks_of_bs bs = Block.Blocks (bs, Meta.none) in
            let block_quote_of_b b =
              Block.(Block_quote (Block.Block_quote.make b, Meta.none))
            in
            let gen_list_block =
              let gen_item =
                map
                  (fun block -> (Block.List_item.make block, Meta.none))
                  (self (n / 2))
              in
              map
                (fun items ->
                  Block.(
                    List (Block.List'.make (`Unordered '-') items, Meta.none)))
                (list_size (int_bound (n / 2)) gen_item)
            in
            oneof_weighted
              [
                (2, gen_block_leaf ());
                ( 1,
                  map blocks_of_bs
                    (list_size (int_bound (n / 2)) (self (n / 2))) );
                (1, map block_quote_of_b (self (n / 2)));
                (1, gen_list_block);
              ])) *)

let%expect_test _ =
  Pp_distr.pp_gen () Format.std_formatter (gen_inline ()) Stat.inline_stats;
  [%expect
    {|
                                             Boxplot
    ┌──────────────────────────┬────────────────────────────────────────────────────────────┐
    │n=1000                    │↓0                                                       30↓│
    ├──────────────────────────┼────────────────────────────────────────────────────────────┤
    │text                      │[-----+------------------------]                            │
    │p5=0.00|p95=16.00|mu=3.32 │                                                            │
    ├──────────────────────────┼────────────────────────────────────────────────────────────┤
    │code_span                 │[-----+----------------------]                              │
    │p5=0.00|p95=15.00|mu=3.21 │                                                            │
    ├──────────────────────────┼────────────────────────────────────────────────────────────┤
    │autolink                  │[-----+--------------------]                                │
    │p5=0.00|p95=14.00|mu=3.22 │                                                            │
    ├──────────────────────────┼────────────────────────────────────────────────────────────┤
    │break                     │[-----+------------------------]                            │
    │p5=0.00|p95=16.00|mu=3.25 │                                                            │
    ├──────────────────────────┼────────────────────────────────────────────────────────────┤
    │raw_html                  │[-----+----------------------]                              │
    │p5=0.00|p95=15.00|mu=3.28 │                                                            │
    ├──────────────────────────┼────────────────────────────────────────────────────────────┤
    │emphasis                  │[-----------------------+----------------------------------~│
    │p5=0.00|p95=60.00|mu=12.58│                                                            │
    ├──────────────────────────┼────────────────────────────────────────────────────────────┤
    │strong_emphasis           │[-----------------------+----------------------------------~│
    │p5=0.00|p95=56.00|mu=12.48│                                                            │
    ├──────────────────────────┼────────────────────────────────────────────────────────────┤
    │link                      │[-----------------------+----------------------------------~│
    │p5=0.00|p95=60.00|mu=12.57│                                                            │
    ├──────────────────────────┼────────────────────────────────────────────────────────────┤
    │image                     │[-----------------------+----------------------------------~│
    │p5=0.00|p95=55.00|mu=12.68│                                                            │
    ├──────────────────────────┼────────────────────────────────────────────────────────────┤
    │inlines                   │[-----------------------+----------------------------------~│
    │p5=0.00|p95=55.00|mu=12.43│                                                            │
    └──────────────────────────┴────────────────────────────────────────────────────────────┘
    |}]

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
