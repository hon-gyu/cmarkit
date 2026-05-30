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
      (* TODO(rule): No empty code span: [``] renders as literal backticks, not a span. *)
      of_string "`add`";
      of_string "``sub``";
      of_string "`` `mul` ``";
    ]
  |> List.map (fun pl -> Inline.(Code_span (pl, Meta.none)))

let (autolink_egs : Inline.t list) =
  Inline.Autolink.(
    (* TODO(rule): URI autolinks need a scheme; [<www.foo.com>] is literal text, not an
       autolink. Emails are recognized as-is. *)
    [
      make ("http://www.foo.com", Meta.none); make ("bar@gmail.com", Meta.none);
    ])
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

module Inline_config = struct
  type t = {
    w_text : int;
    w_code_span : int;
    w_autolink : int;
    w_break : int;
    w_raw_html : int;
    w_inlines : int;
    w_emphasis : int;
    w_strong_emphasis : int;
    w_link : int;
    w_image : int;
    nonempty : bool;
  }

  let default =
    {
      w_text = 1;
      w_code_span = 1;
      w_autolink = 1;
      w_break = 1;
      w_raw_html = 1;
      w_inlines = 1;
      w_emphasis = 1;
      w_strong_emphasis = 1;
      w_link = 1;
      w_image = 1;
      nonempty = false;
    }
end

let gen_inline (ic : Inline_config.t) : Inline.t G.t =
  let open Inline_config in
  let gen_leaf =
    [
      (ic.w_text, G.oneof_list text_egs);
      (ic.w_code_span, G.oneof_list code_span_egs);
      (ic.w_autolink, G.oneof_list autolink_egs);
      (ic.w_break, G.oneof_list break_egs);
      (ic.w_raw_html, G.oneof_list raw_html_egs);
    ]
    |> List.filter (fun (w, _) -> w > 0)
    |> G.oneof_weighted
  in
  let inlines_len n =
    if ic.nonempty then G.int_range 1 (max 1 (n / 2)) else G.int_bound (n / 2)
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
            [
              (1, gen_leaf);
              ( ic.w_inlines,
                map inlines_of_is (list_size (inlines_len n) (self (n / 2))) );
              (ic.w_emphasis, emph_gen);
              (ic.w_strong_emphasis, strong_emph_gen);
              (ic.w_link, map mk_link (self (n - 1)));
              (ic.w_image, map mk_image (self (n - 1)));
            ]
            |> List.filter (fun (w, _) -> w > 0)
            |> oneof_weighted))

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

let html_block_egs : Block.t list =
  [
    [ ("<div>", Meta.none) ];
    [ ("<p>hello</p>", Meta.none) ];
    [ ("<!-- comment -->", Meta.none) ];
  ]
  |> List.map (fun lines -> Block.(Html_block (lines, Meta.none)))

module Config = struct
  type t = {
    no_direct_blank_line : bool;
    no_trailing_blank_line_in_blocks : bool;
    no_empty_paragraph : bool;
    no_empty_blocks : bool;
    no_break_in_atx_heading : bool;
    inline : Inline_config.t;
  }

  let empty =
    {
      no_direct_blank_line = false;
      no_trailing_blank_line_in_blocks = false;
      no_empty_paragraph = false;
      no_empty_blocks = false;
      no_break_in_atx_heading = false;
      inline = Inline_config.default;
    }

  let typed_md =
    {
      empty with
      no_trailing_blank_line_in_blocks = true;
      no_empty_paragraph = true;
      no_empty_blocks = true;
      no_break_in_atx_heading = true;
    }
end

let gen_paragraph (config : Config.t) : Block.t G.t =
  let ic = { config.inline with nonempty = config.no_empty_paragraph } in
  G.map
    (fun inline -> Block.(Paragraph (Block.Paragraph.make inline, Meta.none)))
    (gen_inline ic)

let gen_heading (config : Config.t) : Block.t G.t =
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

let gen_leaf_block_ ?(config = Config.empty) ?(w_blank_line = 1)
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

let gen_leaf_block (config : Config.t) st =
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

let mk_gen_block ?(config = Config.empty) () : Block.t G.t =
  G.(sized_size nat_small @@ gen_block config init_state)

let%expect_test _ =
  Pp_distr.pp_gen () Format.std_formatter
    (gen_inline Inline_config.default)
    Stat.inline_stats;
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
