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

let gen_inline_leaf : Inline.t G.t =
  [ text_egs; code_span_egs; autolink_egs; break_egs; raw_html_egs ]
  |> List.map G.oneof_list |> G.oneof

let gen_inline : Inline.t G.t =
  G.(
    sized_size nat_small
    @@ fix (fun self (n : int) ->
        match n with
        | 0 -> gen_inline_leaf
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
                (1, gen_inline_leaf);
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
  [ "\n"; "  \n"; "\t\n" ]
  |> List.map (fun bl -> Block.(Blank_line (bl, Meta.none)))

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
    gen_inline

let gen_heading : Block.t G.t =
  G.map
    (fun (level, inline) ->
      Block.(Heading (Block.Heading.make ~level inline, Meta.none)))
    G.(pair (int_range 1 6) gen_inline)

let html_block_egs : Block.t list =
  [ [ ("<div>\n", Meta.none) ]; [ ("<p>hello</p>\n", Meta.none) ]; [ ("<!-- comment -->\n", Meta.none) ] ]
  |> List.map (fun lines -> Block.(Html_block (lines, Meta.none)))

let gen_block_leaf : Block.t G.t =
  G.oneof
    [
      G.oneof_list blank_line_egs;
      G.oneof_list thematic_break_egs;
      G.oneof_list code_block_egs;
      G.oneof_list html_block_egs;
      gen_paragraph;
      gen_heading;
    ]

let gen_block : Block.t G.t =
  G.(
    sized_size nat_small
    @@ fix (fun self (n : int) ->
        match n with
        | 0 -> gen_block_leaf
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
                (2, gen_block_leaf);
                ( 1,
                  map blocks_of_bs
                    (list_size (int_bound (n / 2)) (self (n / 2))) );
                (1, map block_quote_of_b (self (n / 2)));
                (1, gen_list_block);
              ]))

let count_in_inline pred i =
  let rec loop acc i =
    let acc = if pred i then acc + 1 else acc in
    match i with
    | Inline.Emphasis (e, _)
    | Inline.Strong_emphasis (e, _) ->
        loop acc (Inline.Emphasis.inline e)
    | Inline.Inlines (is, _) -> List.fold_left loop acc is
    | Inline.Link (l, _)
    | Inline.Image (l, _) ->
        loop acc (Inline.Link.text l)
    | _ -> acc
  in
  loop 0 i

let inline_stats : Inline.t QCheck2.stat list =
  let count pred = count_in_inline pred in
  [
    ("text",           count (function Inline.Text _            -> true | _ -> false));
    ("code_span",      count (function Inline.Code_span _       -> true | _ -> false));
    ("autolink",       count (function Inline.Autolink _        -> true | _ -> false));
    ("break",          count (function Inline.Break _           -> true | _ -> false));
    ("raw_html",       count (function Inline.Raw_html _        -> true | _ -> false));
    ("emphasis",       count (function Inline.Emphasis _        -> true | _ -> false));
    ("strong_emphasis",count (function Inline.Strong_emphasis _ -> true | _ -> false));
    ("link",           count (function Inline.Link _            -> true | _ -> false));
    ("image",          count (function Inline.Image _           -> true | _ -> false));
    ("inlines",        count (function Inline.Inlines _         -> true | _ -> false));
  ] [@@ocamlformat "disable"]

let%expect_test _ =
  Pp_distr.pp_gen () Format.std_formatter gen_inline inline_stats;
  [%expect {|
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

let count_in_block pred b =
  let rec loop acc b =
    let acc = if pred b then acc + 1 else acc in
    match b with
    | Block.Block_quote (bq, _) -> loop acc (Block.Block_quote.block bq)
    | Block.Blocks (bs, _) -> List.fold_left loop acc bs
    | Block.List (l, _) ->
        List.fold_left
          (fun acc (item, _) -> loop acc (Block.List_item.block item))
          acc (Block.List'.items l)
    | _ -> acc
  in
  loop 0 b

let block_stats : Block.t QCheck2.stat list =
  let count pred = count_in_block pred in
  [
    ("blank_line",    count (function Block.Blank_line _     -> true | _ -> false));
    ("block_quote",   count (function Block.Block_quote _    -> true | _ -> false));
    ("blocks",        count (function Block.Blocks _         -> true | _ -> false));
    ("code_block",    count (function Block.Code_block _     -> true | _ -> false));
    ("heading",       count (function Block.Heading _        -> true | _ -> false));
    ("html_block",    count (function Block.Html_block _     -> true | _ -> false));
    ("list",          count (function Block.List _           -> true | _ -> false));
    ("paragraph",     count (function Block.Paragraph _      -> true | _ -> false));
    ("thematic_break",count (function Block.Thematic_break _ -> true | _ -> false));
  ] [@@ocamlformat "disable"]

let%expect_test _ =
  Pp_distr.pp_gen ~display:`Boxplot () Format.std_formatter gen_block block_stats;
  [%expect {|
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
