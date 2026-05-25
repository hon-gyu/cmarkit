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

let gen_block_leaf : Block.t G.t =
  G.oneof
    [
      G.oneof_list blank_line_egs;
      G.oneof_list thematic_break_egs;
      G.oneof_list code_block_egs;
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
                (1, gen_block_leaf);
                ( 2,
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
  let testsuite =
    [
      QCheck2.Test.make ~name:"Inline generator overview" ~stats:inline_stats
        gen_inline (fun _ -> true);
    ]
  in
  let rand = Random.State.make [| 42 |] in
  ignore
  @@ QCheck_base_runner.run_tests ~colors:false ~verbose:false ~rand testsuite;
  [%expect
    {|
    +++ Stats for Inline generator overview ++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    stats text:
      num: 100, avg: 2.35, stddev: 9.80, median 0, min 0, max 59
       0.. 2: #######################################################          92
       3.. 5: #                                                                 2
       6.. 8:                                                                   0
       9..11:                                                                   1
      12..14:                                                                   1
      15..17:                                                                   0
      18..20:                                                                   0
      21..23:                                                                   0
      24..26:                                                                   0
      27..29:                                                                   1
      30..32:                                                                   0
      33..35:                                                                   0
      36..38:                                                                   0
      39..41:                                                                   0
      42..44:                                                                   0
      45..47:                                                                   0
      48..50:                                                                   1
      51..53:                                                                   0
      54..56:                                                                   1
      57..59:                                                                   1

    stats code_span:
      num: 100, avg: 2.62, stddev: 10.67, median 0, min 0, max 66
       0.. 3: #######################################################          92
       4.. 7: #                                                                 2
       8..11:                                                                   1
      12..15:                                                                   0
      16..19:                                                                   0
      20..23:                                                                   1
      24..27:                                                                   0
      28..31:                                                                   1
      32..35:                                                                   0
      36..39:                                                                   0
      40..43:                                                                   0
      44..47:                                                                   0
      48..51:                                                                   0
      52..55:                                                                   1
      56..59:                                                                   1
      60..63:                                                                   0
      64..67:                                                                   1
      68..71:                                                                   0
      72..75:                                                                   0
      76..79:                                                                   0

    stats autolink:
      num: 100, avg: 2.70, stddev: 11.49, median 0, min 0, max 76
       0.. 3: #######################################################          93
       4.. 7:                                                                   1
       8..11: #                                                                 2
      12..15:                                                                   0
      16..19:                                                                   0
      20..23:                                                                   0
      24..27:                                                                   0
      28..31:                                                                   0
      32..35:                                                                   0
      36..39:                                                                   1
      40..43:                                                                   0
      44..47:                                                                   0
      48..51:                                                                   0
      52..55:                                                                   0
      56..59: #                                                                 2
      60..63:                                                                   0
      64..67:                                                                   0
      68..71:                                                                   0
      72..75:                                                                   0
      76..79:                                                                   1

    stats break:
      num: 100, avg: 2.59, stddev: 10.76, median 0, min 0, max 67
       0.. 3: #######################################################          93
       4.. 7:                                                                   1
       8..11:                                                                   1
      12..15:                                                                   1
      16..19:                                                                   0
      20..23:                                                                   1
      24..27:                                                                   0
      28..31:                                                                   0
      32..35:                                                                   0
      36..39:                                                                   0
      40..43:                                                                   0
      44..47:                                                                   0
      48..51:                                                                   0
      52..55:                                                                   1
      56..59:                                                                   0
      60..63:                                                                   0
      64..67: #                                                                 2
      68..71:                                                                   0
      72..75:                                                                   0
      76..79:                                                                   0

    stats raw_html:
      num: 100, avg: 2.65, stddev: 11.40, median 0, min 0, max 81
        0..  4: #######################################################          94
        5..  9:                                                                   1
       10.. 14:                                                                   1
       15.. 19:                                                                   0
       20.. 24:                                                                   1
       25.. 29:                                                                   0
       30.. 34:                                                                   0
       35.. 39:                                                                   0
       40.. 44:                                                                   0
       45.. 49:                                                                   1
       50.. 54:                                                                   0
       55.. 59:                                                                   0
       60.. 64:                                                                   0
       65.. 69:                                                                   1
       70.. 74:                                                                   0
       75.. 79:                                                                   0
       80.. 84:                                                                   1
       85.. 89:                                                                   0
       90.. 94:                                                                   0
       95.. 99:                                                                   0

    stats emphasis:
      num: 100, avg: 10.09, stddev: 43.27, median 0, min 0, max 261
        0.. 13: #######################################################          93
       14.. 27:                                                                   1
       28.. 41:                                                                   1
       42.. 55:                                                                   1
       56.. 69:                                                                   0
       70.. 83:                                                                   0
       84.. 97:                                                                   0
       98..111:                                                                   0
      112..125:                                                                   1
      126..139:                                                                   0
      140..153:                                                                   0
      154..167:                                                                   0
      168..181:                                                                   0
      182..195:                                                                   0
      196..209:                                                                   0
      210..223:                                                                   0
      224..237:                                                                   1
      238..251:                                                                   1
      252..265:                                                                   1
      266..279:                                                                   0

    stats strong_emphasis:
      num: 100, avg: 9.61, stddev: 39.57, median 0, min 0, max 236
        0.. 11: #######################################################          92
       12.. 23: #                                                                 2
       24.. 35:                                                                   0
       36.. 47:                                                                   0
       48.. 59: #                                                                 2
       60.. 71:                                                                   0
       72.. 83:                                                                   0
       84.. 95:                                                                   0
       96..107:                                                                   0
      108..119:                                                                   1
      120..131:                                                                   0
      132..143:                                                                   0
      144..155:                                                                   0
      156..167:                                                                   0
      168..179:                                                                   0
      180..191:                                                                   0
      192..203:                                                                   0
      204..215:                                                                   1
      216..227:                                                                   1
      228..239:                                                                   1

    stats link:
      num: 100, avg: 9.17, stddev: 38.98, median 0, min 0, max 224
        0.. 11: #######################################################          92
       12.. 23: #                                                                 2
       24.. 35:                                                                   0
       36.. 47: #                                                                 2
       48.. 59:                                                                   0
       60.. 71:                                                                   0
       72.. 83:                                                                   0
       84.. 95:                                                                   0
       96..107:                                                                   0
      108..119:                                                                   1
      120..131:                                                                   0
      132..143:                                                                   0
      144..155:                                                                   0
      156..167:                                                                   0
      168..179:                                                                   0
      180..191:                                                                   0
      192..203:                                                                   0
      204..215:                                                                   1
      216..227: #                                                                 2
      228..239:                                                                   0

    stats image:
      num: 100, avg: 9.41, stddev: 39.66, median 0, min 0, max 235
        0.. 11: #######################################################          94
       12.. 23:                                                                   0
       24.. 35:                                                                   0
       36.. 47:                                                                   1
       48.. 59:                                                                   1
       60.. 71:                                                                   0
       72.. 83:                                                                   0
       84.. 95:                                                                   0
       96..107:                                                                   0
      108..119:                                                                   1
      120..131:                                                                   0
      132..143:                                                                   0
      144..155:                                                                   0
      156..167:                                                                   0
      168..179:                                                                   0
      180..191:                                                                   0
      192..203:                                                                   1
      204..215:                                                                   0
      216..227:                                                                   0
      228..239: #                                                                 2

    stats inlines:
      num: 100, avg: 9.26, stddev: 40.11, median 0, min 0, max 246
        0.. 12: #######################################################          92
       13.. 25: #                                                                 2
       26.. 38:                                                                   1
       39.. 51:                                                                   1
       52.. 64:                                                                   0
       65.. 77:                                                                   0
       78.. 90:                                                                   0
       91..103:                                                                   1
      104..116:                                                                   0
      117..129:                                                                   0
      130..142:                                                                   0
      143..155:                                                                   0
      156..168:                                                                   0
      169..181:                                                                   0
      182..194:                                                                   0
      195..207:                                                                   1
      208..220:                                                                   0
      221..233:                                                                   0
      234..246: #                                                                 2
      247..259:                                                                   0
    ================================================================================
    success (ran 1 tests)
    |}]
