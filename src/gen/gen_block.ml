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

(* let mk_link_egs i : Inline.Link.t list =
  Inline.Link.[
    make i;
  ] *)

(* TODO: extension strikethrough and math_span *)

let gen_inline_leaf : Inline.t G.t =
  [ text_egs; code_span_egs; autolink_egs; break_egs ]
  |> List.map G.oneof_list |> G.oneof

let gen_inline : Inline.t G.t =
  G.(
    sized_size nat_small
    @@ fix (fun self (n : int) ->
        match n with
        | 0 -> gen_inline_leaf
        | n ->
            let inlines_of_is is = Inline.Inlines (is, Meta.none) in
            oneof_weighted
              [
                (1, gen_inline_leaf);
                (2, map inlines_of_is (list_size (int_bound (n / 2)) (self (n / 2))));
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
                (2, map blocks_of_bs (list_size (int_bound (n / 2)) (self (n / 2))));
                (1, map block_quote_of_b (self (n / 2)));
                (1, gen_list_block);
              ]))

let inline_stats : Inline.t QCheck2.stat list =
  let text_count =
   fun (i : Inline.t) ->
    let rec loop acc = function
      | Inline.Text _ -> acc + 1
      | Inline.Emphasis (e, _) -> loop acc (Inline.Emphasis.inline e)
      | Inline.Strong_emphasis (e, _) -> loop acc (Inline.Emphasis.inline e)
      | Inline.Inlines (is, _) -> List.fold_left loop acc is
      | _ -> acc
    in
    loop 0 i
  in
  let emph_count =
   fun (i : Inline.t) ->
    let rec loop acc = function
      | Inline.Emphasis (e, _) -> loop (acc + 1) (Inline.Emphasis.inline e)
      | Inline.Strong_emphasis (e, _) -> loop acc (Inline.Emphasis.inline e)
      | Inline.Inlines (is, _) -> List.fold_left loop acc is
      | _ -> acc
    in
    loop 0 i
  in
  let strong_emph_count =
   fun (i : Inline.t) ->
    let rec loop acc = function
      | Inline.Emphasis (e, _) -> loop acc (Inline.Emphasis.inline e)
      | Inline.Strong_emphasis (e, _) ->
          loop (acc + 1) (Inline.Emphasis.inline e)
      | Inline.Inlines (is, _) -> List.fold_left loop acc is
      | _ -> acc
    in
    loop 0 i
  in
  [
    ("text_count", text_count);
    ("emphasis_count", emph_count);
    ("strong_emphasis_count", strong_emph_count);
  ]
;;

let%expect_test _ =
  let testsuite =
    [
      QCheck2.Test.make ~name:"Inline generator overview" ~stats:inline_stats
        gen_inline (fun _ -> true);
    ]
  in
  let rand = Random.State.make [| 42 |] in
  ignore @@ QCheck_base_runner.run_tests ~colors:false ~verbose:true ~rand testsuite;
  [%expect {|
    generated error fail pass / total     time test name
    [ ]    0    0    0    0 /  100     0.0s Inline generator overview
    [✓]  100    0    0  100 /  100     0.0s Inline generator overview

    +++ Stats for Inline generator overview ++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    stats text_count:
      num: 100, avg: 7.98, stddev: 31.45, median 0, min 0, max 214
        0.. 10: #######################################################          90
       11.. 21: #                                                                 3
       22.. 32:                                                                   0
       33.. 43:                                                                   0
       44.. 54: #                                                                 2
       55.. 65:                                                                   1
       66.. 76:                                                                   1
       77.. 87:                                                                   0
       88.. 98:                                                                   0
       99..109:                                                                   0
      110..120:                                                                   1
      121..131:                                                                   0
      132..142:                                                                   0
      143..153:                                                                   0
      154..164:                                                                   0
      165..175:                                                                   0
      176..186:                                                                   1
      187..197:                                                                   0
      198..208:                                                                   0
      209..219:                                                                   1

    stats emphasis_count:
      num: 100, avg: 0.00, stddev: 0.00, median 0, min 0, max 0
      0: #######################################################         100

    stats strong_emphasis_count:
      num: 100, avg: 0.00, stddev: 0.00, median 0, min 0, max 0
      0: #######################################################         100
    ================================================================================
    success (ran 1 tests)
    |}];
;;
