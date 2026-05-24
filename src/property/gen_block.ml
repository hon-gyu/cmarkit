open Oymarkit_
module G = QCheck2.Gen

(* Vocabulary
   ========== *)

let words =
  [|
    "alpha";
    "beta";
    "gamma";
    "delta";
    "lorem";
    "ipsum";
    "the";
    "quick";
    "fox";
    "lazy";
    "dog";
  |]

let code_words = [| "id"; "let"; "fun"; "x"; "y"; "n"; "0"; "1" |]

let gen_phrase =
  let open G in
  let* n = int_range 1 3 in
  let* ws = list_size (return n) (oneof_array words) in
  return (String.concat " " ws)

let gen_code_payload =
  let open G in
  let* n = int_range 1 2 in
  let* ws = list_size (return n) (oneof_array code_words) in
  return (String.concat " " ws)

(* Inline
   ====== *)

let mk_text s = Inline.Text (s, Meta.none)
let mk_code s = Inline.Code_span (Inline.Code_span.of_string s, Meta.none)
let mk_emph i = Inline.Emphasis (Inline.Emphasis.make i, Meta.none)
let mk_strong i = Inline.Strong_emphasis (Inline.Emphasis.make i, Meta.none)

let inlines_of = function
  | [ i ] -> i
  | is -> Inline.Inlines (is, Meta.none)

let gen_inline : Inline.t G.t =
  G.sized_size (G.int_range 0 2)
  @@ G.fix (fun self n ->
      let leaves =
        [ (4, G.map mk_text gen_phrase); (1, G.map mk_code gen_code_payload) ]
      in
      if n <= 0 then G.oneof_weighted leaves
      else
        let inner =
          G.map inlines_of (G.list_size (G.int_range 1 3) (self (n - 1)))
        in
        G.oneof_weighted
          (leaves @ [ (1, G.map mk_emph inner); (1, G.map mk_strong inner) ]))

let gen_inlines : Inline.t G.t =
  G.map inlines_of (G.list_size (G.int_range 1 3) gen_inline)

(* Block
   ===== *)

let gen_heading_level = G.int_range 1 6
let gen_code_lines = G.list_size (G.int_range 1 3) gen_code_payload
let mk_para is = Block.Paragraph (Block.Paragraph.make is, Meta.none)
let mk_heading level is = Block.Heading (Block.Heading.make ~level is, Meta.none)
let mk_thematic = Block.Thematic_break (Block.Thematic_break.make (), Meta.none)
let mk_blank = Block.Blank_line ("", Meta.none)

let mk_code_block lines =
  let bls = List.map (fun s -> (s, Meta.none)) lines in
  Block.Code_block (Block.Code_block.make bls, Meta.none)

let blocks_of = function
  | [ b ] -> b
  | bs -> Block.Blocks (bs, Meta.none)

let mk_block_quote bs =
  Block.Block_quote (Block.Block_quote.make (blocks_of bs), Meta.none)

let mk_ulist items =
  let mk_item bs = (Block.List_item.make (blocks_of bs), Meta.none) in
  let l = Block.List'.make (`Unordered '-') (List.map mk_item items) in
  Block.List (l, Meta.none)

let gen_block : Block.t G.t =
  G.sized_size (G.int_range 1 4)
  @@ G.fix (fun self n ->
      let leaves =
        [
          (4, G.map mk_para gen_inlines);
          (2, G.map2 mk_heading gen_heading_level gen_inlines);
          (1, G.return mk_thematic);
          (1, G.return mk_blank);
          (1, G.map mk_code_block gen_code_lines);
        ]
      in
      if n <= 0 then G.oneof_weighted leaves
      else
        let smaller = self (n - 1) in
        let quote =
          G.map mk_block_quote (G.list_size (G.int_range 1 3) smaller)
        in
        let ulist =
          G.map mk_ulist
            (G.list_size (G.int_range 1 3)
               (G.list_size (G.int_range 1 2) smaller))
        in
        G.oneof_weighted (leaves @ [ (2, quote); (2, ulist) ]))

(* Distribution
   ============ *)

module Stats = struct
  type t = {
    mutable text : int;
    mutable code : int;
    mutable emph : int;
    mutable strong : int;
  }

  let make () = { text = 0; code = 0; emph = 0; strong = 0 }

  let rec count_inline t = function
    | Inline.Text _ -> t.text <- t.text + 1
    | Inline.Code_span _ -> t.code <- t.code + 1
    | Inline.Emphasis (e, _) ->
        t.emph <- t.emph + 1;
        count_inline t (Inline.Emphasis.inline e)
    | Inline.Strong_emphasis (e, _) ->
        t.strong <- t.strong + 1;
        count_inline t (Inline.Emphasis.inline e)
    | Inline.Inlines (is, _) -> List.iter (count_inline t) is
    | _ -> ()

  let rec count_block t = function
    | Block.Paragraph (p, _) -> count_inline t (Block.Paragraph.inline p)
    | Block.Heading (h, _) -> count_inline t (Block.Heading.inline h)
    | Block.Block_quote (bq, _) -> count_block t (Block.Block_quote.block bq)
    | Block.Blocks (bs, _) -> List.iter (count_block t) bs
    | Block.List (l, _) ->
        List.iter
          (fun (item, _) -> count_block t (Block.List_item.block item))
          (Block.List'.items l)
    | _ -> ()

  let to_table t =
    let total = t.text + t.code + t.emph + t.strong in
    let pct n =
      if total = 0 then "0.0%"
      else
        Printf.sprintf "%.1f%%" (100.0 *. float_of_int n /. float_of_int total)
    in
    let rows =
      [
        ("Text", t.text);
        ("Code_span", t.code);
        ("Emphasis", t.emph);
        ("Strong_emphasis", t.strong);
      ]
    in
    let open Ascii_table in
    to_string_noattr ~bars:`Ascii ~limit_width_to:60
      [
        Column.create "constructor" fst;
        Column.create "count" (fun (_, c) -> string_of_int c);
        Column.create "share" (fun (_, c) -> pct c);
      ]
      rows
end

let%expect_test "inline constructor distribution" =
  let rand = Random.State.make [| 42 |] in
  let stats = Stats.make () in
  for _ = 1 to 500 do
    Stats.count_block stats (G.generate1 ~rand gen_block)
  done;
  print_string (Stats.to_table stats);
  [%expect
    {|
    |---------------------------------|
    | constructor     | count | share |
    |-----------------+-------+-------|
    | Text            | 1258  | 68.6% |
    | Code_span       | 295   | 16.1% |
    | Emphasis        | 117   | 6.4%  |
    | Strong_emphasis | 164   | 8.9%  |
    |---------------------------------|
    |}]
