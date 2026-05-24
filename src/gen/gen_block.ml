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
  (* A histogram over constructor labels. [t] is a monoid: [empty] is the
     unit and [merge] is associative, so traversals compose by folding. *)
  type t = int String_map.t

  let empty : t = String_map.empty
  let singleton k : t = String_map.singleton k 1
  let merge : t -> t -> t = String_map.union (fun _ a b -> Some (a + b))
  let total t = String_map.fold (fun _ n acc -> acc + n) t 0

  let rec of_inline = function
    | Inline.Text _ -> singleton "Text"
    | Inline.Code_span _ -> singleton "Code_span"
    | Inline.Emphasis (e, _) ->
        merge (singleton "Emphasis") (of_inline (Inline.Emphasis.inline e))
    | Inline.Strong_emphasis (e, _) ->
        merge
          (singleton "Strong_emphasis")
          (of_inline (Inline.Emphasis.inline e))
    | Inline.Inlines (is, _) ->
        List.fold_left (fun acc i -> merge acc (of_inline i)) empty is
    | _ -> empty

  let rec of_block = function
    | Block.Paragraph (p, _) -> of_inline (Block.Paragraph.inline p)
    | Block.Heading (h, _) -> of_inline (Block.Heading.inline h)
    | Block.Block_quote (bq, _) -> of_block (Block.Block_quote.block bq)
    | Block.Blocks (bs, _) ->
        List.fold_left (fun acc b -> merge acc (of_block b)) empty bs
    | Block.List (l, _) ->
        List.fold_left
          (fun acc (item, _) ->
            merge acc (of_block (Block.List_item.block item)))
          empty (Block.List'.items l)
    | _ -> empty

  let to_table t : string =
    let n = total t in
    let pct c =
      if n = 0 then "0.0%"
      else Printf.sprintf "%.1f%%" (100.0 *. float_of_int c /. float_of_int n)
    in
    let rows =
      String_map.bindings t |> List.sort (fun (_, a) (_, b) -> compare b a)
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
  let stats =
    let rec loop acc k =
      if k = 0 then acc
      else
        loop
          (Stats.merge acc (Stats.of_block (G.generate1 ~rand gen_block)))
          (k - 1)
    in
    loop Stats.empty 500
  in
  print_string (Stats.to_table stats);
  [%expect
    {|
    |---------------------------------|
    | constructor     | count | share |
    |-----------------+-------+-------|
    | Text            | 1258  | 68.6% |
    | Code_span       | 295   | 16.1% |
    | Strong_emphasis | 164   | 8.9%  |
    | Emphasis        | 117   | 6.4%  |
    |---------------------------------|
    |}]
