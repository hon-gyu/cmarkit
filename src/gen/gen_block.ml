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
    sized
    @@ fix (fun self (n : int) ->
        match n with
        | 0 -> gen_inline_leaf
        | n ->
            let inlines_of_is is = Inline.Inlines (is, Meta.none) in
            oneof_weighted
              [
                (1, gen_inline_leaf);
                (2, map inlines_of_is (list (self (n / 2))));
              ]))

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

(* let%expect_test "inline constructor distribution" =
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
    |}] *)
