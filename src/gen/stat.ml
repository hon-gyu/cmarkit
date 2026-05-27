open Oymarkit_

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
