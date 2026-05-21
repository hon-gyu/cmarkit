open Oymarkit_

module G = QCheck2.Gen

(* Surface AST
   =========== *)

type s_inline =
  | Text of string
  | Emph of s_inline list
  | Strong of s_inline list
  | Code of string

type s_block =
  | Para of s_inline list
  | Heading of int * s_inline list
  | Thematic
  | Blank
  | Code_block of string list
  | Block_quote of s_block list
  | Ulist of s_block list list

(* Lowering
   ========= *)

let mk_text s = Inline.Text (s, Meta.none)
let mk_inlines = function
  | [ i ] -> i
  | is -> Inline.Inlines (is, Meta.none)

let rec lower_inline = function
  | Text s -> mk_text s
  | Emph is ->
      let inner = mk_inlines (List.map lower_inline is) in
      Inline.Emphasis (Inline.Emphasis.make inner, Meta.none)
  | Strong is ->
      let inner = mk_inlines (List.map lower_inline is) in
      Inline.Strong_emphasis (Inline.Emphasis.make inner, Meta.none)
  | Code s ->
      Inline.Code_span (Inline.Code_span.of_string s, Meta.none)

let to_inline is = mk_inlines (List.map lower_inline is)

let mk_blocks = function
  | [ b ] -> b
  | bs -> Block.Blocks (bs, Meta.none)

let rec lower_block = function
  | Para is ->
      let p = Block.Paragraph.make (to_inline is) in
      Block.Paragraph (p, Meta.none)
  | Heading (level, is) ->
      let h = Block.Heading.make ~level (to_inline is) in
      Block.Heading (h, Meta.none)
  | Thematic ->
      Block.Thematic_break (Block.Thematic_break.make (), Meta.none)
  | Blank ->
      Block.Blank_line ("", Meta.none)
  | Code_block lines ->
      let bls = List.map (fun s -> (s, Meta.none)) lines in
      let cb = Block.Code_block.make bls in
      Block.Code_block (cb, Meta.none)
  | Block_quote bs ->
      let inner = mk_blocks (List.map lower_block bs) in
      let bq = Block.Block_quote.make inner in
      Block.Block_quote (bq, Meta.none)
  | Ulist items ->
      let mk_item bs =
        let inner = mk_blocks (List.map lower_block bs) in
        (Block.List_item.make inner, Meta.none)
      in
      let l = Block.List'.make (`Unordered '-') (List.map mk_item items) in
      Block.List (l, Meta.none)

let to_block = lower_block

(* Generators
   ========== *)

(* Restrict the text alphabet to keep early counterexamples readable and
   to avoid hitting unrelated escape/UTF-8 quirks. Widen later. *)
let safe_char =
  G.oneof_array [| 'a'; 'b'; 'c'; 'x'; 'y'; 'z'; '0'; '1'; ' ' |]

let safe_word =
  let open G in
  let* n = int_range 1 6 in
  string_size ~gen:safe_char (return n)

(* Trim outer spaces and reject empty — paragraphs need at least one visible
   character or the renderer produces no paragraph at all. *)
let nonblank_word =
  let open G in
  safe_word
  |> map String.trim
  |> map (fun s -> if s = "" then "a" else s)

let code_payload =
  (* No backticks; pick a tiny alphabet. *)
  let alpha = G.oneof_array [| 'a'; 'b'; '1'; ' ' |] in
  let open G in
  let* n = int_range 1 4 in
  string_size ~gen:alpha (return n)

let gen_inline : s_inline G.t =
  G.sized_size (G.int_range 0 2) @@ G.fix (fun self n ->
    if n <= 0 then
      G.oneof
        [ G.map (fun s -> Text s) nonblank_word;
          G.map (fun s -> Code s) code_payload ]
    else
      let smaller = self (n - 1) in
      G.oneof_weighted
        [ 3, G.map (fun s -> Text s) nonblank_word;
          1, G.map (fun s -> Code s) code_payload;
          1, G.map (fun is -> Emph is)
               (G.list_size (G.int_range 1 3) smaller);
          1, G.map (fun is -> Strong is)
               (G.list_size (G.int_range 1 3) smaller) ])

let gen_inlines : s_inline list G.t =
  G.list_size (G.int_range 1 3) gen_inline

let gen_heading_level = G.int_range 1 6

let gen_code_lines =
  G.list_size (G.int_range 1 3) code_payload

(* Block generator with explicit size budget. Containers (block_quote,
   ulist) recur on a strictly smaller size, so generation terminates. *)
let gen_block : s_block G.t =
  G.sized_size (G.int_range 1 4) @@ G.fix (fun self n ->
    let leaves =
      [ 4, G.map (fun is -> Para is) gen_inlines;
        2, G.map2 (fun l is -> Heading (l, is)) gen_heading_level gen_inlines;
        1, G.return Thematic;
        1, G.return Blank;
        1, G.map (fun ls -> Code_block ls) gen_code_lines ]
    in
    if n <= 0 then G.oneof_weighted leaves
    else
      let smaller = self (n - 1) in
      let quote =
        G.map (fun bs -> Block_quote bs)
          (G.list_size (G.int_range 1 3) smaller)
      in
      let ulist =
        G.map (fun items -> Ulist items)
          (G.list_size (G.int_range 1 3)
             (G.list_size (G.int_range 1 2) smaller))
      in
      G.oneof_weighted (leaves @ [ 2, quote; 2, ulist ]))

(* ====================================================================== *)
(* Printer                                                                *)
(* ====================================================================== *)

let rec pp_inline ppf = function
  | Text s -> Format.fprintf ppf "Text %S" s
  | Code s -> Format.fprintf ppf "Code %S" s
  | Emph is -> Format.fprintf ppf "@[<2>Emph@ [%a]@]" pp_inlines is
  | Strong is -> Format.fprintf ppf "@[<2>Strong@ [%a]@]" pp_inlines is
and pp_inlines ppf is =
  Format.pp_print_list
    ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
    pp_inline ppf is

let rec pp_block ppf = function
  | Para is -> Format.fprintf ppf "@[<2>Para@ [%a]@]" pp_inlines is
  | Heading (l, is) ->
      Format.fprintf ppf "@[<2>Heading@ %d@ [%a]@]" l pp_inlines is
  | Thematic -> Format.pp_print_string ppf "Thematic"
  | Blank -> Format.pp_print_string ppf "Blank"
  | Code_block lines ->
      Format.fprintf ppf "@[<2>Code_block@ [%a]@]"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
           (fun ppf s -> Format.fprintf ppf "%S" s))
        lines
  | Block_quote bs ->
      Format.fprintf ppf "@[<v 2>Block_quote@,%a@]" pp_blocks bs
  | Ulist items ->
      Format.fprintf ppf "@[<v 2>Ulist@,%a@]"
        (Format.pp_print_list
           ~pp_sep:Format.pp_print_cut
           (fun ppf bs -> Format.fprintf ppf "@[<v 2>- item@,%a@]" pp_blocks bs))
        items
and pp_blocks ppf bs =
  Format.pp_print_list ~pp_sep:Format.pp_print_cut pp_block ppf bs

let print_s_block b = Format.asprintf "%a" pp_block b

(* Rules
   ====== *)

module Rule = struct
  type t = { name : string; check : s_block -> bool }

  (* Walk the whole tree; rules are local predicates lifted via [forall]. *)
  let rec forall_block p b =
    p b &&
    match b with
    | Block_quote bs -> List.for_all (forall_block p) bs
    | Ulist items -> List.for_all (List.for_all (forall_block p)) items
    | _ -> true

  let heading_level_in_range = function
    | Heading (l, _) -> 1 <= l && l <= 6
    | _ -> true

  let paragraph_nonempty = function
    | Para [] -> false
    | _ -> true

  let list_items_nonempty = function
    | Ulist items ->
        items <> [] && List.for_all (fun bs -> bs <> []) items
    | _ -> true

  let block_quote_nonempty = function
    | Block_quote [] -> false
    | _ -> true

  let lift name p =
    { name; check = forall_block p }

  let all =
    [ lift "heading_level_in_range" heading_level_in_range;
      lift "paragraph_nonempty" paragraph_nonempty;
      lift "list_items_nonempty" list_items_nonempty;
      lift "block_quote_nonempty" block_quote_nonempty ]
end

let%expect_test "generator produces a lowerable sample" =
  let rand = Random.State.make [| 42 |] in
  let s = G.generate1 ~rand gen_block in
  let _ : Block.t = to_block s in
  let pass_rules = List.for_all (fun r -> r.Rule.check s) Rule.all in
  Printf.printf "lowered ok; rules pass = %b\n" pass_rules;
  [%expect {| lowered ok; rules pass = true |}]
