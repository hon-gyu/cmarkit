(* {0 Pretty-printers}

   show structure and a preview of inline content, dropping [Meta.t] and
   most layout. Use [~ext] to handle user-defined [Block.t] extensions. *)

open Common_
open Block

let h_epls = "…"

module Listchar = struct
  let tab = "→ "
  let trail = "·"
  let eol = "↵"
  let nbsp = "␣"
  let extends = "»"
  let precedes = "«"
  let leadmultispace = "│ "
end

let pp_ext_default _ppf _b = Format.pp_print_string _ppf "<ext>"

let pp_inline_preview ?ext_inline ppf inline =
  let text = Inline.to_plain_text ?ext:ext_inline ~break_on_soft:true inline in
  let s = String.concat " " (List.map (String.concat "") text) in
  let max_len = 60 in
  let len = String.length s in
  let s =
    if len <= max_len then s
    else
    Printf.sprintf "%s%s<%dc>" (String.sub s 0 max_len) h_epls (len - max_len)
  in
  Format.fprintf ppf "%S" s

(* Commonmark label matching is case-insensitive and whitespace-collapsing *)
let pp_label ppf l = Format.fprintf ppf "%S" (Label.key l)

let rec pp_block_with ?(ext = pp_ext_default) ?ext_inline () ppf = function
| Blank_line _ -> Format.pp_print_string ppf "Blank_line"
| Block_quote (bq, _) ->
    Format.fprintf ppf "@[<v 2>Block_quote@,%a@]" (pp_block_with ~ext ?ext_inline ())
      (Block_quote.block bq)
| Blocks ([], _) -> Format.pp_print_string ppf "Blocks []"
| Blocks (bs, _) ->
    Format.fprintf ppf "@[<v 2>Blocks@,%a@]"
      (Format.pp_print_list ~pp_sep:Format.pp_print_cut (pp_block_with ~ext ?ext_inline ()))
      bs
| Code_block (cb, _) ->
    let name =
      match Code_block.layout cb with
      | `Indented -> "Indented_code_block"
      | `Fenced _ -> "Fenced_code_block"
    in
    let info =
      match Code_block.info_string cb with
      | None -> "-"
      | Some (s, _) -> Printf.sprintf "%S" s
    in
    Format.fprintf ppf "%s { info=%s; lines=%d }" name info
      (List.length (Code_block.code cb))
| Heading (h, _) ->
    Format.fprintf ppf "@[<2>Heading H%d@ %a@]" (Heading.level h)
      (pp_inline_preview ?ext_inline)
      (Heading.inline h)
| Html_block (lines, _) ->
    Format.fprintf ppf "Html_block { lines=%d }" (List.length lines)
| Link_reference_definition (ld, _) ->
    let label =
      match Link_definition.label ld with
      | None -> "-"
      | Some l -> Printf.sprintf "%S" (Label.key l)
    in
    Format.fprintf ppf "Link_reference_definition { label=%s }" label
| List (l, _) ->
    let type_str =
      match List'.type' l with
      | `Unordered c -> Printf.sprintf "unordered %C" c
      | `Ordered (n, c) -> Printf.sprintf "ordered %d%C" n c
    in
    let tight = if List'.tight l then "tight" else "loose" in
    Format.fprintf ppf "@[<v 2>List { %s; %s }@,%a@]" type_str tight
      (Format.pp_print_list ~pp_sep:Format.pp_print_cut (fun ppf (i, _) ->
           Format.fprintf ppf "@[<v 2>- item@,%a@]" (pp_block_with ~ext ?ext_inline ())
             (List_item.block i)))
      (List'.items l)
| Paragraph (p, _) ->
    Format.fprintf ppf "@[<2>Paragraph@ %a@]"
      (pp_inline_preview ?ext_inline)
      (Paragraph.inline p)
| Thematic_break _ -> Format.pp_print_string ppf "Thematic_break"
| Ext_math_block (cb, _) ->
    Format.fprintf ppf "Ext_math_block { lines=%d }"
      (List.length (Code_block.code cb))
| Ext_table (t, _) ->
    Format.fprintf ppf "Ext_table { cols=%d; rows=%d }" (Table.col_count t)
      (List.length (Table.rows t))
| Ext_footnote_definition (fn, _) ->
    Format.fprintf ppf "@[<v 2>Ext_footnote_definition %a@,%a@]" pp_label
      (Footnote.label fn) (pp_block_with ~ext ?ext_inline ()) (Footnote.block fn)
| b -> ext ppf b

let pp_block = pp_block_with ()
