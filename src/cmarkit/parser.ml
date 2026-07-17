open Common_
open Parser_common_

[@@@ocamlformat "disable"]

(*---------------------------------------------------------------------------
   Copyright (c) 2021 The cmarkit programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Block structure parsing. *)

module Block_struct = struct

  (* Moving on the line in the indentation space (columns) and over container
     markers. *)

  let[@inline] current_col p = p.current_char_col + p.tab_consumed_cols
  let[@inline] current_indent p = p.next_non_blank_col - current_col p
  let[@inline] end_of_line p = p.current_char > p.current_line_last_char
  let[@inline] only_blanks p = p.next_non_blank > p.current_line_last_char
  let[@inline] has_next_non_blank p =
    p.next_non_blank <= p.current_line_last_char

  let update_next_non_blank p =
    let rec loop p s last k col =
      if k > last then (p.next_non_blank <- k; p.next_non_blank_col <- col) else
      match s.[k] with
      | ' ' -> loop p s last (k + 1) (col + 1)
      | '\t' -> loop p s last (k + 1) (next_tab_stop col)
      | _ -> p.next_non_blank <- k; p.next_non_blank_col <- col;
    in
    loop p p.i p.current_line_last_char p.current_char p.current_char_col

  let accept_cols ~count p =
    let rec loop p count k col =
      if count = 0 || k > p.current_line_last_char
      then (p.current_char <- k; p.current_char_col <- col) else
      if p.i.[k] <> '\t' then loop p (count - 1) (k + 1) (col + 1) else
      let col' = next_tab_stop col in
      let tab_cols = col' - (col + p.tab_consumed_cols) in
      if tab_cols > count
      then (p.tab_consumed_cols <- count; loop p 0 k col)
      else (p.tab_consumed_cols <- 0; loop p (count - tab_cols) (k + 1) col')
    in
    loop p count p.current_char p.current_char_col;
    update_next_non_blank p

  let match_and_accept_block_quote p =
    (* https://spec.commonmark.org/current/#block-quote-marker *)
    if end_of_line p || p.i.[p.current_char] <> '>' then Match.Nomatch else
    let marker_span =
      current_line_span p ~first:p.current_char ~last:p.current_char
    in
    let next_is_blank =
      let next = p.current_char + 1 in
      next <= p.current_line_last_char && Ascii.is_blank p.i.[next]
    in
    let next_is_eol = p.current_char + 1 > p.current_line_last_char in
    (* Djot's marker is [>] followed by a space or the end of the line; [>text]
       is then a paragraph rather than a quote. *)
    if Oymarkit_mod.block_quote_marker_space p.oymarkit_mod
       && not (next_is_blank || next_is_eol)
    then Match.Nomatch else
    let count = if next_is_blank then (* we eat a space *) 2 else 1 in
    accept_cols ~count p;
    Match.Block_quote_line marker_span

  let accept_list_marker_and_indent p ~marker_size ~last =
    (* Returns min indent after marker for list item  *)
    accept_cols ~count:marker_size p;
    let indent = current_indent p in
    let min_indent =
      if only_blanks p || indent > 4 (* indented code *)
      then 1
      else min indent 4
    in
    accept_cols ~count:min_indent p;
    min_indent

  let accept_code_indent p ~count =
    (* Returns padding for partially consumed tab and content first char *)
    accept_cols p ~count;
    if p.tab_consumed_cols = 0 then 0, p.current_char else
    let col' = next_tab_stop p.current_char_col in
    let pad = col' - (p.current_char_col + p.tab_consumed_cols) in
    pad, p.current_char (* is '\t' *) + 1

  (* These data types are only used during parsing, to find out the
     block structure. All the lists (blocks, lines) are in reverse
     order. We don't extract data from the input here. We just store
     line spans. See:
     https://spec.commonmark.org/current/#phase-1-block-structure *)

  type space_pad = int (* number of space characters to pad content with. *)
  type indented_code_line =
    { pad : space_pad;
      code : line_span;
      is_blank : bool }

  type fence =
    { indent : Layout.indent;
      opening_fence : line_span;
      fence : Char.t * int (* fence length *);
      info_string : line_span option (* we drop the trailing blanks *);
      closing_fence : line_span option; }

  type fenced_code_block =
    { fence : fence;
      code : (space_pad * line_span) list }

  type code_block =
  [ `Indented of indented_code_line list | `Fenced of fenced_code_block ]

  type atx =
    { indent : Layout.indent;
      level : Match.heading_level;
      after_open : byte_pos;
      heading : line_span;
      layout_after : line_span;
      (* Djot heading continuation lines, reversed. Empty unless
         [djot_headings]: in CommonMark a heading is exactly one line. *)
      more : line_span list }

  type setext =
    { level : Match.heading_level;
      heading_lines : line_span list;
      underline : (* Indent, underline char count, blanks *)
        Layout.indent * line_span * line_span; }

  type heading = [ `Atx of atx | `Setext of setext ]

  type html_block =
    { end_cond : Match.html_block_end_cond option;
      html : line_span list }

  type paragraph = { maybe_ref : bool; lines : line_span list }

  type div_fence =
    { indent : Layout.indent;
      fence_len : int; (* number of colons in the opening fence *)
      opening_fence : line_span; (* colons (+ layout before class) *)
      class' : line_span option;
      closing_fence : line_span option; (* [None] until closed *) }

  type jsx_open =
    { indent : Layout.indent;
      name : string;             (* tag name; "" for a fragment "<>" *)
      raw_open : line_span;      (* "<Tag ...>" (or "<>") verbatim *)
      raw_close : line_span option (* "</Tag>" (or "</>"), [None] until closed *) }

  type t =
  | Block_quote of Layout.indent * line_span (* loc of initial marker *) * t list
  | Ext_div of div_fence * t list (* Oymarkit djot div, children reversed *)
  | Ext_jsx_block of jsx_open * t list (* Oymarkit JSX block, children reversed *)
  | Blank_line of space_pad * line_span
  | Code_block of code_block
  | Heading of heading
  | Html_block of html_block
  | List of list'
  | Linkref_def of Link_definition.t node
  | Attribute_specs of Attribute.t list
  | Paragraph of paragraph
  | Thematic_break of Layout.indent * line_span (* including trailing blanks *)
  | Ext_table of
      Layout.indent * (line_span * line_span (* trail blanks *)) list *
      table_caption option *
      (* Blank lines seen after the rows. A djot caption may sit after a blank
         line, so the table cannot close on one: it holds them until it knows
         whether a [^] line follows. If none does, they are flushed as blank
         lines after the table. *)
      line_span list (* reversed *)
  | Ext_footnote of Layout.indent * (Label.t * Label.t option) * t list
  | Ext_def_list of def_list (* Oymarkit djot definition list *)

  and table_caption =
    { caption_indent : Layout.indent (* indent of the '^' *);
      caption_lines : line_span list (* reversed *) }

  and def_item =
   { before_marker : Layout.indent;
     marker : line_span (* the ':' *);
     after_marker : Layout.indent;
     term : line_span (* the rest of the marker line *);
     blocks : t list (* the definition, reversed *) }

  and def_list =
    { last_blank : bool;
      loose : bool;
      colon_indent : int (* indent of the ':' of the last item *);
      (* The definition's indent is whatever its first line has, so it is not
         known when the item is opened: [None] until that line shows up. *)
      def_indent : int option;
      def_items : def_item list; }

  and list_item =
   { before_marker : Layout.indent;
     marker : line_span;
     after_marker : Layout.indent;
     ext_task_marker : (Uchar.t * line_span) option;
     blocks : t list }

  and list' =
    { last_blank : bool; (* last added line was blank and not first line
                            of item *)
      loose : bool; (* inter-item looseness, intra-item is computed later *)
      item_min_indent : int; (* last item minimal indent *)
      list_type : Block.List'.type';
      (* The roman reading of an ambiguous opening marker ([i.] is alpha 9 or
         roman 1). The list stays open to it until a later marker settles the
         style: [i.] then [ii.] is a roman list, since [ii.] can only be roman. *)
      roman_alt : (Block.List'.ordered_style * int) option;
      items : list_item list; }

  let block_is_blank_line = function Blank_line _ -> true | _ -> false

  (* Making blocks from the current line status *)

  let blank_line p =
    let first = p.current_char and last = p.current_line_last_char in
    Blank_line (0, current_line_span p ~first ~last)

  let thematic_break p ~indent ~last:_ =
    let last = p.current_line_last_char (* let's keep everything *) in
    let break = current_line_span p ~first:p.current_char ~last in
    Thematic_break (indent, break)

  let atx_heading p ~indent ~level ~after_open ~first_content ~last_content =
    let heading = current_line_span p ~first:first_content ~last:last_content in
    let layout_after =
      let first = last_content + 1 and last = p.current_line_last_char in
      current_line_span p ~first ~last
    in
    Heading (`Atx { indent; level; after_open; heading; layout_after;
                    more = [] })

  let setext_heading p ~indent ~level ~last_underline heading_lines =
    let u = current_line_span p ~first:p.current_char ~last:last_underline in
    let blanks =
      let first = last_underline + 1 and last = p.current_line_last_char in
      current_line_span p ~first ~last
    in
    let underline = indent, u, blanks in
    Heading (`Setext {level; heading_lines; underline})

  let indented_code_block p = (* Has a side-effect on [p] *)
    let pad, first = accept_code_indent p ~count:4 in
    let code = current_line_span p ~first ~last:p.current_line_last_char in
    Code_block (`Indented [{pad; code; is_blank = false}])

  let fenced_code_block p ~indent ~fence_first ~fence_last ~info =
    let info_string, layout_last = match info with
    | None -> None, p.current_line_last_char
    | Some (first, last) -> Some (current_line_span p ~first ~last), first - 1
    in
    let opening_fence =
      current_line_span p ~first:fence_first ~last:layout_last
    in
    let fence = p.i.[fence_first], (fence_last - fence_first + 1) in
    let closing_fence = None in
    let fence = { indent; opening_fence; fence; info_string; closing_fence } in
    Code_block (`Fenced {fence; code = []})

  let div_block p ~indent ~fence_first ~fence_last ~class_span =
    let fence_len = fence_last - fence_first + 1 in
    let opening_fence, class' = match class_span with
    | None ->
        current_line_span p ~first:fence_first ~last:p.current_line_last_char,
        None
    | Some (cfirst, clast) ->
        current_line_span p ~first:fence_first ~last:(cfirst - 1),
        Some (current_line_span p ~first:cfirst ~last:clast)
    in
    let fence = { indent; fence_len; opening_fence; class'; closing_fence = None}in
    Ext_div (fence, [])

  let jsx_block p ~indent ~name_first ~name_last ~tag_end =
    let raw_open = current_line_span p ~first:p.current_char ~last:tag_end in
    let name =
      if name_last < name_first then "" (* fragment *)
      else String.sub p.i name_first (name_last - name_first + 1)
    in
    Ext_jsx_block ({ indent; name; raw_open; raw_close = None }, [])

  let html_block p ~end_cond ~indent_start =
    let first = indent_start and last = p.current_line_last_char in
    let end_cond = (* Check if the same line matches the end condition. *)
      if Match.html_block_end p.i ~end_cond ~last ~start:p.current_char
      then None (* We are already closed *) else Some end_cond
    in
    Html_block { end_cond; html = [current_line_span p ~first ~last] }

  let paragraph p ~start =
    let last = p.current_line_last_char in
    let maybe_ref = Match.could_be_link_reference_definition p.i ~last ~start in
    Paragraph { maybe_ref; lines = [current_line_span p ~first:start ~last]}

  let add_paragraph_line p ~indent_start par bs =
    let first = indent_start and last = p.current_line_last_char in
    let lines = current_line_span p ~first ~last :: par.lines in
    Paragraph { par with lines } :: bs

  let table_row p ~first ~last =
    current_line_span p ~first ~last,
    current_line_span p ~first:(last + 1) ~last:p.current_line_last_char

  let table p ~indent ~last =
    let row = table_row p ~first:p.current_char ~last in
    Ext_table (indent, [row], None, [])

  (* Djot table caption: a [^] followed by a space or the end of the line, on
     the line after a table. Its continuation lines are indented. *)
  let match_table_caption p ~indent =
    if not (Oymarkit_mod.djot_table_captions p.oymarkit_mod) then None else
    if end_of_line p || p.i.[p.current_char] <> '^' then None else
    let next = p.current_char + 1 in
    if next <= p.current_line_last_char && not (Ascii.is_blank p.i.[next])
    then None else
    let first = Match.first_non_blank p.i ~last:p.current_line_last_char
        ~start:next
    in
    let line =
      current_line_span p ~first ~last:p.current_line_last_char
    in
    Some { caption_indent = indent; caption_lines = [line] }

  let flush_table_blanks blanks bs =
    List.fold_left (fun bs l -> Blank_line (0, l) :: bs) bs (List.rev blanks)

  (* Link reference definition parsing

     This is invoked when we close a paragraph and works on the paragraph
     lines. *)

  (* Djot reference definition: [ [label]: url ], where the destination is the
     rest of the line and there are no titles (a quoted trailer is just more
     URL). The destination may be continued on indented lines, the newlines being
     removed. *)
  let parse_djot_link_reference_definition p lines =
    let none () = raise_notrace Exit in
    let next_line = function line :: lines -> Some (lines, line) | [] -> None in
    try
      let lines, line = match next_line lines with
      | None -> none () | Some v -> v
      in
      let start = first_non_blank_in_span p line in
      let indent = start - line.first in
      let meta_first = { line with first = start } in
      let lines, line, label, start =
        (* A djot definition's label lives on one line: [next_line] stops here so
           that [ [a and\nb]: url ] is not a definition at all. *)
        let next_line _ = None in
        match
          Match.link_label ~djot:true p.buf ~next_line p.i lines ~line ~start
        with
        | None -> none ()
        | Some (_, line, rev_spans, last, key) ->
            let colon = last + 1 in
            if colon > line.last || p.i.[colon] <> ':' then none () else
            let label = Inline_struct.label_of_rev_spans p ~key rev_spans in
            lines, line, label, colon + 1
      in
      let rec collect lines line ~first segs =
        let segs = { line with first; last = line.last } :: segs in
        match next_line lines with
        | None -> lines, line, List.rev segs
        | Some (lines', next) ->
            let nb = first_non_blank_in_span p next in
            (* An indented, non-blank line continues the destination. *)
            if nb > next.last || nb = next.first then lines, line, List.rev segs
            else collect lines' next ~first:nb segs
      in
      let first = Match.first_non_blank p.i ~last:line.last ~start in
      let lines, line, segs = collect lines line ~first [] in
      let dest =
        let seg span =
          if span.first > span.last then "" else
          String.trim (fst (clean_unesc_unref_span p span))
        in
        String.concat "" (List.map seg segs)
      in
      let meta_last = line in
      let meta = meta_of_spans p ~first:meta_first ~last:meta_last in
      let layout =
        { Link_definition.indent; angled_dest = false; before_dest = [];
          after_dest = []; title_open_delim = '\"'; after_title = [] }
      in
      let defined_label = def_label p label in
      let dest = Some (dest, meta) in
      let ld =
        { Link_definition.layout; label = Some label; defined_label; dest;
          title = None; attributes = None }, meta
      in
      begin match defined_label with
      | None -> () | Some def -> set_label_def p def (Link_definition.Def ld)
      end;
      Some (ld, lines)
    with
    | Exit -> None

  let parse_link_reference_definition p lines =
    (* Has no side effect on [p], parsing occurs on [lines] spans. *)
    (* https://spec.commonmark.org/current/#link-reference-definitions *)
    let none () = raise_notrace Exit in
    let next_line = function line :: lines -> Some (lines, line) | [] -> None in
    if Oymarkit_mod.djot_links p.oymarkit_mod
    then parse_djot_link_reference_definition p lines else
    try
      let lines, line = match next_line lines with
      | None -> none () | Some v -> v
      in
      let start = first_non_blank_in_span p line in
      let indent = start - line.first in
      let meta_first = { line with first = start } in
      let lines, line, label, start =
        match Match.link_label ~djot:(Oymarkit_mod.djot_links p.oymarkit_mod) p.buf ~next_line p.i lines ~line ~start with
        | None -> none ()
        | Some (lines, line, rev_spans, last, key) ->
            let colon = last + 1 in
            if colon > line.last || p.i.[colon] <> ':' then none () else
            let label = Inline_struct.label_of_rev_spans p ~key rev_spans in
            lines, line, label, colon + 1
      in
      let lines, line, before_dest, start =
        match first_non_blank_over_nl ~next_line p lines line ~start with
        | None -> none () | Some v -> v
      in
      let angled_dest, dest, start, meta_last =
        match Match.link_destination p.i ~last:line.last ~start with
        | None -> none ()
        | Some (angled, first, last) ->
            let dest = clean_unesc_unref_span p { line with first; last } in
            let next = if angled then last + 2 else last + 1 in
            angled, Some dest, next, { line with last = last }
      in
      let lines, after_dest, title_open_delim, title, after_title, meta_last =
        match first_non_blank_over_nl ~next_line p lines line ~start with
        | None -> lines, [], '\"', None, [], meta_last
        | Some (_, _, _, st) when st = start (* need some space *) -> none ()
        | Some (lines', line', after_dest, start') ->
            let no_newline = line'.line_pos = line.line_pos in
            let title =
              Match.link_title ~next_line p.i lines' ~line:line' ~start:start'
            in
            match title with
            | None ->
                if no_newline then none () (* garbage after dest *) else
                lines, [], '\"', None, [], meta_last
            | Some (lines', line', rev_spans, last) ->
                let after_title =
                  let last = line'.last and start = last + 1 in
                  let nb = Match.first_non_blank p.i ~last ~start in
                  if nb <= line'.last
                  then None
                  else
                  Some [layout_clean_raw_span p { line' with first = start; }]
                in
                match after_title with
                | None when no_newline -> none ()
                | None -> (lines, [], '\"', None, [], meta_last)
                | Some after_title ->
                    let t = tight_block_lines p ~rev_spans in
                    lines', after_dest, p.i.[start'], Some t,
                    after_title,
                    { line' with last }
      in
      let meta = meta_of_spans p ~first:meta_first ~last:meta_last in
      let layout =
        { Link_definition.indent; angled_dest; before_dest;
          after_dest; title_open_delim; after_title }
      in
      let defined_label = def_label p label in
      let label = Some label in
      let ld =
        { Link_definition.layout; label; defined_label; dest; title;
          attributes = None }, meta
      in
      begin match defined_label with
      | None -> () | Some def -> set_label_def p def (Link_definition.Def ld)
      end;
      Some (ld, lines)
    with
    | Exit -> None

  let maybe_add_link_reference_definitions p lines prevs =
    let rec loop p prevs = function
    | [] -> prevs
    | ls ->
        match parse_link_reference_definition p ls with
        | None ->
            (* Link defs can't interrupt a paragraph so we are good now. *)
            Paragraph { maybe_ref = false; lines = List.rev ls } :: prevs
        | Some (ld, ls) -> loop p (Linkref_def ld :: prevs) ls
    in
    loop p prevs (List.rev lines)

  (* Closing blocks and finishing the document. *)

  let close_indented_code_block p lines bs =
    (* Removes trailing blank lines and add them as blank lines *)
    let rec loop blanks lines bs = match lines with
    | { pad; code; is_blank = true} :: lines ->
        loop (Blank_line (pad, code) :: blanks) lines bs
    | [] -> (* likely assert (false) *) List.rev_append blanks bs
    | ls -> List.rev_append blanks ((Code_block (`Indented ls)) :: bs)
    in
    loop [] lines bs

  let close_paragraph p par bs =
    if not par.maybe_ref then Paragraph par :: bs else
    maybe_add_link_reference_definitions p par.lines bs

  let rec close_last_block p = function
  | Code_block (`Indented ls) :: bs -> close_indented_code_block p ls bs
  | Paragraph par :: bs -> close_paragraph p par bs
  | List l :: bs -> close_list p l bs
  | Ext_def_list dl :: bs -> close_def_list p dl bs
  | Ext_footnote (i, l, blocks) :: bs -> close_footnote p i l blocks bs
  | bs -> bs

  and close_def_list p dl bs =
    let i = List.hd dl.def_items in
    let blocks = close_last_block p i.blocks in
    (* Blank-line extraction, as for lists: a trailing blank belongs after the
       list rather than inside its last definition. *)
    match blocks with
    | Blank_line _ as bl :: (_ :: _ as blocks) ->
        let def_items = { i with blocks } :: List.tl dl.def_items in
        bl :: Ext_def_list { dl with def_items } :: bs
    | blocks ->
        let def_items = { i with blocks } :: List.tl dl.def_items in
        Ext_def_list { dl with def_items } :: bs

  and close_list p l bs =
    let l = match l.roman_alt, l.list_type with
    | Some (roman_style, roman_start), `Ext_ordered (_, delim, _) ->
        { l with list_type = `Ext_ordered (roman_style, delim, roman_start);
                 roman_alt = None }
    | _ -> l
    in
    let i = List.hd l.items in
    let blocks = close_last_block p i.blocks in
    (* The final blank line extraction of the list item entails less blank
       line churn for CommonMark rendering but we don't do it on empty list
       items.  *)
    match blocks with
    | Blank_line _ as bl :: (_ :: _ as blocks) ->
        let items = { i with blocks } :: List.tl l.items in
        bl :: List { l with items } :: bs
    | blocks ->
        let items = { i with blocks } :: List.tl l.items in
        List { l with items } :: bs

  and close_footnote p indent label blocks bs =
    let blocks = close_last_block p blocks in
    (* Like for lists above we do blank line extraction (except if blocks
       is only a blank line) *)
    let blanks, blocks =
      let rec loop acc = function
      | Blank_line _ as bl :: (_ :: _ as blocks) -> loop (bl :: acc) blocks
      | blocks -> acc, blocks
      in
      loop [] blocks
    in
    List.rev_append blanks (Ext_footnote (indent, label, blocks) :: bs)

  let close_last_def_item p dl =
    let item = List.hd dl.def_items in
    let item = { item with blocks = close_last_block p item.blocks } in
    { dl with def_items = item :: List.tl dl.def_items }

  let close_last_list_item p l =
    let item = List.hd l.items in
    let item = { item with blocks = close_last_block p item.blocks } in
    { l with items = item :: List.tl l.items }

  let end_doc_close_fenced_code_block p fenced bs = match fenced.code with
  | (_, l) :: code when l.first > l.last (* empty line *) ->
      Blank_line (0, l) :: Code_block (`Fenced { fenced with code }) :: bs
  | _ -> Code_block (`Fenced fenced) :: bs

  let end_doc_close_html p h bs = match h.html with
  | l :: html when l.first > l.last (* empty line *) ->
      Blank_line (0, l) :: Html_block { end_cond = None; html } :: bs
  | _ ->
      Html_block { h with end_cond = None } :: bs

  let rec end_doc p = function
  | Ext_table (ind, rows, caption, blanks) :: bs when blanks <> [] ->
      (* The document ended while the table was still holding blank lines for a
         caption that never came: they are ordinary blank lines after it. *)
      flush_table_blanks blanks (Ext_table (ind, rows, caption, []) :: bs)
  | Block_quote (indent, marker, bq) :: bs ->
      Block_quote (indent, marker, end_doc p bq) :: bs
  | Ext_div (fence, children) :: bs ->
      (* closed by the end of document: [closing_fence] stays [None] *)
      Ext_div (fence, end_doc p children) :: bs
  | Ext_jsx_block (o, children) :: bs ->
      (* closed by the end of document: [raw_close] stays [None] *)
      Ext_jsx_block (o, end_doc p children) :: bs
  | List list :: bs -> close_list p list bs
  | Ext_def_list dl :: bs -> close_def_list p dl bs
  | Paragraph par :: bs -> close_paragraph p par bs
  | Code_block (`Indented ls) :: bs -> close_indented_code_block p ls bs
  | Code_block (`Fenced f) :: bs -> end_doc_close_fenced_code_block p f bs
  | Html_block html :: bs -> end_doc_close_html p html bs
  | Ext_footnote (i, l, blocks) :: bs -> close_footnote p i l blocks bs
  | (Thematic_break _ | Heading _ | Blank_line _ | Linkref_def _
    | Attribute_specs _ | Ext_table _ ) :: _ | [] as bs -> bs

  (* Adding lines to blocks *)

  let match_list_marker p ~last ~start =
    let djot_styles = Oymarkit_mod.djot_ordered_list_styles p.oymarkit_mod in
    Match.list_marker ~djot_styles p.i ~last ~start

  let match_html_block_start p ~last ~start =
    (* Djot has no HTML blocks: a line starting with a tag is a paragraph. *)
    if not (Oymarkit_mod.raw_html p.oymarkit_mod) then Match.Nomatch else
    Match.html_block_start p.i ~last ~start

  let match_line_type ~no_setext ~indent p =
    (* Effects on [p]'s column advance *)
    let no_setext =
      no_setext || not (Oymarkit_mod.setext_headings p.oymarkit_mod)
    in
    let djot_tb = Oymarkit_mod.djot_thematic_break p.oymarkit_mod in
    if only_blanks p then Match.Blank_line else
    if indent >= 4 && Oymarkit_mod.indented_code p.oymarkit_mod
    then Indented_code_block_line else begin
      accept_cols ~count:indent p;
      if end_of_line p then Match.Blank_line else
      let start = p.current_char and last = p.current_line_last_char in
      match p.i.[start] with
      (* Early dispatch shaves a few ms but may not be worth doing vs
         testing all the cases in sequences.  *)
      | '>' ->
          let r = match_and_accept_block_quote p in
          if r <> Nomatch then r else
          Paragraph_line
      | '=' when not no_setext ->
          let r = Match.setext_heading_underline p.i ~last ~start in
          if r <> Nomatch then r else
          Paragraph_line
      | '-' ->
          let r =
            if no_setext then Match.Nomatch else
            Match.setext_heading_underline p.i ~last ~start
          in
          if r <> Nomatch then r else
          let r = Match.thematic_break ~djot:djot_tb p.i ~last ~start in
          if r <> Nomatch then r else
          let r = match_list_marker p ~last ~start in
          if r <> Nomatch then r else
          Paragraph_line
      | '#' ->
          let closing_sequence =
            not (Oymarkit_mod.djot_headings p.oymarkit_mod)
          in
          let r = Match.atx_heading ~closing_sequence p.i ~last ~start in
          if r <> Nomatch then r else
          Paragraph_line
      | '+' | '*' | '0' .. '9' ->
          let r = Match.thematic_break ~djot:djot_tb p.i ~last ~start in
          if r <> Nomatch then r else
          let r = match_list_marker p ~last ~start in
          if r <> Nomatch then r else
          Paragraph_line
      | '(' | 'a' .. 'z' | 'A' .. 'Z'
        when Oymarkit_mod.djot_ordered_list_styles p.oymarkit_mod ->
          (* Djot's alpha/roman markers and its [(a)] form start on characters
             that CommonMark never dispatches on. A word that is not a marker
             falls through to a paragraph, which is what any other letter does
             anyway. *)
          let r = match_list_marker p ~last ~start in
          if r <> Nomatch then r else
          Paragraph_line
      | '_' ->
          (* Djot has no [_] thematic break: [___] is ordinary text. *)
          let r =
            if djot_tb then Match.Nomatch else
            Match.thematic_break p.i ~last ~start
          in
          if r <> Nomatch then r else
          Paragraph_line
      | '~' | '`' ->
          let tilde_fences = Oymarkit_mod.tilde_code_fences p.oymarkit_mod in
          let djot = Oymarkit_mod.djot_code_fences p.oymarkit_mod in
          let r =
            Match.fenced_code_block_start ~tilde_fences ~djot p.i ~last ~start
          in
          if r <> Nomatch then r else
          Paragraph_line
      | ':' when Oymarkit_mod.div p.oymarkit_mod
                 || Oymarkit_mod.djot_definition_lists p.oymarkit_mod ->
          (* A [:::] fence and a [: term] marker both start on a colon; the
             fence is tried first, and only a colon followed by a space or the
             end of the line can be a definition marker, so they never
             compete. *)
          let r =
            if Oymarkit_mod.div p.oymarkit_mod
            then Match.div_open p.i ~last ~start else Match.Nomatch
          in
          if r <> Nomatch then r else
          let r =
            if Oymarkit_mod.djot_definition_lists p.oymarkit_mod
            then Match.definition_list_marker p.i ~last ~start
            else Match.Nomatch
          in
          if r <> Nomatch then r else
          Paragraph_line
      | '<' when Oymarkit_mod.jsx_element p.oymarkit_mod ->
          (* Disambiguate a line that starts with '<'. Self-closing elements and
             inline containers stay paragraphs (inline parsing builds the node);
             an open tag alone on the line opens a JSX block. Anything that is
             not valid JSX still falls through to the untouched HTML-block path
             instead of raising. *)
          begin match Inline_struct.jsx_tag p.i ~last ~start with
          | Inline_struct.Jsx_self_closing _ -> Paragraph_line
          | Inline_struct.Jsx_open_tag { name_first; name_last; tag_end } ->
              (* Block only if the open tag is alone on the line (only trailing
                 whitespace); otherwise inline parsing handles it (container on
                 one line, or fallback to raw HTML). *)
              let after = Match.first_non_blank p.i ~last ~start:(tag_end + 1) in
              if after > last
              then Ext_jsx_block_line (name_first, name_last, tag_end)
              else Paragraph_line
          | Inline_struct.Jsx_not_tag ->
              let r = match_html_block_start p ~last ~start in
              if r <> Nomatch then r else
              Paragraph_line
          end
      | '<' ->
          let r = match_html_block_start p ~last ~start in
          if r <> Nomatch then r else
          Paragraph_line
      | '|' when p.exts ->
          let r = Match.ext_table_row p.i ~last ~start in
          if r <> Nomatch then r else
          Paragraph_line
      | '[' when p.exts ->
          let line_pos = p.current_line_pos in
          let r = Match.ext_footnote_label p.buf p.i ~line_pos ~last ~start in
          if r <> Nomatch then r else
          Paragraph_line
      | _ ->
          Paragraph_line
    end

  let rec list_marker_can_interrupt_paragraph p m =
    if not (Oymarkit_mod.list_marker_interrupts_paragraph p.oymarkit_mod)
    then false else
    list_marker_can_interrupt_paragraph_cmark p m

  and list_marker_can_interrupt_paragraph_cmark p = function
  | `Ordered (1, _), marker_last | `Unordered _, marker_last
  | `Ext_ordered (_, _, 1, _), marker_last ->
      let last = p.current_line_last_char and start = marker_last + 1 in
      let non_blank = Match.first_non_blank p.i ~last ~start in
      non_blank <= p.current_line_last_char (* line is not blank *)
  | _ -> false

  (* A djot marker whose alpha reading is ambiguous ([i.] is alpha 9 or roman 1)
     reads as roman when it opens a list or continues a roman list. *)
  let block_list_type ?open_type (m : Match.list_marker) : Block.List'.type' =
    match m with
    | `Unordered c -> `Unordered c
    | `Ordered (n, c) -> `Ordered (n, c)
    | `Ext_ordered (style, delim, start, alt) ->
        match alt, open_type with
        | Some (roman_style, roman_start),
          Some (`Ext_ordered (open_style, open_delim, _))
          when open_style = roman_style && open_delim = delim ->
            `Ext_ordered (roman_style, delim, roman_start)
        | _ -> `Ext_ordered (style, delim, start)

  let same_list_type (t0 : Block.List'.type') (t1 : Block.List'.type') =
    match t0, t1 with
    | `Ordered (_, c0), `Ordered (_, c1)
    | `Unordered c0, `Unordered c1 -> Char.equal c0 c1
    (* Djot starts a new list on a style change, so both the style and the
       delimiter must match for the marker to continue this list. *)
    | `Ext_ordered (s0, d0, _), `Ext_ordered (s1, d1, _) -> s0 = s1 && d0 = d1
    | _ -> false

  let rec add_open_blocks_with_line_class p ~indent_start ~indent bs = function
  | Match.Blank_line -> blank_line p :: bs
  | Indented_code_block_line -> indented_code_block p :: bs
  | Block_quote_line marker ->
      Block_quote (indent, marker, add_open_blocks p []) :: bs
  | Thematic_break_line last -> thematic_break p ~indent ~last :: bs
  | List_marker_line m -> list p ~indent m bs
  | Atx_heading_line (level, after_open, first_content, last_content) ->
      atx_heading p ~indent ~level ~after_open ~first_content ~last_content ::
      bs
  | Fenced_code_block_line (fence_first, fence_last, info) ->
      fenced_code_block p ~indent ~fence_first ~fence_last ~info :: bs
  | Ext_div_line (fence_first, fence_last, class_span) ->
      div_block p ~indent ~fence_first ~fence_last ~class_span :: bs
  | Ext_jsx_block_line (name_first, name_last, tag_end) ->
      jsx_block p ~indent ~name_first ~name_last ~tag_end :: bs
  | Ext_definition_line last -> def_list p ~indent ~last bs
  | Html_block_line end_cond -> html_block p ~end_cond ~indent_start :: bs
  | Paragraph_line -> paragraph p ~start:indent_start :: bs
  | Ext_table_row last -> table p ~indent ~last :: bs
  | Ext_footnote_label (rev_spans, last, key) ->
      footnote p ~indent ~last rev_spans key :: bs
  | Setext_underline_line _ | Nomatch ->
      (* This function should be called with a line type that comes out
         of match_line_type ~no_setext:true *)
      assert false

  (* A djot block attribute line is a block of its own, recognized while parsing
     rather than peeled out of a paragraph afterwards. That matters for what
     follows it: in

       {.special}
       1. one

     the list only starts if the attribute line is not part of a paragraph — a
     list marker does not interrupt a paragraph in djot. Only a specifier that
     opens and closes on this line is taken here; a multi-line one still goes
     through [split_attribute_paragraph]. *)
  and match_block_attribute p =
    if not (Oymarkit_mod.djot_block_attributes p.oymarkit_mod) then None else
    if end_of_line p then None else
    let start = p.current_char and last = p.current_line_last_char in
    if p.i.[start] <> '{' then None else
    let rec scan k depth in_quote escaped in_comment =
      if k > last then None else
      let c = p.i.[k] in
      if in_comment then scan (k + 1) depth in_quote false (c <> '%') else
      if escaped then scan (k + 1) depth in_quote false false else
      match c with
      | '\\' when in_quote -> scan (k + 1) depth in_quote true false
      | '"' -> scan (k + 1) depth (not in_quote) false false
      | '%' when not in_quote -> scan (k + 1) depth in_quote false true
      | '{' when not in_quote -> scan (k + 1) (depth + 1) in_quote false false
      | '}' when not in_quote ->
          if depth > 1 then scan (k + 1) (depth - 1) in_quote false false else
          (* Nothing but blanks may follow the specifier on the line. *)
          let after = Match.first_non_blank p.i ~last ~start:(k + 1) in
          if after <= last then None else Some k
      | _ -> scan (k + 1) depth in_quote false false
    in
    match scan (start + 1) 1 false false false with
    | None -> None
    | Some close ->
        let spec = String.sub p.i (start + 1) (close - start - 1) in
        match Attribute.of_string spec with
        | None -> None
        | Some a ->
            accept_cols p ~count:(last - p.current_char + 1);
            (* Comment-only or empty specifiers convey nothing and are dropped,
               as djot does. *)
            if Attribute.is_empty a then Some [] else Some [a]

  and add_open_blocks p bs =
    let indent_start = p.current_char and indent = current_indent p in
    match match_block_attribute p with
    | Some [] -> bs
    | Some specs -> Attribute_specs specs :: bs
    | None ->
        let ltype = match_line_type ~no_setext:true ~indent p in
        add_open_blocks_with_line_class p ~indent_start ~indent bs ltype

  and footnote p ~indent ~last rev_spans key =
    let label = Inline_struct.label_of_rev_spans p ~key rev_spans in
    let defined_label = match def_label p label with
    | None -> None
    | Some def as l -> set_label_def p def (Block.Footnote.stub label l); l
    in
    accept_cols p ~count:(last - p.current_char + 1);
    Ext_footnote (indent, (label, defined_label), add_open_blocks p [])

  and list_item ~indent p (list_type, last) =
    let before_marker = indent and marker_size = last - p.current_char + 1 in
    let marker = current_line_span p ~first:p.current_char ~last in
    let after_marker = accept_list_marker_and_indent p ~marker_size ~last in
    let ext_task_marker, _ext_task_marker_size = match p.exts with
    | false -> None, 0
    | true ->
        let start = p.current_char and last = p.current_line_last_char in
        match Match.ext_task_marker p.i ~last ~start with
        | None -> None, 0
        | Some (u, last) ->
            accept_cols p ~count:(last - start + 1);
            let last = match last = p.current_line_last_char with
            | true -> (* empty line *) last
            | false -> (* remove space for locs *) last - 1
            in
            Some (u, current_line_span p ~first:start ~last), 4
    in
    (* CommonMark's item content is what lines up with the content column (past
       the marker and the blanks after it). Djot's is anything indented past the
       *marker*, which is why

         - one
          - two

       is one item whose paragraph continues with the text [- two] rather than a
       second item: the line is inside the item, and there a marker cannot
       interrupt the open paragraph. *)
    let min =
      if Oymarkit_mod.djot_list_indent p.oymarkit_mod then indent + 1
      else indent + marker_size + after_marker
    in
    min, { before_marker; marker; after_marker; ext_task_marker;
           blocks = add_open_blocks p [] }

  and def_item ~indent p ~last =
    let before_marker = indent in
    let marker = current_line_span p ~first:p.current_char ~last in
    let after_marker = accept_list_marker_and_indent p ~marker_size:1 ~last in
    (* The rest of the marker line is the term, taken as a span: it is inline
       content, not blocks, so it is not dispatched like a list item's line. *)
    let term =
      current_line_span p ~first:p.current_char ~last:p.current_line_last_char
    in
    { before_marker; marker; after_marker; term; blocks = [] }

  and def_list ~indent p ~last bs =
    let item = def_item ~indent p ~last in
    Ext_def_list { last_blank = false; loose = false; colon_indent = indent;
                   def_indent = None; def_items = [item] } :: bs

  and list ~indent p (marker, _ as m) bs =
    let item_min_indent, item = list_item ~indent p m in
    let list_type = block_list_type marker in
    let roman_alt = match marker with
    | `Ext_ordered (_, _, _, alt) -> alt
    | `Ordered _ | `Unordered _ -> None
    in
    List { last_blank = false; loose = false;
           item_min_indent; list_type; roman_alt; items = [item] } :: bs

  let try_add_to_list ~indent p (marker, _ as m) l bs =
    let item_min_indent, item = list_item ~indent p m in
    (* An ambiguous opening marker ([i.]) is settled by a marker that can only be
       roman ([ii.]): the list, and its first item's number, become roman. *)
    let l = match l.roman_alt, marker with
    | Some (roman_style, roman_start), `Ext_ordered (style, delim, _, _)
      when style = roman_style
           && (match l.list_type with
               | `Ext_ordered (_, d, _) -> d = delim
               | _ -> false) ->
        { l with list_type = `Ext_ordered (roman_style, delim, roman_start);
                 roman_alt = None }
    | Some _, `Ext_ordered (_, _, _, None) -> { l with roman_alt = None }
    | _ -> l
    in
    let lt = block_list_type ~open_type:l.list_type marker in
    if same_list_type lt l.list_type then
      let l = close_last_list_item p l and last_blank = false in
      let list_type = l.list_type in
      List { last_blank; loose = l.last_blank; item_min_indent; list_type;
             roman_alt = l.roman_alt; items = item :: l.items } :: bs
    else
    let bs = close_list p l bs and last_blank = false in
    let roman_alt = match marker with
    | `Ext_ordered (_, _, _, alt) -> alt
    | `Ordered _ | `Unordered _ -> None
    in
    List { last_blank; loose = false; item_min_indent; list_type = lt;
           roman_alt; items = [item] } :: bs

  let try_add_to_paragraph p par bs =
    let indent_start = p.current_char and indent = current_indent p in
    match match_line_type ~no_setext:false ~indent p with
    (* These can't interrupt paragraphs *)
    | Html_block_line `End_blank_7
    | Indented_code_block_line
    | Ext_table_row _ | Ext_footnote_label _
    | Paragraph_line ->
        add_paragraph_line p ~indent_start par bs
    | List_marker_line m when not (list_marker_can_interrupt_paragraph p m) ->
        add_paragraph_line p ~indent_start par bs
    | Blank_line ->
        blank_line p :: close_paragraph p par bs
    (* Djot: nothing but a blank line ends a paragraph. A line that would open a
       block elsewhere is just more text here. Container fences ([:::], [>]) are
       matched before we get here, so they still close what they close. *)
    | _ when not (Oymarkit_mod.blocks_interrupt_paragraph p.oymarkit_mod) ->
        add_paragraph_line p ~indent_start par bs
    | Block_quote_line marker ->
        Block_quote (indent, marker, add_open_blocks p [])
        :: (close_paragraph p par bs)
    | Setext_underline_line (level, last_underline) ->
        let bs = close_paragraph p par bs in
        begin match bs with
        | Paragraph { lines; _ } :: bs ->
            setext_heading p ~indent ~level ~last_underline lines :: bs
        | bs -> paragraph p ~start:indent_start :: bs
        end
    | Thematic_break_line last ->
        thematic_break p ~indent ~last :: (close_paragraph p par bs)
    | List_marker_line m ->
        list p ~indent m (close_paragraph p par bs)
    | Atx_heading_line (level, after_open, first_content, last_content) ->
        let bs = close_paragraph p par bs in
        atx_heading p ~indent ~level ~after_open ~first_content ~last_content ::
        bs
    | Fenced_code_block_line (fence_first, fence_last, info) ->
        let bs = close_paragraph p par bs in
        fenced_code_block p ~indent ~fence_first ~fence_last ~info :: bs
    | Ext_div_line (fence_first, fence_last, class_span) ->
        let bs = close_paragraph p par bs in
        div_block p ~indent ~fence_first ~fence_last ~class_span :: bs
    | Ext_jsx_block_line (name_first, name_last, tag_end) ->
        let bs = close_paragraph p par bs in
        jsx_block p ~indent ~name_first ~name_last ~tag_end :: bs
    | Html_block_line end_cond ->
        html_block p ~end_cond ~indent_start :: (close_paragraph p par bs)
    | Ext_definition_line last ->
        def_list p ~indent ~last (close_paragraph p par bs)
    | Nomatch -> assert false

  let try_add_to_indented_code_block p ls bs =
    if current_indent p < 4 then
      if has_next_non_blank p
      then add_open_blocks p (close_indented_code_block p ls bs) else
      (* Blank but white is not data, make an empty span *)
      let first = p.current_line_last_char + 1 in
      let last = p.current_line_last_char in
      let code = current_line_span p ~first ~last in
      let l = { pad = 0; code; is_blank = true } in
      Code_block (`Indented (l :: ls)) :: bs
    else
    let pad, first = accept_code_indent p ~count:4 in
    let last = p.current_line_last_char in
    let is_blank = only_blanks p in
    let l = { pad; code = current_line_span p ~first ~last; is_blank } in
    Code_block (`Indented (l :: ls)) :: bs

  let try_add_to_fenced_code_block p f bs = match f with
  | { fence = { closing_fence = Some _; _}; _ } -> (* block is closed *)
      add_open_blocks p ((Code_block (`Fenced f)) :: bs)
  | { fence = { indent; fence; _} ; code = ls} as b ->
      let start = p.current_char and last = p.current_line_last_char in
      match Match.fenced_code_block_continue ~fence p.i ~last ~start with
      | `Code ->
          let strip = Int.min indent (current_indent p) in
          let pad, first = accept_code_indent p ~count:strip in
          let code = (pad, current_line_span p ~first ~last) :: ls in
          Code_block (`Fenced { b with code }) :: bs
      | `Close (first, _fence_last) ->
          let close = current_line_span p ~first ~last (* with layout *)in
          let fence = { b.fence with closing_fence = Some close } in
          Code_block (`Fenced { b with fence }) :: bs

  (* Djot headings run until a blank line: a following line continues the
     heading's inline content, whether or not it repeats the [#] prefix (which
     is stripped when it does). CommonMark headings are exactly one line, so
     without the knob a heading is never open and this is never reached. *)
  let try_add_to_atx_heading p (a : atx) bs =
    if only_blanks p then add_open_blocks p (Heading (`Atx a) :: bs) else
    let indent = current_indent p in
    accept_cols ~count:indent p;
    let start = p.current_char and last = p.current_line_last_char in
    let hashes = Match.run_of ~char:'#' p.i ~last ~start in
    let marker =
      (* A [#] run followed by a blank or the end of the line is a heading
         marker. Only one of the same level continues this heading: a different
         level is a heading of its own, so [## a] then [### b] is two headings
         while [# a] then [# b] is one. *)
      if hashes < start then None else
      let next = hashes + 1 in
      if next <= last && not (Ascii.is_blank p.i.[next]) then None else
      Some (hashes - start + 1, next)
    in
    match marker with
    | Some (level, _) when level <> a.level ->
        (* Close this heading and let the line open its own. *)
        add_open_blocks p (Heading (`Atx a) :: bs)
    | _ ->
        let first = match marker with
        | None -> start
        | Some (_, next) ->
            if next > last then next else
            Match.first_non_blank p.i ~last ~start:next
        in
        let line = current_line_span p ~first ~last in
        Heading (`Atx { a with more = line :: a.more }) :: bs

  let try_add_to_html_block p b bs = match b.end_cond with
  | None -> add_open_blocks p (Html_block { b with end_cond = None} :: bs)
  | Some end_cond ->
      let start = p.current_char and last = p.current_line_last_char in
      let l = current_line_span p ~first:start ~last in
      if not (Match.html_block_end p.i ~end_cond ~last ~start)
      then Html_block { b with html = l :: b.html } :: bs else
      match end_cond with
      | `End_blank | `End_blank_7 ->
          blank_line p :: Html_block { b with end_cond = None } :: bs
      | _ ->
          Html_block { end_cond = None; html = l :: b.html } :: bs

  let rec try_lazy_continuation p ~indent_start = function
  | _ when not (Oymarkit_mod.lazy_continuation p.oymarkit_mod) ->
      (* Djot has no lazy lines: a line that does not carry the container's
         marker or indentation closes it rather than continuing the paragraph
         inside it. Every lazy path goes through here, so refusing here is all
         it takes -- the caller falls back to closing the container. *)
      None
  | Paragraph par :: bs -> Some (add_paragraph_line p ~indent_start par bs)
  | Block_quote (indent, marker, bq) :: bs ->
      begin match try_lazy_continuation p ~indent_start bq with
      | None -> None
      | Some bq -> Some (Block_quote (indent, marker, bq) :: bs)
      end
  | List l :: bs ->
      let i = List.hd l.items in
      begin match try_lazy_continuation p ~indent_start i.blocks with
      | None -> None
      | Some blocks ->
          let items = { i with blocks } :: (List.tl l.items) in
          Some (List { l with items; last_blank = false } :: bs)
      end
  | _ -> None

  let try_add_to_table p ind rows caption blanks bs =
    let indent_start = p.current_char and indent = current_indent p in
    match caption with
    | Some c ->
        (* A caption runs to the blank line that ends it; its continuation lines
           need no indent. *)
        if only_blanks p then begin
          let bs = Ext_table (ind, rows, caption, []) :: bs in
          let ltype = match_line_type ~indent ~no_setext:true p in
          add_open_blocks_with_line_class p ~indent ~indent_start bs ltype
        end else begin
          accept_cols ~count:indent p;
          let line =
            current_line_span p ~first:p.current_char
              ~last:p.current_line_last_char
          in
          let c = { c with caption_lines = line :: c.caption_lines } in
          Ext_table (ind, rows, Some c, []) :: bs
        end
    | None ->
        if only_blanks p then
          (* Hold the blank line: a caption may still follow it. *)
          let first = p.current_char and last = p.current_line_last_char in
          let blank = current_line_span p ~first ~last in
          Ext_table (ind, rows, None, blank :: blanks) :: bs
        else
        match match_table_caption p ~indent with
        | Some c -> Ext_table (ind, rows, Some c, []) :: bs
        | None ->
            (* No caption: the held blank lines belong after the table. *)
            match match_line_type ~indent ~no_setext:true p with
            | Ext_table_row last when blanks = [] ->
                let row = table_row p ~first:p.current_char ~last in
                Ext_table (ind, row :: rows, None, []) :: bs
            | ltype ->
                let bs = Ext_table (ind, rows, None, []) :: bs in
                let bs = flush_table_blanks blanks bs in
                add_open_blocks_with_line_class p ~indent ~indent_start bs ltype

  let rec try_add_to_block_quote p indent_layout bq marker bs =
    let indent_start = p.current_char and indent = current_indent p in
    match match_line_type ~indent ~no_setext:true p with
    | Block_quote_line _ ->
        Block_quote (indent_layout, marker, add_line p bq) :: bs
    | (Indented_code_block_line (* Looks like a *) | Paragraph_line) as ltype ->
        begin match try_lazy_continuation p ~indent_start bq with
        | Some bq -> Block_quote (indent_layout, marker, bq) :: bs
        | None ->
            let bs =
              Block_quote (indent_layout, marker, close_last_block p bq) :: bs
            in
            add_open_blocks_with_line_class p ~indent ~indent_start bs ltype
        end
    | ltype ->
        let bs =
          Block_quote (indent_layout, marker, close_last_block p bq) :: bs
        in
        add_open_blocks_with_line_class p ~indent ~indent_start bs ltype

  and try_add_to_footnote p fn_indent label blocks bs =
    let indent_start = p.current_char and indent = current_indent p in
    if indent < fn_indent + 1 (* position of ^ *) then begin
      match match_line_type ~indent ~no_setext:true p with
      | (Indented_code_block_line (* Looks like a *) | Paragraph_line) as lt ->
          begin match try_lazy_continuation p ~indent_start blocks with
          | Some blocks -> Ext_footnote (fn_indent, label, blocks) :: bs
          | None ->
              let blocks = close_last_block p blocks in
              let bs = (close_footnote p fn_indent label blocks) bs in
              add_open_blocks_with_line_class p ~indent ~indent_start bs lt
          end
      | Blank_line ->
          Ext_footnote (fn_indent, label, add_line p blocks) :: bs
      | ltype ->
          let blocks = close_last_block p blocks in
          let bs = close_footnote p fn_indent label blocks bs in
          add_open_blocks_with_line_class p ~indent ~indent_start bs ltype
    end else begin
      accept_cols p ~count:(fn_indent + 1);
      Ext_footnote (fn_indent, label, add_line p blocks) :: bs
    end

  and try_add_to_list_item p list bs =
    let indent_start = p.current_char and indent = current_indent p in
    if indent >= list.item_min_indent then begin
      let last_blank = only_blanks p in
      let item = List.hd list.items and items = List.tl list.items in
      if list.last_blank && not last_blank &&
         List.for_all block_is_blank_line item.blocks
      then
         (* Item can only start with a single blank line, if we are
            here it's not a new item so the list ends *)
        add_open_blocks p (List list :: bs)
      else begin
        accept_cols ~count:list.item_min_indent p;
        let item = { item with blocks = add_line p item.blocks } in
        List { list with items = item :: items; last_blank } :: bs
      end
    end else match match_line_type ~indent ~no_setext:true p with
    | Blank_line ->
        let item = List.hd list.items and items = List.tl list.items in
        let item = { item with blocks = add_line p item.blocks } in
        List { list with items = item :: items; last_blank = true } :: bs
    | Indented_code_block_line | Paragraph_line as ltype  ->
        let item = List.hd list.items and items = List.tl list.items in
        begin match try_lazy_continuation p ~indent_start item.blocks with
        | Some blocks ->
            let items = { item with blocks } :: items in
            List { list with items; last_blank = false } :: bs
        | None ->
            let bs = close_list p list bs in
            add_open_blocks_with_line_class p ~indent ~indent_start bs ltype
        end
    | List_marker_line m ->
        try_add_to_list p ~indent m list bs
    | ltype ->
        let bs = close_list p list bs in
        add_open_blocks_with_line_class p ~indent ~indent_start bs ltype

  (* Djot definition lists.

     A [: term] line opens an item whose definition is the blocks indented under
     it. Unlike a list item, the definition's indent is not fixed by the marker
     ([:] plus one space would be 2, but djot lets the definition sit at any
     indent past the colon), so it is taken from the first line of the definition
     and every later line must reach it. *)
  and try_add_to_def_list p dl bs =
    let indent_start = p.current_char and indent = current_indent p in
    if only_blanks p then begin
      let item = List.hd dl.def_items in
      let item = { item with blocks = add_line p item.blocks } in
      let def_items = item :: List.tl dl.def_items in
      Ext_def_list { dl with last_blank = true; def_items } :: bs
    end else
    let in_definition = match dl.def_indent with
    | Some def_indent -> indent >= def_indent
    | None -> indent > dl.colon_indent
    in
    if in_definition then begin
      let def_indent = match dl.def_indent with
      | Some def_indent -> def_indent
      | None -> indent (* the first definition line fixes it *)
      in
      accept_cols ~count:def_indent p;
      let item = List.hd dl.def_items in
      let item = { item with blocks = add_line p item.blocks } in
      let def_items = item :: List.tl dl.def_items in
      Ext_def_list { dl with def_indent = Some def_indent; last_blank = false;
                     def_items } :: bs
    end else
    match match_line_type ~indent ~no_setext:true p with
    | Ext_definition_line last ->
        (* Another term: a new item of the same list. A blank line before it
           makes the list loose, exactly as for list items. *)
        let dl = close_last_def_item p dl in
        let item = def_item ~indent p ~last in
        Ext_def_list { loose = dl.loose || dl.last_blank; last_blank = false;
                       colon_indent = indent; def_indent = None;
                       def_items = item :: dl.def_items } :: bs
    | ltype ->
        let bs = close_def_list p dl bs in
        add_open_blocks_with_line_class p ~indent ~indent_start bs ltype

  (* Oymarkit djot divs.

     A div has no per-line marker; its content is block-level and runs until a
     closing fence ([:::] at least as long as the opening) or a boundary. Divs
     nest, and a closing fence closes the {e innermost} open div it is long
     enough for, so [try_close_inner_div] walks the open-div spine inside-out
     before [try_add_to_div] considers closing the div at hand. *)
  and try_close_inner_div p ~line_len ~close = function
  | Ext_div (f, ch) :: bs when f.closing_fence = None ->
      begin match try_close_inner_div p ~line_len ~close ch with
      | Some ch -> Some (Ext_div (f, ch) :: bs)
      | None ->
          if line_len >= f.fence_len
          then Some (Ext_div ({ f with closing_fence = Some (close ()) }, ch):: bs)
          else None
      end
  | _ -> None

  (* Is the innermost still-open block on this spine an open fenced code block?
     A fenced code block captures every line until its own closing fence, so
     while one is open inside a div a [:::] line is code content, not a div
     close -- the fence must see the line first. We walk down through open div /
     jsx containers (only their head child can be open) to that innermost leaf. *)
  and innermost_open_is_fenced_code = function
  | Code_block (`Fenced { fence = { closing_fence = None; _ }; _ }) :: _ -> true
  | Ext_div ({ closing_fence = None; _ }, ch) :: _ ->
      innermost_open_is_fenced_code ch
  | Ext_jsx_block ({ raw_close = None; _ }, ch) :: _ ->
      innermost_open_is_fenced_code ch
  | _ -> false

  and try_add_to_div p fence children bs =
    match fence.closing_fence with
    | Some _ -> (* closed: this line starts a new sibling block *)
        add_open_blocks p (Ext_div (fence, children) :: bs)
    | None ->
        let start = p.current_char and last = p.current_line_last_char in
        match Match.div_close p.i ~last ~start with
        | Some line_len when not (innermost_open_is_fenced_code children) ->
            let close () =
              let first = Match.first_non_blank p.i ~last ~start in
              current_line_span p ~first ~last
            in
            begin match try_close_inner_div p ~line_len ~close children with
            | Some children -> Ext_div (fence, children) :: bs
            | None ->
                if line_len >= fence.fence_len
                then Ext_div ({ fence with closing_fence = Some (close ()) },
                              children) :: bs
                else Ext_div (fence, add_line p children) :: bs
            end
        | Some _ | None -> Ext_div (fence, add_line p children) :: bs

  (* Oymarkit JSX block containers.

     Modeled on the djot div machinery: a JSX block has no per-line marker; its
     content is block-level and runs until a matching close tag [ </name> ] on
     its own line or a boundary. They nest, and a close tag closes the {e
     innermost} open block of that name, so [try_close_inner_jsx] walks the open
     spine inside-out before [try_add_to_jsx_block] considers closing the block
     at hand. *)
  and jsx_close_line p ~start ~last =
    (* If the current line (from its first non-blank at [start]) is a valid JSX
       close tag alone on the line, return its name and a thunk building the
       closing-tag span; else [None]. *)
    match Inline_struct.jsx_close_tag p.i ~last ~start with
    | Some (name_first, name_last, tag_end) ->
        let after = Match.first_non_blank p.i ~last ~start:(tag_end + 1) in
        if after <= last then None else
        let name =
          if name_last < name_first then ""
          else String.sub p.i name_first (name_last - name_first + 1)
        in
        let close () = current_line_span p ~first:start ~last:tag_end in
        Some (name, close)
    | None -> None

  and try_close_inner_jsx p ~name ~close = function
  | Ext_jsx_block (o, ch) :: bs when o.raw_close = None ->
      begin match try_close_inner_jsx p ~name ~close ch with
      | Some ch -> Some (Ext_jsx_block (o, ch) :: bs)
      | None ->
          if o.name = name
          then Some (Ext_jsx_block ({ o with raw_close = Some (close ()) }, ch) :: bs)
          else None
      end
  | _ -> None

  and try_add_to_jsx_block p o children bs =
    match o.raw_close with
    | Some _ -> (* closed: this line starts a new sibling block *)
        add_open_blocks p (Ext_jsx_block (o, children) :: bs)
    | None ->
        let start0 = p.current_char and last = p.current_line_last_char in
        let start = Match.first_non_blank p.i ~last ~start:start0 in
        match jsx_close_line p ~start ~last with
        | Some (cname, close) ->
            begin match try_close_inner_jsx p ~name:cname ~close children with
            | Some children -> Ext_jsx_block (o, children) :: bs
            | None ->
                if o.name = cname
                then Ext_jsx_block ({ o with raw_close = Some (close ()) },
                                    children) :: bs
                else Ext_jsx_block (o, add_line p children) :: bs
            end
        | None -> Ext_jsx_block (o, add_line p children) :: bs

  and add_line p = function
  | Paragraph par :: bs -> try_add_to_paragraph p par bs
  | Heading (`Atx a) :: bs when Oymarkit_mod.djot_headings p.oymarkit_mod ->
      try_add_to_atx_heading p a bs
  | ((Thematic_break _ | Heading _ | Blank_line _ | Linkref_def _
      | Attribute_specs _) :: _)
  | [] as bs -> add_open_blocks p bs
  | List list :: bs -> try_add_to_list_item p list bs
  | Code_block (`Indented ls) :: bs -> try_add_to_indented_code_block p ls bs
  | Code_block (`Fenced f) :: bs -> try_add_to_fenced_code_block p f bs
  | Block_quote (ind, marker, bq) :: bs -> try_add_to_block_quote p ind bq marker bs
  | Ext_div (fence, children) :: bs -> try_add_to_div p fence children bs
  | Ext_jsx_block (o, children) :: bs -> try_add_to_jsx_block p o children bs
  | Html_block html :: bs -> try_add_to_html_block p html bs
  | Ext_table (ind, rows, caption, blanks) :: bs ->
      try_add_to_table p ind rows caption blanks bs
  | Ext_footnote (i, l, blocks) :: bs -> try_add_to_footnote p i l blocks bs
  | Ext_def_list dl :: bs -> try_add_to_def_list p dl bs

  (* Parsing *)

  let get_first_line p =
    let max = String.length p.i - 1 in
    let k = ref 0 in
    let last_char =
      while !k <= max && p.i.[!k] <> '\n' && p.i.[!k] <> '\r' do incr k done;
      !k - 1 (* if the line is empty we have -1 *)
    in
    p.current_line_last_char <- last_char;
    update_next_non_blank p;
    (* Return first used newline (or "\n" if there is none) *)
    if !k > max || p.i.[!k] = '\n' then "\n" else
    let next = !k + 1 in
    if next <= max && p.i.[next] = '\n' then "\r\n" else "\r"

  let get_next_line p =
    let max = String.length p.i - 1 in
    if p.current_line_last_char = max then false else
    let first_char =
      let nl = p.current_line_last_char + 1 in
      if p.i.[nl] = '\n' then nl + 1 else (* assert (p.i.[nl] = '\r') *)
      let next = nl + 1 in
      if next <= max && p.i.[next] = '\n' then next + 1 else next
    in
    let last_char =
      let k = ref first_char in
      while !k <= max && p.i.[!k] <> '\n' && p.i.[!k] <> '\r' do incr k done;
      !k - 1 (* if the line is empty we have last_char = first_char - 1 *)
    in
    p.current_line_pos <- (fst p.current_line_pos + 1), first_char;
    p.current_line_last_char <- last_char;
    p.current_char <- first_char;
    p.current_char_col <- 0;
    p.tab_consumed_cols <- 0;
    update_next_non_blank p;
    true

  let parse p =
    let meta p =
      let first_byte = 0 and last_byte = p.current_line_last_char in
      let first_line = 1, first_byte and last_line = p.current_line_pos in
      let file = p.file in
      meta p (Textloc.v ~file ~first_byte ~last_byte ~first_line ~last_line)
    in
    let rec loop p bs =
      let bs = add_line p bs in
      if get_next_line p then loop p bs else (end_doc p bs), meta p
    in
    let nl = get_first_line p in
    nl, loop p []
end

(* Building the final AST, invokes inline parsing. *)

let block_struct_to_blank_line p pad span =
  Block.Blank_line (clean_raw_span p ~pad span)

let block_struct_to_code_block p = function
| `Indented (ls : Block_struct.indented_code_line list) (* non-empty *) ->
    let line p { Block_struct.pad; code; _} = clean_raw_span ~pad p code in
    let layout = `Indented and info_string = None in
    let last = (List.hd ls).code in
    let code = List.rev_map (line p) ls in
    let meta =
      let last_line = last.line_pos and last_byte = last.last in
      let start = Meta.textloc (snd (List.hd code)) in
      meta p (Textloc.set_last start ~last_byte ~last_line)
    in
    Block.Code_block ({layout; info_string; code}, meta)
| `Fenced { Block_struct.fence; code = ls } ->
    let layout =
      let opening_fence = layout_clean_raw_span p fence.opening_fence in
      let closing_fence =
        Option.map (layout_clean_raw_span p) fence.closing_fence
      in
      { Block.Code_block.indent = fence.indent; opening_fence; closing_fence }
    in
    let info_string = Option.map (clean_unesc_unref_span p) fence.info_string in
    let code = List.rev_map (fun (pad, l) -> clean_raw_span p ~pad l) ls in
    let meta =
      let first = fence.opening_fence in
      let last = match fence.closing_fence with
      | Some last -> last
      | None -> match ls with [] -> first | (_, last_line) :: _ -> last_line
      in
      meta_of_spans p ~first ~last
    in
    let cb = {Block.Code_block.layout = `Fenced layout; info_string; code} in
    let raw_format =
      if not (Oymarkit_mod.djot_raw p.oymarkit_mod) then None else
      Block.Code_block.raw_format_of_info_string info_string
    in
    match raw_format with
    | Some format ->
        Block.Ext_raw_block (Block.Raw_block.make ~format cb, meta)
    | None ->
        if p.exts && Block.Code_block.is_math_block info_string
        then Block.Ext_math_block (cb, meta)
        else Block.Code_block (cb, meta)

(* A heading's auto identifier. Djot's is case-preserving ([djot_id_base]) and
   ignores footnote references; CommonMark's is [Inline.id]. Both the id put on
   the heading (or its section) and the target [register_heading_labels]
   registers must use this, so a [ [Heading][] ] link resolves to a live anchor. *)
let heading_auto_id p inline =
  if Oymarkit_mod.djot_headings p.oymarkit_mod then
    let text =
      Inline.to_plain_text ~skip_link:Inline.is_footnote_reference
        ~break_on_soft:false inline
      |> List.map (String.concat "") |> String.concat " "
    in
    Match.djot_id_base text
  else Inline.id ~buf:p.buf inline

let block_struct_to_heading p = function
| `Atx { Block_struct.indent; level; after_open; heading; layout_after; more } ->
    let after_opening =
      let first = after_open and last = heading.first - 1 in
      layout_clean_raw_span' p { heading with first; last }
    in
    let closing = layout_clean_raw_span' p layout_after in
    let layout = `Atx { Block.Heading.indent; after_opening; closing } in
    let meta =
      meta p (textloc_of_span p { heading with first = after_open - level })
    in
    (* [Inline_struct.parse] takes its lines with the last one at the head, and
       [more] is already in that order. An empty [#] line contributes no content:
       dropping it keeps the heading's text from starting with a newline. *)
    let lines = match more with
    | _ :: _ when heading.first > heading.last -> more
    | _ -> more @ [heading]
    in
    let _layout, inline = Inline_struct.parse p lines in
    let id = match p.heading_auto_ids with
    | false -> None
    | true -> Some (`Auto (heading_auto_id p inline))
    in
    Block.Heading ({layout; level; inline; id}, meta)
| `Setext { Block_struct.level; heading_lines; underline } ->
    let (leading_indent, trailing_blanks), inline =
      Inline_struct.parse p heading_lines
    in
    let underline_indent, u, blanks = underline in
    let underline_blanks = layout_clean_raw_span' p blanks in
    let underline_count = u.last - u.first + 1, meta p (textloc_of_span p u) in
    let layout =
      { Block.Heading.leading_indent; trailing_blanks; underline_indent;
        underline_count; underline_blanks }
    in
    let meta =
      let last_line = u.line_pos and last_byte = u.last in
      let start = Meta.textloc (Inline.meta inline) in
      meta p (Textloc.set_last start ~last_byte ~last_line)
    in
    let id = match p.heading_auto_ids with
    | false -> None
    | true -> Some (`Auto (heading_auto_id p inline))
    in
    Block.Heading ({ layout = `Setext layout; level; inline; id }, meta)

let block_struct_to_html_block p (b : Block_struct.html_block) =
  let last = List.hd b.html in
  let last_byte = last.last and last_line = last.line_pos in
  let lines = List.rev_map (clean_raw_span p) b.html in
  let start_loc = Meta.textloc (snd (List.hd lines)) in
  let meta = meta p (Textloc.set_last start_loc ~last_byte ~last_line) in
  Block.Html_block (lines, meta)

let paragraph_block_id p (par : Block_struct.paragraph) =
  if not (Oymarkit_mod.block_id p.oymarkit_mod) then None else
  let line = List.hd par.lines in
  let first = line.first in
  let last = Match.last_non_blank p.i ~first ~start:line.last in
  let rec identifier_start k =
    if k < first then first else
    match p.i.[k] with
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' ->
        identifier_start (k - 1)
    | _ -> k + 1
  in
  let id_first = identifier_start last in
  let caret = id_first - 1 in
  let valid_first =
    id_first <= last &&
    match p.i.[id_first] with
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
    | _ -> false
  in
  let rec preceding_backslashes k count =
    if k >= first && p.i.[k] = '\\'
    then preceding_backslashes (k - 1) (count + 1)
    else count
  in
  if caret < first || p.i.[caret] <> '^' || not valid_first ||
     preceding_backslashes (caret - 1) 0 mod 2 <> 0
  then None else
  let id_string = String.sub p.i id_first (last - id_first + 1) in
  let span = { line with first = caret; last } in
  Some Block.Block_id.
    { id = id_string; marker = meta p (textloc_of_span p span) }

let block_struct_to_paragraph p par =
  (* Oymarkit begin *)
  (* In extension mode also segment the paragraph on structural colons and ship
     the segments to the Struct pass on the meta (see [Inline_struct]). *)
  let layout, inline, segments =
    if p.exts then Inline_struct.parse_with_segments p par.Block_struct.lines
    else let layout, inline = Inline_struct.parse p par.Block_struct.lines in
      layout, inline, None
  in
  (* Oymarkit end *)
  let leading_indent, trailing_blanks = layout in
  let meta =
    match paragraph_block_id p par with
    | None -> Inline.meta inline
    | Some block_id -> Block.Block_id.add block_id (Inline.meta inline)
  in
  (* Oymarkit begin *)
  let meta = match segments with
  | None -> meta
  | Some segments -> Meta.add Inline_struct.keyed_segments segments meta
  in
  (* Oymarkit end *)
  Block.Paragraph ({ leading_indent; inline; trailing_blanks }, meta)

let split_attribute_paragraph p (par : Block_struct.paragraph) =
  if not (Oymarkit_mod.djot_block_attributes p.oymarkit_mod)
  then [Block_struct.Paragraph par] else
  let lines = List.rev par.lines in
  let parse_spec lines =
    let b = Buffer.create 32 in
    let rec scan line lines k in_quote escaped in_comment =
      if k > line.last then
        match lines with
        | [] -> None
        | next_line :: lines ->
            let first = first_non_blank_in_span p next_line in
            if first <= next_line.first then None else begin
              Buffer.add_char b '\n';
              scan next_line lines first in_quote false in_comment
            end
      else
        let c = p.i.[k] in
        if in_comment then begin
          Buffer.add_char b c;
          scan line lines (k + 1) in_quote false (c <> '%')
        end else if escaped then begin
          Buffer.add_char b c;
          scan line lines (k + 1) in_quote false false
        end else
        match c with
        | '\\' when in_quote ->
            Buffer.add_char b c;
            scan line lines (k + 1) in_quote true false
        | '"' ->
            Buffer.add_char b c;
            scan line lines (k + 1) (not in_quote) false false
        | '%' when not in_quote ->
            Buffer.add_char b c;
            scan line lines (k + 1) false false true
        | '}' when not in_quote ->
            let after = Match.first_non_blank p.i ~last:line.last ~start:(k + 1) in
            if after <= line.last then None else
            begin match Attribute.of_string (Buffer.contents b) with
            | None -> None
            | Some spec -> Some (spec, lines)
            end
        | c ->
            Buffer.add_char b c;
            scan line lines (k + 1) in_quote false false
    in
    match lines with
    | [] -> None
    | line :: lines ->
        let first = first_non_blank_in_span p line in
        if first > line.last || p.i.[first] <> '{' then None else
        scan line lines (first + 1) false false false
  in
  let rec take acc = function
  | _ :: _ as lines ->
      begin match parse_spec lines with
      | Some (spec, lines) -> take (spec :: acc) lines
      | None -> List.rev acc, lines
      end
  | [] -> List.rev acc, []
  in
  match take [] lines with
  | [], _ -> [Block_struct.Paragraph par]
  | specs, lines ->
      (* Comment-only (or empty) specifiers are dropped, per Djot. A block
         made up solely of them disappears entirely. *)
      let specs = List.filter (fun s -> not (Attribute.is_empty s)) specs in
      match specs, lines with
      | [], [] -> []
      | [], lines -> [Block_struct.Paragraph { par with lines = List.rev lines }]
      | specs, [] -> [Block_struct.Attribute_specs specs]
      | specs, lines ->
          (* The lines under the specifier were never offered to link reference
             definition parsing: the paragraph they were part of started with
             the '{' of the specifier, so it did not even look like one. Now that
             the specifier is peeled off, they can be — which is what makes
             [ {.cls}\n[label]: url ] a definition rather than a paragraph.

             Djot merges such attributes onto every link referencing the
             definition, so they also go onto the definition in [p.defs] here.
             This is the right moment: inline parsing, where a link looks its
             definition up, happens later. *)
          let rest =
            Block_struct.maybe_add_link_reference_definitions p
              (List.rev lines) []
          in
          let attach_attributes () =
            let attrs = List.fold_left Attribute.merge Attribute.empty specs in
            let attach (ld, _) = match Link_definition.defined_label ld with
            | None -> ()
            | Some l ->
                let key = Label.key l in
                match Label.Map.find_opt key p.defs with
                | Some (Link_definition.Def (d, m)) ->
                    let d = Link_definition.with_attributes (Some attrs) d in
                    p.defs <- Label.Map.add key (Link_definition.Def (d, m)) p.defs
                | _ -> ()
            in
            (* [rest] is in reverse document order: the first definition under
               the specifier is the last one here. *)
            match List.rev rest with
            | Block_struct.Linkref_def ld :: _ -> attach ld
            | _ -> ()
          in
          attach_attributes ();
          Block_struct.Attribute_specs specs :: List.rev rest

let rec prepare_block_struct p = function
| Block_struct.Block_quote (indent, marker, bs) ->
    [Block_struct.Block_quote (indent, marker, prepare_block_structs p bs)]
| Block_struct.Ext_div (fence, bs) ->
    [Block_struct.Ext_div (fence, prepare_block_structs p bs)]
| Block_struct.Ext_jsx_block (o, bs) ->
    [Block_struct.Ext_jsx_block (o, prepare_block_structs p bs)]
| Block_struct.List l ->
    let item (i : Block_struct.list_item) =
      { i with blocks = prepare_block_structs p i.blocks }
    in
    [Block_struct.List { l with items = List.map item l.items }]
| Block_struct.Ext_footnote (indent, labels, bs) ->
    [Block_struct.Ext_footnote (indent, labels, prepare_block_structs p bs)]
| Block_struct.Paragraph par -> split_attribute_paragraph p par
| b -> [b]

and prepare_block_structs p bs =
  bs |> List.rev |> List.map (prepare_block_struct p) |> List.flatten |> List.rev

let block_struct_to_thematic_break p indent span =
  let layout, meta = (* not layout because of loc *) clean_raw_span p span in
  Block.Thematic_break ({ indent; layout }, meta)

(* A table separator row, read from the source rather than from the parsed cells.

   [Block.Table.parse_sep_row] inspects the cells' inlines, which only works if
   nothing rewrote them first: with smart punctuation on, the [---] of [|---:|]
   has already become an em dash by then, and the row is silently taken for data.
   The source always says what the row is. *)
let raw_sep_row p (row : line_span) =
  let s = p.i in
  let cell first last (* inclusive, may be empty *) =
    let first = Match.first_non_blank s ~last ~start:first in
    let last = Match.last_non_blank s ~first ~start:last in
    if first > last then None else
    let first_colon = s.[first] = ':' and last_colon = s.[last] = ':' in
    let d_first = if first_colon then first + 1 else first in
    let d_last = if last_colon then last - 1 else last in
    if d_first > d_last then None else
    let rec dashes k =
      if k > d_last then true else if s.[k] <> '-' then false else dashes (k + 1)
    in
    if not (dashes d_first) then None else
    let align = match first_colon, last_colon with
    | false, false -> None
    | true, true -> Some `Center
    | true, false -> Some `Left
    | false, true -> Some `Right
    in
    let count = d_last - d_first + 1 in
    let meta = meta p (textloc_of_span p { row with first; last }) in
    Some ((align, count), meta)
  in
  let rec loop acc first k =
    if k > row.last then (if first > row.last then Some (List.rev acc) else None)
    else if s.[k] = '|' && (k = row.first || s.[k - 1] <> '\\') then
      match cell first (k - 1) with
      | None -> None
      | Some sep -> loop (sep :: acc) (k + 1) (k + 1)
    else loop acc first (k + 1)
  in
  if row.first > row.last then None else
  match loop [] row.first row.first with
  | Some (_ :: _ as seps) -> Some seps
  | Some [] | None -> None

let block_struct_to_table p indent rows caption =
  let rec loop p col_count last_was_sep acc = function
  | (row, blanks) :: rs ->
      let meta = meta p (textloc_of_span p row) in
      let row' = { row with first = row.first + 1; last = row.last } in
      let cols = Inline_struct.parse_table_row p row' in
      let col_count = Int.max col_count (List.length cols) in
      let r, last_was_sep = match raw_sep_row p row' with
      | Some seps -> ((`Sep seps), meta), true
      | None ->
          ((if last_was_sep then `Header cols else `Data cols), meta), false
      in
      let acc = (r, layout_clean_raw_span' p blanks) :: acc in
      if rs = [] then row, col_count, acc else
      loop p col_count last_was_sep acc rs
  | [] -> assert false
  in
  let last = fst (List.hd rows) in
  let first, col_count, rows = loop p 0 false [] rows in
  let meta = meta_of_spans p ~first ~last in
  let caption = match caption with
  | None -> None
  | Some { Block_struct.caption_indent; caption_lines } ->
      let cmeta =
        let first = List.nth caption_lines (List.length caption_lines - 1) in
        meta_of_spans p ~first ~last:(List.hd caption_lines)
      in
      let _layout, inline = Inline_struct.parse p caption_lines in
      Some ({ Block.Table.caption_indent; inline }, cmeta)
  in
  Block.Ext_table ({ indent; col_count; rows; caption }, meta)

let rec block_struct_to_block_quote p indent marker bs =
  let add_block p acc b = block_struct_to_block p b :: acc in
  let last = block_struct_to_block p (List.hd bs) in
  let block = List.fold_left (add_block p) [last] (List.tl bs) in
  let block = match block with
  | [b] -> b
  | quote ->
      let first = Block.meta (List.hd quote) and last = Block.meta last in
      Block.Blocks (quote, meta_of_metas p ~first ~last)
  in
  let meta =
    let marker_loc = textloc_of_span p marker in
    let first_meta = meta p marker_loc in
    meta_of_metas p ~first:first_meta ~last:(Block.meta block)
  in
  let meta =
    match Block.Callout.detect (Oymarkit_mod.callout p.oymarkit_mod) block with
    | None -> meta
    | Some callout -> Block.Callout.add callout meta
  in
  Block.Block_quote ({indent; block}, meta)

and block_struct_to_div p (fence : Block_struct.div_fence) bs =
  let block = match bs with
  | [] -> Block.empty
  | last :: rest ->
      let last = block_struct_to_block p last in
      let blocks =
        List.fold_left (fun acc b -> block_struct_to_block p b :: acc) [last] rest
      in
      begin match blocks with
      | [b] -> b
      | blocks ->
          let first = Block.meta (List.hd blocks) and last = Block.meta last in
          Block.Blocks (blocks, meta_of_metas p ~first ~last)
      end
  in
  let opening_fence = layout_clean_raw_span p fence.opening_fence in
  let class' = Option.map (clean_raw_span p) fence.class' in
  let closing_fence = Option.map (layout_clean_raw_span p) fence.closing_fence in
  let meta =
    let first = meta p (textloc_of_span p fence.opening_fence) in
    let last = match fence.closing_fence with
    | Some cf -> meta p (textloc_of_span p cf)
    | None -> Block.meta block
    in
    meta_of_metas p ~first ~last
  in
  let div =
    { Block.Div.indent = fence.indent; opening_fence; class'; closing_fence;
      block }
  in
  Block.Ext_div (div, meta)

and block_struct_to_jsx_block p (o : Block_struct.jsx_open) bs =
  let block = match bs with
  | [] -> Block.empty
  | last :: rest ->
      let last = block_struct_to_block p last in
      let blocks =
        List.fold_left (fun acc b -> block_struct_to_block p b :: acc) [last] rest
      in
      begin match blocks with
      | [b] -> b
      | blocks ->
          let first = Block.meta (List.hd blocks) and last = Block.meta last in
          Block.Blocks (blocks, meta_of_metas p ~first ~last)
      end
  in
  let raw_open = clean_raw_span p o.raw_open in
  let raw_close = Option.map (clean_raw_span p) o.raw_close in
  let meta =
    let first = meta p (textloc_of_span p o.raw_open) in
    let last = match o.raw_close with
    | Some c -> meta p (textloc_of_span p c)
    | None -> Block.meta block
    in
    meta_of_metas p ~first ~last
  in
  let jsx = Block.Jsx_block.make ~indent:o.indent ~raw_open ?raw_close block in
  Block.Ext_jsx_block (jsx, meta)

and block_struct_to_footnote_definition p indent (label, defined_label) bs =
  let add_block p acc b = block_struct_to_block p b :: acc in
  let last = block_struct_to_block p (List.hd bs) in
  let block = List.fold_left (add_block p) [last] (List.tl bs) in
  let last = Block.meta last in
  let block = match block with
  | [b] -> b
  | bs ->
      let first = Block.meta (List.hd bs) in
      Block.Blocks (bs, meta_of_metas p ~first ~last)
  in
  let loc =
    let labelloc = Label.textloc label in
    let lastloc = Meta.textloc last in
    let loc = Textloc.span labelloc lastloc in
    let first_byte = Textloc.first_byte loc - 1 in
    Textloc.set_first loc ~first_byte ~first_line:(Textloc.first_line loc)
  in
  let fn = { Block.Footnote.indent; label; defined_label; block }, meta p loc in
  begin match defined_label with
  | None -> () | Some def -> set_label_def p def (Block.Footnote.Def fn)
  end;
  Block.Ext_footnote_definition fn

and block_struct_to_list_item p (i : Block_struct.list_item) =
  let djot_tight = Oymarkit_mod.djot_list_tightness p.oymarkit_mod in
  let rec loop bstate tight acc = function
  | Block_struct.Blank_line _ as bl :: bs ->
      let bstate = if bstate = `Trail_blank then `Trail_blank else `Blank in
      loop bstate tight (block_struct_to_block p bl :: acc) bs
  | Block_struct.List
      { items = { blocks = Block_struct.Blank_line _ :: _ } :: _ } as l :: bs
    when not djot_tight ->
      loop bstate false (block_struct_to_block p l :: acc) bs
  | b :: bs ->
      let tight = tight && not (bstate = `Blank)  in
      loop `Non_blank tight (block_struct_to_block p b :: acc) bs
  | [] -> tight, acc
  in
  let last_meta, (tight, blocks) = match i.blocks with
  | [Block_struct.Blank_line _ as blank] ->
      let bl = block_struct_to_block p blank in
      Block.meta bl, (true, [bl])
  | Block_struct.Blank_line _ as blank :: bs ->
      let bl = block_struct_to_block p blank in
      (Block.meta bl), loop `Trail_blank true [bl] bs
  | b :: bs ->
      let b = block_struct_to_block p b in
      (Block.meta b), loop `Non_blank true [b] bs
  | [] -> assert false
  in
  let block = match blocks with
  | [i] -> i
  | is ->
      let first = Block.meta (List.hd is) in
      Block.Blocks (is, meta_of_metas p ~first ~last:last_meta)
  in
  let before_marker = i.before_marker and after_marker = i.after_marker in
  let marker = (* not layout to get loc *) clean_raw_span p i.marker in
  let ext_task_marker = match i.ext_task_marker with
  | None -> None
  | Some (u, span) -> Some (u, meta p (textloc_of_span p span))
  in
  let meta = meta_of_metas p ~first:(snd marker) ~last:last_meta in
  let i =
    { Block.List_item.before_marker; marker; after_marker; block;
      ext_task_marker }
  in
  ignore djot_tight;
  (i, meta), tight

(* Djot's list tightness.

   Djot attaches a blank line to the innermost list open at that point, and the
   blank only loosens that list if what comes next is not a list boundary. In AST
   terms, for a list [l]: a blank line in one of its items loosens [l] unless the
   next block — looking past the end of the item, into the following item —
   is a nested list; and a blank that comes after a nested list within the same
   item belongs to that nested list, not to [l].

   So [- a\n\n- b] is loose (the blank is followed by a paragraph), while
   [- a\n\n  - b\n\n- c] is tight (the first blank is followed by a nested list,
   the second belongs to that nested list). *)
and djot_list_is_tight (list : Block_struct.list') =
  let is_blank = function Block_struct.Blank_line _ -> true | _ -> false in
  let is_list = function Block_struct.List _ -> true | _ -> false in
  let blocks (i : Block_struct.list_item) = List.rev i.blocks in
  let rec first_block = function
  | [] -> None
  | i :: items ->
      match List.find_opt (fun b -> not (is_blank b)) (blocks i) with
      | Some _ as b -> b
      | None -> first_block items
  in
  let rec item_ok seen_list next_items = function
  | [] -> true
  | b :: bs when is_blank b ->
      if seen_list then item_ok seen_list next_items bs else
      let next = match List.find_opt (fun b -> not (is_blank b)) bs with
      | Some _ as b -> b
      | None -> first_block next_items
      in
      begin match next with
      | Some b when not (is_list b) -> false
      | Some _ | None -> item_ok seen_list next_items bs
      end
  | b :: bs -> item_ok (seen_list || is_list b) next_items bs
  in
  let rec go = function
  | [] -> true
  | i :: items -> item_ok false items (blocks i) && go items
  in
  go (List.rev list.items)

and block_struct_to_list p list =
  let rec loop p tight acc = function
  | [] -> tight, acc
  | item :: items ->
      let item, item_tight = block_struct_to_list_item p item in
      loop p (tight && item_tight) (item :: acc) items
  in
  let items = list.Block_struct.items in
  let last, tight = block_struct_to_list_item p (List.hd items) in
  let tight, items = loop p (not list.loose && tight) [last] (List.tl items) in
  let tight =
    if Oymarkit_mod.djot_list_tightness p.oymarkit_mod
    then djot_list_is_tight list else tight
  in
  let meta = meta_of_metas p ~first:(snd (List.hd items)) ~last:(snd last) in
  Block.List ({ type' = list.Block_struct.list_type; tight; items }, meta)

and block_struct_to_def_item p (i : Block_struct.def_item) =
  (* Same tightness rule as a list item: a blank line *between* two blocks of
     the definition makes it loose, a trailing one does not (it is the blank
     that separates two items). The [bstate] walk is the one
     [block_struct_to_list_item] does, on the same reversed block list. *)
  let rec loop bstate tight acc = function
  | Block_struct.Blank_line _ as bl :: bs ->
      let bstate = if bstate = `Trail_blank then `Trail_blank else `Blank in
      loop bstate tight (block_struct_to_block p bl :: acc) bs
  | b :: bs ->
      let tight = tight && not (bstate = `Blank) in
      loop `Non_blank tight (block_struct_to_block p b :: acc) bs
  | [] -> tight, acc
  in
  let marker = (* not layout to get loc *) clean_raw_span p i.marker in
  let _term_layout, term = Inline_struct.parse p [i.term] in
  let last_meta, (tight, blocks) = match i.blocks with
  | [] -> snd marker, (true, [])
  | [Block_struct.Blank_line _ as blank] ->
      let bl = block_struct_to_block p blank in
      Block.meta bl, (true, [bl])
  | Block_struct.Blank_line _ as blank :: bs ->
      let bl = block_struct_to_block p blank in
      Block.meta bl, loop `Trail_blank true [bl] bs
  | b :: bs ->
      let b = block_struct_to_block p b in
      Block.meta b, loop `Non_blank true [b] bs
  in
  let definition = match blocks with
  | [] -> Block.empty
  | [b] -> b
  | bs ->
      let first = Block.meta (List.hd bs) in
      Block.Blocks (bs, meta_of_metas p ~first ~last:last_meta)
  in
  let meta = meta_of_metas p ~first:(snd marker) ~last:last_meta in
  let item =
    { Block.Definition_list.before_marker = i.before_marker; marker;
      after_marker = i.after_marker; term; definition }
  in
  (item, meta), tight

and block_struct_to_def_list p (dl : Block_struct.def_list) =
  let rec loop tight acc = function
  | [] -> tight, acc
  | i :: items ->
      let item, item_tight = block_struct_to_def_item p i in
      loop (tight && item_tight) (item :: acc) items
  in
  let items = dl.def_items in
  let last, tight = block_struct_to_def_item p (List.hd items) in
  let tight, items = loop (not dl.loose && tight) [last] (List.tl items) in
  let meta = meta_of_metas p ~first:(snd (List.hd items)) ~last:(snd last) in
  Block.Ext_definition_list ({ tight; items }, meta)

and block_struct_to_block p = function
| Block_struct.Ext_def_list dl -> block_struct_to_def_list p dl
| Block_struct.Block_quote (ind, marker, bs) ->
    block_struct_to_block_quote p ind marker bs
| Block_struct.List list -> block_struct_to_list p list
| Block_struct.Paragraph par -> block_struct_to_paragraph p par
| Block_struct.Thematic_break (i, br) -> block_struct_to_thematic_break p i br
| Block_struct.Code_block cb -> block_struct_to_code_block p cb
| Block_struct.Heading h -> block_struct_to_heading p h
| Block_struct.Html_block html -> block_struct_to_html_block p html
| Block_struct.Blank_line (pad, span) -> block_struct_to_blank_line p pad span
| Block_struct.Linkref_def r -> Block.Link_reference_definition r
| Block_struct.Attribute_specs specs ->
    Block.Ext_attributes
      (Block.Attributes.make ~specs Block.empty, Meta.none)
| Block_struct.Ext_table (i, rows, caption, _) ->
    block_struct_to_table p i rows caption
| Block_struct.Ext_div (fence, bs) -> block_struct_to_div p fence bs
| Block_struct.Ext_jsx_block (o, bs) -> block_struct_to_jsx_block p o bs
| Block_struct.Ext_footnote (i, labels, bs) ->
    block_struct_to_footnote_definition p i labels bs

(* Djot headings are implicit link reference targets: [ [Some Heading][] ] links
   to the heading. This must run before the blocks are converted, because a
   reference may sit in a paragraph *above* the heading it points at, and inline
   parsing (which resolves references) happens during conversion, in document
   order. So we walk the block structure first and register a definition for
   every heading.

   The heading's inline content is parsed here only to derive the key and the
   id; the conversion parses it again, and that second result — resolved against
   the complete [defs] — is the one that ends up in the AST.

   An explicit definition of the same label wins: it is already in [p.defs] by
   now (link reference definitions are collected during block-structure parsing)
   and we do not overwrite it. *)
let register_heading_labels p (doc : Block_struct.t list) =
  if not (Oymarkit_mod.djot_headings p.oymarkit_mod) then () else
  let djot_links = Oymarkit_mod.djot_links p.oymarkit_mod in
  let register ?attr_id lines =
    let _layout, inline = Inline_struct.parse p lines in
    (* Footnote references contribute nothing to the id (djot), so the target
       registered here matches the section id the renderer derives. *)
    let text =
      Inline.to_plain_text ~skip_link:Inline.is_footnote_reference
        ~break_on_soft:false inline
    in
    let text = String.concat " " (List.map (String.concat "") text) in
    let key = Match.label_key ~djot:djot_links p.buf text in
    if key = "" || Label.Map.mem key p.defs then () else
    (* The dest must equal the id the HTML renderer puts on the heading's
       section: an explicit [ {#id} ] attribute if present, else djot's
       case-preserving [djot_id_base] (not the case-folding CommonMark
       [Inline.id]). *)
    let id = match attr_id with
    | Some id -> id
    | None -> heading_auto_id p inline
    in
    let label = Label.make ~key [ "", (text, Meta.none) ] in
    let dest = ("#" ^ id, Meta.none) in
    let ld = Link_definition.make ~defined_label:(Some label) ~dest () in
    set_label_def p label (Link_definition.Def (ld, Meta.none))
  in
  let heading_lines = function
  | `Atx { Block_struct.heading; more; _ } -> more @ [heading]
  | `Setext { Block_struct.heading_lines; _ } -> heading_lines
  in
  let attr_id specs =
    Attribute.id (List.fold_left Attribute.merge Attribute.empty specs)
  in
  (* Reverse document order, so an attribute line that *precedes* a heading in
     the source follows it here (as for [attach_ref_def_attributes]). *)
  let rec blocks (bs : Block_struct.t list) = match bs with
  | Heading h :: (Attribute_specs specs :: _ as bs) ->
      register ?attr_id:(attr_id specs) (heading_lines h); blocks bs
  | b :: bs -> block b; blocks bs
  | [] -> ()
  and block (b : Block_struct.t) = match b with
  | Heading h -> register (heading_lines h)
  | Block_quote (_, _, bs) | Ext_div (_, bs) | Ext_jsx_block (_, bs)
  | Ext_footnote (_, _, bs) -> blocks bs
  | List l ->
      List.iter (fun (i : Block_struct.list_item) -> blocks i.blocks) l.items
  | Ext_def_list dl ->
      List.iter (fun (i : Block_struct.def_item) -> blocks i.blocks)
        dl.def_items
  | Blank_line _ | Code_block _ | Html_block _ | Linkref_def _
  | Attribute_specs _ | Paragraph _ | Thematic_break _ | Ext_table _ -> ()
  in
  blocks doc

(* Djot attributes written above a reference definition merge onto every link
   that references it. They must reach the definition in [p.defs] before any
   inline content is parsed, since that is when a link looks its definition up —
   so this runs on the block structure, like the heading labels above.

   The block-structure list is in reverse document order, so the attribute line
   that *precedes* a definition follows it here. *)
let attach_ref_def_attributes p (doc : Block_struct.t list) =
  if not (Oymarkit_mod.djot_block_attributes p.oymarkit_mod) then () else
  let attach (ld, _) specs =
    let attrs = List.fold_left Attribute.merge Attribute.empty specs in
    match Link_definition.defined_label ld with
    | None -> ()
    | Some l ->
        let key = Label.key l in
        match Label.Map.find_opt key p.defs with
        | Some (Link_definition.Def (d, m)) ->
            let d = Link_definition.with_attributes (Some attrs) d in
            p.defs <- Label.Map.add key (Link_definition.Def (d, m)) p.defs
        | _ -> ()
  in
  let rec walk (bs : Block_struct.t list) = match bs with
  | Linkref_def ld :: (Attribute_specs specs :: _ as bs) ->
      attach ld specs; walk bs
  | b :: bs -> block b; walk bs
  | [] -> ()
  and block (b : Block_struct.t) = match b with
  | Block_quote (_, _, bs) | Ext_div (_, bs) | Ext_jsx_block (_, bs)
  | Ext_footnote (_, _, bs) -> walk bs
  | List l ->
      List.iter (fun (i : Block_struct.list_item) -> walk i.blocks) l.items
  | Ext_def_list dl ->
      List.iter (fun (i : Block_struct.def_item) -> walk i.blocks) dl.def_items
  | Blank_line _ | Code_block _ | Heading _ | Html_block _ | Linkref_def _
  | Attribute_specs _ | Paragraph _ | Thematic_break _ | Ext_table _ -> ()
  in
  walk doc

let block_struct_to_doc p (doc, meta) =
  let rec resolve_block = function
  | Block.Block_quote (bq, meta) ->
      Block.Block_quote ({ bq with block = resolve_block (Block.Block_quote.block bq) }, meta)
  | Block.List (l, meta) ->
      let item (i, imeta) =
        ({ i with Block.List_item.block = resolve_block (Block.List_item.block i) }, imeta)
      in
      Block.List ({ l with items = List.map item (Block.List'.items l) }, meta)
  | Block.Blocks (bs, meta) -> Block.Blocks (resolve_blocks bs, meta)
  | Block.Ext_footnote_definition (fn, meta) ->
      Block.Ext_footnote_definition ({ fn with block = resolve_block fn.block }, meta)
  | Block.Ext_div (d, meta) ->
      Block.Ext_div ({ d with block = resolve_block (Block.Div.block d) }, meta)
  | Block.Ext_jsx_block (j, meta) ->
      Block.Ext_jsx_block
        ({ j with block = resolve_block (Block.Jsx_block.block j) }, meta)
  | b -> b
  and resolve_blocks bs =
    let is_empty = function Block.Blocks ([], _) -> true | _ -> false in
    let is_linkref_def = function
    | Block.Link_reference_definition _ -> true | _ -> false
    in
    let rec loop pending acc = function
    | Block.Ext_attributes (a, _) :: bs
      when is_empty (Block.Attributes.block a) ->
        loop (pending @ Block.Attributes.specs a) acc bs
    | Block.Blank_line _ as blank :: bs when pending <> [] ->
        (* Attributes must come right before a block, so a blank line detaches
           them. They are still kept as an empty [Ext_attributes] so that the
           source can be rendered back; renderers give such a target-less block
           no output. *)
        let marker =
          Block.Ext_attributes
            (Block.Attributes.make ~specs:pending Block.empty, Meta.none)
        in
        loop [] (blank :: marker :: acc) bs
    | b :: bs ->
        let b = resolve_block b in
        let b =
          match pending with
          | [] -> b
          | _ when is_linkref_def b ->
              (* Attributes above a reference definition have already been
                 merged onto the definition itself, so that they reach the links
                 referencing it. Wrapping the definition block in them too would
                 render an empty element: the definition renders as nothing. *)
              b
          | specs ->
              Block.Ext_attributes
                (Block.Attributes.make ~specs b, Block.meta b)
        in
        loop [] (b :: acc) bs
    | [] ->
        let acc =
          match pending with
          | [] -> acc
          | specs ->
              Block.Ext_attributes
                (Block.Attributes.make ~specs Block.empty, Meta.none)
              :: acc
        in
        List.rev acc
    in
    loop [] [] bs
  in
  let doc = prepare_block_structs p doc in
  match resolve_blocks (List.rev_map (block_struct_to_block p) doc) with
  | [b] -> b | bs -> Block.Blocks (bs, meta)
