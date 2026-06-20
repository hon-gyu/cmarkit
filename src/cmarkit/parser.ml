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
      layout_after : line_span }

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

  type t =
  | Block_quote of Layout.indent * line_span (* loc of initial marker *) * t list
  | Ext_div of div_fence * t list (* Oymarkit djot div, children reversed *)
  | Blank_line of space_pad * line_span
  | Code_block of code_block
  | Heading of heading
  | Html_block of html_block
  | List of list'
  | Linkref_def of Link_definition.t node
  | Attribute_specs of Attribute.t list
  | Paragraph of paragraph
  | Thematic_break of Layout.indent * line_span (* including trailing blanks *)
  | Ext_table of Layout.indent * (line_span * line_span (* trail blanks *)) list
  | Ext_footnote of Layout.indent * (Label.t * Label.t option) * t list

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
    Heading (`Atx { indent; level; after_open; heading; layout_after })

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
    Ext_table (indent, [row])

  (* Link reference definition parsing

     This is invoked when we close a paragraph and works on the paragraph
     lines. *)

  let parse_link_reference_definition p lines =
    (* Has no side effect on [p], parsing occurs on [lines] spans. *)
    (* https://spec.commonmark.org/current/#link-reference-definitions *)
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
        match Match.link_label p.buf ~next_line p.i lines ~line ~start with
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
        { Link_definition.layout; label; defined_label; dest; title }, meta
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
  | Ext_footnote (i, l, blocks) :: bs -> close_footnote p i l blocks bs
  | bs -> bs

  and close_list p l bs =
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
  | Block_quote (indent, marker, bq) :: bs ->
      Block_quote (indent, marker, end_doc p bq) :: bs
  | Ext_div (fence, children) :: bs ->
      (* closed by the end of document: [closing_fence] stays [None] *)
      Ext_div (fence, end_doc p children) :: bs
  | List list :: bs -> close_list p list bs
  | Paragraph par :: bs -> close_paragraph p par bs
  | Code_block (`Indented ls) :: bs -> close_indented_code_block p ls bs
  | Code_block (`Fenced f) :: bs -> end_doc_close_fenced_code_block p f bs
  | Html_block html :: bs -> end_doc_close_html p html bs
  | Ext_footnote (i, l, blocks) :: bs -> close_footnote p i l blocks bs
  | (Thematic_break _ | Heading _ | Blank_line _ | Linkref_def _
    | Attribute_specs _ | Ext_table _ ) :: _ | [] as bs -> bs

  (* Adding lines to blocks *)

  let match_line_type ~no_setext ~indent p =
    (* Effects on [p]'s column advance *)
    if only_blanks p then Match.Blank_line else
    if indent >= 4 then Indented_code_block_line else begin
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
          let r = Match.thematic_break p.i ~last ~start in
          if r <> Nomatch then r else
          let r = Match.list_marker p.i ~last ~start in
          if r <> Nomatch then r else
          Paragraph_line
      | '#' ->
          let r = Match.atx_heading p.i ~last ~start in
          if r <> Nomatch then r else
          Paragraph_line
      | '+' | '*' | '0' .. '9' ->
          let r = Match.thematic_break p.i ~last ~start in
          if r <> Nomatch then r else
          let r = Match.list_marker p.i ~last ~start in
          if r <> Nomatch then r else
          Paragraph_line
      | '_' ->
          let r = Match.thematic_break p.i ~last ~start in
          if r <> Nomatch then r else
          Paragraph_line
      | '~' | '`' ->
          let r = Match.fenced_code_block_start p.i ~last ~start in
          if r <> Nomatch then r else
          Paragraph_line
      | ':' when Oymarkit_mod.div p.oymarkit_mod ->
          let r = Match.div_open p.i ~last ~start in
          if r <> Nomatch then r else
          Paragraph_line
      | '<' ->
          let r = Match.html_block_start p.i ~last ~start in
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

  let list_marker_can_interrupt_paragraph p = function
  | `Ordered (1, _), marker_last | `Unordered _, marker_last ->
      let last = p.current_line_last_char and start = marker_last + 1 in
      let non_blank = Match.first_non_blank p.i ~last ~start in
      non_blank <= p.current_line_last_char (* line is not blank *)
  | _ -> false

  let same_list_type t0 t1 = match t0, t1 with
  | `Ordered (_, c0), `Ordered (_, c1)
  | `Unordered c0, `Unordered c1 when Char.equal c0 c1 -> true
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
  | Html_block_line end_cond -> html_block p ~end_cond ~indent_start :: bs
  | Paragraph_line -> paragraph p ~start:indent_start :: bs
  | Ext_table_row last -> table p ~indent ~last :: bs
  | Ext_footnote_label (rev_spans, last, key) ->
      footnote p ~indent ~last rev_spans key :: bs
  | Setext_underline_line _ | Nomatch ->
      (* This function should be called with a line type that comes out
         of match_line_type ~no_setext:true *)
      assert false

  and add_open_blocks p bs =
    let indent_start = p.current_char and indent = current_indent p in
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
    let min = indent + marker_size + after_marker in
    min, { before_marker; marker; after_marker; ext_task_marker;
           blocks = add_open_blocks p [] }

  and list ~indent p (list_type, _ as m) bs =
    let item_min_indent, item = list_item ~indent p m in
    List { last_blank = false; loose = false;
           item_min_indent; list_type; items = [item] } :: bs

  let try_add_to_list ~indent p (lt, _ as m) l bs =
    let item_min_indent, item = list_item ~indent p m in
    if same_list_type lt l.list_type then
      let l = close_last_list_item p l and last_blank = false in
      let list_type = l.list_type in
      List { last_blank; loose = l.last_blank; item_min_indent; list_type;
             items = item :: l.items } :: bs
    else
    let bs = close_list p l bs and last_blank = false in
    List { last_blank; loose = false; item_min_indent; list_type = lt;
           items = [item] } :: bs

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
    | Html_block_line end_cond ->
        html_block p ~end_cond ~indent_start :: (close_paragraph p par bs)
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

  let try_add_to_table p ind rows bs =
    let indent_start = p.current_char and indent = current_indent p in
    match match_line_type ~indent ~no_setext:true p with
    | Ext_table_row last ->
        let row = table_row p ~first:p.current_char ~last in
        Ext_table (ind, row :: rows) :: bs
    | ltype ->
        let bs = Ext_table (ind, rows) :: bs in
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

  and try_add_to_div p fence children bs =
    match fence.closing_fence with
    | Some _ -> (* closed: this line starts a new sibling block *)
        add_open_blocks p (Ext_div (fence, children) :: bs)
    | None ->
        let start = p.current_char and last = p.current_line_last_char in
        match Match.div_close p.i ~last ~start with
        | Some line_len ->
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
        | None -> Ext_div (fence, add_line p children) :: bs

  and add_line p = function
  | Paragraph par :: bs -> try_add_to_paragraph p par bs
  | ((Thematic_break _ | Heading _ | Blank_line _ | Linkref_def _
      | Attribute_specs _) :: _)
  | [] as bs -> add_open_blocks p bs
  | List list :: bs -> try_add_to_list_item p list bs
  | Code_block (`Indented ls) :: bs -> try_add_to_indented_code_block p ls bs
  | Code_block (`Fenced f) :: bs -> try_add_to_fenced_code_block p f bs
  | Block_quote (ind, marker, bq) :: bs -> try_add_to_block_quote p ind bq marker bs
  | Ext_div (fence, children) :: bs -> try_add_to_div p fence children bs
  | Html_block html :: bs -> try_add_to_html_block p html bs
  | Ext_table (ind, rows) :: bs -> try_add_to_table p ind rows bs
  | Ext_footnote (i, l, blocks) :: bs -> try_add_to_footnote p i l blocks bs

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
    if p.exts && Block.Code_block.is_math_block info_string
    then Block.Ext_math_block (cb, meta)
    else Block.Code_block (cb, meta)

let block_struct_to_heading p = function
| `Atx { Block_struct.indent; level; after_open; heading; layout_after } ->
    let after_opening =
      let first = after_open and last = heading.first - 1 in
      layout_clean_raw_span' p { heading with first; last }
    in
    let closing = layout_clean_raw_span' p layout_after in
    let layout = `Atx { Block.Heading.indent; after_opening; closing } in
    let meta =
      meta p (textloc_of_span p { heading with first = after_open - level })
    in
    let _layout, inline = Inline_struct.parse p [heading] in
    let id = match p.heading_auto_ids with
    | false -> None
    | true -> Some (`Auto (Inline.id ~buf:p.buf inline))
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
    | true -> Some (`Auto (Inline.id ~buf:p.buf inline))
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
  let layout, inline = Inline_struct.parse p par.Block_struct.lines in
  let leading_indent, trailing_blanks = layout in
  let meta =
    match paragraph_block_id p par with
    | None -> Inline.meta inline
    | Some block_id -> Block.Block_id.add block_id (Inline.meta inline)
  in
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
  | specs, [] -> [Block_struct.Attribute_specs specs]
  | specs, lines ->
      [ Block_struct.Attribute_specs specs;
        Block_struct.Paragraph { par with lines = List.rev lines } ]

let rec prepare_block_struct p = function
| Block_struct.Block_quote (indent, marker, bs) ->
    [Block_struct.Block_quote (indent, marker, prepare_block_structs p bs)]
| Block_struct.Ext_div (fence, bs) ->
    [Block_struct.Ext_div (fence, prepare_block_structs p bs)]
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

let block_struct_to_table p indent rows =
  let rec loop p col_count last_was_sep acc = function
  | (row, blanks) :: rs ->
      let meta = meta p (textloc_of_span p row) in
      let row' = { row with first = row.first + 1; last = row.last } in
      let cols = Inline_struct.parse_table_row p row' in
      let col_count = Int.max col_count (List.length cols) in
      let r, last_was_sep = match Block.Table.parse_sep_row cols with
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
  Block.Ext_table ({ indent; col_count; rows }, meta)

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
  let rec loop bstate tight acc = function
  | Block_struct.Blank_line _ as bl :: bs ->
      let bstate = if bstate = `Trail_blank then `Trail_blank else `Blank in
      loop bstate tight (block_struct_to_block p bl :: acc) bs
  | Block_struct.List
      { items = { blocks = Block_struct.Blank_line _ :: _ } :: _ } as l :: bs
    ->
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
  (i, meta), tight

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
  let meta = meta_of_metas p ~first:(snd (List.hd items)) ~last:(snd last) in
  Block.List ({ type' = list.Block_struct.list_type; tight; items }, meta)

and block_struct_to_block p = function
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
| Block_struct.Ext_table (i, rows) -> block_struct_to_table p i rows
| Block_struct.Ext_div (fence, bs) -> block_struct_to_div p fence bs
| Block_struct.Ext_footnote (i, labels, bs) ->
    block_struct_to_footnote_definition p i labels bs

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
  | b -> b
  and resolve_blocks bs =
    let is_empty = function Block.Blocks ([], _) -> true | _ -> false in
    let rec loop pending acc = function
    | Block.Ext_attributes (a, _) :: bs
      when is_empty (Block.Attributes.block a) ->
        loop (pending @ Block.Attributes.specs a) acc bs
    | Block.Blank_line _ as blank :: bs when pending <> [] ->
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
