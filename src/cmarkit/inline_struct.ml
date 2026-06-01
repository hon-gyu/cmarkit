open Common_
open Parser_common_

[@@@ocamlformat "disable"]

(*---------------------------------------------------------------------------
   Copyright (c) 2021 The cmarkit programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Inline structure parsing *)

(* Tokens for parsing inlines.

    The list of tokens of a paragraph are the points to consider to
    parse it into inlines. Tokens gradually become [Inline] tokens
    containing parsed inlines. Between two tokens there is implicit
    textual data. This data gradually becomes part of [Inline] tokens
    or, at the end of of the parsing process, becomes [Text] inlines.

    The token list also represents newlines explicitly, either via
    the [Newline] token or via the [Inline] token since inlines may
    start on a line and up on another one. *)

type emphasis_marks =
  { start : byte_pos;
    char : char;
    count : int;
    may_open : bool;
    may_close : bool;
    open_marker : bool;
    close_marker : bool }

type strikethrough_marks =
  { start : byte_pos;
    may_open : bool;
    may_close : bool }

type math_span_marks =
  { start : byte_pos;
    count : int;
    may_open : bool;
    may_close : bool; }

type token =
| Autolink_or_html_start of { start : byte_pos }
| Backticks of
    { start : byte_pos;
      count : int;
      escaped : bool }
| Emphasis_marks of emphasis_marks
| Inline of
    { start : byte_pos;
      inline : Inline.t;
      endline : line_span;
      next : byte_pos }
| Link_start of
    { start : byte_pos;
      image : bool }
| Newline of
    { start : (* points on spaces or \ on the broken line *) byte_pos;
      break_type : Inline.Break.type';
      newline : line_span; }
| Right_brack of { start : byte_pos }
| Right_paren of { start : byte_pos } (* Only used for closer index *)
| Strikethrough_marks of strikethrough_marks
| Math_span_marks of math_span_marks

let token_start = function
| Autolink_or_html_start { start } | Backticks { start }
| Emphasis_marks { start } | Inline { start } -> start |  Link_start { start }
| Newline { start } | Right_brack { start } -> start
| Right_paren { start } -> start
| Strikethrough_marks { start } -> start
| Math_span_marks { start } -> start

let has_backticks ~count ~after cidx =
  Closer_index.closer_exists (Closer.Backticks count) ~after cidx

let has_right_brack ~after cidx =
  Closer_index.closer_exists Closer.Right_brack ~after cidx

let has_right_paren ~after cidx =
  Closer_index.closer_exists Closer.Right_paren ~after cidx

let emphasis_closer_pos ~char ~after cidx =
  Closer_index.closer_pos (Closer.Emphasis_marks char) ~after cidx

let has_emphasis_closer ~char ~after cidx =
  Closer_index.closer_exists (Closer.Emphasis_marks char) ~after cidx

let has_strikethrough_closer ~after cidx =
  Closer_index.closer_exists Closer.Strikethrough_marks ~after cidx

let has_math_span_closer ~count ~after cidx =
  Closer_index.closer_exists (Closer.Math_span_marks count) ~after cidx

let rev_token_list_and_make_closer_index toks =
  let rec loop cidx acc = function
  | Backticks { start; count; _ } as t :: toks ->
      let cidx = Closer_index.add (Closer.Backticks count) start cidx in
      loop cidx (t :: acc) toks
  | Right_brack { start } as t :: toks ->
      let cidx = Closer_index.add Closer.Right_brack start cidx in
      loop cidx (t :: acc) toks
  | Right_paren { start } :: toks ->
      let cidx = Closer_index.add Closer.Right_paren start cidx in
      loop cidx (* we don't use the token for parsing *) acc toks
  | Emphasis_marks { start; char; may_close = true } as t :: toks ->
      let cidx = Closer_index.add (Closer.Emphasis_marks char) start cidx in
      loop cidx (t :: acc) toks
  | Strikethrough_marks { start; may_close = true } as t :: toks ->
      let cidx = Closer_index.add Closer.Strikethrough_marks start cidx in
      loop cidx (t :: acc) toks
  | Math_span_marks { start; count; may_close = true } as t :: toks ->
      let cidx = Closer_index.add (Closer.Math_span_marks count) start cidx in
      loop cidx (t :: acc) toks
  | t :: toks -> loop cidx (t :: acc) toks
  | [] -> cidx, acc
  in
  loop Closer_index.empty [] toks

let rec rev_tokens_and_shorten_last_line ~to_last:last acc = function
(* Used to make the text delimitation precise for nested inlines *)
| Newline ({ newline; _  } as nl) :: toks ->
    let t = Newline { nl with newline = { newline with last }} in
    List.rev_append toks (t :: acc)
| Inline ({ endline; _ } as i) :: toks ->
    let t = Inline { i with endline = { endline with last }} in
    List.rev_append toks (t :: acc)
| t :: toks -> rev_tokens_and_shorten_last_line ~to_last:last (t :: acc) toks
| [] -> acc

let rec drop_stop_after_right_brack = function
| Right_brack _ :: toks -> toks
| _ :: toks -> drop_stop_after_right_brack toks
| [] -> []

let rec drop_until ~start = function
| t :: toks when token_start t < start -> drop_until ~start toks
| toks -> toks

let rec next_line = function
(* N.B. when we use this function considering Inline tokens is not needed. *)
| [] -> None
| Newline { newline; _ } :: toks -> Some (toks, newline)
| _ :: toks -> next_line toks

(* Tokenization *)

let newline_token s prev_line newline =
  (* https://spec.commonmark.org/current/#softbreak *)
  (* https://spec.commonmark.org/current/#hard-line-breaks *)
  let start (* includes spaces or '\\' on prev line *), break_type =
    let first = prev_line.first and last = prev_line.last in
    let non_space = Match.rev_drop_spaces s ~first ~start:last in
    if non_space = last && s.[non_space] = '\\' then (non_space, `Hard) else
    let start = non_space + 1 in
    (start, if last - start + 1 >= 2 then `Hard else `Soft)
  in
  Newline { start; break_type; newline }

let add_backtick_token acc s line ~prev_bslash ~start =
  let last = Match.run_of ~char:'`' s ~last:line.last ~start:(start + 1) in
  let count = last - start + 1 and escaped = prev_bslash in
  Backticks {start; count; escaped} :: acc, last + 1

let try_add_image_link_start_token acc s line ~start =
  let next = start + 1 in
  if next > line.last || s.[next] <> '[' then acc, next else
  Link_start { start; image = true } :: acc, next + 1

let try_add_emphasis_token
    ?oymarkit_mod ?(open_marker = false) ?(close_marker = false) acc s line
    ~start
  =
  let first = line.first and last = line.last in
  let delim_start = if open_marker then start + 1 else start in
  let char = s.[delim_start] in
  let run_last = Match.run_of ~char ~last s ~start:(delim_start + 1) in
  let count = run_last - delim_start + 1 in
  let marker_last = if close_marker then run_last + 1 else run_last in
  let prev_uchar = Match.prev_uchar s ~first ~before:start in
  let next_uchar = Match.next_uchar s ~last ~after:marker_last in
  let prev_white = Cmarkit_data.is_unicode_whitespace prev_uchar in
  let next_white = Cmarkit_data.is_unicode_whitespace next_uchar in
  let prev_punct = Cmarkit_data.is_unicode_punctuation prev_uchar in
  let next_punct = Cmarkit_data.is_unicode_punctuation next_uchar in
  let is_left_flanking =
    not next_white && (not next_punct || (prev_white || prev_punct))
  in
  let is_right_flanking =
    not prev_white && (not prev_punct || (next_white || next_punct))
  in
  let next = marker_last + 1 in
  if not is_left_flanking && not is_right_flanking then acc, next else
  let may_open, may_close =
    match oymarkit_mod with
    | Some oymarkit_mod when is_oymarkit_enabled () ->
        begin let role =
          match open_marker, close_marker with
          | true, false -> Oymarkit_mod.Opener_only
          | false, true -> Oymarkit_mod.Closer_only
          | _ -> Oymarkit_mod.Any
        in
        Oymarkit_mod.emphasis_may_open_close oymarkit_mod ~role ~char
          ~is_left_flanking ~is_right_flanking ~prev_white ~next_white
          ~prev_punct ~next_punct
        end [@ocamlformat "enable"]
    | _ ->
        let may_open =
          (char = '*' && is_left_flanking) ||
          (char = '_' && is_left_flanking &&
            (not is_right_flanking || prev_punct))
        in
        let may_close =
          (char = '*' && is_right_flanking) ||
          (char = '_' && is_right_flanking &&
            (not is_left_flanking || next_punct))
        in
        may_open, may_close
  in
  if not may_open && not may_close then acc, next else
  Emphasis_marks
    { start = delim_start; char; count; may_open; may_close; open_marker;
      close_marker } [@ocamlformat "enable"]
  :: acc, next
[@@@ocamlformat "enable"]

let try_add_marked_emphasis_opener_token oymarkit_mod acc s line ~start =
  let next = start + 1 in
  if
    next > line.last
    || (not (Oymarkit_mod.marked_emphasis_delims oymarkit_mod))
    || not (s.[next] = '*' || s.[next] = '_')
  then (acc, next)
  else try_add_emphasis_token ~oymarkit_mod ~open_marker:true acc s line ~start

let try_add_marked_emphasis_closer_token oymarkit_mod acc s line ~start =
  let run_last =
    Match.run_of ~char:s.[start] ~last:line.last s ~start:(start + 1)
  in
  let marker = run_last + 1 in
  if
    marker > line.last
    || (not (Oymarkit_mod.marked_emphasis_delims oymarkit_mod))
    || s.[marker] <> '}'
  then try_add_emphasis_token ~oymarkit_mod acc s line ~start
  else try_add_emphasis_token ~oymarkit_mod ~close_marker:true acc s line ~start

[@@@ocamlformat "disable"]

let try_add_strikethrough_marks_token acc s line ~start =
  let first = line.first and last = line.last and char = s.[start] in
  let run_last = Match.run_of ~char ~last s ~start:(start + 1) in
  let count = run_last - start + 1 in
  let next = run_last + 1 in
  if count <> 2 then acc, next else
  let prev_uchar = Match.prev_uchar s ~first ~before:start in
  let next_uchar = Match.next_uchar s ~last ~after:run_last in
  let may_close = not (Cmarkit_data.is_unicode_whitespace prev_uchar) in
  let may_open = not (Cmarkit_data.is_unicode_whitespace next_uchar) in
  if not may_open && not may_close then acc, next else
  Strikethrough_marks { start; may_open; may_close } :: acc, next

let try_add_math_span_marks_token acc s line ~start =
  let first = line.first and last = line.last and char = s.[start] in
  let run_last = Match.run_of ~char ~last s ~start:(start + 1) in
  let count = run_last - start + 1 in
  let next = run_last + 1 in
  if count > 2 then acc, next else
  let may_open, may_close =
    if count <> 1 then true, true else
    let prev_uchar = Match.prev_uchar s ~first ~before:start in
    let next_uchar = Match.next_uchar s ~last ~after:run_last in
    let may_close = not (Cmarkit_data.is_unicode_whitespace prev_uchar) in
    let may_open = not (Cmarkit_data.is_unicode_whitespace next_uchar) in
    may_open, may_close
  in
  if not may_open && not may_close then acc, next else
  Math_span_marks { start; count; may_open; may_close } :: acc, next

let tokenize ?oymarkit_mod ~exts s lines =
  (* For inlines this is where we conditionalize for extensions. All code
      paths after that no longer check for p.exts: there just won't be
      extension data to process if [exts] was not [true] here. *)
  let rec loop ~exts s lines line ~prev_bslash acc k =
    if k > line.last then match lines with
    | [] -> rev_token_list_and_make_closer_index acc
    | newline :: lines ->
        let t = newline_token s line newline in
        loop ~exts s lines newline ~prev_bslash:false (t :: acc) newline.first
    else
    if s.[k] = '\\'
    then loop ~exts s lines line ~prev_bslash:(not prev_bslash) acc (k+1) else
    let acc, next = match s.[k] with
    | '`' -> add_backtick_token acc s line ~prev_bslash ~start:k
    | c when prev_bslash ->
        (* For backticks we need to treat backslash specially. Because
            if we are in a code span backslashes are always treated literally.
            But at the tokenization stage we don't know if we are
            in a code span or not. This is the reason why this comes
            after the case for '`'. *)
        acc, k + 1
    | '{' ->
        begin match oymarkit_mod with
        | Some oymarkit_mod when is_oymarkit_enabled () ->
            try_add_marked_emphasis_opener_token oymarkit_mod acc s line
              ~start:k
        | _ -> acc, k + 1
        end
    | '*' | '_' ->
        begin match oymarkit_mod with
        | Some oymarkit_mod when is_oymarkit_enabled () ->
            try_add_marked_emphasis_closer_token oymarkit_mod acc s line
              ~start:k
        | _ -> try_add_emphasis_token acc s line ~start:k
        end
    | ']' -> Right_brack { start = k } :: acc, k + 1
    | '[' -> Link_start { start = k; image = false } :: acc, k + 1
    | '!' -> try_add_image_link_start_token acc s line ~start:k
    | '<' -> Autolink_or_html_start { start = k } :: acc, k + 1
    | ')' -> Right_paren { start = k } :: acc, k + 1
    | '~' when exts -> try_add_strikethrough_marks_token acc s line ~start:k
    | '$' when exts -> try_add_math_span_marks_token acc s line ~start:k
    | _ -> acc, k + 1
    in
    loop ~exts s lines line ~prev_bslash:false acc next
  in
  let line = List.hd lines and lines = List.tl lines in
  let cidx, toks = loop ~exts s lines line ~prev_bslash:false [] line.first in
  cidx, toks, line

(* Making inlines and inline tokens *)

let break_inline p line ~start ~break_type:type' ~newline =
  let layout_before = { line with first = start } in
  let layout_after =
    let non_blank = first_non_blank_in_span p newline in
    { newline with last = non_blank - 1 }
  in
  let m = meta_of_spans p ~first:layout_before ~last:layout_after in
  let layout_before = layout_clean_raw_span p layout_before in
  let layout_after = layout_clean_raw_span p layout_after in
  Inline.Break ({ layout_before; type'; layout_after }, m)

let try_add_text_inline p line ~first ~last acc =
  if first > last then acc else
  let first = match first = line.first with
  | true -> first_non_blank_in_span p line (* strip leading blanks *)
  | false -> first
  in
  Inline.Text (clean_unesc_unref_span p { line with first; last }) :: acc

let inlines_inline p ~first ~last ~first_line ~last_line = function
| [i] -> i
| is ->
    let textloc = textloc_of_lines p ~first ~last ~first_line ~last_line in
    Inline.Inlines (is, meta p textloc)

let code_span_token p ~count ~first ~last ~first_line ~last_line rev_spans =
  let textloc = textloc_of_lines p ~first ~last ~first_line ~last_line in
  let code_layout = raw_tight_block_lines p ~rev_spans in
  let meta = meta p textloc in
  let cs = Inline.Code_span ({ backtick_count = count; code_layout }, meta) in
  Inline { start = first; inline = cs; endline = last_line; next = last + 1 }

let autolink_token p line ~first ~last ~is_email =
  let meta = meta p (textloc_of_span p { line with first; last }) in
  let link = { line with first = first + 1; last = last - 1 } in
  let link = clean_unref_span p link in
  let inline = Inline.Autolink ({ is_email; link }, meta) in
  Inline { start = first; inline; endline = line; next = last + 1 }

let raw_html_token p ~first ~last ~first_line ~last_line rev_spans =
  let raw = raw_tight_block_lines p ~rev_spans in
  let textloc =
    let first = Meta.textloc (snd (snd (List.hd raw))) in
    let last = snd (List.hd rev_spans) in
    let last_byte = last.last and last_line = last.line_pos in
    Textloc.set_last first ~last_byte ~last_line
  in
  let inline = Inline.Raw_html (raw, meta p textloc) in
  Inline { start = first; inline; endline = last_line; next = last + 1 }

let link_token p ~first ~last ~first_line ~last_line ~image link =
  let textloc = textloc_of_lines p ~first ~last ~first_line ~last_line in
  let link = link, meta p textloc in
  let inline = if image then Inline.Image link else Inline.Link link in
  Inline { start = first; inline; endline = last_line; next = last + 1 }

let emphasis_token p ?delim ?(open_marker = false) ?(close_marker = false)
    ~first ~last ~first_line ~last_line ~strong emph =
  let textloc = textloc_of_lines p ~first ~last ~first_line ~last_line in
  let delim = match delim with None -> p.i.[first] | Some delim -> delim in
  let e =
    { Inline.Emphasis.delim; inline = emph; open_marker; close_marker }
    , meta p textloc
  in
  let i = if strong then Inline.Strong_emphasis e else Inline.Emphasis e in
  Inline { start = first; inline = i ; endline = last_line; next = last + 1 }

let ext_strikethrough_token p ~first ~last ~first_line ~last_line s =
  let textloc = textloc_of_lines p ~first ~last ~first_line ~last_line in
  let inline = Inline.Ext_strikethrough (s, meta p textloc) in
  Inline { start = first; inline; endline = last_line; next = last + 1 }

let ext_math_span_token p ~count ~first ~last ~first_line ~last_line rspans =
  let textloc = textloc_of_lines p ~first ~last ~first_line ~last_line in
  let tex_layout = raw_tight_block_lines p ~rev_spans:rspans in
  let meta = meta p textloc in
  let ms = Inline.Math_span.make ~display:(count = 2) tex_layout in
  let inline = Inline.Ext_math_span (ms, meta) in
  Inline { start = first; inline; endline = last_line; next = last + 1 }

(* Parsers *)

let try_code p toks start_line ~start:cstart ~count ~escaped =
  (* https://spec.commonmark.org/current/#code-span *)
  let count = if escaped then count - 1 else count in
  if count <= 0 then None else
  let cstart = if escaped then cstart + 1 else cstart in
  if not (has_backticks ~count ~after:cstart p.cidx) then None else
  let rec match_backticks toks line ~count spans k = match toks with
  | [] -> None
  | Backticks { start; count = c; _ } :: toks ->
      if c <> count then match_backticks toks line ~count spans k else
      let span = line.first, { line with first = k; last = start - 1} in
      let spans = span :: spans in
      let first = cstart and last = start + count - 1 in
      let first_line = start_line and last_line = line in
      let t =
        code_span_token p ~count ~first ~last ~first_line ~last_line spans
      in
      Some (toks, line, t)
  | Newline { newline } :: toks ->
      let spans = (line.first, { line with first = k }) :: spans in
      let k = first_non_blank_in_span p newline in
      match_backticks toks newline ~count spans k
  | _ :: toks -> match_backticks toks line ~count spans k
  in
  let first = cstart + count in
  match_backticks toks { start_line with first } ~count [] first

let try_math_span p toks start_line ~start:cstart ~count =
  if not (has_math_span_closer ~count ~after:cstart p.cidx) then None else
  let rec match_math_marks toks line ~count spans k = match toks with
  | [] -> None
  | Math_span_marks { start; count = c; may_close; _ } :: toks ->
      if c <> count || not may_close
      then match_math_marks toks line ~count spans k else
      let span = line.first, { line with first = k; last = start - 1 } in
      let spans = span :: spans in
      let first = cstart and last = start + count - 1 in
      let first_line = start_line and last_line = line in
      let t =
        ext_math_span_token p ~count ~first ~last ~first_line ~last_line
          spans
      in
      Some (toks, line, t)
  | Newline { newline } :: toks ->
      let spans = (line.first, { line with first = k }) :: spans in
      let k = first_non_blank_in_span p newline in
      match_math_marks toks newline ~count spans k
  | _ :: toks -> match_math_marks toks line ~count spans k
  in
  let first = cstart + count in
  match_math_marks toks { start_line with first } ~count [] first

let try_autolink_or_html p toks line ~start =
  match Match.autolink_uri p.i ~last:line.last ~start with
  | Some last ->
      let t = autolink_token p line ~first:start ~last ~is_email:false in
      let toks = drop_until ~start:(last + 1) toks in
      Some (toks, line, t)
  | None ->
  match Match.autolink_email p.i ~last:line.last ~start with
  | Some last ->
      let t = autolink_token p line ~first:start ~last ~is_email:true in
      let toks = drop_until ~start:(last + 1) toks in
      Some (toks, line, t)
  | None ->
  match Match.raw_html ~next_line p.i toks ~line ~start with
  | None -> None
  | Some (toks, last_line, spans, last) ->
      let first = start and first_line = line in
      let t = raw_html_token p ~first ~last ~first_line ~last_line spans in
      let toks = drop_until ~start:(last + 1) toks in
      Some (toks, last_line, t)

let label_of_rev_spans p ~key rev_spans =
  let meta =
    if p.nolocs || rev_spans = [] then Meta.none else
    let first = snd (List.hd (List.rev rev_spans)) in
    let last = snd (List.hd rev_spans) in
    meta_of_spans p ~first ~last
  in
  let text = tight_block_lines p ~rev_spans in
  { Label.meta; key; text }

let try_full_reflink_remainder p toks line ~image ~start (* is label's [ *) =
  (* https://spec.commonmark.org/current/#full-reference-link *)
  match Match.link_label p.buf ~next_line p.i toks ~line ~start with
  | None -> None
  | Some (toks, line, rev_spans, last, key) ->
      let ref = label_of_rev_spans p ~key rev_spans in
      let toks = drop_stop_after_right_brack toks in
      match find_def_for_ref p ~image ref with
      | None -> Some None
      | Some def -> Some (Some (toks, line, `Ref (`Full, ref, def), last))

let try_shortcut_reflink p toks line ~image ~start (* is starting [ or ! *) =
  (* https://spec.commonmark.org/current/#shortcut-reference-link *)
  let start = if image then start + 1 (* [ *) else start in
  match Match.link_label p.buf ~next_line p.i toks ~line ~start with
  | None -> None
  | Some (toks, line, rev_spans, last, key) ->
      let ref = label_of_rev_spans p ~key rev_spans in
      let toks = drop_stop_after_right_brack toks in
      match find_def_for_ref p ~image ref with
      | None -> None
      | Some def -> Some (toks, line, `Ref (`Shortcut, ref, def), last)

let try_collapsed_reflink p toks line ~image ~start (* is starting [ or ! *) =
  (* https://spec.commonmark.org/current/#collapsed-reference-link *)
  let start = if image then start + 1 (* [ *) else start in
  match Match.link_label p.buf ~next_line p.i toks ~line ~start with
  | None -> None
  | Some (toks, line, rev_spans, last, key) ->
      let ref = label_of_rev_spans p ~key rev_spans in
      let last = last + 2 in (* adjust for ][] *)
      let toks = drop_stop_after_right_brack toks in
      let toks = drop_stop_after_right_brack toks in
      match find_def_for_ref p ~image ref with
      | None -> None
      | Some def -> Some (toks, line, `Ref (`Collapsed, ref, def), last)

let try_inline_link_remainder p toks start_line ~image ~start:st (* is ( *) =
  (* https://spec.commonmark.org/current/#inline-link *)
  if not (has_right_paren ~after:st p.cidx) then None else
  let first_non_blank_over_nl = first_non_blank_over_nl ~next_line in
  match first_non_blank_over_nl p toks start_line ~start:(st + 1) with
  | None -> None
  | Some (toks, line, before_dest, start) ->
      let toks, line, angled_dest, dest, start =
        match Match.link_destination p.i ~last:line.last ~start with
        | None -> toks, line, false, None, start
        | Some (angled, first, last) ->
            let dest = clean_unesc_unref_span p { line with first; last } in
            let next = if angled then last + 2 else last + 1 in
            toks, line, angled, Some dest, next
      in
      let toks, line, after_dest, title_open_delim, title, start =
        match first_non_blank_over_nl p toks line ~start with
        | None ->
            toks, line, [], '\"', None, start
        | Some (_, _, _, start') when start' = start ->
            toks, line, [], '\"', None, start
        | Some (toks, line, after_destination, start) ->
            match Match.link_title ~next_line p.i toks ~line ~start with
            | None -> toks, line, after_destination, '\"', None, start
            | Some (toks, line, rev_spans, last) ->
                let title = tight_block_lines p ~rev_spans in
                toks, line, after_destination, p.i.[start],
                Some title, last + 1
      in
      let toks, line, after_title, last =
        match first_non_blank_over_nl p toks line ~start with
        | None -> toks, line, [], start
        | Some (toks, line, after_title, start as v) -> v
      in
      if last > line.last || p.i.[last] <> ')' then None else
      let layout =
        { Link_definition.indent = 0; angled_dest; before_dest;
          after_dest; title_open_delim; after_title; }
      in
      let label = None and defined_label = None in
      let ld = { Link_definition.layout; label; defined_label; dest; title }in
      let textloc =
        let first = st and last = start in
        textloc_of_lines p ~first ~last ~first_line:start_line ~last_line:line
      in
      let ld = (ld, meta p textloc) in
      let toks = drop_until ~start:(last + 1) toks in
      Some (toks, line, `Inline ld, last)

let find_link_text_tokens p toks start_line ~start =
  (* XXX The repetition with first_pass is annoying here.
      we should figure out something for that not to happen. *)
  (* https://spec.commonmark.org/current/#link-text *)
  let rec loop toks line nest acc = match toks with
  | Right_brack { start = last } :: toks when nest = 0 ->
      let acc = rev_tokens_and_shorten_last_line ~to_last:(last - 1) [] acc in
      Some (toks, line, acc, last)
  | Backticks { start; count; escaped } :: toks ->
      begin match try_code p toks line ~start ~count ~escaped with
      | None -> loop toks line nest acc
      | Some (toks, line, t) -> loop toks line nest (t :: acc)
      end
  | Math_span_marks { start; count; may_open; } :: toks ->
      if not may_open then loop toks line nest acc else
      begin match try_math_span p toks line ~start ~count with
      | None -> loop toks line nest acc
      | Some (toks, line, t) -> loop toks line nest (t :: acc)
      end
  | Autolink_or_html_start { start } :: toks ->
      begin match try_autolink_or_html p toks line ~start with
      | None -> loop toks line nest acc
      | Some (toks, line, t) -> loop toks line nest (t :: acc)
      end
  | Right_brack _ as t :: toks -> loop toks line (nest - 1) (t :: acc)
  | Link_start _  as t :: toks -> loop toks line (nest + 1) (t :: acc)
  | Newline { newline; _ } as t :: toks -> loop toks newline nest (t :: acc)
  | Inline { endline; _ } as t :: toks -> loop toks endline nest (t :: acc)
  | t :: toks -> loop toks line nest (t :: acc)
  | [] -> None
  in
  loop toks start_line 0 []

let try_link_def
    p ~start ~start_toks ~start_line ~toks ~line ~text_last ~image text
  =
  let next = text_last + 1 in
  let link =
    if next > line.last
    then try_shortcut_reflink p start_toks start_line ~image ~start else
    match p.i.[next] with
    | '(' ->
        (match try_inline_link_remainder p toks line ~image ~start:next with
        | None -> try_shortcut_reflink p start_toks start_line ~image ~start
        | Some _ as v -> v)
    | '[' ->
        let next' = next + 1 in
        if next' <= line.last && p.i.[next'] = ']'
        then try_collapsed_reflink p start_toks start_line ~image ~start else
        let r = try_full_reflink_remainder p toks line ~image ~start:next in
        begin match r with
        | None -> try_shortcut_reflink p start_toks start_line ~image ~start
        | Some None -> None (* Example 570 *)
        | Some (Some _ as v) -> v
        end
    | c ->
        try_shortcut_reflink p start_toks start_line ~image ~start
  in
  match link with
  | None -> None
  | Some (toks, endline, reference, last) ->
      let first = start in
      let text =
        let first_line = start_line and last_line = line in
        inlines_inline p text ~first ~last:text_last ~first_line ~last_line
      in
      let link = { Inline.Link.text; reference } in
      let first_line = start_line and last_line = endline in
      let t = link_token p ~image ~first ~last ~first_line ~last_line link in
      let had_link = not image && not p.nested_links in
      Some (toks, endline, t, had_link)

(* The following sequence of mutually recursive functions define
    inline parsing. We have three passes over a paragraph's token
    list see the [parse_tokens] function below. *)

let rec try_link p start_toks start_line ~image ~start =
  if not (has_right_brack ~after:start p.cidx) then None else
  match find_link_text_tokens p start_toks start_line ~start with
  | None -> None
  | Some (toks, line, text_toks, text_last (* with ] delim *)) ->
      let text, had_link =
        let text_start =
          let first = start + (if image then 2 else 1) in
          let last =
            if start_line == line then text_last - 1 else start_line.last
          in
          { start_line with first; last }
        in
        parse_tokens p text_toks text_start
      in
      if had_link && not image
      then None (* Could try to keep render *) else
      try_link_def
        p ~start ~start_toks ~start_line ~toks ~line ~text_last ~image text

and first_pass p toks line =
  (* Parse inline atoms and links. Links are parsed here otherwise
      link reference data gets parsed as atoms. *)
  let rec loop p toks line ~had_link acc = match toks with
  | [] -> List.rev acc, had_link
  | Backticks { start; count; escaped } :: toks ->
      begin match try_code p toks line ~start ~count ~escaped with
      | None -> loop p toks line ~had_link acc
      | Some (toks, line, t) -> loop p toks line ~had_link (t :: acc)
      end
  | Math_span_marks { start; count; may_open; } :: toks ->
      if not may_open then loop p toks line ~had_link acc else
      begin match try_math_span p toks line ~start ~count with
      | None -> loop p toks line ~had_link acc
      | Some (toks, line, t) -> loop p toks line ~had_link (t :: acc)
      end
  | Autolink_or_html_start { start } :: toks ->
      begin match try_autolink_or_html p toks line ~start with
      | None -> loop p toks line ~had_link acc
      | Some (toks, line, t) -> loop p toks line ~had_link (t :: acc)
      end
  | Link_start { start; image } :: toks ->
      begin match try_link p toks line ~image ~start with
      | None -> loop p toks line ~had_link acc
      | Some (toks, line, t, had_link) ->
          loop p toks line ~had_link (t :: acc)
      end
  | Right_brack start :: toks -> loop p toks line ~had_link acc
  | Newline { newline = l } as t :: toks -> loop p toks l ~had_link (t :: acc)
  | t :: toks -> loop p toks line ~had_link (t :: acc)
  in
  loop p toks line ~had_link:false []

(* Second pass *)

and find_emphasis_text p toks line ~opener =
  let marks_match ~marks ~opener =
    (opener.char = marks.char) &&
    (not (marks.may_open || opener.may_close) ||
      marks.count mod 3 = 0 || (opener.count + marks.count) mod 3 != 0)
  in
  let marks_has_precedence p ~marks ~opener =
    if marks.char = opener.char (* Rule 16 *) then true else (* Rule 15 *)
    emphasis_closer_pos ~char:marks.char ~after:marks.start p.cidx <
    emphasis_closer_pos ~char:opener.char ~after:marks.start p.cidx
  in
  let rec loop p toks line acc ~opener = match toks with
  | [] -> Either.Left (List.rev acc) (* No match but keep nested work done *)
  | Emphasis_marks marks as t :: toks ->
      let after = marks.start in
      if marks.may_close && marks_match ~marks ~opener then
        if is_oymarkit_enabled () then begin
          match
            Oymarkit_mod.emphasis_match_used p.oymarkit_mod
              ~char:opener.char ~opener_count:opener.count
              ~closer_count:marks.count
          with
          | Some used ->
              let to_last = marks.start - 1 in
              let acc = rev_tokens_and_shorten_last_line ~to_last [] acc in
              Either.Right (toks, line, used, acc, marks)
          | None ->
              if has_emphasis_closer ~char:opener.char ~after p.cidx
              then loop p toks line (t :: acc) ~opener
              else Either.Left (List.rev_append (t :: acc) toks)
        end else begin
          let used = if marks.count >= 2 && opener.count >= 2 then 2 else 1 in
          let to_last = marks.start - 1 in
          let acc = rev_tokens_and_shorten_last_line ~to_last [] acc in
          Either.Right (toks, line, used, acc, marks)
        end
      else if marks.may_open && marks_has_precedence p ~marks ~opener then
        match try_emphasis p toks line ~opener:marks with
        | Either.Left toks -> loop p toks line acc ~opener
        | Either.Right (toks, line) -> loop p toks line acc ~opener
      else if has_emphasis_closer ~char:opener.char ~after p.cidx then
        loop p toks line (t :: acc) ~opener
      else (Either.Left (List.rev_append (t :: acc) toks))
  | Newline { newline = l } as t :: toks -> loop p toks l (t :: acc) ~opener
  | Inline { endline = l } as t :: toks -> loop p toks l (t :: acc) ~opener
  | t :: toks -> loop p toks line (t :: acc) ~opener
  in
  loop p toks line [] ~opener

and try_emphasis p start_toks start_line ~opener =
  let start = opener.start in
  if not (has_emphasis_closer ~char:opener.char ~after:start p.cidx)
  then Either.Left start_toks else
  match find_emphasis_text p start_toks start_line ~opener with
  | Either.Left _ as r -> r
  | Either.Right (toks, line, used, emph_toks, closer) ->
      let text_first = start + opener.count in
      let text_last = closer.start - 1 (* XXX prev line ? *) in
      let open_marker_width = if opener.open_marker then 1 else 0 in
      let close_marker_width = if closer.close_marker then 1 else 0 in
      let first = text_first - used - open_marker_width in
      let last = closer.start + used - 1 + close_marker_width in
      let first_line = start_line and last_line = line in
      let emph =
        let text_start =
          let last =
            if start_line == line then text_last else start_line.last
          in
          { start_line with first = text_first; last }
        in
        (* No need to redo first pass *)
        let emph_toks = second_pass p emph_toks text_start in
        let text = last_pass p emph_toks text_start in
        inlines_inline p text ~first ~last:text_last ~first_line ~last_line
      in
      let toks =
        let count = closer.count - used in
        if count = 0 then toks else
        Emphasis_marks { closer with start = last + 1; count } :: toks
      in
      let toks =
        let strong = used = 2 in
        emphasis_token p ~delim:opener.char ~first ~last ~first_line
          ~last_line ~strong ~open_marker:opener.open_marker
          ~close_marker:closer.close_marker emph ::
        toks
      in
      let toks =
        let count = opener.count - used in
        if count = 0 then toks else
        Emphasis_marks { opener with count } :: toks
      in
      Either.Right (toks, line)

and find_strikethrough_text p toks start_line =
  let rec loop p toks line acc = match toks with
  | [] -> Either.Left (List.rev acc) (* No match but keep nested work done *)
  | Strikethrough_marks marks :: toks ->
      if marks.may_close then
        let to_last = marks.start - 1 in
        let acc = rev_tokens_and_shorten_last_line ~to_last [] acc in
        Either.Right (toks, line, acc, marks)
      else if marks.may_open then
        match try_strikethrough p toks line ~opener:marks with
        | Either.Left toks -> loop p toks line acc
        | Either.Right (toks, line) -> loop p toks line acc
      else assert false
  | Newline { newline = l } as t :: toks -> loop p toks l (t :: acc)
  | Inline { endline = l } as t :: toks -> loop p toks l (t :: acc)
  | t :: toks -> loop p toks line (t :: acc)
  in
  loop p toks start_line []

and try_strikethrough p start_toks start_line ~opener =
  let start = opener.start in
  if not (has_strikethrough_closer ~after:start p.cidx)
  then Either.Left start_toks else
  match find_strikethrough_text p start_toks start_line with
  | Either.Left _ as r -> r
  | Either.Right (toks, line, stroken_toks, closer) ->
      let first_line = start_line and last_line = line in
      let text =
        let first = start + 2 in
        let last = closer.start - 1 in
        let text_start =
          let last =
            if start_line == line then last else start_line.last
          in
          { start_line with first; last }
        in
        (* No need to redo first pass *)
        let emph_toks = second_pass p stroken_toks text_start in
        let text = last_pass p emph_toks text_start in
        inlines_inline p text ~first ~last ~first_line ~last_line
      in
      let toks =
        let first = opener.start and last = closer.start + 1 in
        ext_strikethrough_token p ~first ~last ~first_line ~last_line text
        :: toks
      in
      Either.Right (toks, line)

and second_pass p toks line =
  let rec loop p toks line acc = match toks with
  | [] -> List.rev acc
  | Emphasis_marks ({ may_open } as opener) :: toks ->
      if not may_open then loop p toks line acc else
      begin match try_emphasis p toks line ~opener with
      | Either.Left toks -> loop p toks line acc
      | Either.Right (toks, line) -> loop p toks line acc
      end
  | Strikethrough_marks ({ may_open } as opener) :: toks ->
      if not may_open then loop p toks line acc else
      begin match try_strikethrough p toks line ~opener with
      | Either.Left toks -> loop p toks line acc
      | Either.Right (toks, line) -> loop p toks line acc
      end
  | Newline { newline } as t :: toks -> loop p toks newline (t :: acc)
  | Inline { endline } as t :: toks -> loop p toks endline (t :: acc)
  | t :: toks -> loop p toks line (t :: acc)
  in
  loop p toks line []

(* Last pass *)

and last_pass p toks line =
  (* Only [Inline] and [Newline] tokens remain. We fold over them to
      convert them to [inline] values and [Break]s. [Text] inlines
      are created for data between them. *)
  let rec loop toks line acc k = match toks with
  | [] ->
      List.rev (try_add_text_inline p line ~first:k ~last:line.last acc)
  | Newline { start; break_type; newline } :: toks ->
      let acc = try_add_text_inline p line ~first:k ~last:(start - 1) acc in
      let break = break_inline p line ~start ~break_type ~newline in
      loop toks newline (break :: acc) newline.first
  | Inline { start; inline; endline; next } :: toks ->
      let acc = try_add_text_inline p line ~first:k ~last:(start - 1) acc in
      let acc = match inline with
      | Inline.Inlines (is, _meta_stub) -> List.rev_append (List.rev is) acc
      | i -> i :: acc
      in
      loop toks endline acc next
  | (Backticks _ | Autolink_or_html_start _ | Link_start _ | Right_brack _
    | Emphasis_marks _ | Right_paren _ | Strikethrough_marks _
    | Math_span_marks _) :: _ ->
      assert false
  in
  loop toks line [] line.first

and parse_tokens p toks first_line =
  let toks, had_link = first_pass p toks first_line in
  let toks = second_pass p toks first_line in
  last_pass p toks first_line, had_link

let strip_paragraph p lines =
  (* Remove initial and final blanks. Initial blank removal on
      other paragraph lines is done during the inline parsing
      and integrated into the AST for layout preservation. *)
  let last, trailing_blanks =
    let line = List.hd lines in
    let first = line.first and start = line.last in
    let non_blank = Match.last_non_blank p.i ~first ~start in
    { line with last = non_blank},
    layout_clean_raw_span' p { line with first = non_blank + 1; }
  in
  let lines = List.rev (last :: List.tl lines) in
  let first, leading_indent =
    let line = List.hd lines in
    let non_blank = first_non_blank_in_span p line in
    { line with first = non_blank },
    non_blank - line.first
  in
  let lines = first :: List.tl lines in
  let meta = meta_of_spans p ~first ~last in
  (leading_indent, trailing_blanks), meta, lines

let parse p lines =
  let layout, meta, lines = strip_paragraph p lines in
  let cidx, toks, first_line =
    tokenize ~oymarkit_mod:p.oymarkit_mod ~exts:p.exts p.i lines
  in
  p.cidx <- cidx;
  let is, _had_link = parse_tokens p toks first_line in
  let inline = match is with [i] -> i | is -> Inline.Inlines (is, meta) in
  layout, inline

(* Parsing table rows *)

let get_blanks p line ~before k =
  let nb = Match.first_non_blank p.i ~last:(before - 1) ~start:k in
  layout_clean_raw_span' p { line with first = k; last = nb - 1 }, nb

let make_col p = function
| [] -> assert false
| [i] -> i
| is ->
    let last = Inline.meta (List.hd is) in
    let is = List.rev is in
    let first = Inline.meta (List.hd is) in
    let meta = meta_of_metas p ~first ~last in
    Inline.Inlines (is, meta)

let find_pipe p line ~before k =
  let text p ~first ~last =
    Inline.Text (clean_unesc_unref_span p { line with first; last })
  in
  let n = Match.first_non_escaped_char '|' p.i ~last:(before - 1) ~start:k in
  if n = before then `Not_found (text p ~first:k ~last:(n - 1)) else
  let nb = Match.last_non_blank p.i ~first:k ~start:(n - 1) in
  let after =
    layout_clean_raw_span' p { line with first = nb + 1; last = n - 1 }
  in
  let text = if nb < k then None else Some (text p ~first:k ~last:nb) in
  `Found (text, after, n + 1)

let start_col p line ~before k =
  let bbefore, k = get_blanks p line ~before k in
  if k >= before then `Start (bbefore, []) else
  match find_pipe p line ~before k with
  | `Not_found text -> `Start (bbefore, [text])
  | `Found (text, bafter, k) ->
      let text = match text with
      | Some text -> text
      | None ->
          let l = textloc_of_span p { line with first = k; last = k - 1 }in
          (Inline.Inlines ([], meta p l))
      in
      `Col ((text, (bbefore, bafter)), k)

let rec finish_col p line blanks_before is toks k = match toks with
| [] ->
    begin match find_pipe p line ~before:(line.last + 1) k with
    | `Found (text, after, k) ->
        let is = match text with Some t -> t :: is | None -> is in
        (make_col p is, (blanks_before, after)), [], k
    | `Not_found _ -> assert false
    end
| Inline { start; inline; next } :: toks when k >= start ->
    finish_col p line blanks_before (inline :: is) toks next
| Inline { start; inline; next } :: toks as toks' ->
    begin match find_pipe p line ~before:start k with
    | `Not_found text ->
        let is = inline :: text :: is in
        finish_col p line blanks_before is toks next
    | `Found (text, after, k) ->
        let is = match text with Some t -> t :: is | None -> is in
        (make_col p is, (blanks_before, after)), toks', k
    end
| (Backticks _ | Autolink_or_html_start _ | Link_start _ | Right_brack _
  | Emphasis_marks _ | Right_paren _ | Strikethrough_marks _
  | Math_span_marks _ | Newline _ ) :: _ ->
    assert false

let rec parse_cols p line acc toks k = match toks with
| [] ->
    if k > line.last then (List.rev acc) else
    begin match start_col p line ~before:(line.last + 1) k with
    | `Col (col, k) -> parse_cols p line (col :: acc) [] k
    | `Start _ -> assert false
    end
| Inline { start; inline; next } :: toks as toks' ->
    begin match start_col p line ~before:start k with
    | `Col (col, k) -> parse_cols p line (col :: acc) toks' k
    | `Start (before, is) ->
        let is = inline :: is in
        let col, toks, k = finish_col p line before is toks next in
        parse_cols p line (col :: acc) toks k
    end
| (Backticks _ | Autolink_or_html_start _ | Link_start _ | Right_brack _
  | Emphasis_marks _ | Right_paren _ | Strikethrough_marks _
  | Math_span_marks _ | Newline _ ) :: _ ->
    assert false

let parse_table_row p line =
  let cidx, toks, first_line =
    tokenize ~oymarkit_mod:p.oymarkit_mod ~exts:p.exts p.i [line]
  in
  p.cidx <- cidx;
  let toks, _had_link = first_pass p toks first_line in
  let toks = second_pass p toks first_line in
  (* We now have modified last pass, inner inlines will have gone through
      the regular [last_pass] which is fine since we are only interested
      in creating the toplevel text nodes further splited on (unescaped)
      [\]. *)
  parse_cols p line [] toks line.first
