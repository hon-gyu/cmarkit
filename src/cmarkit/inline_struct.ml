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

type extra_inline_container_marks =
  { start : byte_pos;
    char : char;
    kind : Inline.Extra_inline_container.kind;
    curly : bool;
    may_open : bool;
    may_close : bool }

type strikethrough_marks =
  { start : byte_pos;
    may_open : bool;
    may_close : bool }

type math_span_marks =
  { start : byte_pos;
    count : int;
    may_open : bool;
    may_close : bool; }

type attribute_spec =
  { start : byte_pos;
    attribute : Attribute.t;
    endline : line_span;
    next : byte_pos }

type token =
| Attribute_spec of attribute_spec
| Autolink_or_html_start of { start : byte_pos }
| Backticks of
    { start : byte_pos;
      count : int;
      escaped : bool }
| Emphasis_marks of emphasis_marks
| Extra_inline_container_marks of extra_inline_container_marks
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
| Wikilink_start of { start : byte_pos; embed : bool }
| Jsx_expr_start of { start : byte_pos }
| Jsx_element_start of { start : byte_pos }

let token_start = function
| Attribute_spec { start } | Autolink_or_html_start { start } | Backticks { start }
| Emphasis_marks { start } | Extra_inline_container_marks { start }
| Inline { start } -> start |  Link_start { start }
| Newline { start } | Right_brack { start } -> start
| Right_paren { start } -> start
| Strikethrough_marks { start } -> start
| Math_span_marks { start } -> start
| Wikilink_start { start } -> start
| Jsx_expr_start { start } -> start
| Jsx_element_start { start } -> start

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

let extra_inline_container_closer_pos ~char ~curly ~after cidx =
  Closer_index.closer_pos
    (Closer.Extra_inline_container_marks (char, curly)) ~after cidx

let has_extra_inline_container_closer ~char ~curly ~after cidx =
  Closer_index.closer_exists
    (Closer.Extra_inline_container_marks (char, curly)) ~after cidx

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
  | Extra_inline_container_marks
      { start; char; curly; may_close = true; _ } as t :: toks ->
      let cidx =
        Closer_index.add
          (Closer.Extra_inline_container_marks (char, curly)) start cidx
      in
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

(* Obsidian wikilinks. [start] points at the opening '[' (link form) or '!'
   (embed form). We require a "[[" opener and a "]]" closer on the same line
   (Obsidian wikilinks never span lines). When found, we emit a single
   [Wikilink_start] token and advance the tokenizer past the whole "]]" so the
   opaque inner content is never tokenized: no stray brackets pollute link
   matching or the closer index, and the inline itself is built later from the
   raw string in [try_wikilink]. Returns [None] to fall back to ordinary link /
   image-link handling. *)
let try_add_wikilink_token oymarkit_mod acc s line ~embed ~start =
  if not (Oymarkit_mod.wikilink oymarkit_mod) then None else
  let bopen = if embed then start + 1 else start in
  if bopen + 1 > line.last || s.[bopen] <> '[' || s.[bopen + 1] <> '[' then None
  else
  let content_start = bopen + 2 in
  let rec find_close k =
    if k + 1 > line.last then None
    else if s.[k] = ']' && s.[k + 1] = ']' then Some k
    else find_close (k + 1)
  in
  match find_close content_start with
  | None -> None
  | Some close -> Some (Wikilink_start { start; embed } :: acc, close + 2)

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
  (* A delimiter that is neither left- nor right-flanking can never open or
     close on its own and is dropped. An explicit marker overrides this: it
     forces its role regardless of flanking (see [emphasis_may_open_close]), so
     it must survive tokenization. *)
  if
    (not is_left_flanking) && (not is_right_flanking) && not open_marker
    && not close_marker
  then acc, next
  else
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

let extra_inline_container_kind_of_char = function
| '=' -> Some Inline.Extra_inline_container.Highlight
| '^' -> Some Inline.Extra_inline_container.Superscript
| '~' -> Some Inline.Extra_inline_container.Subscript
| '+' -> Some Inline.Extra_inline_container.Inserted
| '-' -> Some Inline.Extra_inline_container.Deleted
| _ -> None

let extra_inline_container_syntax oymarkit_mod kind =
  Oymarkit_mod.extra_inline_container_syntax oymarkit_mod kind

let try_add_extra_inline_container_opener_token oymarkit_mod acc s line ~start =
  let next = start + 1 in
  if next > line.last then None
  else
    match extra_inline_container_kind_of_char s.[next] with
    | None -> None
    | Some kind ->
        let open Inline.Extra_inline_container.Config in
        begin match extra_inline_container_syntax oymarkit_mod kind with
        | Disabled -> None
        | Curly_required | Curly_optional ->
            Some
              ( Extra_inline_container_marks
                  { start; char = s.[next]; kind; curly = true;
                    may_open = true; may_close = false }
                :: acc,
                next + 1 )
        end

let try_add_extra_inline_container_closer_token oymarkit_mod acc s line ~start =
  let marker = start + 1 in
  if marker > line.last || s.[marker] <> '}' then None
  else
    match extra_inline_container_kind_of_char s.[start] with
    | None -> None
    | Some kind ->
        let open Inline.Extra_inline_container.Config in
        begin match extra_inline_container_syntax oymarkit_mod kind with
        | Disabled -> None
        | Curly_required | Curly_optional ->
            Some
              ( Extra_inline_container_marks
                  { start; char = s.[start]; kind; curly = true;
                    may_open = false; may_close = true }
                :: acc,
                marker + 1 )
        end

let try_add_extra_inline_container_marks_token oymarkit_mod acc s line ~start =
  match extra_inline_container_kind_of_char s.[start] with
  | None -> None
  | Some kind ->
      let open Inline.Extra_inline_container.Config in
      begin match extra_inline_container_syntax oymarkit_mod kind with
      | Disabled | Curly_required -> None
      | Curly_optional ->
          let first = line.first and last = line.last in
          let prev_uchar = Match.prev_uchar s ~first ~before:start in
          let next_uchar = Match.next_uchar s ~last ~after:start in
          let may_close =
            not (Cmarkit_data.is_unicode_whitespace prev_uchar)
          in
          let may_open =
            not (Cmarkit_data.is_unicode_whitespace next_uchar)
          in
          if not may_open && not may_close then None else
          Some
            ( Extra_inline_container_marks
                { start; char = s.[start]; kind; curly = false; may_open;
                  may_close }
              :: acc,
              start + 1 )
      end

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

let scan_attribute_spec s line lines ~start =
  let b = Buffer.create 32 in
  let rec loop line lines k in_quote escaped in_comment =
    if k > line.last then
      match lines with
      | [] -> None
      | next_line :: lines ->
          Buffer.add_char b '\n';
          loop next_line lines next_line.first in_quote false in_comment
    else
      let c = s.[k] in
      if in_comment then begin
        Buffer.add_char b c;
        loop line lines (k + 1) in_quote false (c <> '%')
      end else if escaped then begin
        Buffer.add_char b c;
        loop line lines (k + 1) in_quote false false
      end else
      match c with
      | '\\' when in_quote ->
          Buffer.add_char b c;
          loop line lines (k + 1) in_quote true false
      | '"' ->
          Buffer.add_char b c;
          loop line lines (k + 1) (not in_quote) false false
      | '%' when not in_quote ->
          Buffer.add_char b c;
          loop line lines (k + 1) false false true
      | '}' when not in_quote ->
          begin match Attribute.of_string (Buffer.contents b) with
          | None -> None
          | Some attribute ->
              Some
                ({ start; attribute; endline = line; next = k + 1 }, lines)
          end
      | c ->
          Buffer.add_char b c;
          loop line lines (k + 1) in_quote false false
  in
  loop line lines (start + 1) false false false

(* JSX expression [ {expr} ]. Find the byte index of the '}' that closes
   the '{' at [start], on the same line ([expr] does not span lines in v1).
   Tracks brace nesting and skips OCaml string literals and [ (* *) ] comments
   (which nest) so that a '}' inside them does not close the expression. Char
   literals containing braces are not handled in v1; the downstream OCaml parser
   validates the extracted span. Returns [None] if no matching close is on the
   line, so the caller can fall back to other interpretations of '{'. *)
let jsx_expr_close s ~last ~start =
  let rec code k depth =
    if k > last then None else
    match s.[k] with
    | '{' -> code (k + 1) (depth + 1)
    | '}' -> if depth = 1 then Some k else code (k + 1) (depth - 1)
    | '"' -> string (k + 1) depth
    | '(' when k + 1 <= last && s.[k + 1] = '*' -> comment (k + 2) depth 1
    | _ -> code (k + 1) depth
  and string k depth =
    if k > last then None else
    match s.[k] with
    | '\\' when k + 1 <= last -> string (k + 2) depth
    | '"' -> code (k + 1) depth
    | _ -> string (k + 1) depth
  and comment k depth nest =
    if k > last then None else
    if k + 1 <= last && s.[k] = '*' && s.[k + 1] = ')'
    then (if nest = 1 then code (k + 2) depth else comment (k + 2) depth (nest - 1))
    else if k + 1 <= last && s.[k] = '(' && s.[k + 1] = '*'
    then comment (k + 2) depth (nest + 1)
    else comment (k + 1) depth nest
  in
  code (start + 1) 1

let try_add_jsx_expr_token oymarkit_mod acc s line ~start =
  if not (Oymarkit_mod.jsx_expr oymarkit_mod) then None else
  match jsx_expr_close s ~last:line.last ~start with
  | None -> None
  | Some close -> Some (Jsx_expr_start { start } :: acc, close + 1)

(* A faithful (single-line, self-closing) JSX element recognizer. [start] points
   at '<'. Returns the byte index of the closing '>' of a well-formed
   [ <name attrs.. /> ], or [None]. Only the self-closing form on a single line
   is recognized for now; container tags with children (e.g. [ <a>b</a> ]) and
   elements spanning lines are not yet handled.

   The decision is pure JSX validity and has nothing to do with autolinks or raw
   HTML: an invalid tag name, a malformed attribute, or a bare '>' (a
   non-self-closing open tag) all yield [None], which simply leaves the '<' for
   the autolink / raw-HTML branches to interpret. Unlike MDX we never raise: an
   invalid element is not an error, just "not a JSX element".

   Case is irrelevant, exactly as in JSX: [ <img/> ] and [ <Foo/> ] are both
   elements. Whether a tag lowers to a host element or a component identifier is
   a downstream choice, not a parsing rule.

   Grammar recognized (ws is ' ' or '\t'; single line):
   {v
     element := '<' name (ws+ attr)* ws* '/' '>'
     name    := ident (('.' ident)+ | ':' ident)?      (* member OR namespaced *)
     attr    := '{' expr '}'                            (* opaque brace expr    *)
              | ident (':' ident)? (ws* '=' ws* value)? (* value optional       *)
     value   := '"' .. '"' | '\'' .. '\'' | '{' expr '}'
     ident   := [A-Za-z_$] [A-Za-z0-9_$-]*
   v}
   Brace expressions [ {expr} ] reuse [jsx_expr_close] to find their matching
   '}', so a '>' or '/' inside them does not end the element. Their contents are
   kept opaque (delimited, not interpreted here). Consequently a brace that
   stands alone as an attribute is accepted as-is rather than required to be a
   JS-style spread [ {...expr} ]: validating and interpreting it is left to
   whatever consumes the [expr]. *)
let jsx_element_close s ~last ~start =
  let is_id_start c =
    (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c = '_' || c = '$'
  in
  let is_id_part c =
    is_id_start c || (c >= '0' && c <= '9') || c = '-'
  in
  let ident k = (* one identifier at [k]; returns position just after it *)
    if k > last || not (is_id_start s.[k]) then None else
    let rec more k = if k <= last && is_id_part s.[k] then more (k + 1) else k in
    Some (more (k + 1))
  in
  let name k = (* JSXElementName; returns position just after the name *)
    match ident k with
    | None -> None
    | Some k ->
        if k <= last && s.[k] = '.' then
          let rec members k = (* [k] at '.' *)
            match ident (k + 1) with
            | None -> None
            | Some k -> if k <= last && s.[k] = '.' then members k else Some k
          in
          members k
        else if k <= last && s.[k] = ':' then ident (k + 1)
        else Some k
  in
  let skip_ws k =
    let rec loop k =
      if k <= last && (s.[k] = ' ' || s.[k] = '\t') then loop (k + 1) else k
    in
    loop k
  in
  let value k = (* attribute value at [k]; returns position just after it *)
    if k > last then None else
    match s.[k] with
    | ('"' | '\'') as q ->
        let rec str k =
          if k > last then None else
          if s.[k] = q then Some (k + 1) else str (k + 1)
        in
        str (k + 1)
    | '{' ->
        (match jsx_expr_close s ~last ~start:k with
         | None -> None
         | Some close -> Some (close + 1))
    | _ -> None (* unquoted values are not valid JSX *)
  in
  let attr k = (* one attribute at [k]; returns position just after it *)
    if k > last then None else
    if s.[k] = '{' then (* spread / opaque brace expression *)
      (match jsx_expr_close s ~last ~start:k with
       | None -> None
       | Some close -> Some (close + 1))
    else
      match ident k with (* attribute name, optionally namespaced *)
      | None -> None
      | Some k ->
          match (if k <= last && s.[k] = ':' then ident (k + 1) else Some k) with
          | None -> None
          | Some k ->
              let j = skip_ws k in
              if j <= last && s.[j] = '=' then value (skip_ws (j + 1))
              else Some k (* boolean attribute; leave separating ws to caller *)
  in
  let rec attrs k = (* [k] just after the name or the previous attribute *)
    let j = skip_ws k in
    if j + 1 <= last && s.[j] = '/' && s.[j + 1] = '>' then Some (j + 1)
    else if j = k then None (* attributes must be whitespace-separated *)
    else match attr j with None -> None | Some k -> attrs k
  in
  match name (start + 1) with
  | None -> None
  | Some k -> attrs k

let try_add_jsx_element_token oymarkit_mod acc s line ~start =
  if not (Oymarkit_mod.jsx_element oymarkit_mod) then None else
  (* Claim '<' purely by JSX validity; on failure return [None] and let the
     autolink / raw-HTML branches interpret it. See [jsx_element_close]. *)
  match jsx_element_close s ~last:line.last ~start with
  | None -> None
  | Some close -> Some (Jsx_element_start { start } :: acc, close + 1)

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
    let jumped = ref None in
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
            begin match try_add_jsx_expr_token oymarkit_mod acc s line ~start:k with
            | Some r -> r
            | None ->
            begin match
              try_add_extra_inline_container_opener_token oymarkit_mod acc s line
                ~start:k
            with
            | Some r -> r
            | None ->
                begin match
                  if Oymarkit_mod.djot_inline_attributes oymarkit_mod
                  then scan_attribute_spec s line lines ~start:k
                  else None
                with
                | Some (spec, remaining) ->
                    jumped := Some (remaining, spec.endline);
                    Attribute_spec spec :: acc, spec.next
                | None ->
                    try_add_marked_emphasis_opener_token oymarkit_mod acc s line
                      ~start:k
                end
            end
            end
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
    | '[' ->
        begin match oymarkit_mod with
        | Some oymarkit_mod when is_oymarkit_enabled () ->
            begin match
              try_add_wikilink_token oymarkit_mod acc s line ~embed:false
                ~start:k
            with
            | Some r -> r
            | None -> Link_start { start = k; image = false } :: acc, k + 1
            end
        | _ -> Link_start { start = k; image = false } :: acc, k + 1
        end
    | '!' ->
        begin match oymarkit_mod with
        | Some oymarkit_mod when is_oymarkit_enabled () ->
            begin match
              try_add_wikilink_token oymarkit_mod acc s line ~embed:true ~start:k
            with
            | Some r -> r
            | None -> try_add_image_link_start_token acc s line ~start:k
            end
        | _ -> try_add_image_link_start_token acc s line ~start:k
        end
    | '<' ->
        begin match oymarkit_mod with
        | Some oymarkit_mod when is_oymarkit_enabled () ->
            begin match try_add_jsx_element_token oymarkit_mod acc s line ~start:k with
            | Some r -> r
            | None -> Autolink_or_html_start { start = k } :: acc, k + 1
            end
        | _ -> Autolink_or_html_start { start = k } :: acc, k + 1
        end
    | ')' -> Right_paren { start = k } :: acc, k + 1
    | '=' | '^' | '+' | '-' ->
        begin match oymarkit_mod with
        | Some oymarkit_mod when is_oymarkit_enabled () ->
            begin match
              try_add_extra_inline_container_closer_token oymarkit_mod acc s line
                ~start:k
            with
            | Some r -> r
            | None ->
                begin match
                  try_add_extra_inline_container_marks_token oymarkit_mod acc s
                    line ~start:k
                with
                | Some r -> r
                | None -> acc, k + 1
                end
            end
        | _ -> acc, k + 1
        end
    | '~' ->
        begin match oymarkit_mod with
        | Some oymarkit_mod when is_oymarkit_enabled () ->
            begin match
              try_add_extra_inline_container_closer_token oymarkit_mod acc s line
                ~start:k
            with
            | Some r -> r
            | None ->
                if exts && k < line.last && s.[k + 1] = '~'
                then try_add_strikethrough_marks_token acc s line ~start:k
                else
                  begin match
                    try_add_extra_inline_container_marks_token oymarkit_mod acc
                      s line ~start:k
                  with
                  | Some r -> r
                  | None ->
                      if exts then
                        try_add_strikethrough_marks_token acc s line ~start:k
                      else acc, k + 1
                  end
            end
        | _ ->
            if exts then try_add_strikethrough_marks_token acc s line ~start:k
            else acc, k + 1
        end
    | '$' when exts -> try_add_math_span_marks_token acc s line ~start:k
    | _ -> acc, k + 1
    in
    match !jumped with
    | None -> loop ~exts s lines line ~prev_bslash:false acc next
    | Some (lines, line) ->
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

let ext_extra_inline_container_token p ~kind ~first ~last ~first_line ~last_line i =
  let textloc = textloc_of_lines p ~first ~last ~first_line ~last_line in
  let c = Inline.Extra_inline_container.make kind i in
  let inline = Inline.Ext_extra_inline_container (c, meta p textloc) in
  Inline { start = first; inline; endline = last_line; next = last + 1 }

let ext_math_span_token p ~count ~first ~last ~first_line ~last_line rspans =
  let textloc = textloc_of_lines p ~first ~last ~first_line ~last_line in
  let tex_layout = raw_tight_block_lines p ~rev_spans:rspans in
  let meta = meta p textloc in
  let ms = Inline.Math_span.make ~display:(count = 2) tex_layout in
  let inline = Inline.Ext_math_span (ms, meta) in
  Inline { start = first; inline; endline = last_line; next = last + 1 }

let ext_wikilink_token p ~first ~last ~line wl =
  let textloc = textloc_of_span p { line with first; last } in
  let inline = Inline.Ext_wikilink (wl, meta p textloc) in
  Inline { start = first; inline; endline = line; next = last + 1 }

let ext_jsx_expr_token p ~first ~last ~line j =
  (* [first] is the '{', [last] is the '}'. The node's textloc spans the whole
     [ {expr} ] so the consumer can offset past the '{' to prime a lexbuf at the
     expression's true source position. *)
  let textloc = textloc_of_span p { line with first; last } in
  let inline = Inline.Ext_jsx_expr (j, meta p textloc) in
  Inline { start = first; inline; endline = line; next = last + 1 }

let ext_jsx_element_token p ~first ~last ~line e =
  (* [first] is the '<', [last] is the closing '>'. The node's textloc spans the
     whole element so the consumer can recover attribute expressions' absolute
     positions by offset into [raw]. *)
  let textloc = textloc_of_span p { line with first; last } in
  let inline = Inline.Ext_jsx_element (e, meta p textloc) in
  Inline { start = first; inline; endline = line; next = last + 1 }

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

let try_wikilink p toks line ~embed ~start =
  (* [start] points at '[' (link) or '!' (embed). The opener and a "]]" closer
     are guaranteed on [line] by [try_add_wikilink_token]; we re-scan the raw
     string to delimit the content and build the inline with proper metadata. *)
  let content_start = (if embed then start + 1 else start) + 2 in
  let rec find_close k =
    if k + 1 > line.last then None
    else if p.i.[k] = ']' && p.i.[k + 1] = ']' then Some k
    else find_close (k + 1)
  in
  match find_close content_start with
  | None -> None
  | Some close ->
      let content = String.sub p.i content_start (close - content_start) in
      let wl = Inline.Wikilink.make ~embed content in
      let first = start and last = close + 1 (* second ']' *) in
      let t = ext_wikilink_token p ~first ~last ~line wl in
      let toks = drop_until ~start:(last + 1) toks in
      Some (toks, line, t)

let try_jsx_expr p toks line ~start =
  (* [start] points at '{'. A matching '}' on [line] is guaranteed by
     [try_add_jsx_expr_token]; re-scan to delimit the opaque expression and
     build the inline with proper metadata. *)
  match jsx_expr_close p.i ~last:line.last ~start with
  | None -> None
  | Some close ->
      let expr = String.sub p.i (start + 1) (close - (start + 1)) in
      let j = Inline.Jsx_expr.make expr in
      let t = ext_jsx_expr_token p ~first:start ~last:close ~line j in
      let toks = drop_until ~start:(close + 1) toks in
      Some (toks, line, t)

let try_jsx_element p toks line ~start =
  (* [start] points at '<'. A self-closing element ending in [ /> ] on [line] is
     guaranteed by [try_add_jsx_element_token]; re-scan to delimit it and build
     the inline with the full verbatim source in [raw]. *)
  match jsx_element_close p.i ~last:line.last ~start with
  | None -> None
  | Some close ->
      let raw = String.sub p.i start (close - start + 1) in
      let e = Inline.Jsx_element.make raw in
      let t = ext_jsx_element_token p ~first:start ~last:close ~line e in
      let toks = drop_until ~start:(close + 1) toks in
      Some (toks, line, t)

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
  | Attribute_spec _ as t :: toks -> loop toks line nest (t :: acc)
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
            (* OYMARKIT CHANGE: was [start_line == line], a physical-equality
               test used as a proxy for "the closing ] is on the same source
               line as the opening [". That proxy is unsound: [try_code] (and
               the other inline scanners) rebuild the line record via
               [{ start_line with first }], so a code span before a nested
               link/image breaks physical identity even though we are still on
               the same line. The [else] branch then over-extends [last] to the
               whole line, leaking the trailing "](dest)" into the link text as
               literal text. Compare line positions instead. *)
            if start_line.line_pos = line.line_pos
            then text_last - 1 else start_line.last
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
  | Attribute_spec _ as t :: toks ->
      loop p toks line ~had_link (t :: acc)
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
  | Wikilink_start { start; embed } :: toks ->
      begin match try_wikilink p toks line ~embed ~start with
      | None -> loop p toks line ~had_link acc
      | Some (toks, line, t) -> loop p toks line ~had_link (t :: acc)
      end
  | Jsx_expr_start { start } :: toks ->
      begin match try_jsx_expr p toks line ~start with
      | None -> loop p toks line ~had_link acc
      | Some (toks, line, t) -> loop p toks line ~had_link (t :: acc)
      end
  | Jsx_element_start { start } :: toks ->
      begin match try_jsx_element p toks line ~start with
      | None -> loop p toks line ~had_link acc
      | Some (toks, line, t) -> loop p toks line ~had_link (t :: acc)
      end
  | Right_brack start :: toks -> loop p toks line ~had_link acc
  | Newline { newline = l } as t :: toks -> loop p toks l ~had_link (t :: acc)
  | t :: toks -> loop p toks line ~had_link (t :: acc)
  in
  loop p toks line ~had_link:false []

(* Second pass *)

and find_emphasis_text p toks line ~(opener : emphasis_marks) =
  let marks_match ~(marks : emphasis_marks) ~(opener : emphasis_marks) =
    (opener.char = marks.char) &&
    (not (marks.may_open || opener.may_close) ||
      marks.count mod 3 = 0 || (opener.count + marks.count) mod 3 != 0)
  in
  let marks_has_precedence p ~(marks : emphasis_marks)
      ~(opener : emphasis_marks) =
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
          | Some { used; strong } ->
              let to_last = marks.start - 1 in
              let acc = rev_tokens_and_shorten_last_line ~to_last [] acc in
              Either.Right (toks, line, used, strong, acc, marks)
          | None ->
              if has_emphasis_closer ~char:opener.char ~after p.cidx
              then loop p toks line (t :: acc) ~opener
              else Either.Left (List.rev_append (t :: acc) toks)
        end else begin
          let used = if marks.count >= 2 && opener.count >= 2 then 2 else 1 in
          let strong = used = 2 in
          let to_last = marks.start - 1 in
          let acc = rev_tokens_and_shorten_last_line ~to_last [] acc in
          Either.Right (toks, line, used, strong, acc, marks)
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
  | Either.Right (toks, line, used, strong, emph_toks, closer) ->
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
            (* OYMARKIT CHANGE: line_pos compare instead of physical equality;
               see the note in [try_link]. *)
            if start_line.line_pos = line.line_pos
            then text_last else start_line.last
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
            (* OYMARKIT CHANGE: line_pos compare instead of physical equality;
               see the note in [try_link]. *)
            if start_line.line_pos = line.line_pos
            then last else start_line.last
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

and find_extra_inline_container_text p toks start_line ~opener =
  let closer_pos =
    match
      extra_inline_container_closer_pos ~char:opener.char ~curly:opener.curly
        ~after:opener.start p.cidx
    with
    | Some pos -> pos
    | None -> assert false
  in
  let rec loop p toks line acc = match toks with
  | [] -> Either.Left (List.rev acc)
  | Extra_inline_container_marks marks :: toks ->
      if
        marks.may_close && marks.char = opener.char
        && marks.curly = opener.curly
      then
        let to_last = marks.start - 1 in
        let acc = rev_tokens_and_shorten_last_line ~to_last [] acc in
        Either.Right (toks, line, acc, marks)
      else if marks.may_open then
        begin match
          extra_inline_container_closer_pos ~char:marks.char
            ~curly:marks.curly ~after:marks.start p.cidx
        with
        | Some nested_closer_pos when nested_closer_pos <= closer_pos ->
            begin match try_extra_inline_container p toks line ~opener:marks with
            | Either.Left toks -> loop p toks line acc
            | Either.Right (toks, line) -> loop p toks line acc
            end
        | None | Some _ ->
            loop p toks line (Extra_inline_container_marks marks :: acc)
        end
      else loop p toks line (Extra_inline_container_marks marks :: acc)
  | Newline { newline = l } as t :: toks -> loop p toks l (t :: acc)
  | Inline { endline = l } as t :: toks -> loop p toks l (t :: acc)
  | t :: toks -> loop p toks line (t :: acc)
  in
  loop p toks start_line []

and try_extra_inline_container p start_toks start_line ~opener =
  let start = opener.start in
  if
    not
      (has_extra_inline_container_closer ~char:opener.char
         ~curly:opener.curly ~after:start p.cidx)
  then Either.Left start_toks else
  match find_extra_inline_container_text p start_toks start_line ~opener with
  | Either.Left _ as r -> r
  | Either.Right (toks, line, contained_toks, closer) ->
      let first_line = start_line and last_line = line in
      let text =
        let first = start + (if opener.curly then 2 else 1) in
        let last = closer.start - 1 in
        let text_start =
          let last =
            (* OYMARKIT CHANGE: line_pos compare instead of physical equality;
               see the note in [try_link]. *)
            if start_line.line_pos = line.line_pos
            then last else start_line.last
          in
          { start_line with first; last }
        in
        let contained_toks = second_pass p contained_toks text_start in
        let text = last_pass p contained_toks text_start in
        inlines_inline p text ~first ~last ~first_line ~last_line
      in
      let toks =
        let first = opener.start in
        let last = closer.start + (if closer.curly then 1 else 0) in
        ext_extra_inline_container_token p ~kind:opener.kind ~first ~last
          ~first_line ~last_line text
        :: toks
      in
      Either.Right (toks, line)

and second_pass p toks line =
  let rec loop p toks line acc = match toks with
  | [] -> List.rev acc
  | Attribute_spec _ as t :: toks -> loop p toks line (t :: acc)
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
  | Extra_inline_container_marks ({ may_open } as opener) :: toks ->
      if not may_open then loop p toks line acc else
      begin match try_extra_inline_container p toks line ~opener with
      | Either.Left toks -> loop p toks line acc
      | Either.Right (toks, line) -> loop p toks line acc
      end
  | Newline { newline } as t :: toks -> loop p toks newline (t :: acc)
  | Inline { endline } as t :: toks -> loop p toks endline (t :: acc)
  | t :: toks -> loop p toks line (t :: acc)
  in
  loop p toks line []

(* Last pass *)

and last_pass p toks start_line =
  (* Only [Inline] and [Newline] tokens remain. We fold over them to
      convert them to [inline] values and [Break]s. [Text] inlines
      are created for data between them. *)
  let rec loop toks line acc k = match toks with
  | [] ->
      (* OYMARKIT CHANGE: bound the trailing text to the text span, not to the
         whole physical line. [line.last] is the end of the current source
         line; but when the last token is an emphasis (or code span) it rebuilds
         the line record and hands back one that still points at the real end of
         line, past the closing [ ] ] of an enclosing link/image. The fill below
         would then leak that enclosing "](dest)" in as literal text. While we
         are still on the line we started on, cap at [start_line.last] (the
         intended text-span end); only on a later line is [line.last] the right
         bound. Same [line_pos] idiom as the [try_link] fix. *)
      let last =
        if start_line.line_pos = line.line_pos then start_line.last else line.last
      in
      List.rev (try_add_text_inline p line ~first:k ~last acc)
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
  | Attribute_spec { start; attribute; endline; next } :: toks
    when Attribute.is_empty attribute ->
      (* Comment-only (or empty) specifier: Djot drops it from the output.
         Flush any pending text up to the specifier, then skip it entirely
         (it neither attaches to a target nor renders literally). *)
      let acc = try_add_text_inline p line ~first:k ~last:(start - 1) acc in
      loop toks endline acc next
  | Attribute_spec { start; attribute; endline; next } :: toks ->
      let wrap target specs =
        let attrs = Inline.Attributes.make ~specs target in
        Inline.Ext_attributes (attrs, Inline.meta target)
      in
      let add_to_target acc =
        match acc with
        | Inline.Ext_attributes (a, meta) :: acc ->
            let specs = Inline.Attributes.specs a @ [attribute] in
            Inline.Ext_attributes
              (Inline.Attributes.make ~specs (Inline.Attributes.inline a), meta)
            :: acc
        | (Inline.Break _ | Inline.Inlines _) :: _ | [] -> []
        | target :: acc -> wrap target [attribute] :: acc
      in
      if k = start then begin
        match add_to_target acc with
        | [] ->
            let literal = "{" ^ Attribute.to_string attribute ^ "}" in
            loop toks endline (Inline.Text (literal, Meta.none) :: acc) next
        | acc -> loop toks endline acc next
      end else begin
        let last = start - 1 in
        let rec target_first i =
          if i < k then k else
          match p.i.[i] with
          | ' ' | '\t' -> i + 1
          | _ -> target_first (i - 1)
        in
        let first = target_first last in
        if first > last then
          let acc = try_add_text_inline p line ~first:k ~last acc in
          let literal = "{" ^ Attribute.to_string attribute ^ "}" in
          loop toks endline (Inline.Text (literal, Meta.none) :: acc) next
        else
          let acc = try_add_text_inline p line ~first:k ~last:(first - 1) acc in
          let target =
            Inline.Text (clean_unesc_unref_span p { line with first; last })
          in
          loop toks endline (wrap target [attribute] :: acc) next
      end
  | (Backticks _ | Autolink_or_html_start _ | Link_start _ | Right_brack _
    | Emphasis_marks _ | Right_paren _ | Strikethrough_marks _
    | Extra_inline_container_marks _ | Math_span_marks _
    | Wikilink_start _ | Jsx_expr_start _ | Jsx_element_start _) :: _ ->
      assert false
  in
  loop toks start_line [] start_line.first

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
| (Attribute_spec _ | Backticks _ | Autolink_or_html_start _ | Link_start _ | Right_brack _
  | Emphasis_marks _ | Right_paren _ | Strikethrough_marks _
  | Extra_inline_container_marks _ | Math_span_marks _ | Newline _
  | Wikilink_start _ | Jsx_expr_start _ | Jsx_element_start _ ) :: _ ->
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
| (Attribute_spec _ | Backticks _ | Autolink_or_html_start _ | Link_start _ | Right_brack _
  | Emphasis_marks _ | Right_paren _ | Strikethrough_marks _
  | Extra_inline_container_marks _ | Math_span_marks _ | Newline _
  | Wikilink_start _ | Jsx_expr_start _ | Jsx_element_start _ ) :: _ ->
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

(* Structural colon splitting for the Struct pass
   =======================================================

   The Struct pass ({!Struct_}) turns colon-keyed paragraphs and list items
   into keyed nodes. Finding *where* the structural colon is belongs here,
   during inline parsing, for the same reason table-cell splitting does (see
   [parse_table_row] above):

   {ul
   {- Only at the token level is a colon known to be {e top-level}: colons
      inside code spans, emphasis, links, etc. are buried inside [Inline]
      tokens and never seen here.}
   {- Only [Match.first_non_escaped_char] distinguishes an escaped [\:] from a
      real one. The assembled [Text] nodes are already unescaped (see
      [clean_unesc_unref_span]), so [\:] and [:] are indistinguishable there.}}

   A {e structural} colon is an unescaped, top-level [:] that is either followed
   by a blank (a [": "] separator) or ends the paragraph (a trailing [:]). We
   split the top-level inline stream at every such colon — the exact analogue of
   [parse_cols] splitting at every [|]. The resulting segments are the chain
   labels followed, for the separator form, by a final value segment. *)

let is_blank_byte = function ' ' | '\t' -> true | _ -> false

(* First structural colon in [start .. last] of [p.i], or [last + 1] if none.
   [plast] is the last byte of the whole paragraph, for trailing detection. *)
let rec next_key_colon p ~start ~last ~plast =
  let n = Match.first_non_escaped_char ':' p.i ~last ~start in
  if n > last then last + 1
  else if n = plast || is_blank_byte p.i.[n + 1] then n
  else next_key_colon p ~start:(n + 1) ~last ~plast

(* [seg_rev] is the current segment's inlines in reverse order. *)
let segment_inline p seg_rev = match seg_rev with
| [] -> Inline.Inlines ([], Meta.none)
| [i] -> i
| last_i :: _ ->
    let is = List.rev seg_rev in
    let first = Inline.meta (List.hd is) and last = Inline.meta last_i in
    Inline.Inlines (is, meta_of_metas p ~first ~last)

let add_text p line ~first ~last seg_rev =
  if first > last then seg_rev else
  Inline.Text (clean_unesc_unref_span p { line with first; last }) :: seg_rev

(* Split the source text range [k .. tlast] at structural colons, threading the
   current segment and the completed segments (both reversed). A colon closes
   the current segment. The label keeps its {e raw} separator -- the colon and
   the whitespace around it stay in the label's text -- and the value begins at
   the first non-blank after the colon. Concatenating the segments thus
   reproduces the source (modulo [Inline.normalize]), which is what lets the
   Struct rewrite be content-preserving: the label's trailing ":" is real
   content, not something the renderer has to synthesise (see {!Struct_.unkey}).
   Because the next segment always starts on a non-blank, labels never carry a
   leading blank, so a non-[Text] key arrives as [Inlines [unit; Text sep]]. *)
let rec emit_text p line ~plast ~k ~tlast seg_rev segs_rev =
  if k > tlast then seg_rev, segs_rev else
  let n = next_key_colon p ~start:k ~last:tlast ~plast in
  if n > tlast then add_text p line ~first:k ~last:tlast seg_rev, segs_rev else
  let value_start = Match.first_non_blank p.i ~last:tlast ~start:(n + 1) in
  let seg_rev = add_text p line ~first:k ~last:(value_start - 1) seg_rev in
  let segs_rev = segment_inline p seg_rev :: segs_rev in
  emit_text p line ~plast ~k:value_start ~tlast [] segs_rev

(* Like [last_pass], but splits the top-level stream into segments at structural
   colons instead of assembling one flat inline list. After the first two passes
   only [Inline], [Newline] and -- in extension mode -- [Attribute_spec] tokens
   remain. [Inline]/[Newline] split into segments; an [Attribute_spec] is folded
   into its target inline exactly as [last_pass] does (see the dedicated arm),
   so the segment stream stays consistent with the normal inline and the rewrite
   stays content-invisible. *)
let keyed_last_pass p toks start_line ~plast =
  let rec loop toks line seg_rev segs_rev k = match toks with
  | [] ->
      let last =
        if start_line.line_pos = line.line_pos then start_line.last else line.last
      in
      let seg_rev, segs_rev = emit_text p line ~plast ~k ~tlast:last seg_rev segs_rev in
      List.rev (segment_inline p seg_rev :: segs_rev)
  | Newline { start; break_type; newline } :: toks ->
      let seg_rev, segs_rev =
        emit_text p line ~plast ~k ~tlast:(start - 1) seg_rev segs_rev
      in
      let break = break_inline p line ~start ~break_type ~newline in
      loop toks newline (break :: seg_rev) segs_rev newline.first
  | Inline { start; inline; endline; next } :: toks ->
      let seg_rev, segs_rev =
        emit_text p line ~plast ~k ~tlast:(start - 1) seg_rev segs_rev
      in
      let seg_rev = match inline with
      | Inline.Inlines (is, _) -> List.rev_append (List.rev is) seg_rev
      | i -> i :: seg_rev
      in
      loop toks endline seg_rev segs_rev next
  | Attribute_spec { start; attribute; endline; next } :: toks
    when Attribute.is_empty attribute ->
      (* Comment-only (or empty) specifier: Djot drops it. Flush pending text up
         to the specifier, then skip it (mirrors [last_pass]). *)
      let seg_rev, segs_rev =
        emit_text p line ~plast ~k ~tlast:(start - 1) seg_rev segs_rev
      in
      loop toks endline seg_rev segs_rev next
  | Attribute_spec { start; attribute; endline; next } :: toks ->
      (* A djot inline attribute wraps its target in [Ext_attributes]; we attach
         it within the current segment, the same way [last_pass] attaches it to
         its accumulator. The target -- the preceding inline or the last word of
         the pending text -- is a run of non-blanks, so it never spans a
         structural colon and stays in the current segment. *)
      let wrap target =
        Inline.Ext_attributes
          (Inline.Attributes.make ~specs:[attribute] target, Inline.meta target)
      in
      if k = start then begin
        (* No pending text: attach to the last inline of the current segment. *)
        match seg_rev with
        | Inline.Ext_attributes (a, meta) :: rest ->
            let specs = Inline.Attributes.specs a @ [attribute] in
            let merged =
              Inline.Ext_attributes
                (Inline.Attributes.make ~specs (Inline.Attributes.inline a), meta)
            in
            loop toks endline (merged :: rest) segs_rev next
        | (Inline.Break _ | Inline.Inlines _) :: _ | [] ->
            (* Nothing attachable (segment start, or after a break): literal. *)
            let literal = "{" ^ Attribute.to_string attribute ^ "}" in
            loop toks endline (Inline.Text (literal, Meta.none) :: seg_rev) segs_rev next
        | target :: rest -> loop toks endline (wrap target :: rest) segs_rev next
      end else begin
        let last = start - 1 in
        let rec target_first i =
          if i < k then k else
          match p.i.[i] with ' ' | '\t' -> i + 1 | _ -> target_first (i - 1)
        in
        let first = target_first last in
        if first > last then begin
          (* A blank precedes the '{': flush the text (incl. it), emit literal. *)
          let seg_rev, segs_rev =
            emit_text p line ~plast ~k ~tlast:last seg_rev segs_rev
          in
          let literal = "{" ^ Attribute.to_string attribute ^ "}" in
          loop toks endline (Inline.Text (literal, Meta.none) :: seg_rev) segs_rev next
        end else begin
          (* Flush text before the target word, then attach the attr to the word. *)
          let seg_rev, segs_rev =
            emit_text p line ~plast ~k ~tlast:(first - 1) seg_rev segs_rev
          in
          let target = Inline.Text (clean_unesc_unref_span p { line with first; last }) in
          loop toks endline (wrap target :: seg_rev) segs_rev next
        end
      end
  | _ :: _ -> assert false
  in
  loop toks start_line [] [] start_line.first

(* Front of the inline pipeline shared by the keyed variants: everything up to
   (but not including) a final pass over the resolved token stream. *)
let run_keyed_passes p lines =
  let layout, meta, lines = strip_paragraph p lines in
  (* [lines] is forward-ordered here, so the paragraph's last byte (for trailing
     colon detection) is on the last line. *)
  let plast = (List.hd (List.rev lines)).last in
  let cidx, toks, first_line =
    tokenize ~oymarkit_mod:p.oymarkit_mod ~exts:p.exts p.i lines
  in
  p.cidx <- cidx;
  let toks, _had_link = first_pass p toks first_line in
  let toks = second_pass p toks first_line in
  layout, meta, toks, first_line, plast

(* Split a paragraph's [lines] into colon-keyed segments. A result of a single
   segment means there was no structural colon (not keyed). *)
let parse_keyed p lines =
  let _layout, _meta, toks, first_line, plast = run_keyed_passes p lines in
  keyed_last_pass p toks first_line ~plast

(* Meta channel carrying parse-time colon segments to the post-parse Struct pass
   ({!Struct_}); the source needed to detect structural colons (escaping,
   opacity) is only available here, so we ship the segments rather than the
   pass re-deriving them. Mirrors {!Block.Block_id}'s use of a [Meta.key]. *)
let keyed_segments : Inline.t list Meta.key = Meta.key ()

(* Like {!parse} but additionally returns the colon segments ([None] when not
   keyed). Both final passes fold the same resolved token stream. *)
let parse_with_segments p lines =
  let layout, meta, toks, first_line, plast = run_keyed_passes p lines in
  let inline = match last_pass p toks first_line with
  | [i] -> i
  | is -> Inline.Inlines (is, meta)
  in
  let segments = match keyed_last_pass p toks first_line ~plast with
  | [_] -> None
  | segments -> Some segments
  in
  layout, inline, segments
