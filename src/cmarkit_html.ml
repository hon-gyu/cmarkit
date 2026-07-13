[@@@ocamlformat "disable"]

(*---------------------------------------------------------------------------
   Copyright (c) 2021 The cmarkit programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Cmarkit
module C = Cmarkit_renderer.Context
module String_set = Set.Make (String)

(* Renderer state *)

type state =
  { safe : bool;
    backend_blocks : bool;
    djot : bool;
    mutable ids : String_set.t;
    mutable footnote_count : int;
    mutable footnotes :
      (* Text, id, ref count, footnote *)
      (string * string * int ref * Block.Footnote.t) Label.Map.t  }

let state : state C.State.t = C.State.make ()
let safe c = (C.State.get c state).safe
let backend_blocks c = (C.State.get c state).backend_blocks
let djot c = (C.State.get c state).djot
let init_context ?(backend_blocks = false) ?(djot = false) ~safe c _ =
  let ids = String_set.empty and footnotes = Label.Map.empty in
  let st = { safe; backend_blocks; djot; ids; footnote_count = 0; footnotes } in
  C.State.set c state (Some st)

let unique_id c id =
  let st = C.State.get c state in
  let rec loop ids id c =
    let id' = if c = 0 then id else (String.concat "-" [id; Int.to_string c]) in
    match String_set.mem id' ids with
    | true -> loop ids id (c + 1)
    | false -> st.ids <- String_set.add id' ids; id'
  in
  loop st.ids id 0

let footnote_id label =
  let make_label l = String.map (function ' ' | '\t' -> '-' | c -> c) l in
  "fn-" ^ (make_label (String.sub label 1 (String.length label - 1)))

let footnote_ref_id fnid c = String.concat "-" ["ref"; Int.to_string c; fnid]

let make_footnote_ref_ids c label fn =
  let st = C.State.get c state in
  match Label.Map.find_opt label st.footnotes with
  | Some (text, id, refc, _) -> incr refc; (text, id, footnote_ref_id id !refc)
  | None ->
      st.footnote_count <- st.footnote_count + 1;
      let text = String.concat "" ["["; Int.to_string st.footnote_count;"]"] in
      let id = footnote_id label in
      st.footnotes <- Label.Map.add label (text, id, ref 1, fn) st.footnotes;
      text, id, footnote_ref_id id 1

(* Escaping *)

let buffer_add_html_escaped_uchar b u = match Uchar.to_int u with
| 0x0000 -> Buffer.add_utf_8_uchar b Uchar.rep
| 0x0026 (* & *) -> Buffer.add_string b "&amp;"
| 0x003C (* < *) -> Buffer.add_string b "&lt;"
| 0x003E (* > *) -> Buffer.add_string b "&gt;"
(* | 0x0027 (* ' *) -> Buffer.add_string b "&apos;" *)
| 0x0022 (* '\"' *) -> Buffer.add_string b "&quot;"
| _ -> Buffer.add_utf_8_uchar b u

let html_escaped_uchar c s = buffer_add_html_escaped_uchar (C.buffer c) s

let buffer_add_html_escaped_string b s =
  let string = Buffer.add_string in
  let len = String.length s in
  let max_idx = len - 1 in
  let flush b start i =
    if start < len then Buffer.add_substring b s start (i - start);
  in
  let rec loop start i =
    if i > max_idx then flush b start i else
    let next = i + 1 in
    match String.get s i with
    | '\x00' ->
        flush b start i; Buffer.add_utf_8_uchar b Uchar.rep; loop next next
    | '&' -> flush b start i; string b "&amp;"; loop next next
    | '<' -> flush b start i; string b "&lt;"; loop next next
    | '>' -> flush b start i; string b "&gt;"; loop next next
(*    | '\'' -> flush c start i; string c "&apos;"; loop next next *)
    | '\"' -> flush b start i; string b "&quot;"; loop next next
    | c -> loop start next
  in
  loop 0 0

let html_escaped_string c s = buffer_add_html_escaped_string (C.buffer c) s

(* Djot writes a non-breaking space as an entity: as a raw U+00A0 it would be
   indistinguishable from a space in the output. *)
let djot_escaped_string c s =
  let b = C.buffer c in
  let len = String.length s in
  let rec loop start i =
    if i >= len then
      (if start < len then buffer_add_html_escaped_string b
         (String.sub s start (len - start)))
    else if i + 1 < len && s.[i] = '\xc2' && s.[i + 1] = '\xa0' then begin
      if start < i then
        buffer_add_html_escaped_string b (String.sub s start (i - start));
      Buffer.add_string b "&nbsp;";
      loop (i + 2) (i + 2)
    end else loop start (i + 1)
  in
  loop 0 0

let text_string c s =
  if djot c then djot_escaped_string c s else html_escaped_string c s

let attributes c a =
  begin match Attribute.id a with
  | None -> ()
  | Some id ->
      C.string c " id=\""; html_escaped_string c id; C.byte c '"'
  end;
  begin match Attribute.classes a with
  | [] -> ()
  | classes ->
      C.string c " class=\"";
      html_escaped_string c (String.concat " " classes);
      C.byte c '"'
  end;
  List.iter
    (fun (key, value) ->
      C.byte c ' '; html_escaped_string c key; C.string c "=\"";
      html_escaped_string c value; C.byte c '"')
    (Attribute.key_values a)

let buffer_add_pct_encoded_string b s = (* Percent encoded + HTML escaped *)
  let byte = Buffer.add_char and string = Buffer.add_string in
  let unsafe_hexdig_of_int i = match i < 10 with
  | true -> Char.unsafe_chr (i + 0x30)
  | false -> Char.unsafe_chr (i + 0x37)
  in
  let flush b max start i =
    if start <= max then Buffer.add_substring b s start (i - start);
  in
  let rec loop b s max start i =
    if i > max then flush b max start i else
    let next = i + 1 in
    match String.get s i with
    | '%' (* In CommonMark destinations may have percent encoded chars *)
    (* See https://tools.ietf.org/html/rfc3986 *)
    (* unreserved *)
    | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '.' | '_' | '~'
    (* sub-delims *)
    | '!' | '$' | (*'&' | '\'' | *) '(' | ')' | '*' | '+' | ',' | ';' | '='
    (* gen-delims *)
    | ':' | '/' | '?' | '#' | (* '[' | ']' cmark escapes them | *) '@' ->
        loop b s max start next
    | '&' -> flush b max start i; string b "&amp;"; loop b s max next next
    | '\'' -> flush b max start i; string b "&apos;"; loop b s max next next
    | c ->
        flush b max start i;
        let hi = (Char.code c lsr 4) land 0xF in
        let lo = (Char.code c) land 0xF in
        byte b '%';
        byte b (unsafe_hexdig_of_int hi);
        byte b (unsafe_hexdig_of_int lo);
        loop b s max next next
  in
  loop b s (String.length s - 1) 0 0

let pct_encoded_string c s = buffer_add_pct_encoded_string (C.buffer c) s

(* Rendering functions *)

let comment c s =
  C.string c "<!-- "; html_escaped_string c s; C.string c " -->"

let comment_undefined_label c l = match Inline.Link.referenced_label l with
| None -> () | Some def -> comment c ("Undefined label " ^ (Label.key def))

let comment_unknown_def_type c l = match Inline.Link.referenced_label l with
| None -> () | Some def ->
    comment c ("Unknown label definition type for " ^ (Label.key def))

let comment_foonote_image c l = match Inline.Link.referenced_label l with
| None -> () | Some def ->
    comment c ("Footnote " ^ (Label.key def) ^ " referenced as image")

let block_lines c = function (* newlines only between lines *)
| [] -> () | (l, _) :: ls ->
    let line c (l, _) = C.byte c '\n'; C.string c l in
    C.string c l; List.iter (line c) ls

(* Inline rendering *)

let autolink c a =
  let pre = if Inline.Autolink.is_email a then "mailto:" else "" in
  let url = pre ^ (fst (Inline.Autolink.link a)) in
  let url = if Inline.Link.is_unsafe url then "" else url in
  C.string c "<a href=\""; pct_encoded_string c url; C.string c "\">";
  html_escaped_string c (fst (Inline.Autolink.link a));
  C.string c "</a>"

let break c b = match Inline.Break.type' b with
| `Hard -> C.string c "<br>\n"
| `Soft -> C.byte c '\n'

let code_span c cs =
  C.string c "<code>";
  html_escaped_string c (Inline.Code_span.code cs);
  C.string c "</code>"

(* Djot raw content. Only [html] is our output format, so that is the only one
   we pass through; anything targeted at another backend is dropped, as djot
   specifies. In [safe] mode passing it through would defeat the point of the
   mode, so it becomes a comment like the other raw HTML paths. *)

let raw_inline c r =
  if Inline.Raw_inline.format r <> "html" then () else
  if safe c then comment c "raw HTML omitted" else
  C.string c (Inline.Raw_inline.code r)

let raw_block c r =
  if Block.Raw_block.format r <> "html" then () else
  if safe c then (comment c "raw HTML block omitted"; C.byte c '\n') else
  block_lines c (Block.Code_block.code (Block.Raw_block.code_block r))

let emphasis c e =
  C.string c "<em>"; C.inline c (Inline.Emphasis.inline e); C.string c "</em>"

let strong_emphasis c e =
  C.string c "<strong>";
  C.inline c (Inline.Emphasis.inline e);
  C.string c "</strong>"

let link_dest_and_title c ld =
  let dest = match Link_definition.dest ld with
  | None -> ""
  | Some (link, _) when safe c && Inline.Link.is_unsafe link -> ""
  | Some (link, _) -> link
  in
  let title = match Link_definition.title ld with
  | None -> ""
  | Some title -> String.concat "\n" (List.map (fun (_, (t, _)) -> t) title)
  in
  dest, title

let image ?(close = " >") ?attrs c i =
  match Inline.Link.reference_definition (C.get_defs c) i with
  | Some (Link_definition.Def (ld, _)) ->
      let plain_text c i =
        let lines = Inline.to_plain_text ~break_on_soft:false i in
        String.concat "\n" (List.map (String.concat "") lines)
      in
      let link, title = link_dest_and_title c ld in
      (* Djot writes [alt] first and closes with [>]; both are cosmetic, but the
         corpus compares bytes. *)
      let djot = djot c in
      let alt c =
        C.string c " alt=\"";
        html_escaped_string c (plain_text c (Inline.Link.text i));
        C.byte c '\"'
      in
      let src c =
        C.string c " src=\""; pct_encoded_string c link; C.byte c '\"'
      in
      C.string c "<img";
      if djot then (alt c; src c) else (src c; alt c);
      if title <> ""
      then (C.string c " title=\""; html_escaped_string c title; C.byte c '\"');
      (match attrs with None -> () | Some a -> attributes c a);
      C.string c (if djot then ">" else close)
  | Some (Block.Footnote.Def _) -> comment_foonote_image c i
  | None -> comment_undefined_label c i
  | Some _ -> comment_unknown_def_type c i

let link_footnote c l fn =
  let key = Label.key (Option.get (Inline.Link.referenced_label l)) in
  let text, label, ref = make_footnote_ref_ids c key fn in
  let is_full_ref = match Inline.Link.reference l with
  | `Ref (`Full, _, _) -> true | _ -> false
  in
  if is_full_ref then begin
    C.string c "<a href=\"#"; pct_encoded_string c label;
    C.string c "\" id=\""; html_escaped_string c ref;
    C.string c "\" role=\"doc-noteref\">";
    C.inline c (Inline.Link.text l); C.string c "</a>"
  end else begin
    C.string c "<sup><a href=\"#"; pct_encoded_string c label;
    C.string c "\" id=\""; html_escaped_string c ref;
    C.string c "\" role=\"doc-noteref\" class=\"fn-label\">";
    C.string c text; C.string c "</a></sup>"
  end

let link ?attrs c l = match Inline.Link.reference_definition (C.get_defs c) l with
| Some (Link_definition.Def (ld, _)) ->
    let link, title = link_dest_and_title c ld in
    C.string c "<a href=\""; pct_encoded_string c link;
    if title <> "" then (C.string c "\" title=\""; html_escaped_string c title);
    C.byte c '\"';
    (match attrs with None -> () | Some a -> attributes c a);
    C.byte c '>'; C.inline c (Inline.Link.text l); C.string c "</a>"
| Some (Block.Footnote.Def (fn, _)) -> link_footnote c l fn
| None when djot c ->
    (* Djot still makes an anchor of an unresolved reference, just without an
       [href]. *)
    C.string c "<a";
    (match attrs with None -> () | Some a -> attributes c a);
    C.byte c '>'; C.inline c (Inline.Link.text l); C.string c "</a>"
| None -> C.inline c (Inline.Link.text l); comment_undefined_label c l
| Some _ -> C.inline c (Inline.Link.text l); comment_unknown_def_type c l

let raw_html c h =
  if safe c then comment c "CommonMark raw HTML omitted" else
  let line c (_, (h, _)) = C.byte c '\n'; C.string c h in
  if h <> []
  then (C.string c (fst (snd (List.hd h))); List.iter (line c) (List.tl h))

let strikethrough c s =
  C.string c "<del>";
  C.inline c (Inline.Strikethrough.inline s);
  C.string c "</del>"

let math_span c ms =
  let tex_line c l = html_escaped_string c (Block_line.tight_to_string l) in
  let tex_lines c = function (* newlines only between lines *)
  | [] -> () | l :: ls ->
      let line c l = C.byte c '\n'; tex_line c l in
      tex_line c l; List.iter (line c) ls
  in
  let tex = Inline.Math_span.tex_layout ms in
  if tex = [] then () else
  let display = Inline.Math_span.display ms in
  (* Djot marks the math up so a consumer can find it: the TeX delimiters alone
     leave nothing to select on. *)
  let djot = djot c in
  if djot then begin
    C.string c "<span class=\"math ";
    C.string c (if display then "display" else "inline");
    C.string c "\">"
  end;
  C.string c (if display then "\\[" else "\\(");
  tex_lines c tex;
  C.string c (if display then "\\]" else "\\)");
  if djot then C.string c "</span>"

let extra_inline_container c ic =
  let tag =
    match Inline.Extra_inline_container.kind ic with
    | Inline.Extra_inline_container.Highlight -> "mark"
    | Inline.Extra_inline_container.Superscript -> "sup"
    | Inline.Extra_inline_container.Subscript -> "sub"
    | Inline.Extra_inline_container.Inserted -> "ins"
    | Inline.Extra_inline_container.Deleted -> "del"
  in
  C.byte c '<';
  C.string c tag;
  C.byte c '>';
  C.inline c (Inline.Extra_inline_container.inline ic);
  C.string c "</";
  C.string c tag;
  C.byte c '>'

let inline_attributes c a =
  let attrs = Inline.Attributes.attributes a in
  match Inline.Attributes.inline a with
  | Inline.Emphasis (e, _) ->
      C.string c "<em"; attributes c attrs; C.byte c '>';
      C.inline c (Inline.Emphasis.inline e); C.string c "</em>"
  | Inline.Strong_emphasis (e, _) ->
      C.string c "<strong"; attributes c attrs; C.byte c '>';
      C.inline c (Inline.Emphasis.inline e); C.string c "</strong>"
  | Inline.Code_span (cs, _) ->
      C.string c "<code"; attributes c attrs; C.byte c '>';
      html_escaped_string c (Inline.Code_span.code cs); C.string c "</code>"
  | Inline.Ext_strikethrough (s, _) ->
      C.string c "<del"; attributes c attrs; C.byte c '>';
      C.inline c (Inline.Strikethrough.inline s); C.string c "</del>"
  | Inline.Ext_extra_inline_container (ic, _) ->
      let tag =
        match Inline.Extra_inline_container.kind ic with
        | Inline.Extra_inline_container.Highlight -> "mark"
        | Inline.Extra_inline_container.Superscript -> "sup"
        | Inline.Extra_inline_container.Subscript -> "sub"
        | Inline.Extra_inline_container.Inserted -> "ins"
        | Inline.Extra_inline_container.Deleted -> "del"
      in
      C.byte c '<'; C.string c tag; attributes c attrs; C.byte c '>';
      C.inline c (Inline.Extra_inline_container.inline ic);
      C.string c "</"; C.string c tag; C.byte c '>'
  | Inline.Link (l, _) -> link ~attrs c l
  | Inline.Image (i, _) -> image ~attrs c i
  | inline ->
      C.string c "<span"; attributes c attrs; C.byte c '>';
      C.inline c inline; C.string c "</span>"

let wikilink c wl =
  (* Vault-level href resolution is out of scope here: we emit a self-link to
     the raw target (plus fragment) and show the display text, escaping both. *)
  let href =
    let b = Buffer.create 32 in
    (match Inline.Wikilink.target wl with Some t -> Buffer.add_string b t | None -> ());
    (match Inline.Wikilink.fragment wl with
     | None -> ()
     | Some (Inline.Wikilink.Heading hs) ->
         List.iter (fun h -> Buffer.add_char b '#'; Buffer.add_string b h) hs
     | Some (Inline.Wikilink.Block_ref id) ->
         Buffer.add_string b "#^"; Buffer.add_string b id);
    Buffer.contents b
  in
  C.string c "<a class=\"wikilink\" href=\"";
  pct_encoded_string c href; C.string c "\">";
  html_escaped_string c (Inline.Wikilink.to_plain_text wl);
  C.string c "</a>"

let inline c = function
| Inline.Autolink (a, _) -> autolink c a; true
| Inline.Break (b, _) -> break c b; true
| Inline.Code_span (cs, _) -> code_span c cs; true
| Inline.Emphasis (e, _) -> emphasis c e; true
| Inline.Image (i, _) -> image c i; true
| Inline.Inlines (is, _) -> List.iter (C.inline c) is; true
| Inline.Link (l, _) -> link c l; true
| Inline.Raw_html (html, _) -> raw_html c html; true
| Inline.Strong_emphasis (e, _) -> strong_emphasis c e; true
| Inline.Text (t, _) -> text_string c t; true
| Inline.Ext_strikethrough (s, _) -> strikethrough c s; true
| Inline.Ext_extra_inline_container (ic, _) -> extra_inline_container c ic; true
| Inline.Ext_attributes (a, _) -> inline_attributes c a; true
| Inline.Ext_math_span (ms, _) -> math_span c ms; true
| Inline.Ext_raw_inline (r, _) -> raw_inline c r; true
| Inline.Ext_smart_punct (sp, _) ->
    C.string c (Inline.Smart_punct.to_utf_8 sp); true
| Inline.Ext_symbol (s, _) ->
    (* Djot renders a symbol literally; a filter may give it a meaning. *)
    html_escaped_string c (Inline.Symbol.to_source s); true
| Inline.Ext_wikilink (wl, _) -> wikilink c wl; true
| Inline.Ext_jsx_expr _ -> true (* opaque JSX expression: no HTML rendering *)
| Inline.Ext_jsx_element (e, _) ->
    (* No native HTML meaning: pass the tag source through verbatim (like raw
       HTML) and render the parsed children. *)
    C.string c (Inline.Jsx_element.raw e);
    (match Inline.Jsx_element.children e with
    | None -> ()
    | Some child ->
        C.inline c child; C.string c (Inline.Jsx_element.close_tag e));
    true
| _ -> comment c "<!-- Unknown Cmarkit inline -->"; true

(* Block rendering *)

let block_quote c bq =
  C.string c "<blockquote>\n";
  C.block c (Block.Block_quote.block bq);
  C.string c "</blockquote>\n"

let callout c co bq =
  let kind = Block.Callout.kind co in
  let inner = Block.Block_quote.block bq in
  let title = Block.Callout.title co inner in (* derived inline, may be None *)
  let body = Block.Callout.strip_header inner in
  (* Non-foldable callouts use <div>/<div class=callout-title>; foldable ones
     use <details>/<summary> with [open] when expanded by default. *)
  let outer, title_tag, openattr = match Block.Callout.fold co with
  | None -> "div", "div", ""
  | Some Block.Callout.Foldable_open -> "details", "summary", " open"
  | Some Block.Callout.Foldable_closed -> "details", "summary", ""
  in
  C.byte c '<'; C.string c outer; C.string c " class=\"callout\" data-callout=\"";
  html_escaped_string c kind; C.byte c '"'; C.string c openattr;
  C.string c ">\n";
  C.byte c '<'; C.string c title_tag; C.string c " class=\"callout-title\">";
  (match title with
   | Some t -> C.inline c t (* preserves emphasis, links, wikilinks, … *)
   | None -> html_escaped_string c (String.capitalize_ascii kind));
  C.string c "</"; C.string c title_tag; C.string c ">\n";
  C.string c "<div class=\"callout-content\">\n";
  C.block c body;
  C.string c "</div>\n";
  C.string c "</"; C.string c outer; C.string c ">\n"

let code_block ?attrs c cb =
  let i = Option.map fst (Block.Code_block.info_string cb) in
  let lang = Option.bind i Block.Code_block.language_of_info_string in
  let line (l, _) = html_escaped_string c l; C.byte c '\n' in
  match lang with
  | Some (lang, _env) when backend_blocks c && lang.[0] = '=' ->
      if lang = "=html" && not (safe c)
      then block_lines c (Block.Code_block.code cb) else ()
  | _ ->
      C.string c "<pre";
      (match attrs with None -> () | Some a -> attributes c a);
      C.string c "><code";
      begin match lang with
      | None -> ()
      | Some (lang, _env) ->
          C.string c " class=\"language-"; html_escaped_string c lang;
          C.byte c '\"'
      end;
      C.byte c '>';
      List.iter line (Block.Code_block.code cb);
      C.string c "</code></pre>\n"

(* Djot identifiers. The base is the heading's text with punctuation and
   whitespace runs turned into single [-], case preserved (so [Foo bar] is
   [Foo-bar], not [foo-bar]). Uniqueness is against every id in the document,
   explicit ones included, by appending [-1], [-2], …; an empty base becomes
   [s-1]. Djot assigns these while parsing, in document order, which is also the
   order we render in — so registering ids as we go gives the same answer. *)

let djot_id_base text =
  let b = Buffer.create 32 in
  let strip = function
    | '[' | ']' | '~' | '!' | '@' | '#' | '$' | '%' | '^' | '&' | '*' | '(' | ')'
    | '{' | '}' | '`' | ',' | '.' | '<' | '>' | '\\' | '|' | '=' | '+' | '/'
    | '?' -> true
    | c -> Cmarkit_base.Ascii.is_white c
  in
  let flush_sep = ref false in
  String.iter
    (fun c ->
      if strip c then (if Buffer.length b > 0 then flush_sep := true) else begin
        if !flush_sep then (Buffer.add_char b '-'; flush_sep := false);
        Buffer.add_char b c
      end)
    text;
  Buffer.contents b

let register_id c id =
  let st = C.State.get c state in
  st.ids <- String_set.add id st.ids

let djot_unique_id c base =
  let st = C.State.get c state in
  let rec loop i =
    let id = if i = 0 then base else base ^ "-" ^ Int.to_string i in
    if id = "" || String_set.mem id st.ids then loop (i + 1) else id
  in
  let id = loop (if base = "" then 1 else 0) in
  let id = if id = "" then "s-1" else id in
  register_id c id; id

(* The heading's own id, if it was given one explicitly, else a fresh djot one
   from its text. *)
let djot_heading_id c ~attrs h =
  match attrs with
  | Some a ->
      (match Attribute.id a with
       | Some id -> register_id c id; id
       | None -> djot_unique_id c (djot_id_base (Inline.to_plain_text ~break_on_soft:false (Block.Heading.inline h) |> List.map (String.concat "") |> String.concat " ")))
  | None ->
      let text =
        Inline.to_plain_text ~break_on_soft:false (Block.Heading.inline h)
        |> List.map (String.concat "") |> String.concat " "
      in
      djot_unique_id c (djot_id_base text)

let heading c h =
  let level = string_of_int (Block.Heading.level h) in
  C.string c "<h"; C.string c level;
  begin match Block.Heading.id h with
  | None -> C.byte c '>';
  | Some (`Auto id | `Id id) ->
      let id = unique_id c id in
      C.string c " id=\""; C.string c id;
      C.string c "\"><a class=\"anchor\" aria-hidden=\"true\" href=\"#";
      C.string c id; C.string c "\"></a>";
  end;
  C.inline c (Block.Heading.inline h);
  C.string c "</h"; C.string c level; C.string c ">\n"

(* Djot sections. A heading opens a [<section>] that runs until a heading of the
   same or a higher level; sections therefore nest by level. The id goes on the
   section, not on the heading. *)

let djot_heading c h =
  let level = string_of_int (Block.Heading.level h) in
  C.string c "<h"; C.string c level; C.byte c '>';
  C.inline c (Block.Heading.inline h);
  C.string c "</h"; C.string c level; C.string c ">\n"

let heading_of = function
| Block.Heading (h, _) -> Some (h, None)
| Block.Ext_attributes (a, _) ->
    (match Block.Attributes.block a with
     | Block.Heading (h, _) -> Some (h, Some (Block.Attributes.attributes a))
     | _ -> None)
| _ -> None

let rec djot_sections c = function
| [] -> ()
| b :: bs ->
    match heading_of b with
    | None -> C.block c b; djot_sections c bs
    | Some (h, attrs) ->
        let level = Block.Heading.level h in
        let id = djot_heading_id c ~attrs h in
        let rec split acc = function
        | b :: bs as rest ->
            begin match heading_of b with
            | Some (h, _) when Block.Heading.level h <= level ->
                List.rev acc, rest
            | _ -> split (b :: acc) bs
            end
        | [] -> List.rev acc, []
        in
        let inside, after = split [] bs in
        C.string c "<section id=\""; html_escaped_string c id; C.string c "\">\n";
        djot_heading c h;
        djot_sections c inside;
        C.string c "</section>\n";
        djot_sections c after

let paragraph c p =
  C.string c "<p>"; C.inline c (Block.Paragraph.inline p); C.string c "</p>\n"

(* Struct keyed nodes (Cmarkit.Struct) have no HTML of their own: we render them
   as plain CommonMark would, i.e. as if the Struct pass had not run. {!Struct.unkey}
   flattens a keyed node back to the blocks it stands for (the label, ":"
   included, rejoined to the value), which we then hand to the ordinary block /
   list-item renderers -- so tight/loose, [<li>] wrapping and xhtml inline
   dispatch are handled for free. *)
let rec item_block ~tight c = function
| Block.Blank_line _ -> ()
| Block.Ext_keyed _ as b -> item_block ~tight c (Struct.unkey b)
| Block.Paragraph (p, _) when tight -> C.inline c (Block.Paragraph.inline p)
| Block.Blocks (bs, _) ->
    let rec loop c add_nl = function
    | Block.Blank_line _ :: bs -> loop c add_nl bs
    | Block.Paragraph (p,_) :: bs when tight ->
        C.inline c (Block.Paragraph.inline p); loop c true bs
    | b :: bs -> (if add_nl then C.byte c '\n'); C.block c b; loop c false bs
    | [] -> ()
    in
    loop c true bs
| b -> C.byte c '\n'; C.block c b

(* Djot list items. The content always sits on its own lines:
   [<li>\ncontent\n</li>], tight or loose, and a task marker is an [<input>] line
   before the content. *)

let ensure_nl c =
  let b = C.buffer c in
  let n = Buffer.length b in
  if n > 0 && Buffer.nth b (n - 1) <> '\n' then C.byte c '\n'

let djot_list_item ~tight c (i, _) =
  C.string c "<li>\n";
  begin match Block.List_item.ext_task_marker i with
  | None -> ()
  | Some (mark, _) ->
      let checked = match Block.List_item.task_status_of_task_marker mark with
      | `Unchecked -> "" | `Checked | `Other _ | `Cancelled -> " checked=\"\""
      in
      C.string c "<input disabled=\"\" type=\"checkbox\"";
      C.string c checked; C.string c "/>\n"
  end;
  (* [item_block] prepends a newline before a block child, which would double the
     one we just wrote after [<li>]. *)
  let child c = function
  | Block.Blank_line _ -> ()
  (* A tight item's paragraphs lose their wrapper, as in a CommonMark tight
     list; the newline is ours, not [item_block]'s, which would double the one
     after [<li>]. *)
  | Block.Paragraph (p, _) when tight ->
      C.inline c (Block.Paragraph.inline p); ensure_nl c
  | b -> C.block c b; ensure_nl c
  in
  begin match Block.List_item.block i with
  | Block.Blocks (bs, _) -> List.iter (child c) bs
  | b -> child c b
  end;
  C.string c "</li>\n"

let list_item ~tight c (i, _) = match Block.List_item.ext_task_marker i with
| None ->
    C.string c "<li>";
    item_block ~tight c (Block.List_item.block i);
    C.string c "</li>\n"
| Some (mark, _) ->
    C.string c "<li>";
    let close = match Block.List_item.task_status_of_task_marker mark with
    | `Unchecked ->
        C.string c
          "<div class=\"task\"><input type=\"checkbox\" disabled><div>";
        "</div></div></li>\n"
    | `Checked | `Other _ ->
        C.string c
          "<div class=\"task\"><input type=\"checkbox\" disabled checked><div>";
        "</div></div></li>\n"
    | `Cancelled ->
        C.string c
          "<div class=\"task\"><input type=\"checkbox\" disabled><del>";
        "</del></div></li>\n"
    in
    item_block ~tight c (Block.List_item.block i);
    C.string c close

let list ?attrs c l =
  let tight = Block.List'.tight l in
  if djot c then begin
    let items = Block.List'.items l in
    let is_task (i, _) = Block.List_item.ext_task_marker i <> None in
    let task_class = if List.exists is_task items then " class=\"task-list\"" else "" in
    let attrs c = match attrs with None -> () | Some a -> attributes c a in
    let close = match Block.List'.type' l with
    | `Unordered _ ->
        C.string c "<ul"; C.string c task_class; attrs c; C.string c ">\n";
        "</ul>\n"
    | `Ordered (start, _) | `Ext_ordered (_, _, start) ->
        C.string c "<ol"; C.string c task_class;
        if start <> 1
        then (C.string c " start=\""; C.string c (string_of_int start);
              C.string c "\"");
        attrs c; C.string c ">\n"; "</ol>\n"
    in
    List.iter (djot_list_item ~tight c) items;
    C.string c close
  end else
  match Block.List'.type' l with
  | `Unordered _ ->
      C.string c "<ul>\n";
      List.iter (list_item ~tight c) (Block.List'.items l);
      C.string c "</ul>\n"
  | `Ordered (start, _) ->
      C.string c "<ol";
      if start = 1 then C.string c ">\n" else
      (C.string c " start=\""; C.string c (string_of_int start);
       C.string c "\">\n");
      List.iter (list_item ~tight c) (Block.List'.items l);
      C.string c "</ol>\n"
  | `Ext_ordered (style, _, start) ->
      (* The djot numbering style maps onto the [type] attribute; the delimiter
         has no HTML rendering, browsers always write a period. *)
      C.string c "<ol";
      begin match style with
      | `Decimal -> ()
      | `Alpha_lower -> C.string c " type=\"a\""
      | `Alpha_upper -> C.string c " type=\"A\""
      | `Roman_lower -> C.string c " type=\"i\""
      | `Roman_upper -> C.string c " type=\"I\""
      end;
      if start = 1 then C.string c ">\n" else
      (C.string c " start=\""; C.string c (string_of_int start);
       C.string c "\">\n");
      List.iter (list_item ~tight c) (Block.List'.items l);
      C.string c "</ol>\n"

let definition_list c d =
  let tight = Block.Definition_list.tight d in
  let item (i, _) =
    C.string c "<dt>";
    C.inline c (Block.Definition_list.item_term i);
    C.string c "</dt>\n<dd>";
    (* A tight definition drops the paragraph wrapper around its content, as a
       tight list item does. *)
    item_block ~tight c (Block.Definition_list.item_definition i);
    C.string c "</dd>\n"
  in
  C.string c "<dl>\n";
  List.iter item (Block.Definition_list.items d);
  C.string c "</dl>\n"

let html_block c lines =
  let line (l, _) = C.string c l; C.byte c '\n' in
  if safe c then (comment c "CommonMark HTML block omitted"; C.byte c '\n') else
  List.iter line lines

let thematic_break ?attrs c =
  C.string c "<hr";
  (match attrs with None -> () | Some a -> attributes c a);
  C.string c ">\n"

let math_block c cb =
  let line l = html_escaped_string c (Block_line.to_string l); C.byte c '\n' in
  let djot = djot c in
  if djot then C.string c "<span class=\"math display\">";
  C.string c "\\[\n";
  List.iter line (Block.Code_block.code cb);
  C.string c "\\]";
  if djot then C.string c "</span>";
  C.byte c '\n'

let table c t =
  (* Djot writes the alignment as an inline style and does not wrap the table in
     the scroll region cmarkit adds. *)
  let djot = djot c in
  let start c align tag =
    C.byte c '<'; C.string c tag;
    match align with
    | None -> C.byte c '>';
    | Some a ->
        let a = match a with
        | `Left -> "left" | `Center -> "center" | `Right -> "right"
        in
        if djot then begin
          C.string c " style=\"text-align: "; C.string c a; C.string c ";\">"
        end else (C.string c " class=\""; C.string c a; C.string c "\">")
  in
  let close c tag = C.string c "</"; C.string c tag; C.string c ">\n" in
  let rec cols c tag ~align count cs = match align, cs with
  | ((a, _) :: align), (col, _) :: cs ->
      start c (fst a) tag; C.inline c col; close c tag;
      cols c tag ~align (count - 1) cs
  | ((a, _) :: align), [] ->
      start c (fst a) tag; close c tag;
      cols c tag ~align (count - 1) []
  | [], (col, _) :: cs ->
      start c None tag; C.inline c col; close c tag;
      cols c tag ~align:[] (count - 1) cs
  | [], [] ->
      for i = count downto 1 do start c None tag; close c tag done;
  in
  let row c tag ~align count cs =
    C.string c "<tr>\n"; cols c tag ~align count cs; C.string c "</tr>\n";
  in
  let header c count ~align cols = row c "th" ~align count cols in
  let data c count ~align cols = row c "td" ~align count cols in
  let rec rows c col_count ~align = function
  | ((`Header cols, _), _) :: rs ->
      let align, rs = match rs with
      | ((`Sep align, _), _) :: rs -> align, rs
      | _ -> align, rs
      in
      header c col_count ~align cols; rows c col_count ~align rs
  | ((`Data cols, _), _) :: rs ->
      data c col_count ~align cols; rows c col_count ~align rs
  | ((`Sep align, _), _) :: rs -> rows c col_count ~align rs
  | [] -> ()
  in
  C.string c (if djot then "<table>\n" else "<div role=\"region\"><table>\n");
  begin match Block.Table.caption t with
  | None -> ()
  | Some (caption, _) ->
      (* HTML wants [caption] to be the table's first child. *)
      C.string c "<caption>";
      C.inline c (Block.Table.caption_inline caption);
      C.string c "</caption>\n"
  end;
  rows c (Block.Table.col_count t) ~align:[] (Block.Table.rows t);
  C.string c (if djot then "</table>\n" else "</table></div>")

let div ?attrs c d =
  C.string c "<div";
  (match attrs with None -> () | Some a -> attributes c a);
  begin match Block.Div.class' d with
  | None -> ()
  | Some (cls, _) ->
      C.string c " class=\""; html_escaped_string c cls; C.byte c '\"'
  end;
  C.string c ">\n";
  C.block c (Block.Div.block d);
  C.string c "</div>\n"

let jsx_block c j =
  (* No native HTML meaning: pass the opening/closing tags through verbatim and
     render the parsed block children between them. *)
  C.string c (fst (Block.Jsx_block.raw_open j)); C.byte c '\n';
  C.block c (Block.Jsx_block.block j);
  match Block.Jsx_block.raw_close j with
  | None -> () | Some (close, _) -> C.string c close; C.byte c '\n'

let block_attributes c a =
  let attrs = Block.Attributes.attributes a in
  (* Djot dedupes heading ids against every id in the document, explicit ones
     included, and assigns them in document order. Registering as we render
     keeps a later heading from stealing an id an explicit one already took. *)
  if djot c then
    (match Attribute.id attrs with None -> () | Some id -> register_id c id);
  match Block.Attributes.block a with
  | Block.Paragraph (p, _) ->
      C.string c "<p"; attributes c attrs; C.byte c '>';
      C.inline c (Block.Paragraph.inline p); C.string c "</p>\n"
  | Block.Block_quote (bq, _) ->
      C.string c "<blockquote"; attributes c attrs; C.string c ">\n";
      C.block c (Block.Block_quote.block bq);
      C.string c "</blockquote>\n"
  | Block.Thematic_break _ -> thematic_break ~attrs c
  | Block.Code_block (cb, _) -> code_block ~attrs c cb
  | Block.List (l, _) -> list ~attrs c l
  (* A div already is the element the attributes belong on: wrapping it in
     another one would nest two divs. *)
  | Block.Ext_div (d, _) -> div ~attrs c d
  | block ->
      C.string c "<div"; attributes c attrs; C.string c ">\n";
      C.block c block; C.string c "</div>\n"

let block c = function
| Block.Block_quote (bq, meta) ->
    (match Block.Callout.find meta with
     | Some co -> callout c co bq
     | None -> block_quote c bq);
    true
| Block.Blocks (bs, _) when djot c -> djot_sections c bs; true
| Block.Blocks (bs, _) -> List.iter (C.block c) bs; true
| Block.Code_block (cb, _) -> code_block c cb; true
| Block.Heading (h, _) -> heading c h; true
| Block.Html_block (h, _) -> html_block c h; true
| Block.List (l, _) -> list c l; true
| Block.Paragraph (p, _) -> paragraph c p; true
| Block.Thematic_break (_, _) -> thematic_break c; true
| Block.Ext_math_block (cb, _) -> math_block c cb; true
| Block.Ext_raw_block (r, _) -> raw_block c r; true
| Block.Ext_definition_list (d, _) -> definition_list c d; true
| Block.Ext_table (t, _) -> table c t; true
| Block.Ext_div (d, _) -> div c d; true
| Block.Ext_jsx_block (j, _) -> jsx_block c j; true
| Block.Ext_attributes (a, _) -> block_attributes c a; true
| Block.Ext_keyed _ as b -> C.block c (Struct.unkey b); true
| Block.Blank_line _
| Block.Link_reference_definition _
| Block.Ext_footnote_definition _ -> true
| _ -> comment c "Unknown Cmarkit block"; C.byte c '\n'; true

(* XHTML rendering *)

let xhtml_block c = function
| Block.Thematic_break _ -> C.string c "<hr />\n"; true
| b -> block c b

let xhtml_inline c = function
| Inline.Break (b, _) when Inline.Break.type' b = `Hard ->
    C.string c "<br />\n"; true
| Inline.Image (i, _) ->
    image ~close:" />" c i; true
| i -> inline c i

(* Document rendering *)

let footnotes c fns =
  (* XXX we could do something about recursive footnotes and footnotes in
     footnotes here. *)
  let fns = Label.Map.fold (fun _ fn acc -> fn :: acc) fns [] in
  let fns = List.sort Stdlib.compare fns in
  let footnote c (_, id, refc, fn) =
    C.string c "<li id=\""; html_escaped_string c id; C.string c "\">\n";
    C.block c (Block.Footnote.block fn);
    C.string c "<span>";
    for r = 1 to !refc do
      C.string c "<a href=\"#"; pct_encoded_string c (footnote_ref_id id r);
      C.string c "\" role=\"doc-backlink\" class=\"fn-label\">↩︎︎";
      if !refc > 1 then
        (C.string c "<sup>"; C.string c (Int.to_string r); C.string c "</sup>");
      C.string c "</a>"
    done;
    C.string c "</span>";
    C.string c "</li>"
  in
  C.string c "<section role=\"doc-endnotes\"><ol>\n";
  List.iter (footnote c) fns;
  C.string c "</ol></section>\n"

let doc c d =
  C.block c (Doc.block d);
  let st = C.State.get c state in
  if Label.Map.is_empty st.footnotes then () else footnotes c st.footnotes;
  true

(* Renderer *)

let renderer ?backend_blocks ?djot ~safe () =
  let init_context = init_context ?backend_blocks ?djot ~safe in
  Cmarkit_renderer.make ~init_context ~inline ~block ~doc ()

let xhtml_renderer ?backend_blocks ?djot ~safe () =
  let init_context = init_context ?backend_blocks ?djot ~safe in
  let inline = xhtml_inline and block = xhtml_block in
  Cmarkit_renderer.make ~init_context ~inline ~block ~doc ()

let of_doc ?backend_blocks ?djot ~safe d =
  Cmarkit_renderer.doc_to_string (renderer ?backend_blocks ?djot ~safe ()) d
