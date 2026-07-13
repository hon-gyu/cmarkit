(*---------------------------------------------------------------------------
   mdast renderer: Cmarkit.Doc.t -> mdast JSON (a `root` node).

   Unlike the HTML/CommonMark backends this does not use the buffer-based
   Cmarkit_renderer framework: mdast is a tree, so we build a small JSON value
   directly by recursion. Extensions with no native mdast type are lowered to a
   generic "oyElement" node carrying data.hName / data.hProperties so that
   mdast-util-to-hast (remark-rehype) renders them without a custom handler.
  ---------------------------------------------------------------------------*)

open Cmarkit

(* Config ===================================================================*)

(* Whether to strip the [^id] block-id marker from a paragraph's rendered text.
   When [false] the marker is kept but wrapped in a dim, styleable span. Set per
   render by {!of_doc}; a module-level flag following the codebase convention,
   which avoids threading config through every recursive map. *)
let block_id_strip = ref true

(* Minimal JSON =============================================================*)

type json =
  | Null
  | Bool of bool
  | Int of int
  | Str of string
  | Arr of json list
  | Obj of (string * json) list

let buffer_add_json_string b s =
  Buffer.add_char b '"';
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | '\n' -> Buffer.add_string b "\\n"
      | '\r' -> Buffer.add_string b "\\r"
      | '\t' -> Buffer.add_string b "\\t"
      | c when Char.code c < 0x20 ->
          Buffer.add_string b (Printf.sprintf "\\u%04x" (Char.code c))
      | c -> Buffer.add_char b c)
    s;
  Buffer.add_char b '"'

let json_to_buffer b j =
  let rec add = function
    | Null -> Buffer.add_string b "null"
    | Bool v -> Buffer.add_string b (if v then "true" else "false")
    | Int i -> Buffer.add_string b (string_of_int i)
    | Str s -> buffer_add_json_string b s
    | Arr [] -> Buffer.add_string b "[]"
    | Arr (x :: xs) ->
        Buffer.add_char b '[';
        add x;
        List.iter
          (fun x ->
            Buffer.add_char b ',';
            add x)
          xs;
        Buffer.add_char b ']'
    | Obj [] -> Buffer.add_string b "{}"
    | Obj ((k, v) :: kvs) ->
        Buffer.add_char b '{';
        buffer_add_json_string b k;
        Buffer.add_char b ':';
        add v;
        List.iter
          (fun (k, v) ->
            Buffer.add_char b ',';
            buffer_add_json_string b k;
            Buffer.add_char b ':';
            add v)
          kvs;
        Buffer.add_char b '}'
  in
  add j

(* mdast node helpers =======================================================*)

(* mdast positions are 1-based line, 1-based column and 0-based offset. A
   cmarkit line position is (line_num, byte_pos_of_line_start), so the column is
   (byte - line_start + 1) and the offset is the absolute byte. *)
let position_of_meta meta =
  let tl = Meta.textloc meta in
  if Textloc.is_none tl then []
  else
    let fl, flpos = Textloc.first_line tl in
    let ll, llpos = Textloc.last_line tl in
    let first_byte = Textloc.first_byte tl in
    let last_byte = Textloc.last_byte tl in
    let point line col off =
      Obj [ ("line", Int line); ("column", Int col); ("offset", Int off) ]
    in
    (* [last_byte] is inclusive in cmarkit; mdast's end point sits just after
       the last byte. *)
    [
      ( "position",
        Obj
          [
            ("start", point fl (first_byte - flpos + 1) first_byte);
            ("end", point ll (last_byte - llpos + 2) (last_byte + 1));
          ] );
    ]

let node ?(props = []) ~meta type' fields =
  Obj ((("type", Str type') :: fields) @ props @ position_of_meta meta)

(* A generic element that lowers to HTML via mdast-util-to-hast's hName /
   hProperties convention. [tag] becomes the element name, [properties] its
   attributes (e.g. className : string array). *)
let element ~meta ?(properties = []) ~children tag =
  let data =
    Obj
      (("hName", Str tag)
      :: (if properties = [] then [] else [ ("hProperties", Obj properties) ]))
  in
  node ~meta "oyElement" [ ("data", data); ("children", Arr children) ]

let class_name classes = ("className", Arr (List.map (fun c -> Str c) classes))

(* mdast footnote identifiers must match between a reference and its definition.
   We use the (normalized) label key with its leading [^] dropped, e.g. the
   label [^1] yields the identifier ["1"]. *)
let footnote_identifier label =
  let k = Label.key label in
  if String.length k > 0 && k.[0] = '^' then
    String.sub k 1 (String.length k - 1)
  else k

let hproperties_of_attributes a =
  let id =
    match Attribute.id a with
    | None -> []
    | Some id -> [ ("id", Str id) ]
  in
  let classes =
    match Attribute.classes a with
    | [] -> []
    | cs -> [ class_name cs ]
  in
  let kvs = List.map (fun (k, v) -> (k, Str v)) (Attribute.key_values a) in
  id @ classes @ kvs

(* Inlines ==================================================================*)

let plain_text i =
  let lines = Inline.to_plain_text ~break_on_soft:false i in
  String.concat "\n" (List.map (String.concat "") lines)

let link_dest_and_title defs l =
  match Inline.Link.reference_definition defs l with
  | Some (Link_definition.Def (ld, _)) ->
      let dest =
        match Link_definition.dest ld with
        | None -> ""
        | Some (d, _) -> d
      in
      let title =
        match Link_definition.title ld with
        | None -> None
        | Some t ->
            Some (String.concat "\n" (List.map (fun (_, (t, _)) -> t) t))
      in
      (dest, title)
  | _ -> ("", None)

let title_field = function
  | None -> []
  | Some t -> [ ("title", Str t) ]

let wikilink_href wl =
  let b = Buffer.create 32 in
  (match Inline.Wikilink.target wl with
  | Some t -> Buffer.add_string b t
  | None -> ());
  (match Inline.Wikilink.fragment wl with
  | None -> ()
  | Some (Inline.Wikilink.Heading hs) ->
      List.iter
        (fun h ->
          Buffer.add_char b '#';
          Buffer.add_string b h)
        hs
  | Some (Inline.Wikilink.Block_ref id) ->
      Buffer.add_string b "#^";
      Buffer.add_string b id);
  Buffer.contents b

let extra_inline_tag ic =
  match Inline.Extra_inline_container.kind ic with
  | Inline.Extra_inline_container.Highlight -> "mark"
  | Inline.Extra_inline_container.Superscript -> "sup"
  | Inline.Extra_inline_container.Subscript -> "sub"
  | Inline.Extra_inline_container.Inserted -> "ins"
  | Inline.Extra_inline_container.Deleted -> "del"

(* [inline defs i] is the list of mdast inline nodes for [i]. It is a list
   because [Inline.Inlines] splices and soft breaks expand. *)
let rec inline defs (i : Inline.t) : json list =
  match i with
  | Inline.Text (t, meta) -> [ node ~meta "text" [ ("value", Str t) ] ]
  | Inline.Inlines (is, _) -> List.concat_map (inline defs) is
  | Inline.Break (b, meta) -> (
      match Inline.Break.type' b with
      | `Hard -> [ node ~meta "break" [] ]
      | `Soft -> [ node ~meta "text" [ ("value", Str "\n") ] ])
  | Inline.Code_span (cs, meta) ->
      [ node ~meta "inlineCode" [ ("value", Str (Inline.Code_span.code cs)) ] ]
  | Inline.Emphasis (e, meta) ->
      [
        node ~meta "emphasis"
          [ ("children", Arr (inline defs (Inline.Emphasis.inline e))) ];
      ]
  | Inline.Strong_emphasis (e, meta) ->
      [
        node ~meta "strong"
          [ ("children", Arr (inline defs (Inline.Emphasis.inline e))) ];
      ]
  | Inline.Link (l, meta) -> (
      match Inline.Link.reference_definition defs l with
      | Some (Block.Footnote.Def (fn, _)) ->
          let id = footnote_identifier (Block.Footnote.label fn) in
          [
            node ~meta "footnoteReference"
              [ ("identifier", Str id); ("label", Str id) ];
          ]
      | _ ->
          let dest, title = link_dest_and_title defs l in
          [
            node ~meta "link"
              ((("url", Str dest) :: title_field title)
              @ [ ("children", Arr (inline defs (Inline.Link.text l))) ]);
          ])
  | Inline.Image (l, meta) ->
      let dest, title = link_dest_and_title defs l in
      [
        node ~meta "image"
          ((("url", Str dest) :: title_field title)
          @ [ ("alt", Str (plain_text (Inline.Link.text l))) ]);
      ]
  | Inline.Autolink (a, meta) ->
      let text, _ = Inline.Autolink.link a in
      let url = (if Inline.Autolink.is_email a then "mailto:" else "") ^ text in
      [
        node ~meta "link"
          [
            ("url", Str url);
            ("children", Arr [ node ~meta "text" [ ("value", Str text) ] ]);
          ];
      ]
  | Inline.Raw_html (h, meta) ->
      let s = String.concat "\n" (List.map (fun (_, (h, _)) -> h) h) in
      [ node ~meta "html" [ ("value", Str s) ] ]
  (* Djot raw inline. mdast/hast can only carry HTML, so that is the only format
     we keep; content aimed at another backend is dropped, as djot specifies. *)
  | Inline.Ext_raw_inline (r, meta) ->
      if Inline.Raw_inline.format r <> "html" then []
      else [ node ~meta "html" [ ("value", Str (Inline.Raw_inline.code r)) ] ]
  | Inline.Ext_strikethrough (s, meta) ->
      [
        node ~meta "delete"
          [ ("children", Arr (inline defs (Inline.Strikethrough.inline s))) ];
      ]
  | Inline.Ext_extra_inline_container (ic, meta) ->
      [
        element ~meta
          ~children:(inline defs (Inline.Extra_inline_container.inline ic))
          (extra_inline_tag ic);
      ]
  | Inline.Ext_attributes (a, meta) ->
      let props = hproperties_of_attributes (Inline.Attributes.attributes a) in
      inline_with_attributes defs ~meta ~props (Inline.Attributes.inline a)
  | Inline.Ext_math_span (ms, meta) ->
      let tex = Inline.Math_span.tex ms in
      let cls =
        if Inline.Math_span.display ms then "math-display" else "math-inline"
      in
      [
        element ~meta
          ~properties:[ class_name [ "math"; cls ] ]
          ~children:[ node ~meta "text" [ ("value", Str tex) ] ]
          "span";
      ]
  | Inline.Ext_smart_punct (sp, meta) ->
      (* Resolved text: mdast consumers expect the curly character, not [--]. *)
      [ node ~meta "text" [ ("value", Str (Inline.Smart_punct.to_utf_8 sp)) ] ]
  | Inline.Ext_symbol (s, meta) ->
      (* Literal text, as in djot. The name is kept in [data] so a downstream
         filter can give the symbol a meaning (an emoji, say) without having to
         re-scan the text for [:...:]. *)
      let data =
        Obj [ ("oySymbol", Obj [ ("name", Str (Inline.Symbol.name s)) ]) ]
      in
      [
        node ~meta "text"
          [ ("value", Str (Inline.Symbol.to_source s)); ("data", data) ];
      ]
  | Inline.Ext_wikilink (wl, meta) ->
      let target = Option.value ~default:"" (Inline.Wikilink.target wl) in
      let frag =
        match Inline.Wikilink.fragment wl with
        | None -> Null
        | Some (Inline.Wikilink.Heading hs) ->
            Obj
              [
                ("kind", Str "heading");
                ("path", Arr (List.map (fun h -> Str h) hs));
              ]
        | Some (Inline.Wikilink.Block_ref id) ->
            Obj [ ("kind", Str "block"); ("id", Str id) ]
      in
      let data =
        Obj
          [
            ("hProperties", Obj [ class_name [ "wikilink" ] ]);
            ( "oyWikilink",
              Obj
                [
                  ("target", Str target);
                  ("fragment", frag);
                  ("embed", Bool (Inline.Wikilink.embed wl));
                ] );
          ]
      in
      [
        node ~meta "link"
          [
            ("url", Str (wikilink_href wl));
            ("data", data);
            ( "children",
              Arr
                [
                  node ~meta "text"
                    [ ("value", Str (Inline.Wikilink.to_plain_text wl)) ];
                ] );
          ];
      ]
  | Inline.Ext_jsx_expr _ ->
      (* Not handling MDX: opaque JSX expressions carry no markdown meaning. *)
      []
  | Inline.Ext_jsx_element (e, meta) ->
      (* Pass the tag source through as raw HTML and render parsed children. *)
      let open' =
        [ node ~meta "html" [ ("value", Str (Inline.Jsx_element.raw e)) ] ]
      in
      let children =
        match Inline.Jsx_element.children e with
        | None -> []
        | Some child ->
            inline defs child
            @ [
                node ~meta "html"
                  [ ("value", Str (Inline.Jsx_element.close_tag e)) ];
              ]
      in
      open' @ children
  | _ -> []

(* Attach [props] as hProperties to the mdast node for [i]. When [i] maps to a
   single *element-backed* node we merge onto it; otherwise (multiple nodes, or a
   bare "text"/"html" node that renders as raw content and can't carry
   attributes) we wrap in a <span> so the attributes land on a real element. *)
and inline_with_attributes defs ~meta ~props i =
  let merge_data fields =
    let has_data = List.mem_assoc "data" fields in
    let data = Obj [ ("hProperties", Obj props) ] in
    if has_data then fields (* keep existing (e.g. wikilink); rare overlap *)
    else ("data", data) :: fields
  in
  match inline defs i with
  | [ Obj (("type", Str ty) :: rest) ] when ty <> "text" && ty <> "html" ->
      [ Obj (("type", Str ty) :: merge_data rest) ]
  | children -> [ element ~meta ~properties:props ~children "span" ]

(* Blocks ===================================================================*)

let code_block_lang_meta cb =
  match Option.map fst (Block.Code_block.info_string cb) with
  | None -> (Null, Null)
  | Some info -> (
      match Block.Code_block.language_of_info_string info with
      | None -> (Null, Null)
      | Some (lang, env) -> (Str lang, if env = "" then Null else Str env))

let code_block_value cb =
  String.concat "\n" (List.map fst (Block.Code_block.code cb))

let rstrip_blanks s =
  let n = ref (String.length s) in
  while !n > 0 && (s.[!n - 1] = ' ' || s.[!n - 1] = '\t') do
    decr n
  done;
  String.sub s 0 !n

(* Replace the last text node of [children] with the nodes [f] returns for it.
   Used to rewrite the trailing [^id] block-id marker. *)
let map_last_text f children =
  let rec go = function
    | [] -> []
    | [ Obj (("type", Str "text") :: rest) ] -> f rest
    | [ last ] -> [ last ]
    | x :: xs -> x :: go xs
  in
  go children

(* The trailing [^id] marker in the last text node, split as [(prose, rest)]
   with the marker removed, or [None] when it isn't there. *)
let match_block_id_marker id rest =
  let suffix = "^" ^ id in
  let slen = String.length suffix in
  match List.assoc_opt "value" rest with
  | Some (Str v)
    when String.length v >= slen
         && String.sub v (String.length v - slen) slen = suffix ->
      Some (String.sub v 0 (String.length v - slen))
  | _ -> None

let set_value rest v =
  List.map (fun (k, x) -> if k = "value" then (k, Str v) else (k, x)) rest

(* An Obsidian block id [^id] is parsed as a paragraph metadatum but its [^id]
   marker is left in the paragraph's inline text. Drop that trailing marker (and
   the blanks before it) so it doesn't render. *)
let strip_block_id_marker id children =
  map_last_text
    (fun rest ->
      match match_block_id_marker id rest with
      | Some prose ->
          [ Obj (("type", Str "text") :: set_value rest (rstrip_blanks prose)) ]
      | None -> [ Obj (("type", Str "text") :: rest) ])
    children

(* Keep the [^id] marker but wrap it in a [span.block-id] so it can be styled
   apart from the surrounding prose. The blanks before it stay in the prose. *)
let keep_block_id_marker ~meta id children =
  map_last_text
    (fun rest ->
      match match_block_id_marker id rest with
      | Some prose ->
          let marker =
            element ~meta
              ~properties:[ class_name [ "block-id" ] ]
              ~children:[ node ~meta "text" [ ("value", Str ("^" ^ id)) ] ]
              "span"
          in
          (if prose = "" then [] else [ Obj (("type", Str "text") :: set_value rest prose) ])
          @ [ marker ]
      | None -> [ Obj (("type", Str "text") :: rest) ])
    children

let rec block defs (b : Block.t) : json list =
  match b with
  | Block.Paragraph (p, meta) -> (
      let children = inline defs (Block.Paragraph.inline p) in
      match Block.Block_id.find meta with
      | None -> [ node ~meta "paragraph" [ ("children", Arr children) ] ]
      | Some bid ->
          let id = Block.Block_id.id bid in
          let children, props =
            if !block_id_strip then
              (strip_block_id_marker id children, [ ("id", Str id) ])
            else
              ( keep_block_id_marker ~meta id children,
                [ ("id", Str id); class_name [ "has-block-id" ] ] )
          in
          [
            node ~meta "paragraph"
              [
                ("data", Obj [ ("hProperties", Obj props) ]);
                ("children", Arr children);
              ];
          ])
  | Block.Heading (h, meta) ->
      [
        node ~meta "heading"
          [
            ("depth", Int (Block.Heading.level h));
            ("children", Arr (inline defs (Block.Heading.inline h)));
          ];
      ]
  | Block.Code_block (cb, meta) ->
      let lang, m = code_block_lang_meta cb in
      [
        node ~meta "code"
          [ ("lang", lang); ("meta", m); ("value", Str (code_block_value cb)) ];
      ]
  | Block.Html_block (lines, meta) ->
      let s = String.concat "\n" (List.map fst lines) in
      [ node ~meta "html" [ ("value", Str s) ] ]
  | Block.Blocks (bs, _) -> List.concat_map (block defs) bs
  | Block.Block_quote (bq, meta) -> (
      let inner = Block.Block_quote.block bq in
      match Block.Callout.find meta with
      | Some co -> [ callout defs ~meta co inner ]
      | None ->
          [ node ~meta "blockquote" [ ("children", Arr (block defs inner)) ] ])
  | Block.List (l, meta) -> [ list defs ~meta l ]
  | Block.Thematic_break (_, meta) -> [ node ~meta "thematicBreak" [] ]
  | Block.Ext_table (t, meta) -> [ table defs ~meta t ]
  | Block.Ext_div (d, meta) ->
      let classes =
        match Block.Div.class' d with
        | None -> []
        | Some (cls, _) ->
            String.split_on_char ' ' cls |> List.filter (( <> ) "")
      in
      let properties = if classes = [] then [] else [ class_name classes ] in
      [
        element ~meta ~properties
          ~children:(block defs (Block.Div.block d))
          "div";
      ]
  | Block.Ext_attributes (a, meta) ->
      let props = hproperties_of_attributes (Block.Attributes.attributes a) in
      block_with_attributes defs ~meta ~props (Block.Attributes.block a)
  | Block.Ext_math_block (cb, meta) ->
      [
        element ~meta
          ~properties:[ class_name [ "math"; "math-display" ] ]
          ~children:
            [
              node ~meta "text"
                [
                  ( "value",
                    Str
                      (String.concat "\n"
                         (List.map Block_line.to_string
                            (Block.Code_block.code cb))) );
                ];
            ]
          "div";
      ]
  | Block.Ext_definition_list (d, meta) ->
      let item (i, imeta) =
        [ element ~meta:imeta
            ~children:(inline defs (Block.Definition_list.item_term i)) "dt";
          element ~meta:imeta
            ~children:(block defs (Block.Definition_list.item_definition i))
            "dd" ]
      in
      [ element ~meta
          ~children:
            (List.concat_map item (Block.Definition_list.items d))
          "dl" ]
  (* Djot raw block, see the raw inline case above. *)
  | Block.Ext_raw_block (r, meta) ->
      if Block.Raw_block.format r <> "html" then []
      else
        let value = code_block_value (Block.Raw_block.code_block r) in
        [ node ~meta "html" [ ("value", Str value) ] ]
  | Block.Ext_jsx_block (j, meta) ->
      let open' =
        [
          node ~meta "html"
            [ ("value", Str (fst (Block.Jsx_block.raw_open j))) ];
        ]
      in
      let children = block defs (Block.Jsx_block.block j) in
      let close =
        match Block.Jsx_block.raw_close j with
        | None -> []
        | Some (c, _) -> [ node ~meta "html" [ ("value", Str c) ] ]
      in
      open' @ children @ close
  | Block.Ext_keyed _ as b -> block defs (Struct.unkey b)
  | Block.Ext_footnote_definition (fn, meta) ->
      let id = footnote_identifier (Block.Footnote.label fn) in
      [
        node ~meta "footnoteDefinition"
          [
            ("identifier", Str id);
            ("label", Str id);
            ("children", Arr (block defs (Block.Footnote.block fn)));
          ];
      ]
  | Block.Blank_line _ | Block.Link_reference_definition _ -> []
  | _ -> []

and block_with_attributes defs ~meta ~props b =
  match block defs b with
  | [ Obj (("type", ty) :: rest) ] when not (List.mem_assoc "data" rest) ->
      [
        Obj
          (("type", ty) :: ("data", Obj [ ("hProperties", Obj props) ]) :: rest);
      ]
  | children -> [ element ~meta ~properties:props ~children "div" ]

and list defs ~meta l =
  let ordered, start =
    match Block.List'.type' l with
    | `Unordered _ -> (false, Null)
    | `Ordered (start, _) | `Ext_ordered (_, _, start) -> (true, Int start)
  in
  let tight = Block.List'.tight l in
  let item (i, item_meta) =
    let checked =
      match Block.List_item.ext_task_marker i with
      | None -> Null
      | Some (mark, _) -> (
          match Block.List_item.task_status_of_task_marker mark with
          | `Checked -> Bool true
          | `Unchecked
          | `Cancelled
          | `Other _ ->
              Bool false)
    in
    node ~meta:item_meta "listItem"
      [
        ("spread", Bool (not tight));
        ("checked", checked);
        ("children", Arr (block defs (Block.List_item.block i)));
      ]
  in
  node ~meta "list"
    [
      ("ordered", Bool ordered);
      ("start", start);
      ("spread", Bool (not tight));
      ("children", Arr (List.map item (Block.List'.items l)));
    ]

and table defs ~meta t =
  let align_of = function
    | Some `Left -> Str "left"
    | Some `Center -> Str "center"
    | Some `Right -> Str "right"
    | None -> Null
  in
  let cells cols =
    List.map
      (fun (col, _) ->
        node ~meta "tableCell" [ ("children", Arr (inline defs col)) ])
      cols
  in
  let row cols = node ~meta "tableRow" [ ("children", Arr (cells cols)) ] in
  let align = ref [] in
  let rows =
    List.filter_map
      (fun ((r, _), _) ->
        match r with
        | `Header cols -> Some (row cols)
        | `Data cols -> Some (row cols)
        | `Sep seps ->
            align := List.map (fun ((a, _), _) -> align_of a) seps;
            None)
      (Block.Table.rows t)
  in
  (* mdast's [table] has no caption field. Dropping the caption would lose
     content, so it rides along as an extra property rather than silently
     disappearing; consumers that do not know it will ignore it. *)
  let caption =
    match Block.Table.caption t with
    | None -> []
    | Some (c, _) ->
        [ ("caption", Arr (inline defs (Block.Table.caption_inline c))) ]
  in
  node ~meta "table"
    ([ ("align", Arr !align); ("children", Arr rows) ] @ caption)

(* Obsidian callout: a blockquote carrying [callout]/[callout-title]/
   [callout-content] structure, mirroring the HTML backend. Foldable callouts
   still render as a blockquote here (details/summary would need a distinct
   element); the fold state is exposed as a data attribute. *)
and callout defs ~meta co inner =
  let kind = Block.Callout.kind co in
  let title = Block.Callout.title co inner in
  let body = Block.Callout.strip_header inner in
  let title_children =
    match title with
    | Some t -> inline defs t
    | None ->
        [ node ~meta "text" [ ("value", Str (String.capitalize_ascii kind)) ] ]
  in
  let fold_prop =
    match Block.Callout.fold co with
    | None -> []
    | Some Block.Callout.Foldable_open -> [ ("dataCalloutFold", Str "open") ]
    | Some Block.Callout.Foldable_closed ->
        [ ("dataCalloutFold", Str "closed") ]
  in
  let title_el =
    element ~meta
      ~properties:[ class_name [ "callout-title" ] ]
      ~children:title_children "div"
  in
  let content_el =
    element ~meta
      ~properties:[ class_name [ "callout-content" ] ]
      ~children:(block defs body) "div"
  in
  node ~meta "blockquote"
    [
      ( "data",
        Obj
          [
            ("hName", Str "blockquote");
            ( "hProperties",
              Obj
                (class_name [ "callout" ] :: ("dataCallout", Str kind)
               :: fold_prop) );
          ] );
      ("children", Arr [ title_el; content_el ]);
    ]

(* Document =================================================================*)

let of_doc ?(strip_block_id = true) d =
  block_id_strip := strip_block_id;
  let defs = Doc.defs d in
  let root =
    node ~meta:Meta.none "root" [ ("children", Arr (block defs (Doc.block d))) ]
  in
  let b = Buffer.create 4096 in
  json_to_buffer b root;
  Buffer.contents b
