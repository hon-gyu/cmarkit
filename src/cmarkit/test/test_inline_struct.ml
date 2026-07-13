open Cmarkit_
open Sexplib0.Sexp_conv
module Inline_struct_ = Cmarkit_.Inline_struct

module Inline_struct = struct
  include Inline_struct_

  type byte_pos = Cmarkit_.byte_pos
  type line_pos = Cmarkit_base.Textloc.line_pos

  let sexp_of_byte_pos = Sexplib0.Sexp_conv.sexp_of_int

  let sexp_of_line_pos =
    Sexplib0.Sexp_conv.sexp_of_pair Sexplib0.Sexp_conv.sexp_of_int
      Sexplib0.Sexp_conv.sexp_of_int

  type line_span = Cmarkit_.line_span = {
    line_pos : line_pos;
    first : byte_pos;
    last : byte_pos;
  }
  [@@deriving sexp_of]

  type inline = Inline.t

  let sexp_of_inline = (Sexp.make_sexp_of ()).inline

  type break_type = Inline.Break.type'

  let sexp_of_break_type = function
    | `Hard -> Sexplib0.Sexp.Atom "Hard"
    | `Soft -> Sexplib0.Sexp.Atom "Soft"

  type emphasis_marks = Inline_struct_.emphasis_marks = {
    start : byte_pos;
    char : char;
    count : int;
    may_open : bool;
    may_close : bool;
    open_marker : bool;
    close_marker : bool;
  }
  [@@deriving sexp_of]

  type extra_inline_container_marks = Inline_struct_.extra_inline_container_marks = {
    start : byte_pos;
    char : char;
    kind : Inline.Extra_inline_container.kind;
    curly : bool;
    may_open : bool;
    may_close : bool;
  }

  let sexp_of_extra_inline_container_kind = function
    | Inline.Extra_inline_container.Highlight -> Sexplib0.Sexp.Atom "Highlight"
    | Inline.Extra_inline_container.Superscript -> Sexplib0.Sexp.Atom "Superscript"
    | Inline.Extra_inline_container.Subscript -> Sexplib0.Sexp.Atom "Subscript"
    | Inline.Extra_inline_container.Inserted -> Sexplib0.Sexp.Atom "Inserted"
    | Inline.Extra_inline_container.Deleted -> Sexplib0.Sexp.Atom "Deleted"

  let sexp_of_extra_inline_container_marks m =
    let open Sexplib0.Sexp in
    List
      [
        List [ Atom "start"; sexp_of_byte_pos m.start ];
        List [ Atom "char"; Sexplib0.Sexp_conv.sexp_of_char m.char ];
        List [ Atom "kind"; sexp_of_extra_inline_container_kind m.kind ];
        List [ Atom "curly"; Sexplib0.Sexp_conv.sexp_of_bool m.curly ];
        List [ Atom "may_open"; Sexplib0.Sexp_conv.sexp_of_bool m.may_open ];
        List [ Atom "may_close"; Sexplib0.Sexp_conv.sexp_of_bool m.may_close ];
      ]

  type strikethrough_marks = Inline_struct_.strikethrough_marks = {
    start : byte_pos;
    may_open : bool;
    may_close : bool;
  }
  [@@deriving sexp_of]

  type math_span_marks = Inline_struct_.math_span_marks = {
    start : byte_pos;
    count : int;
    may_open : bool;
    may_close : bool;
  }
  [@@deriving sexp_of]

  type attribute_spec = Inline_struct_.attribute_spec = {
    start : byte_pos;
    attribute : Attribute.t;
    endline : line_span;
    next : byte_pos;
  }

  let sexp_of_attribute_spec a =
    let open Sexplib0.Sexp in
    List
      [ List [ Atom "start"; sexp_of_byte_pos a.start ]
      ; List [ Atom "attribute"; Atom (Attribute.to_string a.attribute) ]
      ; List [ Atom "endline"; sexp_of_line_span a.endline ]
      ; List [ Atom "next"; sexp_of_byte_pos a.next ]
      ]

  type token = Inline_struct_.token =
    | Attribute_spec of attribute_spec
    | Autolink_or_html_start of { start : byte_pos }
    | Backticks of { start : byte_pos; count : int; escaped : bool }
    | Emphasis_marks of emphasis_marks
    | Extra_inline_container_marks of extra_inline_container_marks
    | Inline of {
        start : byte_pos;
        inline : inline;
        endline : line_span;
        next : byte_pos;
      }
    | Link_start of { start : byte_pos; image : bool }
    | Newline of {
        start : byte_pos;
        break_type : break_type;
        newline : line_span;
      }
    | Right_brack of { start : byte_pos }
    | Right_paren of { start : byte_pos }
    | Strikethrough_marks of strikethrough_marks
    | Math_span_marks of math_span_marks
    | Wikilink_start of { start : byte_pos; embed : bool }
    | Jsx_expr_start of { start : byte_pos }
    | Jsx_element_start of { start : byte_pos }
    | Jsx_open of {
        start : byte_pos;
        name_first : byte_pos;
        name_last : byte_pos;
        tag_end : byte_pos;
      }
    | Jsx_close of {
        start : byte_pos;
        name_first : byte_pos;
        name_last : byte_pos;
        tag_end : byte_pos;
      }
  [@@deriving sexp_of]
end

open Inline_struct

let show_sep ?(h2 = false) ?(title = "") () =
  let sep = String.init 10 (fun _ -> if h2 then '-' else '=') in
  if not (String.equal title "") then Format.printf "%s@.%s@." title sep
  else Format.printf "%s@." sep

let print_newline () = print_endline ""
let print_sexp sexp = Format.printf "%a@." Sexplib0.Sexp.pp_hum sexp

let print_tokens (tokens : token list) =
  print_sexp ([%sexp_of: token list] tokens)

let with_output ?(h2 = false) ~title ~f () =
  show_sep ~h2 ~title ();
  f ();
  print_newline ()

let with_sexp ?(h2 = false) ~title ~f () =
  with_output ~h2 ~title ~f:(fun () -> print_sexp (f ())) ()

let with_inline ?(h2 = false) ~title ~f () =
  with_sexp ~h2 ~title ~f:(fun () -> [%sexp_of: inline] (f ())) ()

module Extra_config = Inline.Extra_inline_container.Config

let tokens_of_string ?intraword_emphasis ?marked_emphasis_delims
    ?extra_inline_containers s =
  let line_spans = Inline_parse_api.line_spans s in
  let parser =
    Cmarkit_.Parser_common.parser ?intraword_emphasis ?marked_emphasis_delims
      ?extra_inline_containers ~strict:true s
  in
  let p, lines = (parser, line_spans) in
  let _layout, _meta, lines = strip_paragraph p lines in
  let _cidx, toks, _first_line =
    tokenize ~p ~oymarkit_mod:p.oymarkit_mod ~exts:p.exts p.i lines
  in
  toks

let commonmark_of_inline inline =
  inline |> Block.Paragraph.make |> fun p ->
  Block.Paragraph (p, Meta.none) |> Doc.make |> Cmarkit_commonmark.of_doc

let block_id_meta meta =
  match Block.Block_id.find meta with
  | None -> None
  | Some block_id ->
      Some
        (Sexplib0.Sexp.List
           [ Sexplib0.Sexp.Atom "block-id"
           ; Sexplib0.Sexp.Atom (Block.Block_id.id block_id)
           ])

let extra_inline_container kind inline =
  Inline.Ext_extra_inline_container
    (Inline.Extra_inline_container.make kind inline, Meta.none)

let () =
  with_sexp ~title:"basic inline parse" ~f:(fun () ->
      let s = "*hello*" in
      let line_spans = Inline_parse_api.line_spans s in
      let parser = Cmarkit_.Parser_common.parser ~strict:true s in
      let (res : (byte_pos * string) * inline) =
        let p, lines = (parser, line_spans) in
        let layout, meta, lines = strip_paragraph p lines in
        let cidx, toks, first_line =
          tokenize ~p ~oymarkit_mod:p.oymarkit_mod ~exts:p.exts p.i lines
        in
        p.cidx <- cidx;
        let toks, _had_link = first_pass p toks first_line in
        let toks = second_pass p toks first_line in
        let is = last_pass p toks first_line in
        let inline =
          match is with
          | [ i ] -> i
          | is -> Inline.Inlines (is, meta)
        in
        (layout, inline)
      in
      [%sexp_of: (byte_pos * string) * inline] res)
    ()

let () =
  show_sep ~title:"block ID paragraph suffix" ();
  let sexp_of = Sexp.make_sexp_of ~metas:[ block_id_meta ] () in
  let parse title markdown =
    with_output ~h2:true ~title
      ~f:(fun () ->
        markdown
        |> Doc.of_string ~block_id:true
        |> sexp_of.doc
        |> print_sexp)
      ()
  in
  parse "terminal suffix" "text ^block-id";
  parse "last candidate wins" "a ^x ^final1";
  parse "multiline final line" "first\nsecond ^final";
  parse "non-final candidate" "first ^not-final\nsecond";
  parse "invalid underscore" "text ^block_id";
  parse "escaped caret" "text \\^block-id";
  parse "list item paragraph" "- item ^item-id"

let () =
  show_sep ~title:"Djot attributes" ();
  let sexp_of = Sexp.make_sexp_of () in
  let parse_inline title markdown =
    with_output ~h2:true ~title
      ~f:(fun () ->
        markdown
        |> Inline_parse_api.of_string ~djot_inline_attributes:true
        |> sexp_of.inline
        |> print_sexp)
      ()
  in
  let parse_block title markdown =
    with_output ~h2:true ~title
      ~f:(fun () ->
        markdown
        |> Doc.of_string ~djot_block_attributes:true
        |> sexp_of.doc
        |> print_sexp)
      ()
  in
  parse_inline "stacked text attributes" "avant{lang=fr}{.blue}";
  parse_inline "emphasis attributes" "_text_{#foo .bar}";
  with_output ~h2:true ~title:"extra container attributes"
    ~f:(fun () ->
      "{=text=}{.marked}"
      |> Inline_parse_api.of_string ~djot_inline_attributes:true
           ~extra_inline_containers:Extra_config.explicit
      |> sexp_of.inline
      |> print_sexp)
    ();
  parse_inline "multiline inline attributes" "text{#foo\n.bar key=\"a b\"}";
  (* Djot comments: [%...%] inside a specifier is ignored. A comment kept
     alongside real attributes is stripped but the attributes apply. A
     specifier that is only a comment (or empty) is dropped entirely: it
     neither attaches to a target nor renders literally. *)
  parse_inline "comment within attributes" "word{#id % keep me %}";
  parse_inline "comment-only specifier dropped" "word{% comment %} tail";
  parse_inline "standalone comment dropped"
    "Foo bar {% multi\nline comment %} baz.";
  parse_inline "empty specifier dropped" "word{} tail";
  parse_block "stacked block attributes"
    "{#water}\n{.important .large}\nFlow.";
  parse_block "block quote attributes" "{source=Iliad}\n> Sing, muse";
  parse_block "multiline block attributes"
    "{#water\n  .important key=\"two words\"}\nFlow.";
  parse_block "block comment-only specifier dropped"
    "{% a block comment %}\nFlow.";
  parse_block "block comment among specifiers dropped"
    "{#water}\n{% note %}\nFlow."

let () =
  show_sep ~title:"intraword emphasis knob tokenization" ();
  let tokens title f =
    with_output ~h2:true ~title ~f:(fun () -> print_tokens (f ())) ()
  in
  tokens "default" (fun () -> tokens_of_string "a*b*c");
  tokens "intraword_emphasis:false" (fun () ->
      tokens_of_string ~intraword_emphasis:false "a*b*c");
  tokens "boundary emphasis still tokenizes" (fun () ->
      tokens_of_string ~intraword_emphasis:false "*hello*")

let () =
  show_sep ~title:"marked emphasis delimiter tokenization" ();
  let tokens title f =
    with_output ~h2:true ~title ~f:(fun () -> print_tokens (f ())) ()
  in
  tokens "default" (fun () -> tokens_of_string "{_hello_}");
  tokens "marked_emphasis_delims:true" (fun () ->
      tokens_of_string ~marked_emphasis_delims:true "{_hello_}");
  tokens "forced opener cannot close" (fun () ->
      tokens_of_string ~marked_emphasis_delims:true "a{_b_");
  tokens "forced closer cannot open" (fun () ->
      tokens_of_string ~marked_emphasis_delims:true "_b_}a")

let () =
  show_sep ~title:"marked emphasis delimiter parse" ();
  with_inline ~h2:true ~title:"default" ~f:(fun () ->
      Inline_parse_api.of_string "{_hello_}") ();
  with_inline ~h2:true ~title:"marked_emphasis_delims:true" ~f:(fun () ->
      Inline_parse_api.of_string ~marked_emphasis_delims:true "{_hello_}") ()

let () =
  show_sep ~title:"strong emphasis width parse" ();
  with_inline ~h2:true ~title:"default" ~f:(fun () ->
      Inline_parse_api.of_string "*hello*") ();
  with_inline ~h2:true ~title:"strong_emphasis_width:1" ~f:(fun () ->
      Inline_parse_api.of_string ~strong_emphasis_width:1 "*hello*") ()

let () =
  show_sep ~title:"extra inline container parse" ();
  with_inline ~h2:true ~title:"default" ~f:(fun () ->
      Inline_parse_api.of_string "{=hello=}") ();
  with_output ~h2:true ~title:"extra_inline_containers:explicit tokens"
    ~f:(fun () ->
      tokens_of_string ~extra_inline_containers:Extra_config.explicit
        "{=hello=}"
      |> print_tokens)
    ();
  with_output ~h2:true ~title:"extra_inline_containers:explicit parse"
    ~f:(fun () ->
      List.iter
        (fun source ->
          Inline_parse_api.of_string
            ~extra_inline_containers:Extra_config.explicit source
          |> [%sexp_of: inline] |> print_sexp)
        [ "{=hello=}"; "{^hello^}"; "{~hello~}"; "{+hello+}"; "{-hello-}" ])
    ();
  with_inline ~h2:true ~title:"nested inline payload" ~f:(fun () ->
      Inline_parse_api.of_string
        ~extra_inline_containers:Extra_config.explicit "{=a *b*=}")
    ();
  let optional =
    Extra_config.make ~highlight:Extra_config.Curly_optional ()
  in
  with_inline ~h2:true ~title:"optional curly shorthand" ~f:(fun () ->
      Inline_parse_api.of_string ~extra_inline_containers:optional "=hello=")
    ()

(** Extra inline containers obey the first-closed-opener rule. Crossing
    delimiters do not overlap: closing the outer highlight turns the pending
    superscript opener into text. Properly closed inner containers remain
    nested, including containers of the same kind. *)
let () =
  show_sep ~title:"extra inline container precedence" ();
  let optional =
    Extra_config.make ~highlight:Extra_config.Curly_optional
      ~superscript:Extra_config.Curly_optional ()
  in
  with_inline ~h2:true ~title:"first closed opener wins" ~f:(fun () ->
      Inline_parse_api.of_string ~extra_inline_containers:optional
        "=a ^b= c^")
    ();
  with_inline ~h2:true ~title:"nested extra containers" ~f:(fun () ->
      Inline_parse_api.of_string ~extra_inline_containers:optional
        "=a ^b^ c=")
    ();
  with_inline ~h2:true ~title:"nested same-kind extra containers" ~f:(fun () ->
      Inline_parse_api.of_string
        ~extra_inline_containers:Extra_config.explicit "{=a {=b=} c=}")
    ()

let () =
  show_sep ~title:"marked emphasis delimiter commonmark rendering" ();
  let emphasis ?(open_marker = false) ?(close_marker = false) () =
    Inline.Emphasis
      ( Inline.Emphasis.make ~delim:'_' ~open_marker ~close_marker
          (Inline.Text ("hello", Meta.none)),
        Meta.none )
  in
  with_output ~h2:true ~title:"without markers" ~f:(fun () ->
      emphasis () |> commonmark_of_inline |> print_string)
    ();
  with_output ~h2:true ~title:"with markers" ~f:(fun () ->
      emphasis ~open_marker:true ~close_marker:true ()
      |> commonmark_of_inline |> print_string)
    ();
  with_output ~h2:true ~title:"parsed marked delimiters" ~f:(fun () ->
      Doc.of_string ~marked_emphasis_delims:true "{_hello_}"
      |> Cmarkit_commonmark.of_doc |> print_string)
    ()

let () =
  show_sep ~title:"wikilink tokenization" ();
  let tokens title f =
    with_output ~h2:true ~title ~f:(fun () -> print_tokens (f ())) ()
  in
  let toks ?(wikilink = true) s =
    let line_spans = Inline_parse_api.line_spans s in
    let parser =
      Cmarkit_.Parser_common.parser ~wikilink ~strict:false s
    in
    let p, lines = (parser, line_spans) in
    let _layout, _meta, lines = strip_paragraph p lines in
    let _cidx, toks, _first_line =
      tokenize ~p ~oymarkit_mod:p.oymarkit_mod ~exts:p.exts p.i lines
    in
    toks
  in
  tokens "disabled keeps link tokens" (fun () -> toks ~wikilink:false "[[a]]");
  tokens "enabled emits wikilink start" (fun () -> toks "[[a]]");
  tokens "embed form" (fun () -> toks "![[a]]");
  tokens "unterminated falls back to links" (fun () -> toks "[[a")

let () =
  show_sep ~title:"wikilink parse" ();
  let sexp_of = Sexp.make_sexp_of () in
  let parse title markdown =
    with_output ~h2:true ~title
      ~f:(fun () ->
        markdown
        |> Inline_parse_api.of_string ~wikilink:true ~strict:false
        |> sexp_of.inline
        |> print_sexp)
      ()
  in
  parse "target only" "[[Note]]";
  parse "heading and display" "[[Note#H1#H2|Display]]";
  parse "block reference" "[[Note#^block-1]]";
  parse "embed" "![[Pic.png]]";
  parse "display only" "[[|just text]]";
  parse "among other inlines" "a [[w]] and *b* and [l](u)";
  parse "not parsed in code span" "`[[w]]`"

let () =
  show_sep ~title:"wikilink commonmark roundtrip" ();
  let roundtrip title markdown =
    with_output ~h2:true ~title
      ~f:(fun () ->
        Doc.of_string ~wikilink:true ~strict:false markdown
        |> Cmarkit_commonmark.of_doc |> print_string)
      ()
  in
  roundtrip "link form" "[[Note#H|D]]";
  roundtrip "embed form" "![[Pic.png]]";
  roundtrip "verbatim spacing preserved" "[[ a # b | c ]]"

let callout_meta meta =
  match Block.Callout.find meta with
  | None -> None
  | Some c ->
      let fold =
        match Block.Callout.fold c with
        | None -> "none"
        | Some Block.Callout.Foldable_open -> "open"
        | Some Block.Callout.Foldable_closed -> "closed"
      in
      Some
        (Sexplib0.Sexp.List
           [ Sexplib0.Sexp.Atom "callout"
           ; Sexplib0.Sexp.Atom (Block.Callout.kind c)
           ; Sexplib0.Sexp.List [ Atom "fold"; Atom fold ]
           ])

let () =
  show_sep ~title:"callout parse" ();
  let sexp_of = Sexp.make_sexp_of ~metas:[ callout_meta ] () in
  let cfg = Block.Callout.Config.make () in
  let parse title markdown =
    with_output ~h2:true ~title
      ~f:(fun () ->
        markdown
        |> Doc.of_string ~callout:cfg ~strict:false
        |> sexp_of.doc
        |> print_sexp)
      ()
  in
  parse "title and body" "> [!info] Title\n> Body";
  parse "title only (default title)" "> [!tip]";
  parse "foldable collapsed" "> [!faq]- Question?\n> Answer";
  parse "foldable expanded" "> [!note]+ Expanded\n> body";
  parse "case insensitive kind" "> [!WARNING] Watch out";
  parse "not a callout" "> just a quote";
  parse "empty kind rejected" "> [!] nope";
  parse "kind with space rejected" "> [!bad kind] x";
  parse "nested callout" "> [!quote]\n> outer\n> > [!note] inner\n> > deep"

let () =
  show_sep ~title:"callout restricted kinds" ();
  let sexp_of = Sexp.make_sexp_of ~metas:[ callout_meta ] () in
  let cfg = Block.Callout.Config.make ~kinds:(Block.Callout.Config.Only [ "info" ]) () in
  let parse title markdown =
    with_output ~h2:true ~title
      ~f:(fun () ->
        markdown |> Doc.of_string ~callout:cfg ~strict:false |> sexp_of.doc |> print_sexp)
      ()
  in
  parse "allowed kind" "> [!info] yes";
  parse "disallowed kind stays blockquote" "> [!tip] no"

let () =
  show_sep ~title:"callout html rendering" ();
  let cfg = Block.Callout.Config.make () in
  let render title markdown =
    with_output ~h2:true ~title
      ~f:(fun () ->
        Doc.of_string ~callout:cfg ~strict:false markdown
        |> Cmarkit_html.of_doc ~safe:false
        |> print_string)
      ()
  in
  render "non-foldable" "> [!info] Title\n> Body";
  render "foldable expanded" "> [!note]+ T\n> body";
  render "foldable collapsed" "> [!faq]- T\n> body";
  (* Title is a full inline container: emphasis and links survive. *)
  render "formatted title" "> [!info] See **bold** and [text](u)\n> body";
  render "title starts with formatting" "> [!tip]**right away**\n> body"

let () =
  show_sep ~title:"callout commonmark roundtrip is stable" ();
  let cfg = Block.Callout.Config.make () in
  let render s = Doc.of_string ~callout:cfg ~strict:false s |> Cmarkit_commonmark.of_doc in
  let check title markdown =
    with_output ~h2:true ~title
      ~f:(fun () ->
        let r1 = render markdown in
        let r2 = render r1 in
        Printf.printf "stable: %b\n%s" (String.equal r1 r2) r1)
      ()
  in
  check "with body" "> [!info] T\n> body\n> more";
  check "foldable" "> [!faq]- Q?\n> A"

let () =
  show_sep ~title:"extra inline container AST support" ();
  let container kind =
    extra_inline_container kind (Inline.Text ("hello", Meta.none))
  in
  with_inline ~h2:true ~title:"sexp" ~f:(fun () ->
      container Inline.Extra_inline_container.Highlight)
    ();
  with_output ~h2:true ~title:"commonmark rendering" ~f:(fun () ->
      List.iter
        (fun kind -> container kind |> commonmark_of_inline |> print_string)
        [ Inline.Extra_inline_container.Highlight;
          Inline.Extra_inline_container.Superscript;
          Inline.Extra_inline_container.Subscript;
          Inline.Extra_inline_container.Inserted;
          Inline.Extra_inline_container.Deleted ])
    ()
