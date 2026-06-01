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

  type token = Inline_struct_.token =
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
    tokenize ~oymarkit_mod:p.oymarkit_mod ~exts:p.exts p.i lines
  in
  toks

let commonmark_of_inline inline =
  inline |> Block.Paragraph.make |> fun p ->
  Block.Paragraph (p, Meta.none) |> Doc.make |> Cmarkit_commonmark.of_doc

let extra_inline_container kind inline =
  Inline.Ext_extra_inline_container
    (Inline.Extra_inline_container.make kind inline, Meta.none)

let () =
  show_sep ~title:"basic inline parse" ();
  let s = "*hello*" in
  let line_spans = Inline_parse_api.line_spans s in
  let parser = Cmarkit_.Parser_common.parser ~strict:true s in
  (* let (res : (byte_pos * string) * inline) = parse parser line_spans in *)
  let (res : (byte_pos * string) * inline) =
    begin
      let p, lines = (parser, line_spans) in
      let layout, meta, lines = strip_paragraph p lines in
      let cidx, toks, first_line =
        tokenize ~oymarkit_mod:p.oymarkit_mod ~exts:p.exts p.i lines
      in
      p.cidx <- cidx;
      (* let is, _had_link = parse_tokens p toks first_line in *)
      let is, _had_link =
        begin
          let toks, had_link = first_pass p toks first_line in
          let toks = second_pass p toks first_line in
          (last_pass p toks first_line, had_link)
        end
      in
      let inline =
        match is with
        | [ i ] -> i
        | is -> Inline.Inlines (is, meta)
      in
      (layout, inline)
    end
  in
  print_sexp ([%sexp_of: (byte_pos * string) * inline] res);
  print_endline "\n"

let () =
  show_sep ~title:"intraword emphasis knob tokenization" ();
  show_sep ~h2:true ~title:"default" ();
  print_tokens (tokens_of_string "a*b*c");
  print_newline ();
  show_sep ~h2:true ~title:"intraword_emphasis:false" ();
  print_tokens (tokens_of_string ~intraword_emphasis:false "a*b*c");
  print_newline ();
  show_sep ~h2:true ~title:"boundary emphasis still tokenizes" ();
  print_tokens (tokens_of_string ~intraword_emphasis:false "*hello*");
  print_newline ()

let () =
  show_sep ~title:"marked emphasis delimiter tokenization" ();
  show_sep ~h2:true ~title:"default" ();
  print_tokens (tokens_of_string "{_hello_}");
  print_newline ();
  show_sep ~h2:true ~title:"marked_emphasis_delims:true" ();
  print_tokens (tokens_of_string ~marked_emphasis_delims:true "{_hello_}");
  print_newline ();
  show_sep ~h2:true ~title:"forced opener cannot close" ();
  print_tokens (tokens_of_string ~marked_emphasis_delims:true "a{_b_");
  print_newline ();
  show_sep ~h2:true ~title:"forced closer cannot open" ();
  print_tokens (tokens_of_string ~marked_emphasis_delims:true "_b_}a");
  print_newline ()

let () =
  show_sep ~title:"marked emphasis delimiter parse" ();
  show_sep ~h2:true ~title:"default" ();
  print_sexp ([%sexp_of: inline] (Inline_parse_api.of_string "{_hello_}"));
  print_newline ();
  show_sep ~h2:true ~title:"marked_emphasis_delims:true" ();
  print_sexp
    ([%sexp_of: inline]
       (Inline_parse_api.of_string ~marked_emphasis_delims:true "{_hello_}"));
  print_newline ()

let () =
  show_sep ~title:"strong emphasis width parse" ();
  show_sep ~h2:true ~title:"default" ();
  print_sexp ([%sexp_of: inline] (Inline_parse_api.of_string "*hello*"));
  print_newline ();
  show_sep ~h2:true ~title:"strong_emphasis_width:1" ();
  print_sexp
    ([%sexp_of: inline]
       (Inline_parse_api.of_string ~strong_emphasis_width:1 "*hello*"));
  print_newline ()

let () =
  show_sep ~title:"extra inline container parse" ();
  show_sep ~h2:true ~title:"default" ();
  print_sexp ([%sexp_of: inline] (Inline_parse_api.of_string "{=hello=}"));
  print_newline ();
  show_sep ~h2:true ~title:"extra_inline_containers:explicit tokens" ();
  print_tokens
    (tokens_of_string ~extra_inline_containers:Extra_config.explicit
       "{=hello=}");
  print_newline ();
  show_sep ~h2:true ~title:"extra_inline_containers:explicit parse" ();
  print_sexp
    ([%sexp_of: inline]
       (Inline_parse_api.of_string
          ~extra_inline_containers:Extra_config.explicit "{=hello=}"));
  print_sexp
    ([%sexp_of: inline]
       (Inline_parse_api.of_string
          ~extra_inline_containers:Extra_config.explicit "{^hello^}"));
  print_sexp
    ([%sexp_of: inline]
       (Inline_parse_api.of_string
          ~extra_inline_containers:Extra_config.explicit "{~hello~}"));
  print_sexp
    ([%sexp_of: inline]
       (Inline_parse_api.of_string
          ~extra_inline_containers:Extra_config.explicit "{+hello+}"));
  print_sexp
    ([%sexp_of: inline]
       (Inline_parse_api.of_string
          ~extra_inline_containers:Extra_config.explicit "{-hello-}"));
  print_newline ();
  show_sep ~h2:true ~title:"nested inline payload" ();
  print_sexp
    ([%sexp_of: inline]
       (Inline_parse_api.of_string
          ~extra_inline_containers:Extra_config.explicit "{=a *b*=}"));
  print_newline ();
  let shorthand =
    Extra_config.make ~highlight:Extra_config.Curly_optional
      ~superscript:Extra_config.Curly_optional ()
  in
  show_sep ~h2:true ~title:"optional curly shorthand" ();
  print_sexp
    ([%sexp_of: inline]
       (Inline_parse_api.of_string ~extra_inline_containers:shorthand
          "=hello="));
  print_newline ();
  show_sep ~h2:true ~title:"first closed opener wins" ();
  print_sexp
    ([%sexp_of: inline]
       (Inline_parse_api.of_string ~extra_inline_containers:shorthand
          "=a ^b= c^"));
  print_newline ();
  show_sep ~h2:true ~title:"nested extra containers" ();
  print_sexp
    ([%sexp_of: inline]
       (Inline_parse_api.of_string ~extra_inline_containers:shorthand
          "=a ^b^ c="));
  print_newline ();
  show_sep ~h2:true ~title:"nested same-kind extra containers" ();
  print_sexp
    ([%sexp_of: inline]
       (Inline_parse_api.of_string
          ~extra_inline_containers:Extra_config.explicit
          "{=a {=b=} c=}"));
  print_newline ()

let () =
  show_sep ~title:"marked emphasis delimiter commonmark rendering" ();
  show_sep ~h2:true ~title:"without markers" ();
  print_string
    (commonmark_of_inline
       (Inline.Emphasis
          ( Inline.Emphasis.make ~delim:'_' (Inline.Text ("hello", Meta.none)),
            Meta.none )));
  print_newline ();
  show_sep ~h2:true ~title:"with markers" ();
  print_string
    (commonmark_of_inline
       (Inline.Emphasis
          ( Inline.Emphasis.make ~delim:'_' ~open_marker:true ~close_marker:true
              (Inline.Text ("hello", Meta.none)),
            Meta.none )));
  print_newline ();
  show_sep ~h2:true ~title:"parsed marked delimiters" ();
  print_string
    (Cmarkit_commonmark.of_doc
       (Doc.of_string ~marked_emphasis_delims:true "{_hello_}"));
  print_newline ()

let () =
  show_sep ~title:"extra inline container AST support" ();
  show_sep ~h2:true ~title:"sexp" ();
  print_sexp
    ([%sexp_of: inline]
       (extra_inline_container Inline.Extra_inline_container.Highlight
          (Inline.Text ("hello", Meta.none))));
  print_newline ();
  show_sep ~h2:true ~title:"commonmark rendering" ();
  let render kind =
    print_string
      (commonmark_of_inline
         (extra_inline_container kind (Inline.Text ("hello", Meta.none))))
  in
  render Inline.Extra_inline_container.Highlight;
  render Inline.Extra_inline_container.Superscript;
  render Inline.Extra_inline_container.Subscript;
  render Inline.Extra_inline_container.Inserted;
  render Inline.Extra_inline_container.Deleted;
  print_newline ()
