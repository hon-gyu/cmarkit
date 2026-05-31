open Cmarkit_

let fail_inline msg i =
  let sexp = (Sexp.make_sexp_of ()).inline i in
  failwith (Fmt.str "%s: %a" msg Sexplib0.Sexp.pp_hum sexp)

let () =
  match Inline_parse.of_string "__jia__" with
  | Inline.Strong_emphasis ({ inline = Inline.Text ("jia", _); _ }, _) -> ()
  | i -> fail_inline "default parser should keep CommonMark strong emphasis" i

let () =
  match
    Inline_parse.of_string ~emphasis_delims:[ '_' ]
      ~strong_emphasis_delims:[ '*' ] "__jia__"
  with
  | Inline.Emphasis
      ( { inline =
            Inline.Emphasis ({ inline = Inline.Text ("jia", _); _ }, _);
          _ },
        _ ) ->
      ()
  | i ->
      fail_inline
        "parser should fall back to nested emphasis when '_' is not strong" i

let () =
  match
    Inline_parse.of_string ~emphasis_delims:[ '_' ]
      ~strong_emphasis_delims:[ '*' ] "*jia*"
  with
  | Inline.Text ("*jia*", _) -> ()
  | i -> fail_inline "parser should reject '*' as an emphasis delimiter" i

let () =
  match
    Inline_parse.of_string ~emphasis_delims:[ '_' ]
      ~strong_emphasis_delims:[ '*' ] "**jia**"
  with
  | Inline.Strong_emphasis ({ inline = Inline.Text ("jia", _); _ }, _) -> ()
  | i -> fail_inline "parser should accept '*' as a strong delimiter" i

let () =
  print_endline "EOF"
