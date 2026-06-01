open Cmarkit_

let inline_of_string = Inline_parse_api.of_string

let fail_inline msg i =
  let sexp = (Sexp.make_sexp_of ()).inline i in
  failwith (Fmt.str "%s: %a" msg Sexplib0.Sexp.pp_hum sexp)

let text s = Inline.Text (s, Meta.none)

let inline_container kind inline =
  Inline.Ext_inline_container
    (Inline.Inline_container.make kind inline, Meta.none)

let () =
  match inline_of_string "__jia__" with
  | Inline.Strong_emphasis ({ inline = Inline.Text ("jia", _); _ }, _) -> ()
  | i -> fail_inline "default parser should keep CommonMark strong emphasis" i

let () =
  match
    inline_of_string ~emphasis_delims:[ '_' ] ~strong_emphasis_delims:[ '*' ]
      "__jia__"
  with
  | Inline.Emphasis
      ( {
          inline = Inline.Emphasis ({ inline = Inline.Text ("jia", _); _ }, _);
          _;
        },
        _ ) ->
      ()
  | i ->
      fail_inline
        "parser should fall back to nested emphasis when '_' is not strong" i

let () =
  match
    inline_of_string ~emphasis_delims:[ '_' ] ~strong_emphasis_delims:[ '*' ]
      "*jia*"
  with
  | Inline.Text ("*jia*", _) -> ()
  | i -> fail_inline "parser should reject '*' as an emphasis delimiter" i

let () =
  match
    inline_of_string ~emphasis_delims:[ '_' ] ~strong_emphasis_delims:[ '*' ]
      "**jia**"
  with
  | Inline.Strong_emphasis ({ inline = Inline.Text ("jia", _); _ }, _) -> ()
  | i -> fail_inline "parser should accept '*' as a strong delimiter" i

let () =
  match inline_of_string ~strong_emphasis_width:1 "*jia*" with
  | Inline.Strong_emphasis ({ inline = Inline.Text ("jia", _); _ }, _) -> ()
  | i -> fail_inline "parser should accept one-character strong emphasis" i

let () =
  match
    inline_of_string ~strong_emphasis_width:1 ~strong_emphasis_delims:[ '*' ]
      "_jia_"
  with
  | Inline.Emphasis ({ inline = Inline.Text ("jia", _); _ }, _) -> ()
  | i ->
      fail_inline
        "parser should keep disallowed one-character strong as emphasis" i

let () =
  let inline =
    inline_container Inline.Inline_container.Highlight
      (Inline.Inlines ([ text "jia" ], Meta.none))
  in
  match Inline.normalize inline with
  | Inline.Ext_inline_container (c, _) ->
      begin match Inline.Inline_container.inline c with
      | Inline.Text ("jia", _) -> ()
      | i -> fail_inline "inline container should normalize payload" i
      end
  | i -> fail_inline "inline container should survive normalization" i

let () =
  let inline = inline_container Inline.Inline_container.Inserted (text "jia") in
  let mapper =
    Mapper.make
      ~inline:(fun _ -> function
        | Inline.Text ("jia", m) -> Mapper.ret (Inline.Text ("x", m))
        | _ -> Mapper.default)
      ()
  in
  match Mapper.map_inline mapper inline with
  | Some (Inline.Ext_inline_container (c, _)) ->
      begin match Inline.Inline_container.inline c with
      | Inline.Text ("x", _) -> ()
      | i -> fail_inline "inline container mapper should map payload" i
      end
  | Some i -> fail_inline "inline container should survive mapper" i
  | None -> failwith "inline container mapper should not delete node"

let () =
  let inline = inline_container Inline.Inline_container.Deleted (text "jia") in
  let folder =
    Folder.make
      ~inline:(fun _ acc -> function
        | Inline.Text _ -> Folder.ret (acc + 1)
        | _ -> Folder.default)
      ()
  in
  match Folder.fold_inline folder 0 inline with
  | 1 -> ()
  | n -> failwith (Fmt.str "inline container folder saw %d text nodes" n)

let () =
  match inline_of_string "{=jia=}" with
  | Inline.Text ("{=jia=}", _) -> ()
  | i ->
      fail_inline "default parser should keep inline container syntax literal" i

let () =
  match inline_of_string ~inline_containers:true "{=jia=}" with
  | Inline.Ext_inline_container (c, _) ->
      begin match
        (Inline.Inline_container.kind c, Inline.Inline_container.inline c)
      with
      | Inline.Inline_container.Highlight, Inline.Text ("jia", _) -> ()
      | _, i -> fail_inline "parser should parse highlight container payload" i
      end
  | i -> fail_inline "parser should parse highlight container" i

let () =
  match inline_of_string ~inline_containers:true "{=a *b*=}" with
  | Inline.Ext_inline_container (c, _) ->
      begin match Inline.Inline_container.inline c with
      | Inline.Inlines
          ( [
              Inline.Text ("a ", _);
              Inline.Emphasis ({ inline = Inline.Text ("b", _); _ }, _);
            ],
            _ ) ->
          ()
      | i -> fail_inline "parser should parse nested inline container payload" i
      end
  | i -> fail_inline "parser should parse nested inline container" i

let () = print_endline "EOF"
