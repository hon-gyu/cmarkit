open Cmarkit_

let inline_of_string = Inline_parse_api.of_string

let fail_inline msg i =
  let sexp = (Sexp.make_sexp_of ()).inline i in
  failwith (Fmt.str "%s: %a" msg Sexplib0.Sexp.pp_hum sexp)

let fail_block msg b =
  let sexp = (Sexp.make_sexp_of ()).block b in
  failwith (Fmt.str "%s: %a" msg Sexplib0.Sexp.pp_hum sexp)

let block_sexp b = (Sexp.make_sexp_of ()).block b

let text s = Inline.Text (s, Meta.none)
module Extra_config = Inline.Extra_inline_container.Config

let extra_inline_container kind inline =
  Inline.Ext_extra_inline_container
    (Inline.Extra_inline_container.make kind inline, Meta.none)

let block_id_of_paragraph = function
  | Block.Paragraph (_, meta) ->
      Option.map Block.Block_id.id (Block.Block_id.find meta)
  | _ -> None

let rec first_block_id = function
  | Block.Blocks (blocks, _) -> List.find_map first_block_id blocks
  | Block.Block_quote (quote, _) ->
      first_block_id (Block.Block_quote.block quote)
  | Block.List (list, _) ->
      List.find_map
        (fun (item, _) -> first_block_id (Block.List_item.block item))
        (Block.List'.items list)
  | block -> block_id_of_paragraph block

let parsed_block_id ?block_id markdown =
  markdown |> Doc.of_string ?block_id |> Doc.block |> first_block_id

let () =
  match parsed_block_id "text ^block-id" with
  | None -> ()
  | Some id ->
      failwith (Fmt.str "default parser unexpectedly parsed block ID %S" id)

let () =
  match parsed_block_id ~block_id:true "text ^block-id" with
  | Some "block-id" -> ()
  | Some id -> failwith (Fmt.str "parser returned wrong block ID %S" id)
  | None -> failwith "parser did not recognize terminal block ID"

let () =
  match
    ( parsed_block_id ~block_id:true "text ^block_id",
      parsed_block_id ~block_id:true "text \\^block-id",
      parsed_block_id ~block_id:true "text ^block-id more" )
  with
  | None, None, None -> ()
  | _ -> failwith "parser accepted an invalid block ID suffix"

let () =
  match
    ( parsed_block_id ~block_id:true "first ^not-final\nsecond",
      parsed_block_id ~block_id:true "first\nsecond ^final",
      parsed_block_id ~block_id:true "- item ^item-id" )
  with
  | None, Some "final", Some "item-id" -> ()
  | _ -> failwith "parser mishandled block ID ownership or final-line rules"

let attribute_string a = Attribute.to_string a

let () =
  match inline_of_string ~djot_inline_attributes:true "avant{lang=fr}{.blue}" with
  | Inline.Ext_attributes (a, _) ->
      begin match
        attribute_string (Inline.Attributes.attributes a),
        Inline.Attributes.inline a
      with
      | ".blue lang=fr", Inline.Text ("avant", _) -> ()
      | attrs, i ->
          fail_inline (Fmt.str "wrong bare-text attributes %S" attrs) i
      end
  | i -> fail_inline "parser should attach stacked attributes to text" i

let () =
  match inline_of_string ~djot_inline_attributes:true "_text_{#last .a}" with
  | Inline.Ext_attributes (a, _) ->
      begin match Inline.Attributes.inline a with
      | Inline.Emphasis _ ->
          if attribute_string (Inline.Attributes.attributes a) <> "#last .a"
          then failwith "parser returned wrong emphasis attributes"
      | i -> fail_inline "attributes should target the complete emphasis" i
      end
  | i -> fail_inline "parser should attach attributes to emphasis" i

let () =
  match
    inline_of_string ~djot_inline_attributes:true
      ~extra_inline_containers:Extra_config.explicit "{=text=}{.marked}"
  with
  | Inline.Ext_attributes (a, _) ->
      begin match Inline.Attributes.inline a with
      | Inline.Ext_extra_inline_container _ -> ()
      | i -> fail_inline "attributes should target the extra container" i
      end
  | i -> fail_inline "parser should attach attributes after extra containers" i

let () =
  match inline_of_string ~djot_inline_attributes:true "text{#foo\n.bar key=\"a b\"}" with
  | Inline.Ext_attributes (a, _) ->
      if attribute_string (Inline.Attributes.attributes a)
         <> "#foo .bar key=\"a b\""
      then failwith "parser returned wrong multiline inline attributes"
  | i -> fail_inline "parser should parse multiline inline attributes" i

let () =
  match inline_of_string "text{.literal}" with
  | Inline.Text ("text{.literal}", _) -> ()
  | i -> fail_inline "default parser should keep attribute syntax literal" i

let rec first_block = function
  | Block.Blocks (b :: _, _) -> first_block b
  | b -> b

let () =
  match
    Doc.of_string ~djot_block_attributes:true
      "{#water}\n{.important .large}\nDon't forget."
    |> Doc.block |> first_block
  with
  | Block.Ext_attributes (a, _) ->
      begin match Block.Attributes.block a with
      | Block.Paragraph _ ->
          if attribute_string (Block.Attributes.attributes a)
             <> "#water .important .large"
          then failwith "parser returned wrong block attributes"
      | _ -> failwith "block attributes should target the paragraph"
      end
  | _ -> failwith "parser should attach repeated block attributes"

let () =
  match
    Doc.of_string ~djot_block_attributes:true "{source=Iliad}\n> Sing, muse"
    |> Doc.block |> first_block
  with
  | Block.Ext_attributes (a, _) ->
      begin match Block.Attributes.block a with
      | Block.Block_quote _ -> ()
      | _ -> failwith "block attributes should target the block quote"
      end
  | _ -> failwith "parser should attach attributes before a block quote"

let () =
  match
    Doc.of_string ~djot_block_attributes:true
      "{#water\n  .important key=\"two words\"}\nFlow."
    |> Doc.block |> first_block
  with
  | Block.Ext_attributes (a, _) ->
      if attribute_string (Block.Attributes.attributes a)
         <> "#water .important key=\"two words\""
      then failwith "parser returned wrong multiline block attributes"
  | _ -> failwith "parser should parse indented multiline block attributes"

let () =
  match
    Doc.of_string ~djot_block_attributes:true "{#orphan}\n\nParagraph."
    |> Doc.block
  with
  | Block.Blocks
      ( Block.Ext_attributes (a, _)
        :: Block.Blank_line _
        :: Block.Paragraph _
        :: _,
        _ )
    ->
      begin match Block.Attributes.block a with
      | Block.Blocks ([], _) -> ()
      | _ -> failwith "orphan block attributes should have no target"
      end
  | _ -> failwith "blank lines should prevent block attribute attachment"

let () =
  let rendered =
    Doc.of_string ~djot_inline_attributes:true "_text_{#foo .bar}"
    |> Cmarkit_commonmark.of_doc
  in
  if rendered <> "_text_{#foo .bar}"
  then failwith (Fmt.str "inline attribute roundtrip rendered %S" rendered)

let () =
  let rendered =
    Doc.of_string ~djot_block_attributes:true "{#water}\nFlow."
    |> Cmarkit_commonmark.of_doc
  in
  if rendered <> "{#water}\nFlow."
  then failwith (Fmt.str "block attribute roundtrip rendered %S" rendered)

let () =
  let source = "text{#foo\n.bar %keep me% key=\"a b\"}" in
  let rendered =
    Doc.of_string ~djot_inline_attributes:true source
    |> Cmarkit_commonmark.of_doc
  in
  if rendered <> source
  then failwith (Fmt.str "multiline attribute roundtrip rendered %S" rendered)

let () =
  let rendered =
    Doc.of_string ~djot_inline_attributes:true "_text_{#foo .bar}"
    |> Cmarkit_html.of_doc ~safe:false
  in
  if rendered <> "<p><em id=\"foo\" class=\"bar\">text</em></p>\n"
  then failwith (Fmt.str "inline attribute HTML rendered %S" rendered)

let () =
  let rendered =
    Doc.of_string ~djot_block_attributes:true "{#water .important}\nFlow."
    |> Cmarkit_html.of_doc ~safe:false
  in
  if rendered <> "<p id=\"water\" class=\"important\">Flow.</p>\n"
  then failwith (Fmt.str "block attribute HTML rendered %S" rendered)

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
    extra_inline_container Inline.Extra_inline_container.Highlight
      (Inline.Inlines ([ text "jia" ], Meta.none))
  in
  match Inline.normalize inline with
  | Inline.Ext_extra_inline_container (c, _) ->
      begin match Inline.Extra_inline_container.inline c with
      | Inline.Text ("jia", _) -> ()
      | i -> fail_inline "extra inline container should normalize payload" i
      end
  | i -> fail_inline "extra inline container should survive normalization" i

let () =
  let inline = extra_inline_container Inline.Extra_inline_container.Inserted (text "jia") in
  let mapper =
    Mapper.make
      ~inline:(fun _ -> function
        | Inline.Text ("jia", m) -> Mapper.ret (Inline.Text ("x", m))
        | _ -> Mapper.default)
      ()
  in
  match Mapper.map_inline mapper inline with
  | Some (Inline.Ext_extra_inline_container (c, _)) ->
      begin match Inline.Extra_inline_container.inline c with
      | Inline.Text ("x", _) -> ()
      | i -> fail_inline "extra inline container mapper should map payload" i
      end
  | Some i -> fail_inline "extra inline container should survive mapper" i
  | None -> failwith "extra inline container mapper should not delete node"

let () =
  let inline = extra_inline_container Inline.Extra_inline_container.Deleted (text "jia") in
  let folder =
    Folder.make
      ~inline:(fun _ acc -> function
        | Inline.Text _ -> Folder.ret (acc + 1)
        | _ -> Folder.default)
      ()
  in
  match Folder.fold_inline folder 0 inline with
  | 1 -> ()
  | n ->
      failwith (Fmt.str "extra inline container folder saw %d text nodes" n)

let () =
  match inline_of_string "{=jia=}" with
  | Inline.Text ("{=jia=}", _) -> ()
  | i ->
      fail_inline
        "default parser should keep extra inline container syntax literal" i

let () =
  match inline_of_string ~extra_inline_containers:Extra_config.explicit "{=jia=}" with
  | Inline.Ext_extra_inline_container (c, _) ->
      begin match
        (Inline.Extra_inline_container.kind c, Inline.Extra_inline_container.inline c)
      with
      | Inline.Extra_inline_container.Highlight, Inline.Text ("jia", _) -> ()
      | _, i -> fail_inline "parser should parse highlight container payload" i
      end
  | i -> fail_inline "parser should parse highlight container" i

let () =
  match
    inline_of_string ~extra_inline_containers:Extra_config.explicit
      "{=a *b*=}"
  with
  | Inline.Ext_extra_inline_container (c, _) ->
      begin match Inline.Extra_inline_container.inline c with
      | Inline.Inlines
          ( [
              Inline.Text ("a ", _);
              Inline.Emphasis ({ inline = Inline.Text ("b", _); _ }, _);
            ],
            _ ) ->
          ()
      | i ->
          fail_inline "parser should parse nested extra inline container payload"
            i
      end
  | i -> fail_inline "parser should parse nested extra inline container" i

let () =
  let config =
    Extra_config.make ~highlight:Extra_config.Curly_required ()
  in
  match
    ( inline_of_string ~extra_inline_containers:config "{=jia=}",
      inline_of_string ~extra_inline_containers:config "{^jia^}" )
  with
  | Inline.Ext_extra_inline_container _, Inline.Text ("{^jia^}", _) -> ()
  | _, i -> fail_inline "disabled extra container should remain literal" i

let () =
  let config =
    Extra_config.make ~superscript:Extra_config.Curly_required ()
  in
  match inline_of_string ~extra_inline_containers:config "^jia^" with
  | Inline.Text ("^jia^", _) -> ()
  | i -> fail_inline "curly-required container should reject shorthand" i

let () =
  let config =
    Extra_config.make ~superscript:Extra_config.Curly_optional ()
  in
  match
    ( inline_of_string ~extra_inline_containers:config "^jia^",
      inline_of_string ~extra_inline_containers:config "{^jia^}" )
  with
  | Inline.Ext_extra_inline_container (short, _),
    Inline.Ext_extra_inline_container (curly, _) ->
      begin match
        ( Inline.Extra_inline_container.kind short,
          Inline.Extra_inline_container.kind curly )
      with
      | Inline.Extra_inline_container.Superscript,
        Inline.Extra_inline_container.Superscript ->
          ()
      | _ -> failwith "optional curly syntax parsed the wrong container kind"
      end
  | short, _ -> fail_inline "curly-optional container should parse shorthand" short

let () =
  match
    inline_of_string ~extra_inline_containers:Extra_config.explicit
      "{=a {=b=} c=}"
  with
  | Inline.Ext_extra_inline_container (outer, _) ->
      begin match Inline.Extra_inline_container.inline outer with
      | Inline.Inlines
          ( [
              Inline.Text ("a ", _);
              Inline.Ext_extra_inline_container (inner, _);
              Inline.Text (" c", _);
            ],
            _ )
        when
          Inline.Extra_inline_container.kind inner
          = Inline.Extra_inline_container.Highlight ->
          ()
      | i -> fail_inline "same-kind extra containers should nest" i
      end
  | i -> fail_inline "parser should parse nested same-kind extra containers" i

let () =
  let thematic_break =
    Block.Thematic_break
      (Block.Thematic_break.make ~indent:1 ~layout:"***" (), Meta.none)
  in
  let code_block = Block.Code_block (Block.Code_block.make [], Meta.none) in
  let item_block = Block.Blocks ([ thematic_break; code_block ], Meta.none) in
  let item = Block.List_item.make item_block, Meta.none in
  let block =
    Block.List (Block.List'.make (`Unordered '-') [ item ], Meta.none)
  in
  let rendered = Cmarkit_commonmark.of_doc (Doc.make block) in
  let reparsed = rendered |> Doc.of_string |> Doc.block in
  if not (Sexplib0.Sexp.equal (block_sexp block) (block_sexp reparsed)) then
    fail_block
      (Fmt.str
         "renderer should indent list-item sibling blocks enough to reparse; \
          rendered %S"
         rendered)
      reparsed

let () = print_endline "EOF"
