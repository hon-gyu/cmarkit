open Cmarkit_

(** {1 No trailing blank lines in blocks}

    [Blank_line] at the tail of a [Blocks] list renders as nothing (the `\n` it
    contributes merely closes the preceding block's last line), so
    [parse(render(Blocks [...; Blank_line]))] will never reconstruct the
    trailing [Blank_line]. Any such node is a generator artifact with no
    syntactic witness. *)

(* Immediate child blocks of [b], so the check can descend into nested
   structures (block quotes, list items, footnote definitions, ...). *)
let child_blocks : Block.t -> Block.t list = function
  | Block.Block_quote (bq, _) -> [ Block.Block_quote.block bq ]
  | Block.Blocks (bs, _) -> bs
  | Block.List (l, _) ->
      List.map (fun (i, _) -> Block.List_item.block i) (Block.List'.items l)
  | Block.Ext_footnote_definition (fn, _) -> [ Block.Footnote.block fn ]
  | _ -> []

let no_trailing_blank_line_in_blocks : Property.t =
  let name = "no trailing blank line in blocks" in
  let rec check : Block.t -> Property.result =
   fun b ->
    let here =
      match b with
      | Block.Blocks (bs, _) as blocks -> (
          match List.rev bs with
          | Block.Blank_line _ :: _ ->
              Property.Fail (b, [ ("blocks", Block blocks) ])
          | _ -> Pass)
      | _ -> Pass
    in
    match here with
    | Property.Fail _ -> here
    | Property.Pass ->
        (* Recurse into nested blocks, returning the first failure. *)
        List.fold_left
          (fun acc child ->
            match acc with
            | Property.Fail _ -> acc
            | Property.Pass -> check child)
          Property.Pass (child_blocks b)
  in
  { name; check }

(** {1 No empty paragraph}

    A [Paragraph] whose inline is empty ([Inlines []] / [Text ""]) renders to no
    characters, so [parse(render(Paragraph empty))] yields a [Blank_line] (or
    nothing), never a paragraph. A paragraph block only exists when there is
    non-blank content to begin with, so the parser never emits an empty one.

    Note this is {e contextual}: an empty [Inlines] is a legitimate value
    elsewhere (it is [Inline.empty], and the parser emits it for empty table
    cells) — the rule bites only when it is the body of a [Paragraph]. *)
let no_empty_paragraph : Property.t =
  let name = "no empty paragraph" in
  let rec check : Block.t -> Property.result =
   fun b ->
    let here =
      match b with
      | Block.Paragraph (p, _) as para ->
          if Inline.is_empty (Block.Paragraph.inline p) then
            Property.Fail (b, [ ("paragraph", Block para) ])
          else Pass
      | _ -> Pass
    in
    match here with
    | Property.Fail _ -> here
    | Property.Pass ->
        List.fold_left
          (fun acc child ->
            match acc with
            | Property.Fail _ -> acc
            | Property.Pass -> check child)
          Property.Pass (child_blocks b)
  in
  { name; check }

(** {1 No empty [Blocks]}

    An empty [Blocks []] renders to nothing, so as a nested block it is never
    reconstructed by the parser. (The empty document is
    [Block.empty = Blocks []], but that is the root, not a nested block.) Same
    family as {!no_empty_paragraph}. *)
let no_empty_blocks : Property.t =
  let name = "no empty blocks" in
  let rec check : Block.t -> Property.result =
   fun b ->
    let here =
      match b with
      | Block.Blocks ([], _) as blocks ->
          Property.Fail (b, [ ("blocks", Block blocks) ])
      | _ -> Pass
    in
    match here with
    | Property.Fail _ -> here
    | Property.Pass ->
        List.fold_left
          (fun acc child ->
            match acc with
            | Property.Fail _ -> acc
            | Property.Pass -> check child)
          Property.Pass (child_blocks b)
  in
  { name; check }

(* All rules aggregated *)
let typed : Property.t =
  let p =
    Property.(
      none
      (* & no_trailing_blank_line_in_blocks *)
      & no_empty_paragraph
      & no_empty_blocks)
  in
  let name' = "typed: " ^ p.name in
  { p with name = name' }

(** {1 Others} *)

(* Layout.blanks is only spaces and tabs, no newline

   @source cmarkit.mli:Layout.blanks
*)
let blank_line : Block.t -> bool = function
  | Block.Blank_line (s, _) -> String.for_all (fun c -> c = ' ' || c = '\t') s
  | _ -> true

(* An ATX heading should not contain a [Break] inline.

   @otherwise
      A [Break] inline emits a [newline]
      call in the renderer, which cuts the heading line short — anything after the
      break is lost on re-parse. *)

let rec no_break : Inline.t -> bool = function
  | Inline.Break _ -> false
  | Inline.Inlines (is, _) -> List.for_all no_break is
  | Inline.Emphasis (e, _)
  | Inline.Strong_emphasis (e, _) ->
      no_break (Inline.Emphasis.inline e)
  | Inline.Link (l, _)
  | Inline.Image (l, _) ->
      no_break (Inline.Link.text l)
  | _ -> true

let no_break_in_atx_heading : Block.t -> bool = function
  | Block.Heading (h, _) -> (
      match Block.Heading.layout h with
      | `Atx _ -> no_break (Block.Heading.inline h)
      | `Setext _ -> true)
  | _ -> true
