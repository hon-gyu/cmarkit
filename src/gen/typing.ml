(** Predicates that define well-formed AST

    In an ideal world, when our generator respects these predicates (which means
    the generated AST is typed), then it should follow the nice properties we'd
    like to see. *)

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

(** {1 No trailing blank line in blocks}
    A trailing Blank_line inside Blocks is not roundtrippable because it has no
    stable Markdown syntax of its own.

    In the AST, Blocks [a; Blank_line] says: “there is a blank-line node after
    a, and it belongs to this exact Blocks container.” But when rendered, that
    final blank line only becomes whitespace at the end of the container. On
    parse, CommonMark does not preserve “this blank belongs inside that nested
    Blocks wrapper”; it uses blank lines to close or separate blocks. So the
    parser may drop it, or attach it to a surrounding container. *)
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

(** {1 No empty list}

    A [List] with zero items renders to nothing: a list has no syntax of its
    own, it exists only as the grouping of its item markers. With no item there
    is no marker, so the parser never emits an empty list (it would emit a
    [Blank_line] / nothing instead). Same family as {!no_empty_blocks}. *)
let no_empty_list : Property.t =
  let name = "no empty list" in
  let rec check : Block.t -> Property.result =
   fun b ->
    let here =
      match b with
      | Block.List (l, _) as list when Block.List'.items l = [] ->
          Property.Fail (b, [ ("list", Block list) ])
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

(** {1 No marker-colliding thematic break in a list item}

    A bullet list item whose {e leading} block is a thematic break of the same
    character as the bullet marker has no syntactic witness. The marker and the
    thematic break share the item's first line, e.g. [- ---], which is a uniform
    run of [-] and therefore parses as a {!Block.Thematic_break} (a thematic
    break takes precedence over a list item), not a list. Only [-] and [*] are
    affected: they are the characters that are both bullet markers and thematic
    break characters ([+] is marker-only, [_] is thematic-break-only, ordered
    markers never collide). So [* ---] is fine — mixed characters, no uniform
    run — and must not be rejected. *)

(* First non-blank character of a thematic break's layout art. *)
let thematic_break_char (tb : Block.Thematic_break.t) : char option =
  let s = Block.Thematic_break.layout tb in
  let n = String.length s in
  let rec find i =
    if i >= n then None
    else
      match s.[i] with
      | ' '
      | '\t' ->
          find (i + 1)
      | c -> Some c
  in
  find 0

(* The block that renders on the item's first (marker) line: peel [Blocks]
   splicing down to its head. *)
let rec leading_block (b : Block.t) : Block.t option =
  match b with
  | Block.Blocks (b0 :: _, _) -> leading_block b0
  | Block.Blocks ([], _) -> None
  | other -> Some other

let no_marker_colliding_thematic_break : Property.t =
  let name = "no marker-colliding thematic break in list item" in
  let item_collides marker (item, _) =
    match leading_block (Block.List_item.block item) with
    | Some (Block.Thematic_break (tb, _)) ->
        thematic_break_char tb = Some marker
    | _ -> false
  in
  let rec check : Block.t -> Property.result =
   fun b ->
    let here =
      match b with
      | Block.List (l, _) as list -> (
          match Block.List'.type' l with
          | `Unordered marker
            when List.exists (item_collides marker) (Block.List'.items l) ->
              Property.Fail (b, [ ("list", Block list) ])
          | _ -> Pass)
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

(** {1 No HTML-block-starting paragraph}

    A paragraph whose rendered first line starts with CommonMark HTML block
    syntax is not reconstructed as a paragraph: block parsing classifies the
    line as an [Html_block] before inline raw HTML is considered. *)

let paragraph_starts_html_block (p : Block.Paragraph.t) : bool =
  let block = Block.Paragraph (p, Meta.none) in
  let cm = Common_.to_commonmark block in
  let last =
    match String.index_opt cm '\n' with
    | None -> String.length cm - 1
    | Some i -> i - 1
  in
  if last < 0 then false
  else
    let start = Match.first_non_blank cm ~last ~start:0 in
    start <= last
    &&
    match Match.html_block_start cm ~last ~start with
    | Match.Html_block_line _ -> true
    | _ -> false

let no_html_block_starting_paragraph : Property.t =
  let name = "no HTML-block-starting paragraph" in
  let rec check : Block.t -> Property.result =
   fun b ->
    let here =
      match b with
      | Block.Paragraph (p, _) as para ->
          if paragraph_starts_html_block p then
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

(** {1 No HTML block absorbing its successor}

    A type-6/7 HTML block (or any whose end condition its own lines never meet)
    stays open at its last line, so on reparse it swallows whatever block
    renders right after it — unless that successor is a [Blank_line] (which
    closes it) or a container boundary intervenes. We check on the
    {!Cmarkit_.Block.normalize}d tree so render-order adjacency is literal:
    normalize splices every nested [Blocks] flat, so a trailing html block
    buried in an inner [Blocks] sits directly before its real successor. Only
    [Blocks] siblings can collide; a [Block_quote]/[List] boundary stops
    absorption, so scanning each flat [Blocks] list is enough. *)
let no_html_block_absorbing_successor : Property.t =
  let name = "no html block absorbing successor" in
  let absorbing = function
    | Block.Html_block (lines, _) -> Common_.html_block_absorbs lines
    | _ -> false
  in
  let blank = function
    | Block.Blank_line _ -> true
    | _ -> false
  in
  let rec has_bad_pair = function
    | a :: (b :: _ as rest) ->
        (absorbing a && not (blank b)) || has_bad_pair rest
    | _ -> false
  in
  let rec check : Block.t -> Property.result =
   fun b ->
    let here =
      match b with
      | Block.Blocks (bs, _) as blocks when has_bad_pair bs ->
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
  let check b = check (Block.normalize b) in
  { name; check }

(** {1 No ambiguous indented code after a list}

    An indented code block that renders after a list, with only blank lines
    between them, is ambiguous when the final list item's continuation indent is
    at most four columns. The code block's four-space prefix then continues the
    item, so the parser keeps the line inside the list rather than opening a
    top-level code block. Wider list markers can close the list and are valid. A
    fenced code block preserves the same content and block structure in every
    case.

    We check the normalized tree so nested [Blocks] wrappers cannot hide the
    render-order adjacency. Container boundaries still stop the interaction. *)
let no_ambiguous_indented_code_after_list : Property.t =
  let name = "no ambiguous indented code after list" in
  let ambiguous_list = function
    | Block.List (l, _) -> (
        match Common_.list_last_item_continuation_indent l with
        | Some indent -> indent <= 4
        | None -> false)
    | _ -> false
  in
  let indented_code = function
    | Block.Code_block (cb, _) -> Block.Code_block.layout cb = `Indented
    | _ -> false
  in
  let rec has_bad_sequence after_list = function
    | [] -> false
    | Block.Blank_line _ :: bs -> has_bad_sequence after_list bs
    | (Block.List _ as list) :: bs -> has_bad_sequence (ambiguous_list list) bs
    | b :: _ when after_list && indented_code b -> true
    | _ :: bs -> has_bad_sequence false bs
  in
  let rec check : Block.t -> Property.result =
   fun b ->
    let here =
      match b with
      | Block.Blocks (bs, _) as blocks when has_bad_sequence false bs ->
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
  let check b = check (Block.normalize b) in
  { name; check }

(** {1 No adjacent block quotes}

    Two adjacent [Block_quote] siblings render as one uninterrupted run of
    quote-marker lines, so the parser produces one quote container rather than
    recovering the sibling boundary. A top-level [Blank_line] between them is
    sufficient to close the first quote and preserve both containers.

    We considered canonicalizing

    [Block_quote a; Block_quote b]

    to

    [Block_quote (Blocks [a; b])].

    That is not valid in general because parsing the contiguous quoted lines is
    not equivalent to structurally appending the inner blocks. For example, two
    paragraph payloads may become one continued paragraph; lists and indented
    code have their own merging rules; and HTML blocks may absorb following
    content. A correct general canonicalization would have to reproduce block
    parsing inside the quote. We therefore retain the intended two-container
    structure and require an explicit outside separator instead.

    The check runs on the normalized tree so nested [Blocks] wrappers cannot
    hide render-order adjacency. It recurses independently into block quotes,
    lists, and other containers. *)
let no_adjacent_block_quotes : Property.t =
  let rec has_adjacent_quotes = function
    | Block.Block_quote _ :: Block.Block_quote _ :: _ -> true
    | _ :: bs -> has_adjacent_quotes bs
    | [] -> false
  in
  let rec check : Block.t -> Property.result =
   fun b ->
    let here =
      match b with
      | Block.Blocks (bs, _) as blocks when has_adjacent_quotes bs ->
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
  let check b = check (Block.normalize b) in
  { name = "no adjacent block quotes"; check }

(* All rules aggregated *)
let typed : Property.t =
  let p =
    Property.(
      none
      & no_trailing_blank_line_in_blocks
      & no_empty_paragraph
      & no_empty_blocks & no_empty_list & no_marker_colliding_thematic_break
      & no_html_block_absorbing_successor
      & no_ambiguous_indented_code_after_list & no_adjacent_block_quotes
      & no_html_block_starting_paragraph)
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
