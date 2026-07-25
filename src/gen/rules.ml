(** {0 Rules}

    Each is stated once here, as a {!Typing.t}, and consulted from both sides.
    The prose is the justification: why the parser can never emit this shape, so
    why a generator that produces it is producing something outside the
    language. *)

open Cmarkit_
open Typing

let is_blank = function
  | Block.Blank_line _ -> true
  | _ -> false

(** {1 No trailing blank lines in nested blocks}

    [Blank_line] at the tail of a nested [Blocks] list after non-blank sibling
    content has no stable ownership: the blank line only closes or separates
    surrounding blocks, so [parse(render(Blocks [nonblank; ...; Blank_line]))]
    can attach it to an enclosing container instead of the nested [Blocks].
    Top-level trailing blank lines and blank-only nested containers are
    different: the parser can emit them, so this rule intentionally allows those
    shapes. *)
let no_trailing_blank_line_in_blocks =
  make ~name:"no trailing blank line in blocks" (fun ctx b ->
      match b with
      | Block.Blocks (bs, _) when not ctx.at_root -> (
          match List.rev bs with
          | Block.Blank_line _ :: rest
            when List.exists (Fun.negate is_blank) rest ->
              Some [ ("blocks", Block b) ]
          | _ -> None)
      | _ -> None)

(** {1 No empty paragraph}

    A [Paragraph] whose inline is empty ([Inlines []] / [Text ""]) renders to no
    characters, so [parse(render(Paragraph empty))] yields a [Blank_line] (or
    nothing), never a paragraph. A paragraph block only exists when there is
    non-blank content to begin with, so the parser never emits an empty one.

    Note this is {e contextual}: an empty [Inlines] is a legitimate value
    elsewhere (it is [Inline.empty], and the parser emits it for empty table
    cells) — the rule bites only when it is the body of a [Paragraph]. *)
let no_empty_paragraph =
  make ~name:"no empty paragraph" (fun _ b ->
      match b with
      | Block.Paragraph (p, _) when Inline.is_empty (Block.Paragraph.inline p)
        ->
          Some [ ("paragraph", Block b) ]
      | _ -> None)

(** {1 No empty [Blocks]}

    An empty [Blocks []] renders to nothing, so as a nested block it is never
    reconstructed by the parser. (The empty document is
    [Block.empty = Blocks []], but that is the root, not a nested block.) *)
let no_empty_blocks =
  make ~name:"no empty blocks" (fun _ b ->
      match b with
      | Block.Blocks ([], _) -> Some [ ("blocks", Block b) ]
      | _ -> None)

(** {1 No empty list}

    A [List] with zero items renders to nothing: a list has no syntax of its
    own, it exists only as the grouping of its item markers. With no item there
    is no marker, so the parser never emits an empty list (it would emit a
    [Blank_line] / nothing instead). Same family as {!no_empty_blocks}. *)
let no_empty_list =
  make ~name:"no empty list" (fun _ b ->
      match b with
      | Block.List (l, _) when Block.List'.items l = [] ->
          Some [ ("list", Block b) ]
      | _ -> None)

(** {1 No leading blank prefix before list-item content}

    A list item may start with one blank line before its first non-blank block:

    {[
    -x
    ]}

    still parses as one list item containing [Blank_line; Paragraph "x"]. With
    two or more leading blanks, the parser has already closed the blank-only
    item before the following non-blank line is processed:

    {[
    -x
    ]}

    reparses as a blank-only list item followed by an outside paragraph. This is
    about the prefix before the first real item content; blank-only items remain
    parser-emittable and are intentionally allowed. *)
let no_list_item_leading_blank_prefix =
  let bad_item_block block =
    match Block.normalize block with
    | Block.Blocks (bs, _) ->
        let rec count_blanks count = function
          | b :: bs when is_blank b -> count_blanks (count + 1) bs
          | [] -> false
          | _ :: _ -> count >= 2
        in
        count_blanks 0 bs
    | _ -> false
  in
  let item_bad (item, _) = bad_item_block (Block.List_item.block item) in
  make ~name:"no list-item leading blank prefix" (fun _ b ->
      match b with
      | Block.List (l, _) when List.exists item_bad (Block.List'.items l) ->
          Some [ ("list", Block b) ]
      | _ -> None)

(** {1 No marker-colliding thematic break in a list item}

    A bullet list item whose {e leading} block is a thematic break of the same
    character as the bullet marker has no syntactic witness. The marker and the
    thematic break share the item's first line, e.g. [- ---], which is a uniform
    run of [-] and therefore parses as a {!Cmarkit_.Block.Thematic_break} (a
    thematic break takes precedence over a list item), not a list. Only [-] and
    [*] are affected: they are the characters that are both bullet markers and
    thematic break characters ([+] is marker-only, [_] is thematic-break-only,
    ordered markers never collide). So [* ---] is fine — mixed characters, no
    uniform run — and must not be rejected. *)

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

let no_marker_colliding_thematic_break =
  let item_collides marker (item, _) =
    match leading_block (Block.List_item.block item) with
    | Some (Block.Thematic_break (tb, _)) ->
        thematic_break_char tb = Some marker
    | _ -> false
  in
  make ~name:"no marker-colliding thematic break in list item" (fun _ b ->
      match b with
      | Block.List (l, _) -> (
          match Block.List'.type' l with
          | `Unordered marker
            when List.exists (item_collides marker) (Block.List'.items l) ->
              Some [ ("list", Block b) ]
          | _ -> None)
      | _ -> None)

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

let no_html_block_starting_paragraph =
  make ~name:"no HTML-block-starting paragraph" (fun _ b ->
      match b with
      | Block.Paragraph (p, _) when paragraph_starts_html_block p ->
          Some [ ("paragraph", Block b) ]
      | _ -> None)

(** {1 No HTML block absorbing its successor}

    A type-6/7 HTML block (or any whose end condition its own lines never meet)
    stays open at its last line, so on reparse it swallows whatever block
    renders right after it — unless that successor is a [Blank_line] (which
    closes it) or a container boundary intervenes.

    Stated against {!ctx.prev}: the traversal walks in render order, so "the
    block before me absorbs, and I render something it can swallow" is the whole
    rule. Both conditions on the successor are needed, and both are read off its
    own summary rather than its constructor: a {!field-transparent} subtree (an
    empty [Blocks]) renders nothing, so there is nothing to absorb; and
    {!field-leads_with_blank} is what actually closes the html block, which is
    not the same as the node {e being} a [Blank_line] — a [Blocks] whose first
    render-order child is blank closes it just as well.

    The generating side splits, because {!choice} is too coarse for "the next
    block leads with a blank". [`Block_quote] and [`List] always render a marker
    on their first line, so they are forbidden outright when the predecessor
    absorbs; [`Blocks] is transparent, so the constraint reaches its first child
    by recursion; and the [`Leaf] half — whose leading edge [choice] cannot
    see — is enforced where the leaf is built, via {!must_lead_blank}. *)
let prev_absorbs (ctx : ctx) : bool =
  match ctx.prev with
  | Some s -> s.trailing_absorbing
  | None -> false

(** Must the block generated at [ctx] lead with a blank line? True exactly when
    an absorbing html block sits immediately before it in render order and would
    otherwise swallow it. The leaf counterpart to the container choices
    {!no_html_block_absorbing_successor} forbids; see its comment. *)
let must_lead_blank (ctx : ctx) : bool = prev_absorbs ctx

let no_html_block_absorbing_successor =
  {
    name = "no html block absorbing successor";
    normalize = false;
    forbids = (fun ctx c -> prev_absorbs ctx && (c = `Block_quote || c = `List));
    violated =
      (fun ctx b ->
        let s = summarize b in
        if prev_absorbs ctx && (not s.transparent) && not s.leads_with_blank
        then Some [ ("block", Block b) ]
        else None);
  }

(** {1 No ambiguous indented code after a list}

    An indented code block that renders after a list, with only blank lines
    between them, is ambiguous when the final list item's continuation indent is
    at most four columns. The code block's four-space prefix then continues the
    item, so the parser keeps the line inside the list rather than opening a
    top-level code block. Wider list markers can close the list and are valid. A
    fenced code block preserves the same content and block structure in every
    case.

    Blank lines do not break the interaction, so this cannot read [ctx.prev]
    alone — a blank sibling would erase the list. It stays a scan over the
    enclosing sequence. *)
let no_ambiguous_indented_code_after_list =
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
  make ~name:"no ambiguous indented code after list" ~normalize:true (fun _ b ->
      match b with
      | Block.Blocks (bs, _) when has_bad_sequence false bs ->
          Some [ ("blocks", Block b) ]
      | _ -> None)

(** {1 No adjacent block quotes}

    Two adjacent [Block_quote] siblings render as one uninterrupted run of
    quote-marker lines, so the parser produces one quote container rather than
    recovering the sibling boundary. A top-level [Blank_line] between them is
    sufficient to close the first quote and preserve both containers.

    We considered canonicalizing [Block_quote a; Block_quote b] to
    [Block_quote (Blocks [a; b])]. That is not valid in general because parsing
    the contiguous quoted lines is not equivalent to structurally appending the
    inner blocks. For example, two paragraph payloads may become one continued
    paragraph; lists and indented code have their own merging rules; and HTML
    blocks may absorb following content. A correct general canonicalization
    would have to reproduce block parsing inside the quote. We therefore retain
    the intended two-container structure and require an explicit outside
    separator instead. *)
let no_adjacent_block_quotes =
  let prev_is_quote ctx =
    match ctx.prev with
    | Some s -> s.trailing_block_quote
    | None -> false
  in
  {
    name = "no adjacent block quotes";
    normalize = false;
    forbids = (fun ctx c -> c = `Block_quote && prev_is_quote ctx);
    violated =
      (fun ctx b ->
        match b with
        | Block.Block_quote _ when prev_is_quote ctx ->
            Some [ ("block_quote", Block b) ]
        | _ -> None);
  }

(** {1 All rules aggregated}

    As a {!Property.t}, for the property runners. *)
let typed : Property.t =
  let r = property_of_rule in
  let p =
    Property.(
      none
      (* & r no_trailing_blank_line_in_blocks *)
      & r no_empty_paragraph
      & r no_empty_blocks & r no_empty_list
      & r no_marker_colliding_thematic_break
      & r no_list_item_leading_blank_prefix
      & r no_html_block_absorbing_successor
      & r no_ambiguous_indented_code_after_list & r no_adjacent_block_quotes
      & r no_html_block_starting_paragraph)
  in
  { p with name = "typed: " ^ p.name }
