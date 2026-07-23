(** {0 One description of a well-formed AST, consulted from both sides}

    A typing rule is stated once, here, as a value with two clauses:

    - {!field-forbids} restricts the candidates a generator may pick at a choice
      point, so bad shapes are never produced;
    - {!field-violated} decides whether a finished node breaks the rule, so a
      given AST can be checked.

    Both clauses read the same attributes — {!ctx} flowing down, {!summary}
    flowing across — and {!check} computes them with the same traversal the
    generator uses while building. That is what makes the two clauses testable
    against each other rather than merely written next to each other: every AST
    from a generator honouring [forbids] must pass [violated], and disabling a
    rule must eventually produce an AST that [violated] flags. Neither test is
    expressible while the generating and checking sides are written separately.

    Why attributes at all: a constraint becomes {e local} once the right
    attribute exists, because everything non-local it depended on has been
    carried to it. Each rule here is non-local only in needing one or two facts
    about a parent or a previous sibling, so each becomes a guard on a choice
    rather than a repair traversal after the fact. Guards compose — they only
    ever remove candidates, so switching a rule on cannot break a rule that
    already held — where repairs do not: an inserted separator can create the
    very shape another repair forbids, and their composition order is
    load-bearing and unchecked.

    This module sits below {!module:Gen} and knows nothing about configs. A rule
    says {e what} it forbids; whether it is switched on is a separate question,
    answered by [Gen.Bconfig]. *)

open Cmarkit_

(** {1 Synthesized attributes} *)

type summary = {
  transparent : bool;
      (** No render-order content at all: an empty [Blocks], or a [Blocks] of
          nothing but those. Such a subtree neither establishes nor clears
          context for its successor, so a sequence passes the previous summary
          straight through it. *)
  trailing_block_quote : bool;
      (** The last block in render order is a [Block_quote]. Two flush quote
          markers parse as one quote. *)
  trailing_absorbing : bool;
      (** The last block in render order is an html block left open at its last
          line, which swallows whatever renders after it. *)
  leads_with_blank : bool;
      (** The first block in render order is a [Blank_line]. A successor that
          already starts blank needs no separator inserted before it. *)
  list_continuation_indent : int option;
      (** For a trailing [List], its final item's continuation indent. At most
          four columns and a following indented code block is absorbed into the
          item. *)
}
(** What a subtree looks like from the outside, to whatever follows it in render
    order.

    Peeling follows {e render order}, not tree structure: nested [Blocks] are
    transparent, so a [Blocks]'s trailing edge is its last child's and its
    leading edge is its first child's. [Block_quote], [List] and footnote
    definitions are container boundaries and stop the peel. *)

let summary_nil =
  {
    transparent = true;
    trailing_block_quote = false;
    trailing_absorbing = false;
    leads_with_blank = false;
    list_continuation_indent = None;
  }

let summary_opaque = { summary_nil with transparent = false }

(* Combine a [Blocks]'s children in render order: leading edge from the first
   contentful child, trailing edges from the last. *)
let summary_seq (ss : summary list) : summary =
  match List.filter (fun s -> not s.transparent) ss with
  | [] -> summary_nil
  | first :: _ as ss ->
      let last = List.nth ss (List.length ss - 1) in
      { last with leads_with_blank = first.leads_with_blank }

(** Recover the synthesized attributes of a finished block. The generator gets
    them for free while building; the checker computes them bottom-up. *)
let rec summarize (b : Block.t) : summary =
  match b with
  | Block.Blocks (bs, _) -> summary_seq (List.map summarize bs)
  | Block.Blank_line _ -> { summary_opaque with leads_with_blank = true }
  | Block.Block_quote _ -> { summary_opaque with trailing_block_quote = true }
  | Block.Html_block (lines, _) ->
      {
        summary_opaque with
        trailing_absorbing = Common_.html_block_absorbs lines;
      }
  | Block.List (l, _) ->
      {
        summary_opaque with
        list_continuation_indent = Common_.list_last_item_continuation_indent l;
      }
  | _ -> summary_opaque

(** {1 Inherited attributes} *)

type ctx = {
  lead_exclude : char list;
      (** Characters a thematic break may not use here, because this block sits
          at the leading (marker) line of a list item and a break of the marker
          char would collapse the item. A [Block_quote]'s [>] absorbs the
          leading position, so descending into one clears this. *)
  prev : summary option;
      (** Summary of the previous sibling {e in render order}, which is not the
          same as the previous sibling in the tree: a nested [Blocks] is
          transparent, so its first child sees the [Blocks]'s own predecessor,
          and a transparent subtree passes its predecessor through. [None] at
          the start of a sequence and immediately inside a container, whose
          marker breaks any adjacency with what came before it. *)
  is_last : bool;  (** Last position of the enclosing render-order sequence. *)
  at_root : bool;  (** This block is the whole tree, not a child of anything. *)
  in_root_seq : bool;
      (** The sequence this block belongs to is the document's root [Blocks], so
          a trailing [Blank_line] here trails the document rather than sitting
          inside a nested [Blocks]. *)
}
(** What a block needs to know about where it sits, carried down from its
    parent. Read at every choice point and modified when descending. *)

let init_ctx ?(lead_exclude = []) () : ctx =
  {
    lead_exclude;
    prev = None;
    is_last = true;
    at_root = true;
    in_root_seq = false;
  }

(** Descending through a container marker ([>], an item marker, a footnote
    label). The marker absorbs the leading position and breaks adjacency with
    whatever preceded the container, so the child starts a fresh sequence. *)
let enter_container (_ctx : ctx) : ctx =
  {
    lead_exclude = [];
    prev = None;
    is_last = true;
    at_root = false;
    in_root_seq = false;
  }

(** Context for the [i]th of [len] children of a [Blocks], given the summary of
    the preceding sibling in render order.

    Shared by the generator's fold and the checker's walk so the two cannot
    drift; [prev] is seeded from the [Blocks]'s own predecessor because a nested
    [Blocks] is transparent. *)
let nth_child (ctx : ctx) ~(i : int) ~(len : int) ~(prev : summary option) : ctx
    =
  {
    (* Only the head sits at the leading position; the rest start fresh lines. *)
    lead_exclude = (if i = 0 then ctx.lead_exclude else []);
    prev;
    is_last = i = len - 1;
    at_root = false;
    in_root_seq = ctx.at_root;
  }

(** Advance a sequence's accumulator past a child. A transparent child passes
    its own predecessor along rather than becoming one. *)
let advance (prev : summary option) (s : summary) : summary option =
  if s.transparent then prev else Some s

(** {1 Rules} *)

type choice = [ `Leaf | `Blocks | `Block_quote | `List ]
(** The constructor choices a block generator picks between. A rule restricts
    this list; it never rewrites what comes out of it. *)

let string_of_choice : choice -> string = function
  | `Leaf -> "leaf"
  | `Blocks -> "blocks"
  | `Block_quote -> "block_quote"
  | `List -> "list"

type t = {
  name : string;
  forbids : ctx -> choice -> bool;
      (** Generation side. May consult only {!ctx}: everything non-local the
          rule depends on has already been carried here as an attribute, which
          is the entire point of the frame.

          A rule about a node's {e own} synthesized attribute cannot be a guard,
          because at choice time the subtree does not exist yet. Such a rule
          leaves this [fun _ _ -> false] and is enforced at the point of
          generation instead. *)
  violated : ctx -> Block.t -> Common_.metadata option;
      (** Checking side. [Some metadata] names the offending node for the
          counterexample printer. Evaluated at every node of the traversal, so
          it states only what is wrong {e here}. *)
  normalize : bool;
      (** Run the check on {!Cmarkit_.Block.normalize}d input. Rules phrased as
          a scan over a flat sibling list need it; rules phrased in terms of
          {!ctx.prev} do not, because the traversal already walks in render
          order. *)
}

let make ?(forbids = fun _ _ -> false) ?(normalize = false) ~name violated =
  { name; forbids; violated; normalize }

(** Is any enabled rule violated by choosing [c] here? Returns the rule that
    said no, so the rejection can be attributed. *)
let first_forbidding (rules : t list) (ctx : ctx) (c : choice) : t option =
  List.find_opt (fun r -> r.forbids ctx c) rules

(** Immediate child blocks of [b], so the traversal can descend into nested
    structures (block quotes, list items, footnote definitions, ...). *)
let child_blocks : Block.t -> Block.t list = function
  | Block.Block_quote (bq, _) -> [ Block.Block_quote.block bq ]
  | Block.Blocks (bs, _) -> bs
  | Block.List (l, _) ->
      List.map (fun (i, _) -> Block.List_item.block i) (Block.List'.items l)
  | Block.Ext_footnote_definition (fn, _) -> [ Block.Footnote.block fn ]
  | _ -> []

(** The attribute engine: walk a finished tree computing the same [ctx] the
    generator threads, evaluating [violated] at each node and returning the
    first offender.

    This replaces the bespoke recursive [check] that each rule used to carry,
    every one of which re-implemented this descent and this
    fold-until-first-failure. *)
let check (r : t) (b : Block.t) : (Block.t * Common_.metadata) option =
  let rec walk ctx b =
    match r.violated ctx b with
    | Some meta -> Some (b, meta)
    | None -> descend ctx b
  and descend ctx b =
    match b with
    | Block.Blocks (bs, _) ->
        let len = List.length bs in
        let rec fold i prev = function
          | [] -> None
          | child :: rest -> (
              let child_ctx = nth_child ctx ~i ~len ~prev in
              match walk child_ctx child with
              | Some _ as found -> found
              | None -> fold (i + 1) (advance prev (summarize child)) rest)
        in
        fold 0 ctx.prev bs
    | b ->
        (* Every other container starts its children a fresh sequence. *)
        List.fold_left
          (fun acc child ->
            match acc with
            | Some _ -> acc
            | None -> walk (enter_container ctx) child)
          None (child_blocks b)
  in
  walk (init_ctx ()) (if r.normalize then Block.normalize b else b)

(** {1 The rules}

    Each is stated once here and consulted from both sides. The prose is the
    justification: why the parser can never emit this shape, so why a generator
    that produces it is producing something outside the language. *)

let blank = function
  | Block.Blank_line _ -> true
  | _ -> false

let meta_of key b : Common_.metadata = [ (key, Common_.Block b) ]

(** {2 No trailing blank lines in nested blocks}

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
          | Block.Blank_line _ :: rest when List.exists (Fun.negate blank) rest
            ->
              Some (meta_of "blocks" b)
          | _ -> None)
      | _ -> None)

(** {2 No empty paragraph}

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
          Some (meta_of "paragraph" b)
      | _ -> None)

(** {2 No empty [Blocks]}

    An empty [Blocks []] renders to nothing, so as a nested block it is never
    reconstructed by the parser. (The empty document is
    [Block.empty = Blocks []], but that is the root, not a nested block.) *)
let no_empty_blocks =
  make ~name:"no empty blocks" (fun _ b ->
      match b with
      | Block.Blocks ([], _) -> Some (meta_of "blocks" b)
      | _ -> None)

(** {2 No empty list}

    A [List] with zero items renders to nothing: a list has no syntax of its
    own, it exists only as the grouping of its item markers. With no item there
    is no marker, so the parser never emits an empty list (it would emit a
    [Blank_line] / nothing instead). Same family as {!no_empty_blocks}. *)
let no_empty_list =
  make ~name:"no empty list" (fun _ b ->
      match b with
      | Block.List (l, _) when Block.List'.items l = [] ->
          Some (meta_of "list" b)
      | _ -> None)

(** {2 No leading blank prefix before list-item content}

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
          | b :: bs when blank b -> count_blanks (count + 1) bs
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
          Some (meta_of "list" b)
      | _ -> None)

(** {2 No marker-colliding thematic break in a list item}

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
              Some (meta_of "list" b)
          | _ -> None)
      | _ -> None)

(** {2 No HTML-block-starting paragraph}

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
          Some (meta_of "paragraph" b)
      | _ -> None)

(** {2 No HTML block absorbing its successor}

    A type-6/7 HTML block (or any whose end condition its own lines never meet)
    stays open at its last line, so on reparse it swallows whatever block
    renders right after it — unless that successor is a [Blank_line] (which
    closes it) or a container boundary intervenes.

    Stated against {!ctx.prev}: the traversal already walks in render order, so
    "the block before me absorbs, and I render something it can swallow" is the
    whole rule. The flat-list scan the checker used to do, and the
    [Block.normalize] it needed to make that scan see through nested [Blocks],
    are both subsumed by the attributes.

    Both conditions on the successor are needed, and both are read off its own
    summary rather than its constructor. A {!field-transparent} subtree (an
    empty [Blocks]) renders nothing, so there is nothing to absorb; and
    {!field-leads_with_blank} is what actually closes the html block, which is
    not the same as the node {e being} a [Blank_line] — a [Blocks] whose first
    render-order child is blank closes it just as well. The old formulation got
    both cases via [Block.normalize] flattening them away first.

    As a guard, this is the first rule whose successor granularity does not
    match {!choice}. The rule wants "the next block leads with a blank"; the
    choice list only distinguishes constructors. Two of them — [`Block_quote]
    and [`List] — always render a marker on their first line and so can never
    lead with a blank, so they are forbidden outright when the predecessor
    absorbs. A [`Blocks] is transparent: its first render-order child inherits
    the same absorbing predecessor through {!nth_child}, so the constraint
    reaches it by recursion and needs no separate case. That leaves [`Leaf],
    whose leading edge is invisible here — a leaf can be a [Blank_line] or a
    paragraph, and [choice] cannot tell them apart. So the leaf half is enforced
    where the leaf is built, via {!must_lead_blank}, not as a guard. The seam is
    exactly the render edge that step 7 removes: once a leaf's leading edge is
    an attribute, both halves collapse into one. *)
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
        then Some (meta_of "block" b)
        else None);
  }

(** {2 No ambiguous indented code after a list}

    An indented code block that renders after a list, with only blank lines
    between them, is ambiguous when the final list item's continuation indent is
    at most four columns. The code block's four-space prefix then continues the
    item, so the parser keeps the line inside the list rather than opening a
    top-level code block. Wider list markers can close the list and are valid. A
    fenced code block preserves the same content and block structure in every
    case.

    Blank lines do not break the interaction, so this cannot read [ctx.prev]
    alone — a blank sibling would erase the list. It stays a scan over the
    enclosing sequence until the render edge lands. *)
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
          Some (meta_of "blocks" b)
      | _ -> None)

(** {2 No adjacent block quotes}

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
    separator instead.

    This is the rule with both clauses filled in, and the pair is worth reading
    together: [forbids] refuses to {e pick} a quote when the previous sibling in
    render order is one, and [violated] flags a quote that {e is} in that
    position. Same attribute, same vocabulary, opposite directions. *)
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
            Some (meta_of "block_quote" b)
        | _ -> None);
  }
