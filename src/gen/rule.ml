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

(** Immediate child blocks of [b] *)
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
