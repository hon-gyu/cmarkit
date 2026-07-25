(** {0 Typing rules}

    A {e typing rule} states a shape a generated AST should not have. Two kinds
    of reason put a shape here.

    - No markdown text parses to it. [Blocks [Block_quote a; Block_quote b]] is
      one: rendered, the two [>] markers land on consecutive lines and parse
      back as a single quote, so nothing the parser produces has that shape.
    - It is reachable, but it breaks a property we want generated trees to have,
      such as round-trip stability or an unambiguous re-parse. A [Blank_line] at
      the tail of a nested [Blocks] is one: the parser does emit trailing blank
      lines, but which container owns one is not recoverable from the rendered
      text.

    Either way the consequence for testing is the same. A tree with such a shape
    fails a property for a reason that says nothing about the code under test,
    so the generator has to keep the shape out.

    The rules themselves live in {!module:Rules}; this module holds the
    vocabulary they are written in and the traversal that runs them. Which rules
    are switched on is a third question, answered by [Gen.Bconfig]: rules are
    discovered by analysing counterexamples, so the set is provisional and each
    rule has to be measurable with and without it.

    {1 The two forms of a rule}

    A rule is consulted at two different times, and the two uses cannot share
    one formulation.

    The generator ({!module:Gen}) consults it before the node exists: it is at a
    branch point holding the constructors it could recurse into, and needs to
    know which of them to drop. That is {!field-forbids}.

    The checker consults it on a finished tree, produced either by a generator
    with the rule switched off or by parsing real input, and needs to report the
    node that breaks the rule. That is {!field-violated}.

    A {!t} pairs the two under one name. The pairing is what makes them testable
    against each other: any tree from a generator that obeys [forbids] must pass
    [violated], and switching a rule off must eventually produce a tree that
    [violated] reports.

    {1 Non-local rules}

    "No block quote after a block quote" is not a property of a node in
    isolation. It relates a node to the block rendered before it, which is an
    arbitrary subtree the generator finished building earlier.

    Such rules are made local in two steps. Every finished subtree reports a
    fixed set of facts about itself ({!summary}), and that report is handed to
    the next block as part of what it knows about its position ({!ctx}). Both
    clauses of a rule read only those two records, so a rule is a predicate over
    one node and its recorded surroundings, which the generator can evaluate
    before committing to a branch. (These are the synthesized and inherited
    attributes of an attribute grammar; nothing below depends on that
    vocabulary.)

    A second consequence is that the generator never produces a bad shape and
    then repairs it. Repairs do not compose: inserting a separator to fix one
    shape can create the shape another repair removes, which makes their order
    significant and unchecked. Dropping candidates does compose, because it only
    ever removes. Switching a rule on cannot break a rule that already held. *)

open Cmarkit_

type metadata = Common_.metadata

(** {1 What the generator decides at a branch point} *)

type choice = [ `Leaf | `Blocks | `Block_quote | `List ]
(** The branches of the block generator's recursion, and so the whole vocabulary
    available before a node exists.

    It is much coarser than [Block.t]. [`Leaf] covers paragraph, heading, code
    block, html block, blank line and thematic break alike, because which leaf
    to build is a later and separate decision. Extension blocks ([Ext_div],
    [Ext_table], footnote definitions and the rest) are absent because the
    generator does not produce them yet; the list grows when it does. There is
    no [Block.t -> choice] function: the checking side is given the block
    itself, so it never needs one.

    The coarseness has a consequence. A rule whose condition separates two
    blocks that map to the same choice cannot be stated as {!field-forbids}.
    "The next block must lead with a blank line" is such a rule: a [`Leaf] can
    be a [Blank_line] or a paragraph, and this type cannot tell them apart. Such
    a rule states as a guard whatever part it can and is enforced where the leaf
    is built for the rest. *)

(** Key for the per-rule rejection counts [Gen] keeps, which record how often
    each rule dropped each candidate. *)
let string_of_choice : choice -> string = function
  | `Leaf -> "leaf"
  | `Blocks -> "blocks"
  | `Block_quote -> "block_quote"
  | `List -> "list"

(** {1 What a finished block looks like to whatever follows it} *)

type summary = {
  transparent : bool;
      (** This subtree renders no text at all: an empty [Blocks], or a [Blocks]
          containing only such subtrees. It can therefore not be anybody's
          predecessor; see {!advance}, which passes the real predecessor through
          it. *)
  trailing_block_quote : bool;
      (** The last block this subtree renders is a [Block_quote], which includes
          the case where the subtree is one. Two [>] markers on consecutive
          lines parse as a single quote. *)
  trailing_absorbing : bool;
      (** The last block this subtree renders is an html block still open on its
          final line, which swallows whatever is rendered after it. *)
  leads_with_blank : bool;
      (** The first block this subtree renders is a [Blank_line]. A successor
          that already starts blank needs no separator inserted before it. *)
  list_continuation_indent : int option;
      (** For a subtree whose last rendered block is a [List], that list's final
          item's continuation indent. At four columns or fewer, a following
          indented code block is absorbed into the item instead of standing on
          its own. *)
}
(** What a finished subtree reports about itself to whatever is rendered after
    it. Each field is here because some rule needs that fact; this is not a
    general-purpose digest.

    Every field is about the {e rendered text} of the whole subtree, not about
    its direct children. "The last block this subtree renders" is found by
    descending: for a [Blocks] it is the last child's last block, recursively,
    because [Blocks] emits no syntax of its own and [Blocks [Blocks [a]; b]]
    renders exactly as [Blocks [a; b]] does. At a [Block_quote], a [List] or a
    footnote definition the descent stops and the container answers for itself,
    since its own syntax ([>], an item marker, [[^x]:]) separates its contents
    from what surrounds it. "The first block this subtree renders" works the
    same way. *)

let summary_nil =
  {
    transparent = true;
    trailing_block_quote = false;
    trailing_absorbing = false;
    leads_with_blank = false;
    list_continuation_indent = None;
  }

let summary_opaque = { summary_nil with transparent = false }

(** The report for a [Blocks], from its children's: trailing fields from the
    last child that renders anything, {!field-leads_with_blank} from the first. *)
let summary_seq (ss : summary list) : summary =
  match List.filter (fun s -> not s.transparent) ss with
  | [] -> summary_nil
  | first :: _ as ss ->
      let last = List.nth ss (List.length ss - 1) in
      { last with leads_with_blank = first.leads_with_blank }

(** Recover a finished block's report. The generator builds these as it goes and
    never calls this; the checker, handed a tree it did not build, recomputes
    them bottom-up. *)
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

(** {1 What a block knows about where it sits} *)

type ctx = {
  lead_exclude : char list;
      (** Characters a thematic break may not use here, because this block sits
          on a list item's marker line and a break made of the marker character
          would collapse the item. A [Block_quote]'s [>] takes over that
          position, so descending into one clears this. *)
  prev : summary option;
      (** The report of the block rendered just before this one, which is not
          the same as the previous sibling in the tree: a [Blocks] renders no
          syntax, so its first child inherits the [Blocks]'s own predecessor,
          and a subtree that renders nothing hands its predecessor onward.

          [None] at the start of a sequence, and immediately inside a container,
          whose marker cuts off adjacency with what came before it. *)
  is_last : bool;  (** Last position of the sequence this block belongs to. *)
  at_root : bool;  (** This block is the whole tree, not a child of anything. *)
  in_root_seq : bool;
      (** The sequence this block belongs to is the document's root [Blocks], so
          a trailing [Blank_line] here trails the document rather than sitting
          inside a nested [Blocks]. *)
}
(** Everything a rule may consult about a node's surroundings, carried down from
    the parent and along from the previous sibling. It is read at every branch
    point and adjusted on the way down by the three functions below, which the
    generator's fold and the checker's walk both call. Sharing them is what
    keeps the two sides from disagreeing about what "the previous block" means. *)

let init_ctx ?(lead_exclude = []) () : ctx =
  {
    lead_exclude;
    prev = None;
    is_last = true;
    at_root = true;
    in_root_seq = false;
  }

(** Descending through a container's marker ([>], an item marker, a footnote
    label). The marker takes over the leading position and cuts off adjacency
    with whatever preceded the container, so the child starts fresh. *)
let enter_container (_ctx : ctx) : ctx =
  {
    lead_exclude = [];
    prev = None;
    is_last = true;
    at_root = false;
    in_root_seq = false;
  }

(** Descending from a [Blocks] into its [i]th child of [len], given the report of
    the block rendered before that child. Returns the child's context.

    At [i = 0], [prev] is the [Blocks]'s own predecessor rather than [None],
    because a [Blocks] renders no syntax of its own and so does not interrupt
    adjacency. *)
let enter_nth_child (ctx : ctx) ~(i : int) ~(len : int) ~(prev : summary option)
    : ctx =
  {
    (* Only the head sits at the leading position; the rest start fresh lines. *)
    lead_exclude = (if i = 0 then ctx.lead_exclude else []);
    prev;
    is_last = i = len - 1;
    at_root = false;
    in_root_seq = ctx.at_root;
  }

(** Move a sequence's running predecessor past a child. A child that renders
    nothing does not become the predecessor; it passes the old one along. *)
let advance (prev : summary option) (s : summary) : summary option =
  if s.transparent then prev else Some s

(** {1 Rules} *)

type t = {
  name : string;
  forbids : ctx -> choice -> bool;
      (** Consulted by the generator at a branch point: would recursing into
          this choice, here, break the rule? [true] removes the candidate. Only
          {!ctx} is available, since the node does not exist yet.

          Some rules cannot be stated here at all, because they constrain the
          node's own contents, which at branch time are unbuilt. "No empty
          [Blocks]" is one: at the branch point nothing is decided but
          [`Blocks]. Such a rule leaves this at [fun _ _ -> false] and is
          enforced instead where the node is built, by a generator that cannot
          produce the bad shape. For that example [Gen.gen_blocks] draws its
          child count from [int_range 1 n] when the rule is on; for the leaf
          half of "no html block absorbing successor", which {!choice} is too
          coarse to express, [Gen.gen_leaf_block] emits a blank line. *)
  violated : ctx -> Block.t -> metadata option;
      (** Consulted on a finished tree by {!check}, which visits every node and
          evaluates this at each one. The [Block.t] is the node being visited
          and the {!ctx} describes where that node sits; the question is whether
          {e that} node breaks the rule. [Some metadata] names it for the
          counterexample printer.

          {!check} does the descending, so a rule answers for one node and never
          recurses itself. *)
  normalize : bool;
      (** Evaluate {!field-violated} on [Cmarkit_.Block.normalize b] instead of
          on [b]. Normalizing flattens nested [Blocks], so a rule phrased as a
          scan over one flat list of siblings sees blocks that a nested [Blocks]
          would otherwise hide. Only "no ambiguous indented code after list"
          needs it today.

          A rule phrased in terms of {!field-prev} does not, because the
          traversal already visits blocks in the order they render and already
          sees through subtrees that render nothing. *)
}
(** One rule, in the two forms described at the top of this module. *)

let make ?(forbids = fun _ _ -> false) ?(normalize = false) ~name violated =
  { name; forbids; violated; normalize }

(** The first of [rules] that forbids choosing [c] here, if any. It is returned
    rather than a [bool] so that dropping a candidate can be attributed to the
    rule responsible. *)
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

(** Walk a finished tree, rebuilding at each node the same {!ctx} the generator
    threaded on the way down, evaluating {!field-violated} there, and returning
    the first node that breaks the rule.

    Two callers. {!module:Typing} turns each rule into a [Property.t] over this,
    which is how a rule is checked against generated or parsed trees. [Gen] uses
    it as a shrink invariant, so that shrinking a counterexample cannot leave
    the set of trees the enabled rules allow.

    One traversal serves every rule, so no rule implements its own descent. *)
let check (r : t) (b : Block.t) : (Block.t * metadata) option =
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
              let child_ctx = enter_nth_child ctx ~i ~len ~prev in
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
