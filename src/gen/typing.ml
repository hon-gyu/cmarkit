(** Typing rules: the vocabulary and the shared traversal.

    A {e typing rule} states a shape a generated AST must not have. The
    rationale can be the shape breaks a property we test against (no markdown
    text parses to the shape, round-trip stability, unambiguous re-parse).

    A rule ({!t}) has two clauses: {!field-forbids}, read by the generator at a
    branch point before the node exists, and {!field-violated}, read by the
    checker on a finished tree. [test/test_rule_agreement.ml] tests the two
    clauses against each other.

    Non-local rules are stated locally against two records: {!summary}, the
    facts a finished subtree reports about itself, and {!ctx}, what a node knows
    about its position, including the previous block's summary.
    (Attribute-grammar inspiration: [summary] ~ synthesized, [ctx] ~ inherited;
    nothing here depends on that vocabulary.)

    Rules restrict the generator's choices instead of repairing finished nodes:
    removing candidates composes, rewrites do not.

    The rules live in {!module:Rules}; [Gen.Bconfig] selects which are enabled.
*)

open Cmarkit_

type metadata = Common_.metadata

(** {1 What the generator decides at a branch point} *)

type choice = [ `Leaf | `Blocks | `Block_quote | `List ]
(** The branches of the block generator's recursion. Coarser than [Block.t]:
    [`Leaf] covers every leaf block (paragraph, heading, code block, html block,
    blank line, thematic break); extension blocks are absent until the generator
    produces them. A rule that must distinguish blocks within one choice cannot
    use {!field-forbids} for that part; it is enforced where the node is built.
*)

(** Key for [Gen]'s per-rule rejection counters. *)
let string_of_choice : choice -> string = function
  | `Leaf -> "leaf"
  | `Blocks -> "blocks"
  | `Block_quote -> "block_quote"
  | `List -> "list"

(** {1 What a finished block looks like to whatever follows it} *)

type summary = {
  transparent : bool;
      (** Renders no text at all: an empty [Blocks], possibly nested. Never
          becomes a predecessor; {!advance} passes the previous one through. *)
  trailing_block_quote : bool;
      (** The last rendered block is a [Block_quote] (possibly the subtree
          itself). *)
  trailing_absorbing : bool;
      (** The last rendered block is an html block still open on its final line,
          which absorbs whatever is rendered after it. *)
  leads_with_blank : bool;  (** The first rendered block is a [Blank_line]. *)
  list_continuation_indent : int option;
      (** When the last rendered block is a [List], its final item's
          continuation indent. At [<= 4] columns, a following indented code
          block is absorbed into the item. *)
}
(** Facts a finished subtree reports to whatever renders after it; each field
    exists because some rule reads it.

    Fields describe the subtree's {e rendered text}, not its direct children.
    [Blocks] emits no syntax of its own, so "first/last rendered block" descends
    through nested [Blocks]; [Block_quote], [List] and footnote definitions
    answer for themselves, since their own syntax ([>], an item marker, [[^x]:])
    separates their contents from the outside. *)

let summary_nil =
  {
    transparent = true;
    trailing_block_quote = false;
    trailing_absorbing = false;
    leads_with_blank = false;
    list_continuation_indent = None;
  }

let summary_opaque = { summary_nil with transparent = false }

(** A [Blocks]'s summary from its children's: trailing fields from the last
    non-transparent child, {!field-leads_with_blank} from the first. *)
let summary_seq (ss : summary list) : summary =
  match List.filter (fun s -> not s.transparent) ss with
  | [] -> summary_nil
  | first :: _ as ss ->
      let last = List.nth ss (List.length ss - 1) in
      { last with leads_with_blank = first.leads_with_blank }

(** A finished block's summary, computed bottom-up. The checker's counterpart to
    the summaries the generator builds as it generates. *)
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
      (** Thematic-break characters forbidden here: this block sits on a list
          item's marker line, and a break made of the marker character would
          collapse the item. *)
  prev : summary option;
      (** Summary of the block rendered just before this one — not necessarily
          the previous tree sibling, because nested [Blocks] are transparent to
          adjacency (see {!enter_nth_child} and {!advance}). [None] at the start
          of a sequence and immediately inside a container. *)
  is_last : bool;  (** Last position of the sequence this block belongs to. *)
  at_root : bool;  (** This block is the whole tree, not a child of anything. *)
  in_root_seq : bool;
      (** This block's sequence is the document's root [Blocks], so a trailing
          [Blank_line] here trails the document, not a nested [Blocks]. *)
}
(** What a rule may consult about a node's surroundings. Maintained by the three
    functions below, which the generator's fold and the checker's walk both
    call, so the two sides agree on what "the previous block" means. *)

let init_ctx ?(lead_exclude = []) () : ctx =
  {
    lead_exclude;
    prev = None;
    is_last = true;
    at_root = true;
    in_root_seq = false;
  }

(** Enter a container's contents. The marker ([>], an item marker, a footnote
    label) takes the leading position and cuts adjacency with what preceded the
    container, so the child starts fresh. *)
let enter_container (_ctx : ctx) : ctx =
  {
    lead_exclude = [];
    prev = None;
    is_last = true;
    at_root = false;
    in_root_seq = false;
  }

(** Context of a [Blocks]'s [i]th child of [len], where [prev] is the summary of
    the block rendered before that child. At [i = 0], [prev] is the [Blocks]'s
    own predecessor: a [Blocks] emits no syntax and does not interrupt
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

(** Advance a sequence's running predecessor past a child. A transparent child
    passes the old predecessor along. *)
let advance (prev : summary option) (s : summary) : summary option =
  if s.transparent then prev else Some s

(** {1 Rules} *)

type t = {
  name : string;
  forbids : ctx -> choice -> bool;
      (** Generator side: does choosing this candidate here break the rule?
          [true] drops it. Only {!ctx} is available — the node does not exist
          yet. A rule that constrains the node's own contents ("no empty
          [Blocks]") or needs a distinction {!choice} cannot express leaves this
          as [fun _ _ -> false] and is enforced where the node is built. *)
  violated : ctx -> Block.t -> metadata option;
      (** Checker side: does the visited node, sitting at {!ctx}, break the
          rule? [Some metadata] labels it for the counterexample printer.
          {!check} does the descending; a rule answers for one node and never
          recurses. *)
  normalize : bool;
      (** Evaluate {!field-violated} on [Block.normalize b], so a rule written
          as a scan over a flat sibling list sees through nested [Blocks]. Rules
          written against {!field-prev} do not need it: the traversal already
          visits blocks in render order. *)
}
(** One rule, both clauses under one name. *)

let make ?(forbids = fun _ _ -> false) ?(normalize = false) ~name violated =
  { name; forbids; violated; normalize }

(** First of [rules] that forbids choosing [c] at [ctx], so the rejection can be
    attributed to it. *)
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

(** The first node of [b] that breaks [r], walking the tree with the same {!ctx}
    maintenance the generator uses. One traversal serves every rule. *)
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

(** A rule's checking clause as a {!Property.t}. *)
let property_of_rule (r : t) : Property.t =
  let check_ b =
    match check r b with
    | None -> Property.Pass
    | Some (b, meta) -> Property.Fail (b, meta)
  in
  { Property.name = r.name; check = check_ }

(** {1 Other predicates}

    Not {!t}s: plain predicates with no generating clause and no need for the
    traversal. *)

(* Layout.blanks is only spaces and tabs, no newline

   @source cmarkit.mli:Layout.blanks
*)
let blank_line : Block.t -> bool = function
  | Block.Blank_line (s, _) -> String.for_all (fun c -> c = ' ' || c = '\t') s
  | _ -> true

(* An ATX heading should not contain a [Break] inline.

   otherwise
    A [Break] inline emits a [newline]
    call in the renderer, which cuts the heading line short
    -- anything after the break is lost on re-parse. *)

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
