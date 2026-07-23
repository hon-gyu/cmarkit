(** Predicates that define well-formed AST

    In an ideal world, when our generator respects these predicates (which means
    the generated AST is typed), then it should follow the nice properties we'd
    like to see.

    The rules themselves — and their justifications — live in {!module:Rule},
    where each is stated once and read from both the generating and the checking
    side. This module is the {!Property.t} face of them: it runs {!Rule.check},
    the shared attribute traversal, and reports the first offending node in the
    shape the counterexample printer wants.

    Every rule used to carry its own recursive [check] that re-implemented the
    same descent and the same fold-until-first-failure. Those are gone; there is
    one traversal now. *)

open Cmarkit_

(** Run a rule's checking clause over the shared attribute traversal. *)
let property_of_rule (r : Rule.t) : Property.t =
  let check b =
    match Rule.check r b with
    | None -> Property.Pass
    | Some (b, meta) -> Property.Fail (b, meta)
  in
  { name = r.Rule.name; check }

let no_trailing_blank_line_in_blocks =
  property_of_rule Rules.no_trailing_blank_line_in_blocks

let no_empty_paragraph = property_of_rule Rules.no_empty_paragraph
let no_empty_blocks = property_of_rule Rules.no_empty_blocks
let no_empty_list = property_of_rule Rules.no_empty_list

let no_list_item_leading_blank_prefix =
  property_of_rule Rules.no_list_item_leading_blank_prefix

let no_marker_colliding_thematic_break =
  property_of_rule Rules.no_marker_colliding_thematic_break

let no_html_block_starting_paragraph =
  property_of_rule Rules.no_html_block_starting_paragraph

let no_html_block_absorbing_successor =
  property_of_rule Rules.no_html_block_absorbing_successor

let no_ambiguous_indented_code_after_list =
  property_of_rule Rules.no_ambiguous_indented_code_after_list

let no_adjacent_block_quotes = property_of_rule Rules.no_adjacent_block_quotes

(* All rules aggregated *)
let typed : Property.t =
  let p =
    Property.(
      none
      (* & no_trailing_blank_line_in_blocks *)
      & no_empty_paragraph
      & no_empty_blocks & no_empty_list & no_marker_colliding_thematic_break
      & no_list_item_leading_blank_prefix & no_html_block_absorbing_successor
      & no_ambiguous_indented_code_after_list & no_adjacent_block_quotes
      & no_html_block_starting_paragraph)
  in
  let name' = "typed: " ^ p.name in
  { p with name = name' }

(** {1 Others}

    Not yet {!Rule.t}s: these are plain predicates with no generating clause and
    no need for the traversal. *)

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
