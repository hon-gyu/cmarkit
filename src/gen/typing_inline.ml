open Cmarkit_
(** {0 Inline-level typing rules} *)

(** {1 No link nested in a link}

    CommonMark forbids a link from containing another link at any nesting depth.
    When the inner link is formed the parser deactivates every preceding [ opener
    (even one separated by an intervening image), so the outer brackets degrade to
    literal text on re-parse. The parser therefore never emits a [Link] with a
    [Link] descendant; such a node is a generator artifact that fails round-trip.

    An image's description {e may} contain a link, so the rule bites only on
    [Link] subtrees — but it descends through images, since a link nested under
    an image that is itself under a link is equally invalid. *)

(* Does this inline subtree contain a [Link] at any depth (descending through
   images and emphasis)? *)
let rec has_link : Inline.t -> bool = function
  | Inline.Link _ -> true
  | Inline.Image (l, _) -> has_link (Inline.Link.text l)
  | Inline.Emphasis (e, _)
  | Inline.Strong_emphasis (e, _) ->
      has_link (Inline.Emphasis.inline e)
  | Inline.Inlines (is, _) -> List.exists has_link is
  | _ -> false

(* No [Link] in this subtree has a [Link] descendant. *)
let rec no_nested_link : Inline.t -> bool = function
  | Inline.Link (l, _) ->
      let txt = Inline.Link.text l in
      (not (has_link txt)) && no_nested_link txt
  | Inline.Image (l, _) -> no_nested_link (Inline.Link.text l)
  | Inline.Emphasis (e, _)
  | Inline.Strong_emphasis (e, _) ->
      no_nested_link (Inline.Emphasis.inline e)
  | Inline.Inlines (is, _) -> List.for_all no_nested_link is
  | _ -> true

let inline_typing_rules = [ no_nested_link ]
