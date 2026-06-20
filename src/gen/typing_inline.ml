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

(** {1 No empty emphasis}

    The parser cannot emit empty emphasis or strong emphasis. Rendering
    [Emphasis Inline.empty] produces bare delimiter characters such as [**],
    which parse back as literal text rather than an emphasis node. *)

let rec no_empty_emphasis : Inline.t -> bool = function
  | Inline.Emphasis (e, _)
  | Inline.Strong_emphasis (e, _) ->
      let inline = Inline.Emphasis.inline e |> Inline.normalize in
      (not (Inline.is_empty inline)) && no_empty_emphasis inline
  | Inline.Link (l, _)
  | Inline.Image (l, _) ->
      no_empty_emphasis (Inline.Link.text l)
  | Inline.Inlines (is, _) -> List.for_all no_empty_emphasis is
  | _ -> true

(** {1 No adjacent code spans}

    Two code spans rendered flush against each other have no witness: their
    backtick fences merge into one run, and the parser reads a single span (or
    literal backticks) rather than two adjacent ones. Unlike emphasis there is
    no marker escape, so an [Inlines] with two consecutive code spans is a
    generator artifact that fails round-trip.

    Checked on the normalized inline: normalization splices nested [Inlines] and
    drops empty filler, so any surviving fusion shows up as two directly
    consecutive [Code_span] cases in a flat list. *)
let no_adjacent_code_spans (i : Inline.t) : bool =
  let rec check = function
    | Inline.Inlines (is, _) ->
        let is = List.filter (fun e -> not (Inline.is_empty e)) is in
        let rec no_consec = function
          | Inline.Code_span _ :: (Inline.Code_span _ :: _) -> false
          | _ :: tl -> no_consec tl
          | [] -> true
        in
        no_consec is && List.for_all check is
    | Inline.Emphasis (e, _) | Inline.Strong_emphasis (e, _) ->
        check (Inline.Emphasis.inline e)
    | Inline.Link (l, _) | Inline.Image (l, _) -> check (Inline.Link.text l)
    | _ -> true
  in
  check (Inline.normalize i)

let inline_typing_rules =
  [ no_nested_link; no_empty_emphasis; no_adjacent_code_spans ]
