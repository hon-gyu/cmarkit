open Oymarkit_

(* Layout.blanks is only spaces and tabs, no newline

   @source cmarkit.mli:Layout.blanks
*)
let blank_line : Block.t -> bool = function
  | Block.Blank_line (s, _) -> String.for_all (fun c -> c = ' ' || c = '\t') s
  | _ -> true

let rec inline_no_break : Inline.t -> bool = function
  | Inline.Break _ -> false
  | Inline.Inlines (is, _) -> List.for_all inline_no_break is
  | Inline.Emphasis (e, _) | Inline.Strong_emphasis (e, _) ->
      inline_no_break (Inline.Emphasis.inline e)
  | Inline.Link (l, _) | Inline.Image (l, _) ->
      inline_no_break (Inline.Link.text l)
  | _ -> true

(* An ATX heading should not contain a [Break] inline.

   @otherwise
      A [Break] inline emits a [newline]
      call in the renderer, which cuts the heading line short — anything after the
      break is lost on re-parse. *)
let no_break_in_atx_heading : Block.t -> bool = function
  | Block.Heading (h, _) -> (
      match Block.Heading.layout h with
      | `Atx _ -> inline_no_break (Block.Heading.inline h)
      | `Setext _ -> true)
  | _ -> true
