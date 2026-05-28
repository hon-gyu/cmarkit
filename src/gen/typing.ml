open Oymarkit_

(** {1 No trailing blank lines in blocks}

    [Blank_line] at the tail of a [Blocks] list renders as nothing (the `\n` it
    contributes merely closes the preceding block's last line), so
    [parse(render(Blocks [...; Blank_line]))] will never reconstruct the
    trailing [Blank_line]. Any such node is a generator artifact with no
    syntactic witness. *)

let no_trailing_blank_line_in_blocks : Property.t =
  let name = "no trailing blank line in blocks" in
  let rec check : Block.t -> Property.result =
   fun b ->
    match b with
    | Block.Blocks (bs, _) as blocks -> (
        match List.rev bs with
        | Block.Blank_line _ :: _ ->
            Fail (b, [ ("blocks", Block blocks) ])
        | _ -> Pass)
    | _ -> Pass
  in
  { name; check }

(** {1 Others} *)

(* Layout.blanks is only spaces and tabs, no newline

   @source cmarkit.mli:Layout.blanks
*)
let blank_line : Block.t -> bool = function
  | Block.Blank_line (s, _) -> String.for_all (fun c -> c = ' ' || c = '\t') s
  | _ -> true

let rec inline_no_break : Inline.t -> bool = function
  | Inline.Break _ -> false
  | Inline.Inlines (is, _) -> List.for_all inline_no_break is
  | Inline.Emphasis (e, _)
  | Inline.Strong_emphasis (e, _) ->
      inline_no_break (Inline.Emphasis.inline e)
  | Inline.Link (l, _)
  | Inline.Image (l, _) ->
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
