(** Generator for a surface AST of CommonMark blocks.

    This module deliberately {b does not} generate {!Oymarkit_.Block.t}
    directly. Instead it defines a narrow surface AST whose shape and
    generator-side preconditions {b are} the typing discipline we are
    discovering.

    The pipeline is:

    {[
      s_block  --[to_block]-->  Block.t  --[Property.check]-->  result
    ]}

    Every "typing rule" we discover from a counterexample lives in one
    of three places:

    {ol
    {- The constructors of {!s_block} and {!s_inline} (what is even
       expressible).}
    {- The shape of {!gen_block} / {!gen_inline} (what is sampled).}
    {- The {!Rule} predicates (declarative filters; can be used to
       reject samples or to label/report counterexamples).}} *)

open Oymarkit_

(* ====================================================================== *)
(* Surface AST                                                            *)
(* ====================================================================== *)

type s_inline =
  | Text of string       (** Plain text. No backticks, no leading container
                             markers, no line breaks. *)
  | Emph of s_inline list
  | Strong of s_inline list
  | Code of string       (** Inline code span. No backticks inside. *)

type s_block =
  | Para of s_inline list             (** Non-empty inline content. *)
  | Heading of int * s_inline list    (** Level in [1..6]. *)
  | Thematic
  | Blank
  | Code_block of string list         (** Fenced; lines do not contain ``` *)
  | Block_quote of s_block list       (** Non-empty body. *)
  | Ulist of s_block list list        (** Non-empty list of non-empty items. *)

(* ====================================================================== *)
(* Lowering to the real AST                                               *)
(* ====================================================================== *)

val to_inline : s_inline list -> Inline.t
val to_block : s_block -> Block.t

(* ====================================================================== *)
(* Generators                                                             *)
(* ====================================================================== *)

val gen_inline : s_inline QCheck2.Gen.t
val gen_inlines : s_inline list QCheck2.Gen.t
val gen_block : s_block QCheck2.Gen.t

val print_s_block : s_block -> string
(** For QCheck2 counterexample printing. *)

(* ====================================================================== *)
(* Rules                                                                  *)
(* ====================================================================== *)

(** A typing rule is a predicate on [s_block]. Used for filtering samples
    and for labelling counterexamples in reports.

    Each rule has a {!name} and a {!check} — when {!check} returns
    [false], the sample violates the rule. *)
module Rule : sig
  type t =
    { name : string;
      check : s_block -> bool }

  val all : t list
  (** Currently known rules. Grows as we discover counterexamples. *)
end
