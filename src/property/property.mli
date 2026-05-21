(** Properties for counterexample-guided typing-rule discovery.

    Each property takes a [Block.t] and returns a result. The set of
    blocks for which {!all} properties hold defines (operationally) the
    "well-typed" subset of [Block.t]

    Intended workflow
    =================

    {ol
    {- Generate broadly with {!Oymarkit_property.Gen_block} (the generator
       is the syntactic encoding of currently-known rules).}
    {- Run the battery {!all} on each sample.}
    {- For each counterexample, shrink to a minimal witness.}
    {- Promote the witness into a generator precondition (a new rule).}
    {- Repeat until the battery passes at large [count].}} *)

open Oymarkit

type result =
  | Pass
  | Fail of
      { reason : string;
        expected : string; (** Canonical print of expected. *)
        actual : string    (** Canonical print of actual. *) }

type t =
  { name : string;
    check : Block.t -> result }

(* Helpers
=========== *)

val canonical : Block.t -> string
(** Canonical printed form. Currently {!Oymarkit.Pp.pp_block} applied
    to the {!Oymarkit.Block.normalize}d block, with meta/layout dropped.
    Two blocks are considered equal iff their canonical forms match.

    {b Warning.} This is lossy. It is the right granularity for
    discovering structural typing rules but may hide layout-level
    round-trip bugs. Sharpen later if needed. *)

val block_equal : Block.t -> Block.t -> bool

val render : Block.t -> string
(** [render b] is [b] wrapped in a fresh {!Oymarkit.Doc.t} and serialized
    via {!Cmarkit_commonmark.of_doc}. *)

val reparse : Block.t -> Block.t
(** [reparse b] is [render b |> Doc.of_string |> Doc.block]. *)

(* Properties
   =========== *)

val roundtrip : t
(** [parse (render b) ≡ b] modulo {!canonical}. *)

val normalize_idempotent : t
(** [normalize (normalize b) = normalize b] structurally. *)

val render_determinism : t
(** [render b = render (parse (render b))] as strings. *)

val uniformity_block_quote : t
(** Wrapping [b] in a [Block_quote] then round-tripping yields a
    [Block_quote] whose inner block matches [b]. *)

val uniformity_list_item : t
(** Wrapping [b] in a single-item unordered list then round-tripping
    yields a list whose first item's block matches [b]. *)

val all : t list
(** [roundtrip; normalize_idempotent; render_determinism;
    uniformity_block_quote; uniformity_list_item]. *)
