(** Do a rule's two clauses agree?

    Every rule is stated once, as a {!Cmarkit_generator.Rule.t} with a
    [forbids] clause the generator consults and a [violated] clause the checker
    consults. Nothing so far forces those two to describe the same set. This
    test does, by toggling one rule at a time and counting violations of that
    same rule in the output:

    - {b enabled} must be 0. Whatever mechanism enforces the rule during
      generation — a guard, a weight, a surviving repair pass — has to imply the
      checker. A non-zero count means the generating side is weaker than the
      checking side, which is the bug this whole arrangement exists to prevent.
    - {b disabled} should be non-zero. Otherwise the rule is vacuous, or the
      guard is a no-op, or the generator simply cannot reach the shape the rule
      forbids — and in every one of those cases the "enabled = 0" column above
      proves nothing.

    A zero in {b disabled} is not automatically a failure, because a rule can be
    {e implied} by another knob that is still on. The {b isolated} column
    settles which it is: the same count with every other rule switched off too.
    Non-zero there means the shape is reachable and something else in
    [typed_md] was masking it — a fact about how the knobs interact, worth
    recording. Zero there means genuinely unreachable, and the rule earns no
    confidence from the enabled column.

    This is the "@requirement ... in both positive and negative cases" note in
    the [typed_md] docstring, which was discharged by hand until now.

    The counts are baselined rather than merely asserted because they are
    informative in themselves: they say how often each rule actually bites, and
    a large movement is worth a look even when both invariants still hold. *)

module G = Cmarkit_generator.Gen
module R = Cmarkit_generator.Rule

let n = 100

(* Violations of [r] in [n] samples drawn from [config]. *)
let violations (r : R.t) (config : G.Bconfig.t) : int =
  let gen = G.mk_gen_block ~config () in
  let rand = Random.State.make [| 0 |] in
  let count = ref 0 in
  for _ = 1 to n do
    let b = QCheck2.Gen.generate1 ~rand gen in
    if R.check r b <> None then incr count
  done;
  !count

(* [typed_md] with every rule knob switched off, so a rule can be measured
   without another one masking it. *)
let all_off =
  List.fold_left
    (fun c (_, _, set) -> set false c)
    G.Bconfig.typed_md G.rule_knobs

let () =
  Printf.printf "%-46s %-10s %-10s %-10s\n" "rule" "enabled" "disabled"
    "isolated";
  Printf.printf "%s\n" (String.make 80 '-');
  let failures = ref 0 in
  let masked = ref [] in
  List.iter
    (fun (r, _get, set) ->
      let on = violations r (set true G.Bconfig.typed_md) in
      let off = violations r (set false G.Bconfig.typed_md) in
      (* Only pay for the third column when the second is uninformative. *)
      let isolated = if off > 0 then off else violations r (set false all_off) in
      let verdict =
        if on > 0 then begin
          incr failures;
          "  FAIL: enabled but violated"
        end
        else if off > 0 then ""
        else if isolated > 0 then begin
          masked := r.R.name :: !masked;
          "  (implied by another knob)"
        end
        else begin
          incr failures;
          "  FAIL: vacuous, generator cannot reach it"
        end
      in
      Printf.printf "%-46s %-10s %-10s %-10s%s\n" r.R.name
        (Printf.sprintf "%d/%d" on n)
        (Printf.sprintf "%d/%d" off n)
        (Printf.sprintf "%d/%d" isolated n)
        verdict)
    G.rule_knobs;
  Printf.printf "\n%s\n"
    (if !failures = 0 then "all rules agree with their checkers"
     else Printf.sprintf "%d rule(s) disagree" !failures);
  match List.rev !masked with
  | [] -> ()
  | ms ->
      Printf.printf
        "\n\
         implied by another enabled knob, so the enabled column proves nothing\n\
         about them on their own: %s\n"
        (String.concat ", " ms)
