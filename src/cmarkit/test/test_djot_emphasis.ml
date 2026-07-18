open Cmarkit_

(** Djot emphasis classification, behind [simple_emphasis_flanking].

    CommonMark decides what a delimiter run may do with
    {{:https://spec.commonmark.org/0.31.2/#left-flanking-delimiter-run}flanking}
    rules, plus extra punctuation clauses for [_] that exist to keep
    [snake_case] intact.

    Djot has no flanking classification at all: a run may open if it is not
    followed by whitespace, and may close if it is not preceded by whitespace.
    [_] plays by the same rules as [*]. A run may therefore both open and close;
    which one it does is settled by matching.

    [intraword_emphasis] stays orthogonal — it is the knob that keeps
    [snake_case] intact, rather than that being baked into the [_] rules. *)

let html ?simple_emphasis_flanking ?intraword_emphasis s =
  let doc = Doc.of_string ~strict:false ?simple_emphasis_flanking ?intraword_emphasis s in
  print_string (Cmarkit_html.of_doc ~safe:false doc)

let%expect_test "both: the ordinary cases agree" =
  html "*a* and _b_";
  html ~simple_emphasis_flanking:true "*a* and _b_";
  [%expect {|
    <p><em>a</em> and <em>b</em></p>
    <p><em>a</em> and <em>b</em></p>
    |}]

let%expect_test "both: whitespace after the opener blocks it" =
  (* Written with a leading word so the [*] cannot start a list. *)
  html "x * a*";
  html ~simple_emphasis_flanking:true "x * a*";
  [%expect {|
    <p>x * a*</p>
    <p>x * a*</p>
    |}]

let%expect_test "both: whitespace before the closer blocks it" =
  html "*a * x";
  html ~simple_emphasis_flanking:true "*a * x";
  [%expect {|
    <p>*a * x</p>
    <p>*a * x</p>
    |}]

let%expect_test "the [_] rules diverge: a closer with a word after it" =
  (* CommonMark's extra [_] clauses make this run neither open nor close, so it
     stays literal. Djot only asks that the closer not be preceded by
     whitespace, which it is not, so [a] is emphasised. *)
  html "_a_b";
  html ~simple_emphasis_flanking:true "_a_b";
  [%expect {|
    <p>_a_b</p>
    <p><em>a</em>b</p>
    |}]

let%expect_test "[*] has no such clauses, so both agree on it" =
  html "*a*b";
  html ~simple_emphasis_flanking:true "*a*b";
  [%expect {|
    <p><em>a</em>b</p>
    <p><em>a</em>b</p>
    |}]

let%expect_test "intraword: CommonMark keeps snake_case intact via its [_] rules" =
  html "snake_case_here";
  [%expect {| <p>snake_case_here</p> |}]

let%expect_test "djot: [intraword_emphasis] is what keeps snake_case intact" =
  (* With djot's classification and intraword emphasis allowed, the underscores
     do emphasise; turning [intraword_emphasis] off is what restores
     [snake_case]. That is the knob doing the job CommonMark folds into [_]. *)
  html ~simple_emphasis_flanking:true "snake_case_here";
  html ~simple_emphasis_flanking:true ~intraword_emphasis:false "snake_case_here";
  [%expect {|
    <p>snake<em>case</em>here</p>
    <p>snake_case_here</p>
    |}]

let%expect_test "djot: [_] and [*] behave the same intraword" =
  html ~simple_emphasis_flanking:true "a*b*c";
  html ~simple_emphasis_flanking:true "a_b_c";
  [%expect {|
    <p>a<em>b</em>c</p>
    <p>a<em>b</em>c</p>
    |}]
