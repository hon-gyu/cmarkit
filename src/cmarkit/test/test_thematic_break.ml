open Cmarkit_

(** Djot thematic breaks: a line of three or more [*] or [-] and nothing else
    but spaces or tabs. Two departures from CommonMark's thematic break:

    - [_] is not a break character, so [___] is ordinary text;
    - a break may be indented arbitrarily. That one is not enforced here: it
      follows from [indented_code], since only an indented code block competes
      for a deep indent. The [indented_code] tests below pin that down.

    [setext_headings] is the third knob in play: [---] under a paragraph is a
    setext [<h2>] as long as setext headings are on, whatever the break config
    says. *)

let html ?djot_thematic_break ?indented_code ?setext_headings s =
  let doc =
    Doc.of_string ~strict:false ?djot_thematic_break ?indented_code
      ?setext_headings s
  in
  print_string (Cmarkit_html.of_doc ~safe:false doc)

(* Break characters
   ================ *)

let%expect_test "commonmark: *, - and _ all break" =
  html "***";
  html "---";
  html "___";
  [%expect {|
    <hr>
    <hr>
    <hr>
    |}]

let%expect_test "djot: only * and - break, _ is text" =
  html ~djot_thematic_break:true "***";
  html ~djot_thematic_break:true "---";
  html ~djot_thematic_break:true "___";
  [%expect {|
    <hr>
    <hr>
    <p>___</p>
    |}]

let%expect_test "djot: a long _ run stays text" =
  html ~djot_thematic_break:true "________";
  [%expect {| <p>________</p> |}]

let%expect_test "djot: spaces and tabs between markers are allowed" =
  html ~djot_thematic_break:true "* * *";
  html ~djot_thematic_break:true "-\t-\t-";
  [%expect {|
    <hr>
    <hr>
    |}]

let%expect_test "djot: markers may be mixed" =
  (* djot.js's [pattThematicBreak] is [-*] runs mixed freely -- three or more
     of [*] / [-] with optional spaces -- so [*-*] is a thematic break, not
     emphasis. *)
  html ~djot_thematic_break:true "*-*";
  [%expect {| <hr> |}]

(* Indentation, via [indented_code]
   ================================ *)

let%expect_test "indent < 4 breaks under either config" =
  html "   ***";
  html ~djot_thematic_break:true "   ***";
  [%expect {|
    <hr>
    <hr>
    |}]

let%expect_test "indent >= 4 is code while indented code blocks exist" =
  html ~djot_thematic_break:true "    ***";
  [%expect {|
    <pre><code>***
    </code></pre>
    |}]

let%expect_test "indent >= 4 breaks once indented code is off" =
  html ~djot_thematic_break:true ~indented_code:false "    ***";
  html ~djot_thematic_break:true ~indented_code:false "\t\t***";
  [%expect {|
    <hr>
    <hr>
    |}]

let%expect_test "indented_code:false: a deep indent is dispatched normally" =
  html ~indented_code:false "    a paragraph, not code";
  html ~indented_code:false "    # a heading, not code";
  [%expect
    {|
    <p>a paragraph, not code</p>
    <h1>a heading, not code</h1>
    |}]

let%expect_test "indented_code:false leaves fenced code alone" =
  html ~indented_code:false "```\n    indented text\n```";
  [%expect {|
    <pre><code>    indented text
    </code></pre>
    |}]

(* Setext, via [setext_headings]
   ============================= *)

let%expect_test "setext wins over --- while setext headings are on" =
  html ~djot_thematic_break:true "para\n---";
  [%expect {| <h2>para</h2> |}]

let%expect_test "--- under a paragraph breaks once setext is off" =
  html ~djot_thematic_break:true ~setext_headings:false "para\n---";
  [%expect {|
    <p>para</p>
    <hr>
    |}]

let%expect_test "setext_headings:false: === under a paragraph is text" =
  html ~setext_headings:false "para\n===";
  [%expect {|
    <p>para
    ===</p>
    |}]

let%expect_test "setext_headings:false leaves atx headings alone" =
  html ~setext_headings:false "# heading";
  [%expect {| <h1>heading</h1> |}]

(* The full djot configuration
   =========================== *)

let%expect_test "djot: all three knobs together" =
  html ~djot_thematic_break:true ~indented_code:false ~setext_headings:false
    "para\n---\n\n  ___\n\n      * * *";
  [%expect {|
    <p>para</p>
    <hr>
    <p>___</p>
    <hr>
    |}]
