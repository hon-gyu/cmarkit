open Cmarkit_

(** Smart punctuation:
    - Quotes curl by context
    - [...] is an ellipsis, and
    - hyphen runs become em- and en-dashes.

    Everything is decided locally so there is no pairing pass and no way for a
    quote to "fail to match".

    A quote closes exactly when it is right-flanking, which is also what turns
    the apostrophes of [don't] and [Socrates'] the right way. [{"] and ["}]
    override the inference, and a backslash keeps a quote straight. *)

module Extra_config = Inline.Extra_inline_container.Config

let html ?extra_inline_containers s =
  let doc =
    Doc.of_string ~strict:false ~smart_punctuation:true ?extra_inline_containers
      s
  in
  print_string (Cmarkit_html.of_doc ~safe:false doc)

let commonmark s =
  let doc = Doc.of_string ~strict:false ~smart_punctuation:true s in
  print_string (Cmarkit_commonmark.of_doc doc)

let latex s =
  let doc = Doc.of_string ~strict:false ~smart_punctuation:true s in
  print_string (Cmarkit_latex.of_doc doc)

(* Quotes
   ====== *)

let%expect_test "djot reference example" =
  html "\"Hello,\" said the spider.";
  html "\"'Shelob' is my name.\"";
  [%expect
    {|
    <p>“Hello,” said the spider.</p>
    <p>“‘Shelob’ is my name.”</p>
    |}]

let%expect_test "apostrophes curl closed" =
  html "don't";
  html "Socrates' season";
  [%expect {|
    <p>don’t</p>
    <p>Socrates’ season</p>
    |}]

let%expect_test "a quote after an opening bracket opens" =
  html "(\"quoted\")";
  [%expect {| <p>(“quoted”)</p> |}]

let%expect_test "braces override the inference" =
  html "'}Tis Socrates' season to be jolly!";
  [%expect {| <p>’Tis Socrates’ season to be jolly!</p> |}]

let%expect_test "an opener brace forces a left quote" =
  html "{\"forced opener";
  [%expect {| <p>“forced opener</p> |}]

let%expect_test "a backslash keeps a quote straight" =
  html "5\\'11\\\"";
  [%expect {| <p>5'11&quot;</p> |}]

(* Ellipsis and dashes
   =================== *)

let%expect_test "djot reference example" =
  html "57--33 oxen---and no sheep...";
  [%expect {| <p>57–33 oxen—and no sheep…</p> |}]

let%expect_test "hyphen runs divide uniformly, preferring em-dashes" =
  (* The djot spec's own worked cases: 4 hyphens are two en-dashes, 6 are two
     em-dashes. *)
  html "a----b c------d";
  [%expect {| <p>a––b c——d</p> |}]

let%expect_test "a lone hyphen is left alone" =
  html "well-known";
  [%expect {| <p>well-known</p> |}]

let%expect_test "runs that cannot divide uniformly mix, em-dashes first" =
  html "a-----b";
  html "a-------b";
  [%expect {|
    <p>a—–b</p>
    <p>a—––b</p>
    |}]

(* Runs 2..9, embedded in text: a run alone on its line is a thematic break,
   which block parsing settles before inline parsing ever sees it. *)
let%expect_test "the whole hyphen run is always accounted for" =
  for n = 2 to 9 do
    html ("a" ^ String.make n '-' ^ "b")
  done;
  [%expect
    {|
    <p>a–b</p>
    <p>a—b</p>
    <p>a––b</p>
    <p>a—–b</p>
    <p>a——b</p>
    <p>a—––b</p>
    <p>a––––b</p>
    <p>a———b</p>
    |}]

let%expect_test "periods group in threes, leftovers stay text" =
  html "a... b.... c..";
  [%expect {| <p>a… b…. c..</p> |}]

(* Interaction with the rest of the grammar
   ======================================== *)

let%expect_test "off by default" =
  let doc = Doc.of_string ~strict:false "\"x\" a--b..." in
  print_string (Cmarkit_html.of_doc ~safe:false doc);
  [%expect {| <p>&quot;x&quot; a--b...</p> |}]

let%expect_test "no smart punctuation inside a code span" =
  html "`\"x\" -- ...`";
  [%expect {| <p><code>&quot;x&quot; -- ...</code></p> |}]

let%expect_test "a quoted link title still parses" =
  html "[x](u \"title\")";
  [%expect {| <p><a href="u" title="title">x</a></p> |}]

let%expect_test "a thematic break is not a dash run" =
  html "a\n\n---\n\nb";
  [%expect {|
    <p>a</p>
    <hr>
    <p>b</p>
    |}]

let%expect_test "a setext heading underline is not a dash run" =
  html "heading\n---";
  [%expect {| <h2>heading</h2> |}]

let%expect_test "a list marker is not a dash run" =
  html "- a\n- b";
  [%expect {|
    <ul>
    <li>a</li>
    <li>b</li>
    </ul>
    |}]

(* Against the deleted inline container, which also delimits with hyphens.
   A run of two or more is punctuation; a lone hyphen stays a delimiter. *)

let%expect_test "a curly deleted container survives" =
  let extra_inline_containers = Extra_config.make ~deleted:Curly_required () in
  html ~extra_inline_containers "{-gone-} and a--b";
  [%expect {| <p><del>gone</del> and a–b</p> |}]

(* The closer takes the last hyphen, so [{-a--}] leaves a run of one — and a
   lone hyphen is not punctuation, so it stays literal. With one more hyphen
   there is a run of two left over, and that one does become an en-dash. *)
let%expect_test "a hyphen closing a curly container is held back from the run" =
  let extra_inline_containers = Extra_config.make ~deleted:Curly_required () in
  html ~extra_inline_containers "{-a--}";
  html ~extra_inline_containers "{-a---}";
  [%expect {|
    <p><del>a-</del></p>
    <p><del>a–</del></p>
    |}]

let%expect_test "a bare deleted container keeps its lone hyphens" =
  let extra_inline_containers = Extra_config.make ~deleted:Curly_optional () in
  html ~extra_inline_containers "-gone- and a--b";
  [%expect {| <p><del>gone</del> and a–b</p> |}]

(* Roundtrip
   ========= *)

let%expect_test "commonmark renders back to source, markers included" =
  commonmark "57--33 oxen---and no sheep...";
  print_string "\n";
  commonmark "\"Hello,\" said the spider.";
  print_string "\n";
  commonmark "'}Tis the season";
  [%expect
    {|
    57--33 oxen---and no sheep...
    "Hello," said the spider.
    '}Tis the season
    |}]

(* Latex
   ===== *)

let%expect_test "latex uses TeX's own spellings" =
  latex "\"Hello,\" said the spider---or so I heard...";
  [%expect {| ``Hello,'' said the spider---or so I heard\ldots{} |}]
