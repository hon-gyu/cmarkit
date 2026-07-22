open Cmarkit_

(** Djot links, behind [djot_links]. Two changes, both about what counts as the
    destination:

    - there are no titles. Everything between the [(] and the matching [)] is
      the URL, so the quoted trailer of [ [a](url "title") ] is just more URL.
      Same for a reference definition: the rest of the line is the URL.
    - the destination may be split over lines; the newlines are removed.

    This means djot destinations may contain spaces and quotes, which is why
    they cannot go through CommonMark's destination grammar (which stops at a
    space and hands the rest to the title parser). *)

let html ?djot_links ?case_sensitive_labels s =
  let doc = Doc.of_string ~strict:false ?djot_links ?case_sensitive_labels s in
  let djot = djot_links = Some true in
  print_string (Cmarkit_html.of_doc ~safe:false ~djot doc)

(* Inline links
   ============ *)

let%expect_test "commonmark: the quoted trailer is a title" =
  html "[a](https://example.org \"The title\")";
  [%expect {| <p><a href="https://example.org" title="The title">a</a></p> |}]

let%expect_test "djot: there are no titles, it is all destination" =
  html ~djot_links:true "[a](https://example.org \"The title\")";
  [%expect {| <p><a href="https://example.org &quot;The title&quot;">a</a></p> |}]

let%expect_test "both: a plain destination is unaffected" =
  html "[a](https://example.org)";
  html ~djot_links:true "[a](https://example.org)";
  [%expect {|
    <p><a href="https://example.org">a</a></p>
    <p><a href="https://example.org">a</a></p>
    |}]

let%expect_test "djot: the destination may be split over lines" =
  html ~djot_links:true "[a](https://example.org/\n  a/long/path)";
  [%expect {| <p><a href="https://example.org/a/long/path">a</a></p> |}]

let%expect_test "djot: multiline destination bytes stay literal" =
  html ~djot_links:true "[closed](hello *a\nb*)";
  [%expect {| <p><a href="hello *ab*">closed</a></p> |}]

let%expect_test "djot: an empty destination" =
  html ~djot_links:true "[a]()";
  [%expect {| <p><a href="">a</a></p> |}]

let%expect_test "djot: implicit labels use parsed plain text" =
  html ~djot_links:true
    "[link _and_ link][]\n\n[link and link]: url\n";
  [%expect {| <p><a href="url">link <em>and</em> link</a></p> |}]

let%expect_test "djot: an attempted link clears unresolved text openers" =
  html ~djot_links:true "[x_y](x_";
  [%expect {| <p>[x_y](x_</p> |}]

(* Reference definitions
   ===================== *)

let%expect_test "commonmark: a definition may carry a title" =
  html "[a]: https://example.org \"The title\"\n\n[a]\n";
  [%expect {| <p><a href="https://example.org" title="The title">a</a></p> |}]

let%expect_test "djot: the rest of the line is the destination" =
  html ~djot_links:true "[a]: https://example.org\n\n[a]\n";
  [%expect {| <p><a href="https://example.org">a</a></p> |}]

let%expect_test "djot: no titles in definitions either" =
  html ~djot_links:true "[a]: https://example.org \"The title\"\n\n[a]\n";
  [%expect {| <p><a href="https://example.org &quot;The title&quot;">a</a></p> |}]

let%expect_test "djot: an indented line continues the destination" =
  html ~djot_links:true "[a]: https://example.org/\n  a/long/path\n\n[a]\n";
  [%expect {| <p><a href="https://example.org/a/long/path">a</a></p> |}]

let%expect_test "labels are case-insensitive by default" =
  html "[Link]: /url\n\n[link][]\n";
  html ~djot_links:true ~case_sensitive_labels:false
    "[Link]: /url\n\n[link][]\n";
  [%expect {|
    <p><a href="/url">link</a></p>
    <p><a href="/url">link</a></p>
    |}]

let%expect_test "case-sensitive labels are independent of link grammar" =
  html ~case_sensitive_labels:true "[Link]: /url\n\n[link][]\n";
  [%expect {| <p>[link][]</p> |}]
