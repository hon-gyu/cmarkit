open Cmarkit_

let html s =
  let doc = Doc.of_string ~djot:true s in
  print_string (Cmarkit_html.of_doc ~safe:false ~djot:true doc)

let%expect_test "footnote collection follows references inside notes" =
  html
    "text[^footnote].\n\n[^footnote]: very long footnote[^another-footnote]\n[^another-footnote]: bla bla[^another-footnote]\n";
  [%expect {|
    <p>text<a id="fnref1" href="#fn1" role="doc-noteref"><sup>1</sup></a>.</p>
    <section role="doc-endnotes">
    <hr>
    <ol>
    <li id="fn1">
    <p>very long footnote<a id="fnref2" href="#fn2" role="doc-noteref"><sup>2</sup></a><a href="#fnref1" role="doc-backlink">↩︎</a></p>
    </li>
    <li id="fn2">
    <p>bla bla<a href="#fn2" role="doc-noteref"><sup>2</sup></a><a href="#fnref2" role="doc-backlink">↩︎</a></p>
    </li>
    </ol>
    </section>
    |}]
