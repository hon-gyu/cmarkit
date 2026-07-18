open Cmarkit_

(** Djot raw content, behind [format_raw_content]:

    - a raw block is a code fence whose info string is [=format];
    - a raw inline is a verbatim span followed by a [ {=format} ] specifier.

    A renderer passes the content through verbatim when [format] is its own
    output format and drops it otherwise, so the same document renders
    differently — and correctly — to HTML and to LaTeX. This is djot's answer to
    having no raw HTML at all (see [raw_html] in [test_djot_compat]): raw output
    is asked for explicitly rather than falling out of a stray tag.

    The specifier is not attribute syntax. [Attribute.of_string] rejects a spec
    starting with ['='], and [ {=html} ] only means raw when it is adjacent to a
    verbatim span — elsewhere it stays text. *)

let html ?(safe = false) ?format_raw_content s =
  let doc = Doc.of_string ~strict:false ?format_raw_content s in
  print_string (Cmarkit_html.of_doc ~safe doc)

let latex ?format_raw_content s =
  let doc = Doc.of_string ~strict:false ?format_raw_content s in
  print_string (Cmarkit_latex.of_doc doc)

let commonmark ?format_raw_content s =
  let doc = Doc.of_string ~strict:false ?format_raw_content s in
  print_string (Cmarkit_commonmark.of_doc doc)

let sexp ?format_raw_content s =
  let doc = Doc.of_string ~strict:false ?format_raw_content s in
  Format.printf "%a@." Sexplib0.Sexp.pp_hum ((Sexp.make_sexp_of ()).doc doc)

(* Raw block
   ========= *)

let%expect_test "off: an =html fence is an ordinary code block" =
  html "```=html\n<p>hi</p>\n```\n";
  [%expect {|
    <pre><code class="language-=html">&lt;p&gt;hi&lt;/p&gt;
    </code></pre>
    |}]

let%expect_test "on: an =html fence passes through to HTML" =
  html ~format_raw_content:true "```=html\n<p>hi</p>\n```\n";
  [%expect {| <p>hi</p> |}]

let%expect_test "on: a raw block for another format is dropped" =
  html ~format_raw_content:true "```=latex\n\\emph{hi}\n```\n";
  latex ~format_raw_content:true "```=latex\n\\emph{hi}\n```\n";
  latex ~format_raw_content:true "```=html\n<p>hi</p>\n```\n";
  [%expect {| \emph{hi} |}]

let%expect_test "on: safe mode does not pass raw HTML through" =
  html ~safe:true ~format_raw_content:true "```=html\n<p>hi</p>\n```\n";
  [%expect {| <!-- raw HTML block omitted --> |}]

let%expect_test "on: the format is the info string's only word" =
  (* [=html extra] is not a raw block: it is a code block whose language
     happens to be [=html]. *)
  html ~format_raw_content:true "```=html extra\n<p>hi</p>\n```\n";
  [%expect {|
    <pre><code class="language-=html">&lt;p&gt;hi&lt;/p&gt;
    </code></pre>
    |}]

let%expect_test "on: a raw block round-trips through CommonMark" =
  commonmark ~format_raw_content:true "```=html\n<p>hi</p>\n```\n";
  [%expect {|
    ```=html
    <p>hi</p>
    ```
    |}]

(* Raw inline
   ========== *)

let%expect_test "off: the specifier is text after a code span" =
  html "a `<a>`{=html} b";
  [%expect {| <p>a <code>&lt;a&gt;</code>{=html} b</p> |}]

let%expect_test "on: a verbatim span with {=html} passes through" =
  html ~format_raw_content:true "a `<a href=\"x\">link</a>`{=html} b";
  [%expect {| <p>a <a href="x">link</a> b</p> |}]

let%expect_test "on: a raw inline for another format is dropped" =
  html ~format_raw_content:true "a `\\emph{x}`{=latex} b";
  latex ~format_raw_content:true "a `\\emph{x}`{=latex} b";
  [%expect {|
    <p>a  b</p>
    a \emph{x} b
    |}]

let%expect_test "on: safe mode does not pass raw HTML through" =
  html ~safe:true ~format_raw_content:true "a `<b>`{=html} c";
  [%expect {| <p>a <!-- raw HTML omitted --> c</p> |}]

let%expect_test "on: the specifier must be adjacent to the verbatim span" =
  html ~format_raw_content:true "a `<b>` {=html} c";
  [%expect {| <p>a <code>&lt;b&gt;</code> {=html} c</p> |}]

let%expect_test "on: braces holding anything but a format are not raw" =
  html ~format_raw_content:true "a `<b>`{=} c";
  html ~format_raw_content:true "a `<b>`{.cls} c";
  [%expect {|
    <p>a <code>&lt;b&gt;</code>{=} c</p>
    <p>a <code>&lt;b&gt;</code>{.cls} c</p>
    |}]

let%expect_test "on: a raw inline round-trips through CommonMark" =
  commonmark ~format_raw_content:true "a `<b>`{=html} c";
  [%expect {| a `<b>`{=html} c |}]

let%expect_test "on: the AST carries the format and the content" =
  sexp ~format_raw_content:true "`<b>`{=html}";
  [%expect {| (Paragraph (Raw_inline html <b>)) |}]
