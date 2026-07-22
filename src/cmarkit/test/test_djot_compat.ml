open Cmarkit_

(** Djot compatibility knobs that flip the behavior of an {e existing}
    CommonMark construct, plus the [djot] preset that turns all of them on at
    once. Each knob is tested off (CommonMark behavior, the default) and on
    (djot behavior), since both configurations must keep working: the preset is
    only a default, not a mode.

    The knobs here are the ones that {e remove} or {e change} behavior, rather
    than add syntax:

    - [lazy_continuation]: an unmarked line no longer continues the paragraph
      inside a block quote or list item. This one is {e not} a djot behavior:
      djot keeps CommonMark's lazy continuation, so the preset leaves the knob
      on. The knob stands on its own, for a stricter dialect;
    - [raw_html]: djot parses no HTML, inline or block;
    - [entity_refs]: djot's only escape is the backslash;
    - [backslash_space_nbsp]: backslash-space is a non-breaking space;
    - [hard_break_trailing_blanks]: a hard-break backslash may precede blanks;
    - [two_space_hard_break]: two trailing spaces create a hard break;
    - [whitespace_free_info_string]: fence info strings cannot contain
      whitespace;
    - [block_quote_marker_space]: djot's [>] wants a space after it. *)

let html ?djot ?lazy_continuation ?raw_html ?entity_refs ?backslash_space_nbsp
    ?hard_break_trailing_blanks ?two_space_hard_break ?list_indent
    ?list_tightness ?indented_code ?tilde_code_fences
    ?whitespace_free_info_string ?verbatim_style ?block_quote_marker_space s =
  let doc =
    Doc.of_string ~strict:false ?djot ?lazy_continuation ?raw_html ?entity_refs
      ?backslash_space_nbsp ?hard_break_trailing_blanks ?two_space_hard_break
      ?list_indent ?list_tightness ?indented_code ?tilde_code_fences
      ?whitespace_free_info_string ?verbatim_style ?block_quote_marker_space s
  in
  print_string (Cmarkit_html.of_doc ~safe:false doc)

(* Lazy continuation
   ================= *)

let%expect_test "commonmark: an unmarked line continues the quoted paragraph" =
  html "> quoted\nlazy\n";
  [%expect
    {|
    <blockquote>
    <p>quoted
    lazy</p>
    </blockquote>
    |}]

let%expect_test "off: an unmarked line closes the block quote" =
  html ~lazy_continuation:false "> quoted\nlazy\n";
  [%expect
    {|
    <blockquote>
    <p>quoted</p>
    </blockquote>
    <p>lazy</p>
    |}]

let%expect_test "off: a marked line still continues the block quote" =
  html ~lazy_continuation:false "> quoted\n> more\n";
  [%expect
    {|
    <blockquote>
    <p>quoted
    more</p>
    </blockquote>
    |}]

let%expect_test "commonmark: an unindented line continues the list item" =
  html "- item\nlazy\n";
  [%expect {|
    <ul>
    <li>item
    lazy</li>
    </ul>
    |}]

let%expect_test "off: an unindented line closes the list" =
  html ~lazy_continuation:false "- item\nlazy\n";
  [%expect {|
    <ul>
    <li>item</li>
    </ul>
    <p>lazy</p>
    |}]

let%expect_test "off: an indented line still continues the list item" =
  html ~lazy_continuation:false "- item\n  more\n";
  [%expect {|
    <ul>
    <li>item
    more</li>
    </ul>
    |}]

(* Raw HTML
   ======== *)

let%expect_test "commonmark: inline tags are raw HTML" =
  html "a <b>bold</b> tag";
  [%expect {| <p>a <b>bold</b> tag</p> |}]

let%expect_test "djot: inline tags are text" =
  html ~raw_html:false "a <b>bold</b> tag";
  [%expect {| <p>a &lt;b&gt;bold&lt;/b&gt; tag</p> |}]

let%expect_test "commonmark: a lone tag opens an HTML block" =
  html "<div>\nnot a paragraph\n</div>\n";
  [%expect {|
    <div>
    not a paragraph
    </div>
    |}]

let%expect_test "djot: a lone tag is just a paragraph line" =
  html ~raw_html:false "<div>\nnot a paragraph\n</div>\n";
  [%expect
    {|
    <p>&lt;div&gt;
    not a paragraph
    &lt;/div&gt;</p>
    |}]

let%expect_test "djot: autolinks survive, they are not HTML" =
  html ~raw_html:false "<https://example.org> and <me@example.org>";
  [%expect
    {| <p><a href="https://example.org">https://example.org</a> and <a href="mailto:me@example.org">me@example.org</a></p> |}]

(* Entity and character references
   =============================== *)

let%expect_test "commonmark: references resolve" =
  html "&amp; &#38; &#x26; &nonsense;";
  [%expect {| <p>&amp; &amp; &amp; &amp;nonsense;</p> |}]

let%expect_test "djot: references stay literal" =
  html ~entity_refs:false "&amp; &#38; &#x26; &nonsense;";
  [%expect {| <p>&amp;amp; &amp;#38; &amp;#x26; &amp;nonsense;</p> |}]

let%expect_test "djot: a backslash escape still works without entities" =
  html ~entity_refs:false "\\*not emphasis\\*";
  [%expect {| <p>*not emphasis*</p> |}]

(* Escapes and hard breaks
   ======================= *)

let%expect_test "commonmark: two trailing spaces are a hard break" =
  html "one  \ntwo";
  [%expect {|
    <p>one<br>
    two</p>
    |}]

let%expect_test "off: two trailing spaces are just spaces" =
  html ~two_space_hard_break:false "one  \ntwo";
  [%expect {|
    <p>one
    two</p>
    |}]

let%expect_test "independent: djot escapes do not disable two-space breaks" =
  html ~backslash_space_nbsp:true "one  \ntwo";
  [%expect {|
    <p>one<br>
    two</p>
    |}]

let%expect_test "both: a trailing backslash is the hard break" =
  html "one\\\ntwo";
  [%expect {|
    <p>one<br>
    two</p>
    |}]

let%expect_test "trailing backslash runs use escape parity" =
  html ~hard_break_trailing_blanks:true {|foo\\
bar|};
  html ~hard_break_trailing_blanks:true {|foo\\\
bar|};
  [%expect {|
    <p>foo\
    bar</p>
    <p>foo\<br>
    bar</p>
    |}]

let%expect_test "hard-break backslashes may tolerate trailing blanks" =
  html "one\\  \ntwo";
  html ~hard_break_trailing_blanks:true "one\\  \ntwo";
  [%expect {|
    <p>one\<br>
    two</p>
    <p>one<br>
    two</p>
    |}]

let%expect_test "commonmark: backslash-space is a literal backslash" =
  html "a\\ b";
  [%expect {| <p>a\ b</p> |}]

let%expect_test "djot: backslash-space is a non-breaking space" =
  html ~backslash_space_nbsp:true "a\\ b";
  [%expect {| <p>a&nbsp;b</p> |}]

let%expect_test "djot: an escaped backslash can precede a non-breaking space" =
  html ~backslash_space_nbsp:true {|foo\\\ bar|};
  [%expect {| <p>foo\&nbsp;bar</p> |}]

let%expect_test "djot: backslash before punctuation is unchanged" =
  html ~backslash_space_nbsp:true "\\*x\\* \\\\ \\a";
  [%expect {| <p>*x* \ \a</p> |}]

let%expect_test "list indentation is a policy" =
  html ~list_indent:`Content_column "- one\n - two\n";
  html ~list_indent:`Marker_plus_one "- one\n - two\n";
  [%expect
    {|
    <ul>
    <li>one</li>
    <li>two</li>
    </ul>
    <ul>
    <li>one
    <ul>
    <li>two</li>
    </ul>
    </li>
    </ul>
    |}]

let%expect_test "list tightness is a policy" =
  let source = "- a\n\n  - b\n\n- c\n" in
  html ~list_tightness:`Any_blank source;
  html ~list_tightness:`Non_list_boundary_blank source;
  [%expect
    {|
    <ul>
    <li>
    <p>a</p>
    <ul>
    <li>b</li>
    </ul>
    </li>
    <li>
    <p>c</p>
    </li>
    </ul>
    <ul>
    <li>a
    <ul>
    <li>b</li>
    </ul>
    </li>
    <li>c</li>
    </ul>
    |}]

(* Code fences
   =========== *)

let%expect_test "commonmark: both fence characters open a code block" =
  html "```\ncode\n```\n";
  html "~~~\ncode\n~~~\n";
  [%expect
    {|
    <pre><code>code
    </code></pre>
    <pre><code>code
    </code></pre>
    |}]

let%expect_test "djot: only backticks fence, a tilde run is text" =
  html ~tilde_code_fences:false "```\ncode\n```\n";
  html ~tilde_code_fences:false "~~~\ncode\n~~~\n";
  [%expect
    {|
    <pre><code>code
    </code></pre>
    <p>~~~
    code
    ~~~</p>
    |}]

let%expect_test "commonmark: fence info may contain whitespace" =
  html "``` not a code block";
  [%expect {| <pre><code class="language-not"></code></pre> |}]

let%expect_test "djot: fence info may not contain whitespace" =
  html ~whitespace_free_info_string:true ~verbatim_style:`Verbatim_span
    "``` not a code block";
  [%expect {| <p><code> not a code block</code></p> |}]

(* Block quote marker
   ================== *)

let%expect_test "commonmark: the marker needs no space" =
  html ">text";
  [%expect {|
    <blockquote>
    <p>text</p>
    </blockquote>
    |}]

let%expect_test "djot: the marker needs a space or the end of the line" =
  html ~block_quote_marker_space:true ">text";
  html ~block_quote_marker_space:true "> text";
  html ~block_quote_marker_space:true ">\n> text";
  [%expect
    {|
    <p>&gt;text</p>
    <blockquote>
    <p>text</p>
    </blockquote>
    <blockquote>
    <p>text</p>
    </blockquote>
    |}]

(* The [djot] preset
   ================= *)

let%expect_test "preset: lazy lines stay, no HTML, no entities, djot escapes" =
  (* Djot has lazy continuation, so the preset keeps it: the quoted paragraph
     swallows the unmarked line. *)
  html ~djot:true "> quoted\nlazy\n";
  html ~djot:true "<b>text</b> &amp;";
  (* Two trailing spaces are not a hard break in djot, and they are not layout
     either: they are text, and they survive into the output. *)
  html ~djot:true "one  \ntwo";
  [%expect
    {|
    <blockquote>
    <p>quoted
    lazy</p>
    </blockquote>
    <p>&lt;b&gt;text&lt;/b&gt; &amp;amp;</p>
    <p>one
    two</p>
    |}]

let%expect_test "preset: no indented code, no setext heading, no _ break" =
  html ~djot:true "    not code";
  (* No block start interrupts a paragraph, so the [---] is neither a setext
     underline nor a thematic break: it is text, and smart punctuation reads it
     as an em dash. A blank line above it is what lets it be a break. *)
  html ~djot:true "heading?\n---\n";
  html ~djot:true "heading?\n\n---\n";
  html ~djot:true "___";
  [%expect
    {|
    <p>not code</p>
    <p>heading?
    —</p>
    <p>heading?</p>
    <hr>
    <p>___</p>
    |}]

let%expect_test "preset: an explicit knob wins over the preset" =
  html ~djot:true ~indented_code:true "    code";
  html ~djot:true ~raw_html:true "<b>bold</b>";
  [%expect
    {|
    <pre><code>code
    </code></pre>
    <p><b>bold</b></p>
    |}]
