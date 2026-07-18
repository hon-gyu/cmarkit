open Cmarkit_

(** Djot definition lists, behind [definition_lists]:

    {v
: term
  the blocks that define it
    v}

    The [:] marker must be followed by a space or the end of the line, which is
    what keeps it from competing with a [:::] div fence. The term is the rest of
    the marker line (inline content), and the definition is the blocks indented
    under it.

    The definition's indent is {e not} fixed by the marker the way a list item's
    is: djot lets the definition sit at any indent past the colon. So the first
    line indented at least two columns fixes the definition indent and every
    later line must reach it. A line indented only one column continues the
    term instead.
    Tightness follows the same rule as list items: a blank line between two
    blocks of a definition makes the list loose, a trailing one does not. *)

let html ?definition_lists s =
  let doc = Doc.of_string ~strict:false ?definition_lists s in
  print_string (Cmarkit_html.of_doc ~safe:false doc)

let commonmark ?definition_lists s =
  let doc = Doc.of_string ~strict:false ?definition_lists s in
  print_string (Cmarkit_commonmark.of_doc doc)

let pp ?definition_lists s =
  let doc = Doc.of_string ~strict:false ?definition_lists s in
  Format.printf "%a@." Pp.pp_block (Doc.block doc)

(* The marker
   ========== *)

let%expect_test "off: a [: term] line is a paragraph" =
  html ": apple\n  A fruit.\n";
  [%expect {|
    <p>: apple
    A fruit.</p>
    |}]

let%expect_test "on: a term and its definition" =
  html ~definition_lists:true ": apple\n  A fruit.\n";
  [%expect {|
    <dl>
    <dt>apple</dt>
    <dd>A fruit.</dd>
    </dl>
    |}]

let%expect_test "on: several items" =
  html ~definition_lists:true ": apple\n  A fruit.\n: onion\n  A vegetable.\n";
  [%expect {|
    <dl>
    <dt>apple</dt>
    <dd>A fruit.</dd>
    <dt>onion</dt>
    <dd>A vegetable.</dd>
    </dl>
    |}]

let%expect_test "on: the marker needs a space or the end of the line" =
  html ~definition_lists:true ":not a term\n";
  [%expect {| <p>:not a term</p> |}]

let%expect_test "on: a div fence is still a div, not a term" =
  let doc = Doc.of_string ~strict:false ~definition_lists:true ~div:true
      "::: warning\ncontent\n:::\n"
  in
  print_string (Cmarkit_html.of_doc ~safe:false doc);
  [%expect {|
    <div class="warning">
    <p>content</p>
    </div>
    |}]

(* The definition's indent
   ====================== *)

let%expect_test "on: a deeper indent opens the definition" =
  html ~definition_lists:true ": apple\n    A deeply indented fruit.\n";
  [%expect {|
    <dl>
    <dt>apple</dt>
    <dd>A deeply indented fruit.</dd>
    </dl>
    |}]

let%expect_test "on: one-column indentation continues the term" =
  html ~definition_lists:true ": apple\n fruit\n\n  A definition.\n";
  [%expect {|
    <dl>
    <dt>apple
    fruit</dt>
    <dd>
    <p>A definition.</p>
    </dd>
    </dl>
    |}]

let%expect_test "on: a fence on the marker line starts the definition" =
  html ~definition_lists:true ": ```\n  code\n  ```\n";
  [%expect {|
    <dl>
    <dt></dt>
    <dd>
    <pre><code>code
    </code></pre>
    </dd>
    </dl>
    |}]

let%expect_test "on: an unindented line ends the list" =
  html ~definition_lists:true ": apple\n  A fruit.\nback to a paragraph\n";
  [%expect {|
    <dl>
    <dt>apple</dt>
    <dd>A fruit.</dd>
    </dl>
    <p>back to a paragraph</p>
    |}]

let%expect_test "on: a definition may hold several blocks" =
  html ~definition_lists:true
    ": apple\n  A fruit.\n\n  Still the definition.\n";
  [%expect {|
    <dl>
    <dt>apple</dt>
    <dd>
    <p>A fruit.</p>
    <p>Still the definition.</p>
    </dd>
    </dl>
    |}]

let%expect_test "on: a definition may be empty" =
  html ~definition_lists:true ": apple\n: onion\n";
  [%expect {|
    <dl>
    <dt>apple</dt>
    <dd>
    </dd>
    <dt>onion</dt>
    <dd>
    </dd>
    </dl>
    |}]

(* Tightness
   ========= *)

let%expect_test "on: a tight definition drops the paragraph wrapper" =
  html ~definition_lists:true ": apple\n  A fruit.\n: onion\n  A vegetable.\n";
  [%expect {|
    <dl>
    <dt>apple</dt>
    <dd>A fruit.</dd>
    <dt>onion</dt>
    <dd>A vegetable.</dd>
    </dl>
    |}]

let%expect_test "on: a blank line between items makes the list loose" =
  html ~definition_lists:true
    ": apple\n  A fruit.\n\n: onion\n  A vegetable.\n";
  [%expect {|
    <dl>
    <dt>apple</dt>
    <dd>
    <p>A fruit.</p>
    </dd>
    <dt>onion</dt>
    <dd>
    <p>A vegetable.</p>
    </dd>
    </dl>
    |}]

let%expect_test "on: a blank line between term and definition is loose" =
  html ~definition_lists:true ": apple\n\n  A fruit.\n";
  [%expect {|
    <dl>
    <dt>apple</dt>
    <dd>
    <p>A fruit.</p>
    </dd>
    </dl>
    |}]

(* AST and round-trip
   ================== *)

let%expect_test "on: the AST carries the term and the definition" =
  pp ~definition_lists:true ": apple\n  A *fruit*.\n";
  [%expect {|
    Blocks
      Ext_definition_list { tight }
        : "apple"
          Paragraph "A fruit."
      Blank_line
    |}]

let%expect_test "on: a definition list round-trips through CommonMark" =
  commonmark ~definition_lists:true
    ": apple\n  A fruit.\n: onion\n  A vegetable.\n";
  [%expect {|
    : apple
      A fruit.
    : onion
      A vegetable.
    |}]
