open Cmarkit_

(** Djot attributes on a reference definition.

    Attributes written above a reference definition merge onto every link that
    references it:

    {v
{.external}
[docs]: https://example.org

See [docs][].
    v}

    The attributes live on the {!Cmarkit.Link_definition.t}, not on the
    [Link_reference_definition] block, because a link needs them at inline
    parsing time, when all it has in hand is the definition. They are attached
    in a pass over the block structure before any inline content is parsed, and
    the link is wrapped in the same [Ext_attributes] node an inline [{...}]
    specifier produces, so nothing downstream needs to know where they came
    from. *)

let html s =
  let doc =
    Doc.of_string ~strict:false ~djot_block_attributes:true
      ~djot_inline_attributes:true s
  in
  print_string (Cmarkit_html.of_doc ~safe:false doc)

let%expect_test "a definition's attributes merge onto a reference to it" =
  html "{.external}\n[docs]: https://example.org\n\nSee [docs][].\n";
  [%expect {| <p>See <a href="https://example.org" class="external">docs</a>.</p> |}]

let%expect_test "they merge onto every reference, and onto shortcut ones" =
  html "{.external}\n[docs]: https://example.org\n\nSee [docs] and [docs][].\n";
  [%expect {| <p>See <a href="https://example.org" class="external">docs</a> and <a href="https://example.org" class="external">docs</a>.</p> |}]

let%expect_test "a definition without attributes is unchanged" =
  html "[docs]: https://example.org\n\nSee [docs][].\n";
  [%expect {| <p>See <a href="https://example.org">docs</a>.</p> |}]

let%expect_test "an inline link takes no attributes from any definition" =
  html "{.external}\n[docs]: https://example.org\n\nSee [x](https://other.org).\n";
  [%expect {| <p>See <a href="https://other.org">x</a>.</p> |}]

let%expect_test "an inline specifier still applies on top" =
  html "{.external}\n[docs]: https://example.org\n\nSee [docs][]{#here}.\n";
  [%expect {| <p>See <a href="https://example.org" id="here" class="external">docs</a>.</p> |}]
