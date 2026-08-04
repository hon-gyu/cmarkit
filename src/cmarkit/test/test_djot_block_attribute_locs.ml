open Cmarkit_

(** Where a block attribute line is.

    A djot attribute line above a block becomes a {!Block.Ext_attributes}
    wrapping that block. The wrapper used to be given the block's own meta
    verbatim, which made it positionally identical to its child: the
    [ {#id} ] line had no location anywhere in the tree, and a consumer that
    wanted to point at it — jump to an anchor's definition, rename an id,
    underline a duplicate — could not find it.

    The wrapper now spans from the first specifier line through the block it
    attributes. Its inline counterpart has always done this: the
    {!Inline.Ext_attributes} of [ [text]\{#id\} ] covers the specifier too.

    Two properties are pinned below: the wrapper starts at the specifier, and
    it is strictly wider than its child. *)

let locs s =
  let doc = Doc.of_string ~strict:false ~locs:true ~block_attributes:true s in
  let show label meta =
    let tl = Meta.textloc meta in
    if Textloc.is_none tl
    then Printf.printf "%s: none\n" label
    else
      Printf.printf
        "%s: %d-%d %S\n"
        label
        (Textloc.first_byte tl)
        (Textloc.last_byte tl)
        (String.sub s (Textloc.first_byte tl)
           (Textloc.last_byte tl - Textloc.first_byte tl + 1))
  in
  let folder =
    Folder.make
      ~block:(fun f acc b ->
        (match b with
        | Block.Ext_attributes (a, meta) ->
            show "attrs" meta;
            show "  block" (Block.meta (Block.Attributes.block a))
        | _ -> ());
        ignore f;
        Folder.default)
      ~inline:(fun _f acc _i -> Folder.ret acc)
      ~inline_ext_default:(fun _f acc _i -> acc)
      ~block_ext_default:(fun _f acc _b -> acc)
      ()
  in
  Folder.fold_doc folder () doc

let%expect_test "the wrapper spans the specifier and the block" =
  locs "{#aside}\n> An aside block.\n";
  [%expect {|
    attrs: 0-25 "{#aside}\n> An aside block."
      block: 9-25 "> An aside block."
    |}]

let%expect_test "stacked specifiers span from the first" =
  locs "{#a}\n{.b}\nA paragraph.\n";
  [%expect {|
    attrs: 0-21 "{#a}\n{.b}\nA paragraph."
      block: 10-21 "A paragraph."
    |}]

let%expect_test "a multi-line specifier is covered whole" =
  locs "{#wide\n  .cls}\nA paragraph.\n";
  [%expect {|
    attrs: 0-26 "{#wide\n  .cls}\nA paragraph."
      block: 15-26 "A paragraph."
    |}]

(* A heading whose id is written above it: the wrapper is what tells a
   consumer where that line is, since the heading's own meta cannot. *)
let%expect_test "heading" =
  locs "{#intro}\n# Introduction\n";
  [%expect {|
    attrs: 0-22 "{#intro}\n# Introduction"
      block: 9-22 "# Introduction"
    |}]

(* Detached by a blank line, the specifiers attribute nothing and are kept as
   an empty wrapper so the source can be rendered back. It keeps the location
   it was written at rather than having none. *)
let%expect_test "detached specifier" =
  locs "{#lost}\n\nA paragraph.\n";
  [%expect {|
    attrs: 0-6 "{#lost}"
      block: none
    |}]

let%expect_test "specifier at the end of the document" =
  locs "A paragraph.\n\n{#trailing}\n";
  [%expect {|
    attrs: 14-24 "{#trailing}"
      block: none
    |}]

(* Nested in a container, the offsets are still absolute. *)
let%expect_test "inside a block quote" =
  locs "> {#inner}\n> A quoted paragraph.\n";
  [%expect {|
    attrs: 2-31 "{#inner}\n> A quoted paragraph."
      block: 13-31 "A quoted paragraph."
    |}]

let%expect_test "no locations requested" =
  let doc = Doc.of_string ~strict:false ~locs:false ~block_attributes:true "{#a}\nP.\n" in
  let folder =
    Folder.make
      ~block:(fun _f acc b ->
        (match b with
        | Block.Ext_attributes (_, meta) ->
            print_endline
              (if Textloc.is_none (Meta.textloc meta) then "none" else "located")
        | _ -> ());
        Folder.default)
      ~inline:(fun _f acc _i -> Folder.ret acc)
      ~inline_ext_default:(fun _f acc _i -> acc)
      ~block_ext_default:(fun _f acc _b -> acc)
      ()
  in
  Folder.fold_doc folder () doc;
  [%expect {| none |}]
