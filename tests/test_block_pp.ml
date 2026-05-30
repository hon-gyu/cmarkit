open Cmarkit_

let md = {|# Heading 1
foo

- bar
- baz

- ja

```yi
bing
```

## ding

---

> blockquote
> > inner blockquote|}

let%expect_test "" =
  let doc = Doc.of_string md in
  let block = Doc.block doc in
  Format.printf "%a" Pp.pp_block block;
  [%expect {|
    Blocks
      Heading H1 "Heading 1"
      Paragraph "foo"
      Blank_line
      List { unordered '-'; loose }
        - item
          Paragraph "bar"
        - item
          Blocks
            Paragraph "baz"
            Blank_line
        - item
          Paragraph "ja"
      Blank_line
      Fenced_code_block { info="yi"; lines=1 }
      Blank_line
      Heading H2 "ding"
      Blank_line
      Thematic_break
      Blank_line
      Block_quote
        Blocks
          Paragraph "blockquote"
          Block_quote
            Paragraph "inner blockquote"
    |}];
