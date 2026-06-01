# PR log

## Progress

- Added `?intraword_emphasis` parser knob.
  - Default remains CommonMark-compatible.
  - `false` prevents delimiter runs between two non-whitespace, non-punctuation characters from opening or closing emphasis.

- Added `?marked_emphasis_delims` parser knob.
  - Default remains CommonMark-compatible.
  - With the knob enabled, `{*` and `{_` mark emphasis delimiters as opener-only.
  - With the knob enabled, `*}` and `_}` mark emphasis delimiters as closer-only.
  - Parser tokens track `open_marker` and `close_marker`.
  - Parsed emphasis spans consume marker braces while preserving the actual delimiter character in the AST.

- Extended `Inline.Emphasis.t` with marker intent.
  - Added `open_marker` and `close_marker`.
  - Parser-created emphasis carries marker intent from matched delimiter tokens.
  - `Cmarkit_commonmark` emits marker braces when marker intent is present.

- Added `.expected` style tests in `src/cmarkit/test/test_inline_struct.ml`.
  - Shows default vs `?intraword_emphasis:false` tokenization.
  - Shows default vs `?marked_emphasis_delims:true` tokenization.
  - Shows marked delimiter parsing.
  - Shows CommonMark rendering for direct AST and parsed marked delimiters.

- Added `?strong_emphasis_width` parser knob.
  - Default remains CommonMark-compatible at width `2`.
  - Width `1` lets a permitted strong-emphasis delimiter form strong emphasis
    from a single delimiter character.
  - The emphasis matcher now returns a semantic match decision, not only the
    number of consumed delimiters.
  - Added parser-config and `.expected` style tests for default vs width `1`.

## Verification

- `dune runtest src/cmarkit/test`
- `dune runtest`
- `dune build`

## Next step

Add the typed oymarkit inline-container AST extension shape.

Expected shape:

- Add a shared inline-container kind for highlight, superscript, subscript,
  inserted text, and deleted text.
- Use cmarkit's extensible `Inline.t` mechanism with typed oymarkit-owned
  payloads.
- Update core support code that must know about the new inline cases:
  normalization, mapping/folding, sexp/debug output, and renderers as needed.
