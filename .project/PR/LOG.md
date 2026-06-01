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

## Verification

- `dune runtest src/cmarkit/test`
- `dune runtest`
- `dune build`

## Next step

Implement configurable one-character strong emphasis.

Expected shape:

- Keep CommonMark default: strong emphasis consumes two delimiters.
- Add an explicit parser knob for strong-emphasis delimiter width.
- Change emphasis matching to return a semantic decision, not only a consumed delimiter count, so `used = 1` can still mean strong emphasis when the knob allows it.
- Add `.expected` tests showing default CommonMark behavior and knob-enabled one-character strong behavior.
