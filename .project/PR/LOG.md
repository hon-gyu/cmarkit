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

- Added typed oymarkit extra inline container AST extension shape.
  - Added `Inline.Extra_inline_container.kind` with highlight, superscript, subscript,
    inserted text, and deleted text.
  - Added `Inline.Ext_extra_inline_container` using cmarkit's extensible `Inline.t`.
  - Updated normalization, plain-text extraction, mapper/folder traversal,
    sexp/debug output, CommonMark/HTML/LaTeX renderers, and locs tooling.
  - CommonMark rendering uses explicit curly forms: `{=...=}`, `{^...^}`,
    `{~...~}`, `{+...+}`, and `{-...-}`.

- Added `?extra_inline_containers` parser configuration for extra inline
  containers.
  - Default remains CommonMark-compatible.
  - With the knob enabled, parses `{=highlight=}`, `{^sup^}`, `{~sub~}`,
    `{+inserted+}`, and `{-deleted-}` into
    `Inline.Ext_extra_inline_container`.
  - Added tokenization support for `{x` opener and `x}` closer pairs under the
    knob.
  - Container payloads recurse through the existing inline parser, so nested
    emphasis and other inline structure still parse.
  - Added `.expected` style tests for default literal behavior, tokenization,
    each parsed kind, and nested inline payloads.

## Verification

- `dune runtest src/cmarkit/test`
- `dune runtest`
- `dune build`

## Next step

Continue exercising extra inline container interactions with other inline
features.

Expected shape:

- `Inline.Extra_inline_container.Config` now enables each kind independently.
- Each kind is `Disabled`, `Curly_required`, or `Curly_optional`.
- `Config.explicit` enables all kinds with compulsory curly braces.
- Overlap resolution follows the fixed rule that the first opener to be closed
  wins; intervening potential openers become literal text, while nested
  containers remain valid.
