# Inline extension plan

## Goals

- Keep the default parser behavior standard CommonMark.
- Keep original cmarkit code paths readable and mostly intact.
- Route oymarkit behavior through explicit parser knobs.
- Centralize inline-extension policy so `inline_struct.ml` only makes light-weight calls into oymarkit code.
- Use cmarkit's extensible `Inline.t` mechanism for typed oymarkit-owned AST extensions.

## AST extension shape

Use cmarkit's inline extension mechanism, but keep the extension constructors owned and typed by oymarkit rather than opaque caller-provided payloads.

The likely shape is one shared inline-container extension for syntax with similar behavior:

```ocaml
type inline_container_kind =
  | Highlight
  | Superscript
  | Subscript
  | Inserted
  | Deleted

type inline_container =
  { kind : inline_container_kind;
    inline : Inline.t }

type Inline.t +=
| Ext_inline_container of inline_container node
```

The exact fields can expand if layout preservation or delimiter provenance becomes important for rendering or roundtrip tests.

Avoid a fully generic parser-extension registry for now. The parser still needs to understand precedence, nesting, delimiter conflicts, and fallback behavior, so a registry would be a larger design problem than the current need.

## Centralized inline policy

Add a dedicated oymarkit inline policy layer, most likely near the existing `Parser_common_.Oymarkit_mod` parser configuration.

This policy should own:

- allowed emphasis delimiter characters;
- allowed strong-emphasis delimiter characters;
- whether intraword emphasis is allowed;
- whether `{` and `}` can mark delimiters as opener-only or closer-only;
- strong-emphasis delimiter width;
- delimiter specs for future inline containers such as highlight, superscript, subscript, inserted, and deleted text.

`inline_struct.ml` should call this policy at tokenization and delimiter matching boundaries. It should not accumulate extension-specific branching inline when that branching can live in the centralized policy.

## Implementation order

1. Add emphasis ambiguity-reduction knobs.

   First add the knob for forbidding intraword emphasis. Preserve CommonMark behavior by default.

2. Add `{` / `}` delimiter disambiguation for emphasis.

   Add a knob that lets `{_` or `{*` act as opener-only emphasis delimiters, and `_}` or `*}` act as closer-only delimiters. This should be represented during tokenization as delimiter role information, so the later emphasis pass does not need to know about literal braces except through the token span and delimiter metadata.

3. Add one-character strong-emphasis configuration.

   CommonMark strong emphasis is always two delimiters. Add a knob that allows oymarkit modes to treat a configured one-character delimiter as strong emphasis, while keeping the default at two characters.

   This likely requires the emphasis matcher to return a semantic match decision, not only the number of consumed delimiter characters.

4. Add typed inline-container AST extensions.

   Add the shared oymarkit inline-container AST shape and update the core support code that must know about new inline cases: normalization, mapping/folding, sexp/debug output, and renderers as needed.

5. Add inline-container parser support.

   Add parser support behind knobs for:

   - highlight: `{=highlighted text=}`;
   - superscript: `djot^TM^`, with optional curly-brace form such as `{^one two^}`;
   - subscript: `H~2~O`, with optional curly-brace form such as `{~one two~}`;
   - inserted text: `{+inserted+}`;
   - deleted text: `{-deleted-}`.

   For inserted and deleted text, make the curly-brace requirement configurable. Djot requires the braces; oymarkit can expose a knob while keeping defaults explicit.

6. Future TODO: inline-container precedence policy.

   The Djot-style property:

   > The first opener that gets closed takes precedence.

   should be handled last, after the delimiter and AST substrate exists. This is related to, but not identical to, CommonMark's existing emphasis precedence rules. Treat it as a future policy knob rather than assuming the current CommonMark path already implements it exactly.

## Testing strategy

- Add focused parser-config tests for each knob.
- Include positive and negative tests for default CommonMark behavior.
- Add tests where the same input parses differently with and without each knob.
- Keep property-test work exploratory: counterexamples may indicate generator typing rules, parser policy decisions, or canonical comparison rules rather than parser bugs.
