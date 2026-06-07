# Same content or different content?

When `parse (render b)` does not give back `b`, we have a choice of where to
encode that knowledge. This note states the principle we use to decide.

## The two levers

A round-trip counterexample can be resolved in one of two places:

1. **Equality (`Cmarkit.Block.normalize`).** Make the comparison coarser so the
   two ASTs are considered equal. The round-trip property compares
   `canonical b` against `canonical (parse (render b))`, where `canonical`
   normalizes first (`src/gen/property.ml`). Extending `normalize` to quotient
   away a representational difference makes the counterexample *not a
   counterexample*.

2. **The generator (a typing rule / `Bconfig` knob).** Stop generating the
   offending shape, because it is genuinely not in the image of the parser.

The two levers are not interchangeable. Picking the wrong one either hides a
real defect (using equality to paper over lost content) or needlessly shrinks
the AST space we can explore (forbidding a shape that was only ever a different
spelling of a legal one).

## The test: is it the *same content* or *different content*?

> Render both the AST and the AST the parser reads back. If they carry the
> **same content** and differ only in how that content is *grouped or spelled*,
> coarsen **equality** (`normalize`). If rendering **loses or changes** content,
> it is a genuine constraint — restrict the **generator**.

This sorts the failures into three buckets.

### Bucket A — same content, different grouping → `normalize`

A *syntax-free container* (one with no surface syntax of its own, only a
grouping role) admits several spellings of the same content. The parser only
ever emits one of them; the others are legal `Block.t` values a caller can
construct but the parser never produces. These belong in `normalize`.

| non-canonical | canonical (parser image) | container |
|---|---|---|
| `Blocks [Blocks [a]; b]` | `Blocks [a; b]` | `Blocks` (nested) |
| `Blocks [a]` | `a` | `Blocks` (singleton) |
| `Blocks [List '-' [x]; List '-' [y]]` | `List '-' [x; y]` | `List` (adjacent, same kind) |

The first two rows `normalize` already handles. The third is the same idea: a
`List` is purely a grouping of `List_item`s — the syntax lives in the item
markers, not the list — so two adjacent lists of the same *kind* (same bullet
character, or same ordered delimiter; the ordered start number is irrelevant)
are one list spelled as two. Blank lines between them do not separate them
either; they only make the merged list loose. The parser always fuses such
lists, so `normalize` should too.

This merge is implemented in `Block.normalize` (`src/cmarkit/block.ml`), gated
behind the `OYMARKIT_MERGE_ADJACENT_LISTS` environment variable.

### Bucket B — content lost or changed → generator

Rendering produces text the parser reads back as *different content*. No
coarsening of equality is honest here, because the two ASTs really do mean
different things.

- **HTML block absorbing its successor.** `Blocks [Html_block "<div>";
  Thematic_break]` renders `<div>\n---`, which parses as a *single* HTML block
  whose body is the literal text `<div>\n---`. The thematic break is gone —
  reborn as raw HTML text. The rendered strings happen to match, but the content
  does not (a thematic break vs. literal text). → typing rule
  `no_html_block_absorbing_successor`.
- **Setext collision**, **two paragraphs fusing**, **marker-colliding thematic
  break**: same family — adjacency changes how the parser classifies a line, so
  content changes. → typing rules.

### Bucket C — contentless atoms → generator

A node that renders to nothing cannot come back: `Paragraph` with empty inline,
item-less `List`, `Blocks []`. There is no content to relocate or regroup, only
an empty node to *not create*. These are generator restrictions
(`no_empty_paragraph`, `no_empty_list`, `no_empty_blocks`), not `normalize`
rules: `normalize` relocates and regroups content, it does not prune nodes.

## Why not just put everything in one lever?

- **Everything in `normalize`** would make equality lie: it would call
  `Blocks [Html_block "<div>"; Thematic_break]` equal to the absorbed HTML
  block, erasing a real distinction (the thematic break) and making the property
  unable to catch a class of genuine bugs.
- **Everything in the generator** would forbid perfectly legal, content-faithful
  ASTs (two adjacent same-kind lists, nested `Blocks`) — shapes a library user
  can build and that round-trip fine *once equality is set correctly*. It also
  forces the generator to reason non-locally about render-order adjacency across
  splice boundaries, which `normalize` already knows how to do.

## Rule of thumb

- Two spellings, one content → **`normalize`** (coarsen equality).
- One spelling loses/garbles content → **generator** (it is not realizable).
- Empty node with no content → **generator** (do not create it).
