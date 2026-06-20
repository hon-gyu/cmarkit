---
ai-disclosure: autonomous
---
# Why an Alternative Parser

This project is not only trying to implement CommonMark plus extensions. A more
specific research question is:

> Is there a CommonMark-like grammar variant whose parsed AST has nicer typing
> properties?

Here, "typing" means that we can describe the AST shapes emitted by the parser
with reasonably local structural rules, and then generate those ASTs directly for
property-based tests. The important properties include render roundtrip,
container uniformity, and predictable interactions between extensions.

## Why Standard CommonMark Is Not Enough

Standard CommonMark is designed around source compatibility and a precise parser
algorithm. That does not always line up with an AST grammar that is easy to
describe locally.

Emphasis is the clearest example. Whether a delimiter run opens or closes
depends on flanking conditions, punctuation, word boundaries, run length, and
interactions with nearby delimiter runs. A generated AST like adjacent emphasis
nodes may render to Markdown whose delimiter runs are then resolved differently
by the CommonMark parser.

For example, two adjacent underscore emphasis nodes can render as:

```markdown
_jia__jia_
```

The middle `__` is inside a word boundary, so the parser does not necessarily
read this as "close the first emphasis and open the second emphasis". It may
instead parse the text as a different emphasis shape. This is not obviously a
parser bug; it is a sign that CommonMark emphasis is hard to characterize as a
simple AST typing rule.

If we keep adding generator-side typing rules to model this exactly, we risk
reimplementing the CommonMark emphasis algorithm in the generator. That is not a
good boundary.

## Parser Knobs as Grammar Variants

The alternative parser exists so we can define explicit grammar variants with
better properties. These variants should remain close to CommonMark, but they do
not have to preserve every ambiguous convenience if doing so prevents a clean AST
model.

One candidate knob is to forbid intraword emphasis consistently. Combined with
existing delimiter knobs, a typed grammar may require:

- emphasis and strong emphasis use distinct delimiter characters;
- intraword emphasis using plain CommonMark delimiters is disabled;
- ambiguous delimiter-run interactions are avoided by construction.

This does not mean dropping the feature permanently. Intraword emphasis can be
reintroduced with explicit syntax that has clear left/right delimiter semantics,
such as Djot-style `{*` and `*}`. That gives users the expressiveness while
giving the parser and generator an unambiguous grammar.

## The Intended Split

The project should support at least two modes:

- CommonMark-compatible mode: preserve standard behavior and accept that some AST
  reachability properties are algorithmic rather than locally typable.
- Typed oymarkit mode: use parser knobs and extension syntax to define a
  CommonMark-like grammar whose emitted ASTs can be described by clearer typing
  rules.

Property-based testing is then used for both sides:

- AST generators test whether the typed grammar has the desired roundtrip and
  uniformity properties.
- Raw Markdown generators test whether the typing rules reject any AST that the
  selected parser mode can actually emit.

The goal of an alternative parser is therefore not gratuitous divergence from
CommonMark. It is a way to ask, experimentally and precisely, which small grammar
changes make Markdown ASTs more predictable.
