# Overview - 260530
This project is a fork of cmarkit. We call it oymarkit.

The ultimate long-term goal is to support various pluggable extensions in addition to regular CommonMark grammar. For example, pandoc extensions, djot extensions, pymarkdown extensions, Obsidian extensions, and some custom ones. We want to make sure they interact well with each other, no ambiguity. Another goal is to strengthen CommonMark with knobs so that we can reduce its ambiguity and make it more predictable. One example is we can add a flag to forbid intra-word emphasis and request the character for emphasis and strong emphasis to be different. This will make the parsing easier and take less "pass".

Current work focus (as of 2026-05-30) is adding QCheck generators for AST and use it to explore the AST space and test against some desirable properties (render roundtrip, uniformity of container, etc.). We are encoding those AST "typing" rules inside generator and we want to encode our knowledge in a structured way. One example is you can construct an AST with list items containing ATX headings, but this is actually impossible in markdown level. Parser cannot generate such AST. We want to use PBT to explore such rules and find counterexamples. So the workflow will be like:
1. starting with current generator config
2. test against properties
3. if counterexample found, encode it as a new generator config (typing rule). and repeat

Counterexamples are knowledge and we might go back and forth on those typing rules, so it's important to test the behavior with and without those rules enabled.

There are two-level of PBT tests. One is test against the properties using acknowledged rules and try to dig new ones. The other is test the typing rules itself to make sure we have good generator (test the generator itself).

# Note - 260531
- When there's an error it's also possible that there's a bug in parser and commonmark renderer?
- To make sure that our typing rule does not introduce unrealistic restrictive behaviors, we should make sure that it does not forbid any valid input, i.e., we need PBT that generates raw string and parse it to AST, and our rule should not reject it. If it's rejected, either there's a bug in our rule or the parser is buggy.

# Note - 260531
- We are now starting to hack on the parser itself, adding new features. But we'd like to keep the original code as intact as possible. So we are using a global module-level mutatble flag to gate our features. Also we tend to centralize our code in a clear and distinct place (a dedicated module)
