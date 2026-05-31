- reference: <https://github.com/avsm/ocaml-ai-disclosure>

Value 	Meaning 	W3C HTML 	IETF HTTP
none 	No AI involvement; a human-only assertion 	none 	none
ai-assisted 	Human-authored, AI edited or refined 	ai-assisted 	ai-modified
ai-generated 	AI-generated with human prompting and/or review 	ai-generated 	ai-originated
autonomous 	AI-generated without human oversight 	autonomous 	machine-generated


You can find such flags in `@meta{[]}` odoc flag, markdown frontmatter, file path, etc.

It reflects the value and trustworthiness of a piece of code / document. If a piece is "autonomous", it is generally okay to rewrite it entirely, ignore its style, etc. And it's generally required to have a stricter test suite for such pieces.
