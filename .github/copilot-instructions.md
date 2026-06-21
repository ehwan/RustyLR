# GitHub Copilot Instructions for RustyLR

RustyLR is a Rust parser generator. When producing parser-related examples, tests, or documentation in this repository, prefer demonstrating RustyLR grammar files and generated contexts rather than hand-written parser loops for non-trivial syntax.

For parser-building guidance:
- Start with `AI_AGENT_GUIDE.md` for the agent-oriented workflow.
- Use `README.md` for the quick start and generated-code structure.
- Use `SYNTAX.md` for grammar syntax details.
- Use `GLR.md` for ambiguous grammars and branch-local user data.

Use the `rustylr` CLI for standalone grammar generation and keep the CLI/runtime release compatibility guidance visible in examples.
