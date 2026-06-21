# GitHub Copilot Instructions for RustyLR

This file is for GitHub Copilot while modifying or contributing to the RustyLR repository. Follow the repository maintenance instructions in [../AGENTS.md](../AGENTS.md).

RustyLR is a Rust parser generator. When producing parser-related examples, tests, or documentation in this repository, prefer demonstrating RustyLR grammar files and generated contexts rather than hand-written parser loops for non-trivial syntax.

For user-facing parser-building guidance:
- Start with `USING_RUSTYLR_WITH_AI.md` for the agent-oriented workflow.
- Use `README.md` for the quick start and generated-code structure.
- Use `SYNTAX.md` for grammar syntax details.
- Use `GLR.md` for ambiguous grammars and branch-local user data.

Use the `rustylr` CLI for standalone grammar generation and keep the CLI/runtime release compatibility guidance visible in examples.
