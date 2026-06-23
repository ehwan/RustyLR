# RustyLR Repository Agent Instructions

This file is for AI agents modifying or contributing to the RustyLR repository.

If you are working in another project and deciding whether to use RustyLR for a user's parser, read [USING_RUSTYLR_WITH_AI.md](USING_RUSTYLR_WITH_AI.md) instead.

## 1. Keep Documentation Synchronized

Whenever a change is made to the parser generator's specifications, APIs, or runtime behavior, update the corresponding documentation files immediately:
- `README.md`
- `SYNTAX.md`
- `GLR.md`
- `USING_RUSTYLR_WITH_AI.md`
- `llms.txt`

## 2. Document Syntax Changes

When new grammar productions, directives, pattern operators, or reduce action features are introduced:
- Add a detailed explanation with examples inside [SYNTAX.md](SYNTAX.md).
- Match the formatting, tone, and organization of the existing syntax reference.
- Preserve exact header anchor names referenced as diagnostic URLs in code, especially from [rusty_lr_parser/src/parser/parser.rustylr](rusty_lr_parser/src/parser/parser.rustylr).

## 3. Verify and Bootstrap

After implementation work or documentation refactoring, run:

```bash
./scripts/bootstrap_test.sh
```

This verifies that the parser compiles its own grammar correctly, checks generated output identity, compiles example crates, and runs workspace tests.

## 4. Comments and Tests

When modifying code:
- Add comments only where they explain purpose, design rationale, or non-obvious behavior.
- Add unit or integration tests for new functionality.

## 5. External Link Validity

When adding or modifying Markdown files, ensure external links referenced from `rusty_lr_parser/src/parser/parser.rustylr` and `rusty_lr_buildscript/src/lib.rs` remain valid and reachable.

## 6. Pull Requests

When asked to write a pull request:
- Write the PR title and description in clean, professional English.
- Output the content inside raw Markdown blocks so it can be pasted directly.

## 7. Implementation Plans

If an implementation plan artifact is created, also print the full plan directly in chat because some WSL displays may not render the artifact correctly.

## 8. Terminology Alignment

When modifying code, comments, or documentation, use formal terminology based on Programming Language Theory, Theory of Computation, and Type Theory for internal logic. Prefer terms such as `Symbol` and `Production` internally.

For user-facing Bison-inspired syntax, keep familiar Bison terminology such as `%token` and `%tokentype`.

## 9. Keep LSP Synchronized with Grammar Changes

Whenever changes are made to the grammar syntax, directives, patterns, or variables:
- Update the LSP implementation in `rusty_lr_lsp` to fully support and recognize the updated grammar.
- Ensure that semantic tokens, hover information, completions, inlay hints, and diagnostic handling are kept aligned with the new grammar specifications.
