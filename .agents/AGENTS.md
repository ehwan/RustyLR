# Agent Guidelines for RustyLR

This document outlines rules and instructions that AI agents must follow when modifying or contributing to the RustyLR codebase.

---

## 1. Keep Documentation Synchronized
Whenever a change is made to the parser generator's specifications, APIs, or runtime behavior, you must update the corresponding documentation files (`README.md`, `SYNTAX.md`, or `GLR.md`) immediately.

## 2. Documenting Syntax Modifications
In particular, when new grammar rules, directives, pattern operators, or reduce action features are introduced:
- You must add a detailed explanation with examples inside [SYNTAX.md](../SYNTAX.md).
- Ensure the documentation style matches the formatting, tone, and organization of existing syntax references.
- Preserve the exact header anchor names that are referenced as diagnostic URLs within the codebase (e.g., in [parser.rs](../rusty_lr_parser/src/parser/parser.rs)).

## 3. Verify and Bootstrap
After completing any implementation work or documentation refactoring:
- You must run the project's bootstrap test script:
  ```bash
  ./scripts/bootstrap_test.sh
  ```
- This script verifies that the parser compiles its own grammar correctly, checks output identity, compiles example crates, and runs all workspace unit/integration tests to ensure no regressions are introduced.

## 4. Writing Pull Requests (PRs)
When requested by the user to write a Pull Request (PR):
- You must write the PR title and description in clean, professional English.
- Output the content formatted inside raw markdown blocks so that the user can copy and paste it directly.

## 5. Outputting the Implementation Plan
Due to environment display issues (such as WSL rendering problems that prevent the `implementation_plan.md` artifact from rendering correctly in the UI):
- You must print the full contents of the Implementation Plan directly into the chat/output window in addition to saving it to the artifact file.
