# RustyLR Language Support

This extension provides rich language support for the [RustyLR](https://github.com/ehwan/RustyLR) parser generator grammar files (`*.rustylr` and `rustylr.rs`).

## Features

- **Diagnostics & Error Reporting:** Real-time diagnostics for grammar syntax errors, unused symbols, conflict resolutions, and more.
- **Go to Definition:** Quickly navigate to rule definitions, terminal declarations, and precedence rules.
- **Find References:** Find all occurrences and usages of terminals, non-terminals, and precedence symbols.
- **Syntax Highlighting (Semantic Tokens):** Distinct, theme-aligned colors for terminal names, non-terminal rules, directives, bindings, location bindings (`@loc`), and variables (`$var`).
- **Formatting:** Automatic document formatter that standardizes directives, separates rules, and indents rule lines and reduce-action bodies.
- **Code Actions (Quick Fixes):** Fast diagnostic suppression actions using the `%allow` directive.
- **Hover tooltips:** Documented explanations and types for terminal tokens, non-terminal rules, keywords, and patterns.
- **Inlay Hints:** Inline type annotations and reduce action indicators.
- **Auto-Completion:** Intelligent suggestions for directives, symbols, locations, variables, and diagnostics.

## Extension Settings

This extension contributes the following settings to control the language server behavior:

* `rustylr.server.command`: Path to the `rusty_lr_lsp` server binary. Leave empty to automatically detect or run from Cargo.
* `rustylr.server.args`: Arguments passed to the language server command.
* `rustylr.server.cwd`: Working directory for the language server.
* `rustylr.semanticTokens.enabled`: Toggle semantic token syntax highlighting.

## Requirements

The language features require the `rusty_lr_lsp` server, which is part of the RustyLR cargo workspace. You can build it from the repository root:

```bash
cargo build -p rusty_lr_lsp
```

By default, the extension will attempt to auto-detect the built binary in your workspace target folder or run it dynamically using Cargo.
