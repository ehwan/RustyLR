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

* `rustylr.server.command`: Path to the `rustylr` executable. Leave empty to automatically detect or run from Cargo.
* `rustylr.server.args`: Arguments passed to the language server command. When pointing directly at `rustylr`, use `["lsp"]`.
* `rustylr.server.cwd`: Working directory for the language server.
* `rustylr.semanticTokens.enabled`: Toggle semantic token syntax highlighting.

## Installation & Requirements

This extension requires the **RustyLR Language Server**, which is built into the `rustylr` executable and started with `rustylr lsp`.

### 1. Install the Language Server (Recommended)

You can install the RustyLR executable globally using Cargo:

```bash
cargo install rustylr
```

Ensure that your cargo binary directory (usually `~/.cargo/bin`) is in your system's `PATH`. The extension will automatically detect it.

### 2. For RustyLR Workspace Contributors

If you are developing or contributing to the RustyLR repository:
- The extension will automatically detect built binaries in the `target/debug` or `target/release` folders of your repository root.
- If no prebuilt binary is found, it falls back to running the server dynamically using `cargo run --package rustylr -- lsp`.
