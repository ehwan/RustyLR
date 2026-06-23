# RustyLR LSP

RustyLR LSP provides rich language support for [RustyLR](https://github.com/ehwan/RustyLR) grammar files (`*.rustylr` and `rustylr.rs`). Language features are powered by the `rustylr lsp` language server.

Install it from the [Visual Studio Marketplace](https://marketplace.visualstudio.com/items?itemName=ehwan.rustylr-lsp), or run this command from VS Code Quick Open:

```text
ext install ehwan.rustylr-lsp
```

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
* `rustylr.server.args`: Arguments passed to the language server command. Leave empty for automatic detection; when pointing directly at `rustylr`, use `["lsp"]`.
* `rustylr.server.cwd`: Working directory for the language server.
* `rustylr.server.documentPatterns`: Additional file globs handled by the RustyLR language server.
* `rustylr.semanticTokens.enabled`: Toggle semantic token syntax highlighting.

## Installation & Requirements

This extension requires the **RustyLR Language Server**, which is built into the `rustylr` executable and started with `rustylr lsp`.

### 1. Install the VSCode Extension

Install [RustyLR LSP](https://marketplace.visualstudio.com/items?itemName=ehwan.rustylr-lsp) from the Marketplace, or use VS Code Quick Open:

```text
ext install ehwan.rustylr-lsp
```

### 2. Install the Language Server

You can install the RustyLR executable globally using Cargo:

```bash
cargo install rustylr
```

Ensure that your cargo binary directory (usually `~/.cargo/bin`) is in your system's `PATH`. The extension will automatically detect it.

The extension checks the `rustylr` executable version before starting the language server. If the installed version is not compatible with this extension release, install the expected version:

```bash
cargo install rustylr --version 1.34.0 --force
```

If you use a custom path instead of automatic detection:

```json
{
  "rustylr.server.command": "/path/to/rustylr",
  "rustylr.server.args": ["lsp"]
}
```

### 3. Open a RustyLR Grammar File

Open any `*.rustylr` file or a file named `rustylr.rs`. The extension starts `rustylr lsp` automatically and enables diagnostics, completion, hover, formatting, go to definition, find references, inlay hints, semantic tokens, and quick fixes.

### 4. For RustyLR Workspace Contributors

If you are developing or contributing to the RustyLR repository:
- The extension will automatically detect built binaries in the `target/debug` or `target/release` folders of your repository root.
- If no prebuilt binary is found, it falls back to running the server dynamically using `cargo run --package rustylr -- lsp`.

## Marketplace Status

This extension is published as [RustyLR LSP](https://marketplace.visualstudio.com/items?itemName=ehwan.rustylr-lsp) and remains in preview while the RustyLR language server continues to evolve.
