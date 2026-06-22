# RustyLR LSP

`rusty_lr_lsp` is an experimental language server for RustyLR grammar files. It communicates over stdio and is intended to be used by editor clients such as the temporary VSCode extension in `editors/vscode-rustylr`.

## Supported Files

The current VSCode client targets:

- `*.rustylr`
- `rustylr.rs`

Other Rust files are intentionally not matched by default.

## Features

- **Diagnostics:** Parses open RustyLR grammar files and publishes grammar errors, recovered parser errors, warnings, and conflict diagnostics.
- **Go to Definition:** Resolves terminal and non-terminal references to their `%token` declarations or production definitions.
- **Completion for symbols:** Suggests declared terminal names and non-terminal names in grammar positions. Completion details include the resolved Rust type for terminals and non-terminals, including inferred placeholders and a note when the value is boxed for parser storage.
- **Completion for directives and keywords:** Suggests directives such as `%token`, `%start`, `%tokentype`, `%left`, `%right`, `%precedence`, `%prec`, `%dprec`, `%glr`, `%lalr`, `%nooptim`, `%allow`, and common identifiers such as `error`, `$sep`, `data`, `lookahead`, and `shift`.
- **Completion for `$...` variables:** Suggests built-in substitutions (`$tokentype`, `$location`, `$userdata`, `$error`, `$errortype`), terminal and non-terminal substitutions (`$terminal_name`, `$NonTerminalName`), current reduce-action bindings (`$left`, `$value`, etc.), and positional semantic variables (`$1`, `$2`, ...).
- **Completion for locations:** Suggests `@$`, `@0`, positional locations (`@1`, `@2`, ...), and named binding locations (`@left`, `@value`, etc.).
- **Completion for `%allow`:** Suggests valid diagnostic names such as `nonterm_unreachable`, `unused_terminals`, and conflict-resolution diagnostic identifiers.

## Running the Server

Build the server from the workspace root:

```bash
cargo build -p rusty_lr_lsp
```

The debug binary is then available at:

```bash
target/debug/rusty_lr_lsp
```

The server expects to be launched by an LSP client over stdio. For quick VSCode testing, use the extension client in `editors/vscode-rustylr`.

## VSCode Test Client

From the repository root:

```bash
cargo build -p rusty_lr_lsp
cd editors/vscode-rustylr
npm install
code .
```

Press `F5` in VSCode to open an Extension Development Host, then open the RustyLR repository or another workspace containing `*.rustylr` or `rustylr.rs` grammar files.

The extension auto-detects `target/debug/rusty_lr_lsp` when it exists. You can override the server command with VSCode settings:

```json
{
  "rustylr.server.command": "/home/ehwan/workspace/RustyLR/target/debug/rusty_lr_lsp",
  "rustylr.server.args": [],
  "rustylr.server.cwd": "/home/ehwan/workspace/RustyLR"
}
```

Use `RustyLR: Restart Language Server` from the command palette after changing server settings.
