# RustyLR VSCode Extension

Temporary VSCode extension client for the `rusty_lr_lsp` server in this repository.

## Run From This Repository

1. Build the language server once:

   ```bash
   cargo build -p rusty_lr_lsp
   ```

2. Install the extension client dependencies:

   ```bash
   cd editors/vscode-rustylr
   npm install
   ```

3. Open this extension folder in VSCode:

   ```bash
   code editors/vscode-rustylr
   ```

4. Press `F5` and choose `VS Code Extension Development` if prompted.

5. In the Extension Development Host window, open the RustyLR repository folder and then open a grammar file such as `example/calculator/src/parser.rustylr`, or `src/rustylr.rs` in a downstream project.

The extension starts the already-built server binary when it exists:

```bash
/home/ehwan/workspace/RustyLR/target/debug/rusty_lr_lsp
```

If that binary does not exist yet, it falls back to `cargo run --quiet --package rusty_lr_lsp`.

The extension searches upward for the RustyLR repository root and uses that as the server working directory. You can override the command, arguments, and working directory with VSCode settings:

```json
{
  "rustylr.server.command": "/home/ehwan/workspace/RustyLR/target/debug/rusty_lr_lsp",
  "rustylr.server.args": [],
  "rustylr.server.cwd": "/home/ehwan/workspace/RustyLR"
}
```

## File Matching

The extension contributes a `rustylr` language mode for:

- `*.rustylr`
- `rustylr.rs`

It also sends those file patterns to the LSP server even when the VSCode language mode is not manually changed.

## Features

The extension is intentionally thin: VSCode starts `rusty_lr_lsp` over stdio and the server provides the language features.

See [`rusty_lr_lsp/README.md`](../../rusty_lr_lsp/README.md) for the current diagnostics, go-to-definition, and completion feature details.
