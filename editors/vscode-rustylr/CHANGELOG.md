# Changelog

All notable changes to the "RustyLR" extension will be documented in this file.

## 0.1.0

- First public release of RustyLR language support!
- Published as a preview extension while the language server continues to evolve.
- Fully integrated with the `rustylr lsp` language server:
  - **Syntax Highlighting (Semantic Tokens):** Distinct syntax coloring for terminals, non-terminals, directives, bindings, location bindings, and variables.
  - **Diagnostics:** Inline warning and error reporting directly in the editor.
  - **Code Actions:** Quick-fix actions to suppress warnings with `%allow` directives.
  - **Formatting:** Code formatting and indentation support for rule definitions and reduce actions.
  - **Go to Definition:** Jump directly to token declarations, production definitions, and precedence definitions.
  - **Find References:** Find all usages of terminals, non-terminals, and precedence symbols across the grammar document.
  - **Hover Tooltips:** Interactive documentation tooltips for keywords, patterns, and variables.
  - **Inlay Hints:** Inline type hints for grammar patterns and reduce actions.
  - **Auto-Completion:** Intelligent suggestions for symbols, directives, variables, and locations.
