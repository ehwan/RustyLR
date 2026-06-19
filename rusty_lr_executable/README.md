# rustylr
Executable for rusty_lr, a bison-like parser generator & compiler frontend for Rust supporting IELR(1), LALR(1) parser tables, with deterministic LR and non-deterministic LR (GLR) parsing.


## Installation
```bash
cargo install rustylr
```

## Usage
```bash
$ rustylr my_grammar.rs my_parser.rs
```

The first argument is the grammar file. The optional second argument is the generated Rust output file; when omitted, RustyLR writes `out.tab.rs`.

## Advanced CLI Options

The options below are mainly useful for formatting control, parser-mode overrides, and diagnostics.

```bash
Usage: rustylr [OPTIONS] <INPUT_FILE> [OUTPUT_FILE]

Arguments:
  <INPUT_FILE>
          Input_file to read

  [OUTPUT_FILE]
          Output_file to write

          [default: out.tab.rs]

Options:
      --no-format
          Do not rustfmt the output

  -c, --no-conflict
          Do not print note information about any shift/reduce, reduce/reduce conflicts.

          If the target is deterministic parser, conflict will be treated as an error, so this option will be ignored. This option is only for non-deterministic GLR parser.

  -r, --no-conflict-resolve
          Do not print debug information about conflicts resolving process by any `%left`, `%right`, or `%precedence` directive

  -o, --no-optimization
          Do not print debug information about optimization process

  -b, --no-backtrace
          Do not print backtrace of production rules when conflicts occurred. ruleset could be messed up

      --glr <GLR>
          Override the written code and set generated parser use GLR parsing algorithm

          [possible values: true, false]

      --dense <DENSE>
          Override the written code and set generated parser table to use dense arrays

          [possible values: true, false]

      --state <STATE>
          Print the details of a specific state

  -h, --help
          Print help (see a summary with '-h')

  -V, --version
          Print version
```

### Diagnostic and Debug Options

- `--state <STATE>` prints details for a specific generated state.
- `--no-conflict`, `--no-conflict-resolve`, and `--no-backtrace` reduce conflict diagnostic output.
- `--glr <true|false>` and `--dense <true|false>` override parser-mode settings from the grammar file.

## Grammar File Format
The program searches for `%%` in the input file to separate Rust code from grammar definitions.

- **Before `%%`**: Regular Rust code (imports, type definitions, etc.) that will be copied to the output file as-is
- **After `%%`**: Context-free grammar definition that must follow the [RustyLR syntax](../SYNTAX.md)

## Example

Here's a simple example showing how to create a grammar file and generate a parser:

**Input file** (`my_grammar.rs`):
```rust
// Rust imports and type definitions
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum Token {
    Identifier(String),
    Number(i32),
    Punct(char),
}

%% // Grammar definition starts here

%tokentype Token;
%start E;

%token id Token::Identifier(_);
%token num Token::Number(_);
%token lparen Token::Punct('(');
%token rparen Token::Punct(')');

E: lparen E rparen { E }
 | id { 
     if let Token::Identifier(name) = id {
         println!("Found identifier: {}", name);
     }
 }
 | num {
     if let Token::Number(value) = num {
         println!("Found number: {}", value);
     }
 }
 ;
```

**Generate the parser:**
```bash
$ rustylr my_grammar.rs my_parser.rs
```

This will create `my_parser.rs` containing the generated parser code.

**Using the generated parser:**
Initialize the state context with initial user data (or `with_default_userdata()` when the user data type implements `Default`), and feed your tokens to it:
```rust
include!("my_parser.rs");

fn main() {
    let mut context = EContext::with_default_userdata();

    // Parse some tokens
    let tokens = vec![
        Token::Punct('('),
        Token::Identifier("hello".to_string()),
        Token::Punct(')'),
    ];

    for token in tokens {
        match context.feed(token) {
            Ok(_) => println!("Token accepted"),
            Err(e) => println!("Parse error: {}", e),
        }
    }

    // Get the final result
    if let Ok((result, _userdata)) = context.accept() {
        println!("Parse successful: {:?}", result);
    }
}
```

## The `--state` Option
You can inspect the details of a specific parser state using the `--state` option. This is useful for debugging and understanding how the parser processes input.

```bash
$ rustylr my_grammar.rs --state 5 // get details of state 5
```
```
State 5:
Production Rules: {
    Pattern -> TerminalSet •
}
Reduce on Terminals: {
    ident => { Pattern -> TerminalSet }
    semicolon => { Pattern -> TerminalSet }
    pipe => { Pattern -> TerminalSet }
    percent => { Pattern -> TerminalSet }
    plus => { Pattern -> TerminalSet }
    star => { Pattern -> TerminalSet }
    question => { Pattern -> TerminalSet }
    minus => { Pattern -> TerminalSet }
    exclamation => { Pattern -> TerminalSet }
     dot => { Pattern -> TerminalSet }
    dollar => { Pattern -> TerminalSet }
    comma => { Pattern -> TerminalSet }
    literal => { Pattern -> TerminalSet }
    bracegroup => { Pattern -> TerminalSet }
    lparen => { Pattern -> TerminalSet }
    rparen => { Pattern -> TerminalSet }
    lbracket => { Pattern -> TerminalSet }
    error => { Pattern -> TerminalSet }
}
From States: {
    State 4
    State 6
    State 11
    State 13
    State 35
    State 38
    State 40
    State 44
    State 48
    State 66
    State 70
}
```

For more usage examples and detailed documentation, see the [main README](../README.md).
