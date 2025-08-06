# rustylr
Executable for rusty_lr, a bison-like parser generator & compiler frontend for Rust supporting IELR(1), LALR(1) parser tables, with deterministic LR and non-deterministic LR (GLR) parsing.


## Installation
```bash
cargo install rustylr
```

## Usage
```bash
$ rustylr --help
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

      --runtime <RUNTIME>
          Override the written code and set parser table to be runtime-calculated

          [possible values: true, false]

      --dense <DENSE>
          Override the written code and set generated parser table to use dense arrays

          [possible values: true, false]

  -h, --help
          Print help (see a summary with '-h')

  -V, --version
          Print version
```

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
```rust
include!("my_parser.rs");

fn main() {
    let parser = EParser::new();
    let mut context = EContext::new();
    
    // Parse some tokens
    let tokens = vec![
        Token::Punct('('),
        Token::Identifier("hello".to_string()),
        Token::Punct(')'),
    ];
    
    for token in tokens {
        match context.feed(&parser, token, &mut ()) {
            Ok(_) => println!("Token accepted"),
            Err(e) => println!("Parse error: {}", e),
        }
    }
    
    // Get the final result
    if let Ok(result) = context.accept( &parser, &mut () ) {
        println!("Parse successful: {:?}", result);
    }
}
```

For more usage examples and detailed documentation, see the [main README](../README.md).