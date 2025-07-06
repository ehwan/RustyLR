# rustylr
A command-line tool that converts context-free grammars into state automaton tables
and generates Rust code that can be used as a parser for that grammar.

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
          input_file to read

  [OUTPUT_FILE]
          Output file for generated Rust code
          [default: out.tab.rs]

Options:
      --no-format
          do not rustfmt the output

  -v, --verbose
          Enable all verbose options

  -c, --verbose-conflict
          Show verbose output for any shift/reduce or reduce/reduce conflicts.
          This option is useful for GLR parsers where conflicts are not treated as errors.

  -r, --verbose-conflict-resolve
          Show verbose output for the conflict resolution process using '%left' or '%right' directives

      --verbose-optimization
          Show verbose output for the terminal class optimization process

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
    EOF,
}

%% // Grammar definition starts here

%tokentype Token;
%start E;
%eof Token::EOF;

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
        Token::EOF,
    ];
    
    for token in tokens {
        match context.feed(&parser, token, &mut ()) {
            Ok(_) => println!("Token accepted"),
            Err(e) => println!("Parse error: {}", e),
        }
    }
    
    // Get the final result
    if let Some(result) = context.accept() {
        println!("Parse successful: {:?}", result);
    }
}
```

For more usage examples and detailed documentation, see the [main README](../README.md).