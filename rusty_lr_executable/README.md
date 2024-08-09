# rustylr
Converts a context-free grammar into a deterministic finite automaton (DFA) tables, and generates a Rust code that can be used as a parser for that grammar.

```
cargo install rustylr
```

## Usage
```
$ rustylr --help
Usage: rustylr [OPTIONS] <INPUT_FILE> [OUTPUT_FILE]

Arguments:
  <INPUT_FILE>
          input_file to read

  [OUTPUT_FILE]
          output_file to write

          [default: out.tab.rs]

Options:
      --no-format
          do not rustfmt the output

  -r, --runtime
          the generated code will `build()` at runtime

  -l, --lalr
          build LALR(1) parser

  -v, --verbose
          print debug information.

          Print the whole rule set (include auto-generated rules), and the shift/reduce resolving process.
```
This program searches for '%%' in the input file.

The contents before '%%' will be copied into the output file as it is.
Context-free grammar must be followed by '%%'.
Each line must follow the syntax of [rusty_lr#syntax](https://github.com/ehwan/RustyLR?tab=readme-ov-file#proc-macro-syntax).

```rust
// my_grammar.rs
use some_crate::some_module::SomeStruct;

enum SomeEnum {
    A,
    B,
    C,
}

%% // <-- input file splitted here

%tokentype u8;
%start E;
%eof b'\0';

%token a b'a';
%token lparen b'(';
%token rparen b')';

E: lparen E rparen
 | P
 ;

P: a;
```

Calling the command will generate a Rust code `my_parser.rs`.
```
$ rustylr my_grammar.rs my_parser.rs
```

For usage of the generated code, please refer to the documents [rusty_lr#Start Parsing](https://github.com/ehwan/RustyLR?tab=readme-ov-file#start-parsing).