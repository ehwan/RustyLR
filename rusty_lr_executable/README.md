# rustylr
Converts a context-free grammar into a state automaton tables,
and generates a Rust code that can be used as a parser for that grammar.

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

  -v, --verbose
          turns on all verbose options

  -c, --verbose-conflict
          verbose output for any shift/reduce or reduce/reduce conflicts.

          This option is for GLR parser. Since such conflicts are not treated as errors, this option is useful for debugging.

  -r, --verbose-conflict-resolve
          verbose output for the conflict resolution process, by '%left' or '%right' directive

  -r, --verbose-optimization
          verbose output for the terminal class optimization process

  -h, --help
          Print help (see a summary with '-h')

  -V, --version
          Print version
```
This program searches for '%%' in the input file.

The contents before '%%' will be copied into the output file as it is.
Context-free grammar must be followed by '%%'.
Each line must follow the [SYNTAX](../SYNTAX.md).

```rust
// my_grammar.rs
use some_crate::some_module::SomeStruct;

enum SomeTypeDef {
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

For usage of the generated code, please refer to the documents [Quick Start](../README.md#quick-start).