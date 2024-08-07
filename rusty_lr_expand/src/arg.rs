use clap::Parser;

//The yacc command converts a context-free grammar into a set of tables for a simple automaton that executes an LALR(1) parsing algorithm. The grammar can be ambiguous. Specified precedence rules are used to break ambiguities.

// The output file, y.tab.c, must be compiled by the C compiler to produce a function yyparse(). This program must be loaded with the lexical analyzer program, yylex(), as well as main() and yyerror(), an error handling routine. These routines must be supplied by the user. The lex(1) command is useful for creating lexical analyzers usable by yacc.

/// Parse the macro lines from the input file.
/// Converts a context-free grammar into a deterministic finite automaton (DFA) tables, and generates a Rust code that can be used to parse the input.
///
/// Struct <StartSymbol> will be generated in the output
#[derive(Parser, Debug)]
#[command(version)]
#[command(about)]
pub struct Args {
    /// input_file to read
    pub input_file: String,

    /// output_file to write
    #[arg(default_value = "out.tab.rs")]
    pub output_file: String,

    /// do not rustfmt the output
    #[arg(long, default_value = "false")]
    pub no_format: bool,

    /// call `grammar.build()` at runtime
    #[arg(short, long, default_value = "false")]
    pub runtime: bool,

    /// build LALR(1) parser
    #[arg(short, long, default_value = "false")]
    pub lalr: bool,
}
