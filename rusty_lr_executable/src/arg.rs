use clap::Parser;

/// Converts a context-free grammar into a deterministic finite automaton (DFA) tables, and generates a Rust code that can be used as a parser for that grammar.
///
/// This program searches for '%%' in the input file.
///
/// The contents before '%%' is the Rust code that will be included in the output.
/// Context-free grammar must be followed by '%%'. Each line must follow thw syntax of [`rusty_lr`](https://github.com/ehwan/RustyLR).
///
/// structs and enums will be generated in the output file. The generated code will be formatted by `rustfmt` on default.
///
/// For usage of the generated code, please refer to the documentation of [`rusty_lr`](https://github.com/ehwan/RustyLR).
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

    /// the generated code will `build()` at runtime
    #[arg(short, long, default_value = "false")]
    pub runtime: bool,

    /// build LALR(1) parser
    #[arg(short, long, default_value = "false")]
    pub lalr: bool,

    /// print debug information.
    ///
    /// Print the whole rule set (include auto-generated rules),
    /// and the shift/reduce resolving process.
    #[arg(short, long, default_value = "false")]
    pub verbose: bool,
}
