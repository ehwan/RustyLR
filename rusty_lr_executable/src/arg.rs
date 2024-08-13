use clap::Parser;

/// Converts a context-free grammar into a deterministic finite automaton (DFA) tables,
/// and generates a Rust code that can be used as a parser for that grammar.
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

    /// build LALR(1) parser
    #[arg(short, long, default_value = "false")]
    pub lalr: bool,

    /// print debug information.
    ///
    /// print the auto-generated rules, and where they are originated from.
    /// print the shift/reduce conflicts, and the resolving process.
    #[arg(short, long, default_value = "false")]
    pub verbose: bool,
}
