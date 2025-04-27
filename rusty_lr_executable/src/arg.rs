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

    /// turns on all verbose options
    #[arg(short, long, default_value = "true")]
    pub verbose: bool,

    /// verbose output for any shift/reduce or reduce/reduce conflicts.
    ///
    /// This option is for GLR parser.
    /// Since such conflicts are not treated as errors, this option is useful for debugging.
    #[arg(short = 'c', long, default_value = "false")]
    pub verbose_conflict: bool,

    /// verbose output for the conflict resolution process, by '%left' or '%right' directive.
    #[arg(short = 'r', long, default_value = "false")]
    pub verbose_conflict_resolve: bool,

    /// verbose output for the terminal class optimization process
    #[arg(short = 'r', long, default_value = "false")]
    pub verbose_optimization: bool,
}
