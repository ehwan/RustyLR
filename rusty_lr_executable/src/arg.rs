use clap::Parser;

/// Converts a context-free grammar into a deterministic finite automaton (DFA) tables,
/// and generates a Rust code that can be used as a parser for that grammar.
///
/// For usage of the generated code, please refer to the documentation of [`rusty_lr`](https://github.com/ehwan/RustyLR).
#[derive(Parser, Debug)]
#[command(version)]
#[command(about)]
pub struct Args {
    /// Input_file to read
    pub input_file: String,

    /// Output_file to write
    #[arg(default_value = "out.tab.rs")]
    pub output_file: String,

    /// Do not rustfmt the output
    #[arg(long, default_value = "false")]
    pub no_format: bool,

    /// Do not print note information about any shift/reduce, reduce/reduce conflicts.
    ///
    /// If the target is deterministic parser, conflict will be treated as an error, so this option will be ignored.
    /// This option is only for non-deterministic GLR parser.
    #[arg(short = 'c', long, default_value = "false")]
    pub no_conflict: bool,

    /// Do not print debug information about conflicts resolving process by any `%left`, `%right`, or `%precedence` directive.
    #[arg(short = 'r', long, default_value = "false")]
    pub no_conflict_resolve: bool,

    /// Do not print debug information about optimization process.
    #[arg(short = 'o', long, default_value = "false")]
    pub no_optimization: bool,

    /// Do not print backtrace of production rules when conflicts occurred. ruleset could be messed up
    #[arg(short = 'b', long, default_value = "false")]
    pub no_backtrace: bool,

    /// Override the written code and set generated parser use GLR parsing algorithm
    #[arg(long)]
    pub glr: Option<bool>,

    /// Override the written code and set parser table to be runtime-calculated
    #[arg(long)]
    pub runtime: Option<bool>,

    /// Override the written code and set generated parser table to use dense arrays
    #[arg(long)]
    pub dense: Option<bool>,
}
