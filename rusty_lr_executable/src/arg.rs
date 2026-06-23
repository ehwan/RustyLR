use clap::{Parser, Subcommand};

/// Converts a context-free grammar into a deterministic finite automaton (DFA) tables,
/// and generates a Rust code that can be used as a parser for that grammar.
///
/// For usage of the generated code, please refer to the documentation of [`rusty_lr`](https://github.com/ehwan/RustyLR).
#[derive(Parser, Debug)]
#[command(version)]
#[command(about)]
pub struct Args {
    #[command(subcommand)]
    pub command: Option<Command>,

    #[command(flatten)]
    pub generate: GenerateArgs,
}

#[derive(Subcommand, Debug)]
pub enum Command {
    /// Run the RustyLR language server over stdio.
    Lsp(LspArgs),
}

#[derive(clap::Args, Debug)]
pub struct LspArgs {
    /// Run the language server over stdio.
    #[arg(long, default_value = "false")]
    pub stdio: bool,
}

#[derive(clap::Args, Debug)]
pub struct GenerateArgs {
    /// Input_file to read
    pub input_file: Option<String>,

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

    /// Set generated parser table layout representation (auto: optimal choice based on table size, dense: O(1) array, sparse: binary search)
    #[arg(long, value_enum, default_value_t = TableLayoutArg::Auto)]
    pub layout: TableLayoutArg,

    /// Set dense limit in bytes for auto-layout detection
    #[arg(long, default_value_t = 32768)]
    pub dense_limit: usize,

    /// Print the details of a specific state
    #[arg(long)]
    pub state: Option<usize>,
}

#[derive(clap::ValueEnum, Clone, Copy, Debug, PartialEq, Eq)]
pub enum TableLayoutArg {
    Auto,
    Dense,
    Sparse,
}

impl From<TableLayoutArg> for rusty_lr_buildscript::TableLayout {
    fn from(arg: TableLayoutArg) -> Self {
        match arg {
            TableLayoutArg::Auto => rusty_lr_buildscript::TableLayout::Auto,
            TableLayoutArg::Dense => rusty_lr_buildscript::TableLayout::Dense,
            TableLayoutArg::Sparse => rusty_lr_buildscript::TableLayout::Sparse,
        }
    }
}
