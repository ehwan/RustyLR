use crate::grammar::TerminalClass;
use crate::parser::location::Located;
use crate::parser::location::Location;

/// for syntax <Ident> '=' <Symbol>
#[derive(Debug, Clone)]
pub struct MappedSymbol {
    /// terminal or non-terminal name
    pub symbol: rusty_lr_core::Symbol<rusty_lr_core::TerminalSymbol<TerminalClass>, usize>,

    /// variable name that the symbol's data will be mapped to
    pub mapto: Option<Located<String>>,

    /// When optimizing out single-symbol production rules like A -> B { ... },
    /// All occurrences of A will be replaced with B, but the reduce action `{ ... }` still needs to be called.
    ///
    /// Say we have production rule X -> a b A c d { ... },
    /// and A is replaced with B, then X -> a b B c d { ... } is the new rule,
    /// but we still need to call A's reduce action before calling X's reduce action.
    /// So basically we need to keep a chain of reduce actions to be called.
    ///
    /// This symbol data has to be r_n( r_n-1( ... r_0(symbol) ... ) )
    /// where r_i is the i'th reduce action in the chain.
    pub reduce_action_chains: Vec<usize>,

    /// location of the symbol
    pub location: Location,
}
