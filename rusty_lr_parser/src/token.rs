use proc_macro2::{Ident, Span};

/// for syntax <Ident> '=' <Token>
#[derive(Debug, Clone)]
pub struct TokenMapped {
    /// terminal or non-terminal name
    pub token: rusty_lr_core::Token<rusty_lr_core::TerminalSymbol<usize>, usize>,

    /// variable name that the token's data will be mapped to
    pub mapto: Option<Ident>,

    /// When optimizing out single-token production rules like A -> B { ... },
    /// All occurrences of A will be replaced with B, but the reduce action `{ ... }` still needs to be called.
    ///
    /// Say we have production rule X -> a b A c d { ... },
    /// and A is replaced with B, then X -> a b B c d { ... } is the new rule,
    /// but we still need to call A's reduce action before calling X's reduce action.
    /// So basically we need to keep a chain of reduce actions to be called.
    ///
    /// This token data has to be r_n( r_n-1( ... r_0(token) ... ) )
    /// where r_i is the i'th reduce action in the chain.
    pub reduce_action_chains: Vec<usize>,

    /// span of the token
    pub begin_span: Span,
    pub end_span: Span,
}
