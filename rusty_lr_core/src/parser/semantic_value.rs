/// A trait for semantic values stored on the parser data stack.
///
/// Generated parsers implement this trait for the enum that represents terminal,
/// non-terminal, and empty semantic values. Parser contexts store these values
/// directly in `Vec<Self>`.
pub trait SemanticValue: Sized {
    /// Type for terminal symbols.
    type Term: 'static;
    /// Type for non-terminal symbols.
    type NonTerm: crate::parser::nonterminal::NonTerminal + 'static;
    /// Type for user data owned by the parsing context.
    type UserData;
    /// Type for `Err` variant returned by reduce action.
    type ReduceActionError;
    /// Type for location of the token.
    type Location: crate::Location;

    fn new_empty() -> Self;
    fn new_terminal(term: Self::Term) -> Self;

    /// Performs a reduce action with the given production rule index.
    fn reduce_action(
        // the child values for the reduction
        // the caller (usually from generated code) must pop all of the values used for this reduce_action
        data_stack: &mut Vec<Self>,
        location_stack: &mut Vec<Self::Location>,
        push_data: bool,

        // the index of the production rule to reduce
        rule_index: usize,

        // for runtime-conflict-resolve.
        // if this variable is set to false in the action, the shift action will not be performed. (GLR parser)
        shift: &mut bool,
        // the lookahead token that caused this reduce action
        lookahead: &crate::TerminalSymbol<Self::Term>,
        // user data owned by the parsing context
        userdata: &mut Self::UserData,
        // location of this non-terminal, e.g. `@$`
        location0: &mut Self::Location,
    ) -> Result<(), Self::ReduceActionError>;
}
