/// A struct to hold information about non-terminal symbols
pub trait NonTerminal {
    /// Whether this non-terminal is auto-generated by rustylr.
    /// Some non-terminals could be auto-generated to handle regex patterns, character sets, etc.
    fn is_auto_generated(&self) -> bool {
        self.nonterm_type().is_some()
    }
    /// Augmented rule will be generated for entry point of the grammar.
    fn is_augmented(&self) -> bool {
        self.nonterm_type() == Some(NonTerminalType::Augmented)
    }
    /// whether this non-terminal is set as %trace
    fn is_trace(&self) -> bool;

    /// for internal use only;
    /// If this non-terminal is auto-generated, gets the pattern where this non-terminal was generated from.
    fn nonterm_type(&self) -> Option<NonTerminalType>;

    /// Gets the pretty name of this non-terminal.
    fn as_str(&self) -> &'static str;

    /// converts this non-terminal to a usize
    fn to_usize(&self) -> usize;
}

/// If the non-terminal is auto-generated,
/// the pattern where this non-terminal was generated from.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NonTerminalType {
    /// zero or more repetitions
    Star,
    /// one or more repetitions with left recursion
    PlusLeft,
    /// one or more repetitions with right recursion
    PlusRight,
    /// zero or one repetition
    Optional,
    /// Augmented rule
    Augmented,
    /// error recovery non-terminal
    Error,

    /// terminal set enclosed in brackets ( [a-zA-Z0-9] )
    TerminalSet,
    /// rule with explicit lookaheads
    Lookahead,

    /// sequence of tokens enclosed in parentheses ( a B c ... )
    Group,

    /// "abc" or b"abc"
    LiteralString,
}

#[allow(type_alias_bounds)]
pub type ReduceArgsStack<T: TokenData> = smallvec::SmallVec<[(T, T::Location); 3]>;

/// A trait for token that holds data.
/// This will be used for data stack in the parser.
pub trait TokenData: Sized {
    /// Type for terminal symbols
    type Term;
    /// Type for non-terminal symbols - this must be enum type that was auto-generated by rusty_lr
    type NonTerm;
    /// Type for user data that is passed to the parser from the user.
    type UserData;
    /// Type for `Err` variant returned by reduce action
    type ReduceActionError;
    /// The value of the start symbol
    type StartType;
    /// Type for location of the token
    type Location: crate::Location;

    /// performs a reduce action with the given rule index
    fn reduce_action(
        // the index of the production rule to reduce
        rule_index: usize,
        // the child tokens for the reduction
        // .len() must match with the number of symbols in the rule
        // the order must be in reverse order. That is,
        // if the rule is A -> B C D,
        // this must be in the order of [D, C, B]
        reduce_args: &mut ReduceArgsStack<Self>,
        // for runtime-conflict-resolve.
        // if this variable is set to false in the action, the shift action will not be performed. (GLR parser)
        shift: &mut bool,
        // the lookahead token that caused this reduce action
        lookahead: &crate::TerminalSymbol<Self::Term>,
        // user input data
        userdata: &mut Self::UserData,
        // location of this non-terminal, e.g. `@$`
        location0: &mut Self::Location,
    ) -> Result<Self, Self::ReduceActionError>;

    /// create new token data for error recovery
    fn new_error() -> Self;

    /// create new token data that is empty.
    fn new_empty() -> Self {
        Self::new_error()
    }

    /// create new terminal variant
    fn new_terminal(term: Self::Term) -> Self;
}
