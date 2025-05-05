pub trait Stack: Sized {
    type Term;
    type NonTerm;
    type ReduceActionError;
    type UserData;

    type StartType;

    /// create new(empty) stack
    fn new() -> Self;

    /// reduce action
    fn reduce(
        &mut self,
        rule_index: usize,
        userdata: &mut Self::UserData,
        lookahead: &Self::Term,
    ) -> Result<(), Self::ReduceActionError>;

    /// push terminal to data stack
    fn push(&mut self, term: Self::Term);

    /// pop start non-terminal from data stack
    fn pop_start(&mut self) -> Self::StartType;

    fn pop(&mut self, nonterm: Self::NonTerm);
    fn pop_term(&mut self);
}
