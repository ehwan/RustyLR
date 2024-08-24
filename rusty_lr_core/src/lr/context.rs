/// Context trait for LR parser.
/// This hadles actual data stack for reduce action.
pub trait Context {
    type Term;
    type ReduceActionError;
    type UserData;

    /// reduce action, this will be automatically called by parser
    fn reduce(
        &mut self,
        ruleid: usize,
        userdata: &mut Self::UserData,
        lookahead: &Self::Term,
    ) -> Result<(), Self::ReduceActionError>;
    /// push terminal to data stack, this will be automatically called by parser
    fn push(&mut self, term: Self::Term);

    /// get current state stack
    fn get_state_stack(&self) -> &[usize];
    /// get current state stack mutable reference
    fn get_state_stack_mut(&mut self) -> &mut Vec<usize>;
}
