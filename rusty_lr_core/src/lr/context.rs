use super::Stack;

/// Context for LR parser.
/// This hadles actual data stack for reduce action.
#[derive(Debug, Clone)]
pub struct Context<Stack> {
    /// state stack
    pub state_stack: Vec<usize>,
    pub(crate) data_stack: Stack,
}

impl<S> Context<S> {
    pub fn new() -> Self
    where
        S: Stack,
    {
        Context {
            state_stack: vec![0],
            data_stack: S::new(),
        }
    }
    /// pop value from start rule
    #[inline]
    pub fn accept(&mut self) -> S::StartType
    where
        S: Stack,
    {
        self.data_stack.pop_start()
    }
}
