/// struct for parsing context
#[derive(Debug, Clone)]
pub struct Context {
    /// stack for state transition
    pub state_stack: Vec<usize>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            state_stack: vec![0],
        }
    }
}

impl Default for Context {
    fn default() -> Self {
        Self::new()
    }
}
