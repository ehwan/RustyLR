use crate::tree::Tree;

pub struct Context<'a, Term, NonTerm> {
    /// stack for state transition
    pub state_stack: Vec<usize>,
    /// stack for constructing Tree
    pub stack: Vec<Tree>,
    /// input sequence
    pub terms: &'a [Term],
    /// current index of input sequence
    pub idx: usize,

    _phantom: std::marker::PhantomData<NonTerm>,
}

impl<'a, Term, NonTerm> Context<'a, Term, NonTerm> {
    pub(crate) fn new(terms: &'a [Term], main_state: usize) -> Self {
        Context {
            state_stack: vec![main_state],
            stack: Vec::new(),
            terms,
            idx: 0,
            _phantom: std::marker::PhantomData,
        }
    }

    /// get current terminal
    pub fn term(&self) -> &'a Term {
        &self.terms[self.idx]
    }
    /// get current state. If DFA is generated correctly, this should not panic
    pub fn state(&self) -> usize {
        // if DFA is generated correctly, this should not panic
        *self.state_stack.last().unwrap()
    }
    pub(crate) fn state_safe(&self) -> Option<&usize> {
        self.state_stack.last()
    }

    pub(crate) fn push_state(&mut self, state: usize) {
        self.state_stack.push(state);
    }
    pub(crate) fn shift(&mut self) {
        self.stack.push(Tree::Terminal(self.idx));
    }
    pub(crate) fn reduce(&mut self, rule: usize, items: usize) {
        self.state_stack.truncate(self.state_stack.len() - items);

        let tree = Tree::NonTerminal(rule, self.stack.split_off(self.stack.len() - items));
        self.stack.push(tree);
    }
    pub(crate) fn inc(&mut self) {
        self.idx += 1;
    }
}
