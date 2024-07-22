use super::{ischar::IsChar, tree::Tree, tree::TreeStr};

pub struct Context<'a, Term, NonTerm> {
    /// stack for state transition
    pub state_stack: Vec<usize>,
    /// stack for constructing Tree
    pub stack: Vec<Tree>,
    /// input sequence
    pub terms: &'a [Term],
    /// current index in input sequence
    pub idx: usize,

    _phantom: std::marker::PhantomData<NonTerm>,
}
impl<'a, Term, NonTerm> Clone for Context<'a, Term, NonTerm> {
    fn clone(&self) -> Self {
        Context {
            state_stack: self.state_stack.clone(),
            stack: self.stack.clone(),
            terms: self.terms,
            idx: self.idx,
            _phantom: std::marker::PhantomData,
        }
    }
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
    /// get input sequence
    pub fn input(&self) -> &'a [Term] {
        self.terms
    }
    /// get top of stack.
    /// If DFA is generated correctly, this should not panic.
    /// This returns the lastly reduced tree or the lastly shifted terminal.
    pub fn top(&self) -> &Tree {
        self.stack.last().unwrap()
    }
    /// get slice range of top element of stack
    pub fn top_slice(&self) -> &'a [Term] {
        self.top().slice(self.terms)
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

pub struct ContextStr<'a, Term, NonTerm> {
    /// stack for state transition
    pub state_stack: Vec<usize>,
    /// stack for constructing Tree
    pub stack: Vec<TreeStr>,
    /// input sequence
    pub terms: &'a str,

    chars: std::str::Chars<'a>,

    /// current byte index and unicode char in input sequence
    cur: Option<(char, usize, usize)>,

    _phantom: std::marker::PhantomData<(Term, NonTerm)>,
}
impl<'a, Term, NonTerm> Clone for ContextStr<'a, Term, NonTerm> {
    fn clone(&self) -> Self {
        ContextStr {
            state_stack: self.state_stack.clone(),
            stack: self.stack.clone(),
            terms: self.terms,
            chars: self.terms.chars(),
            cur: self.cur,
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<'a, Term, NonTerm> ContextStr<'a, Term, NonTerm> {
    pub(crate) fn new(terms: &'a str, main_state: usize) -> Self {
        let mut chars = terms.chars();
        let len0 = chars.as_str().len();
        let cur = chars.next();
        let len1 = chars.as_str().len();
        ContextStr {
            state_stack: vec![main_state],
            stack: Vec::new(),
            terms,
            chars,
            _phantom: std::marker::PhantomData,
            cur: cur.map(|c| (c, 0, len0 - len1)),
        }
    }

    /// get current terminal
    pub fn term(&self) -> Term
    where
        char: IsChar<Term>,
    {
        self.cur.unwrap().0.as_term()
    }
    /// get current state. If DFA is generated correctly, this should not panic
    pub fn state(&self) -> usize {
        // if DFA is generated correctly, this should not panic
        *self.state_stack.last().unwrap()
    }
    /// get input sequence
    pub fn input(&self) -> &'a str {
        self.terms
    }
    /// get top of stack.
    /// If DFA is generated correctly, this should not panic.
    /// This returns the lastly reduced tree or the lastly shifted terminal.
    pub fn top(&self) -> &TreeStr {
        self.stack.last().unwrap()
    }
    /// get &str range of top element of stack
    pub fn top_str(&self) -> &'a str {
        self.top().str(self.terms)
    }

    pub(crate) fn state_safe(&self) -> Option<&usize> {
        self.state_stack.last()
    }

    pub(crate) fn push_state(&mut self, state: usize) {
        self.state_stack.push(state);
    }
    pub(crate) fn shift(&mut self) {
        self.stack
            .push(TreeStr::Terminal(self.cur.unwrap().1, self.cur.unwrap().2));
    }
    pub(crate) fn reduce(&mut self, rule: usize, items: usize) {
        self.state_stack.truncate(self.state_stack.len() - items);

        let tree = TreeStr::NonTerminal(rule, self.stack.split_off(self.stack.len() - items));
        self.stack.push(tree);
    }
    pub(crate) fn inc(&mut self) {
        let len0 = self.chars.as_str().len();
        let cur = self.chars.next();
        let len1 = self.chars.as_str().len();
        let len = len0 - len1;
        let beg = self.cur.unwrap().2;
        self.cur = cur.map(|c| (c, beg, beg + len));
    }
}
