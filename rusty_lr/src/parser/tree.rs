/// A tree struct for result of parsing
#[derive(Debug, Clone)]
pub enum Tree {
    /// index on input sequence
    Terminal(usize),
    /// rule index for reduce, children Trees
    NonTerminal(usize, Vec<Tree>),
}

impl Tree {
    /// get the slice of input sequence that this tree represents
    pub fn slice<'a, Term>(&self, input: &'a [Term]) -> &'a [Term] {
        &input[self.begin()..self.end()]
    }

    /// get the begin index of input sequence that this tree represents
    pub fn begin(&self) -> usize {
        match self {
            Tree::Terminal(idx) => *idx,
            Tree::NonTerminal(_, children) => children.first().map(|x| x.begin()).unwrap_or(0),
        }
    }
    /// get the end index of input sequence that this tree represents
    pub fn end(&self) -> usize {
        match self {
            Tree::Terminal(idx) => *idx + 1,
            Tree::NonTerminal(_, children) => children.last().map(|x| x.end()).unwrap_or(0),
        }
    }
}
