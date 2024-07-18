/// A tree struct for result of parsing (for slice)
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

/// A tree struct for result of parsing (for &str)
#[derive(Debug, Clone)]
pub enum TreeStr {
    /// byte range of single character (terminal symbol) in input sequence
    Terminal(usize, usize),
    /// rule index for reduce, children Trees
    NonTerminal(usize, Vec<TreeStr>),
}

impl TreeStr {
    /// get the slice of input sequence that this tree represents
    pub fn str<'a>(&self, input: &'a str) -> &'a str {
        &input[self.begin()..self.end()]
    }

    /// get the begin index of input sequence that this tree represents
    pub fn begin(&self) -> usize {
        match self {
            TreeStr::Terminal(beg, _end) => *beg,
            TreeStr::NonTerminal(_, children) => children.first().map(|x| x.begin()).unwrap_or(0),
        }
    }
    /// get the end index of input sequence that this tree represents
    pub fn end(&self) -> usize {
        match self {
            TreeStr::Terminal(_beg, end) => *end,
            TreeStr::NonTerminal(_, children) => children.last().map(|x| x.end()).unwrap_or(0),
        }
    }
}
