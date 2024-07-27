/// A tree struct for result of parsing (for slice)
#[derive(Debug, Clone)]
pub enum Tree {
    /// index on input sequence
    Terminal(usize),
    /// rule index for reduce, children Trees, index on input sequence
    NonTerminal(usize, Vec<Tree>, usize),
}

impl Tree {
    /// get the slice of input sequence that this tree represents
    pub fn slice<'a, Term>(&self, input: &'a [Term]) -> &'a [Term] {
        &input[self.range()]
    }
    /// get the range on input sequence that this tree represents
    pub fn range(&self) -> std::ops::Range<usize> {
        self.begin()..self.end()
    }

    /// get the begin index of input sequence that this tree represents
    pub fn begin(&self) -> usize {
        match self {
            Tree::Terminal(idx) => *idx,
            Tree::NonTerminal(_, _children, idx) => *idx,
        }
    }
    /// get the end index of input sequence that this tree represents
    pub fn end(&self) -> usize {
        match self {
            Tree::Terminal(idx) => *idx + 1,
            Tree::NonTerminal(_, children, idx) => children.last().map(|x| x.end()).unwrap_or(*idx),
        }
    }
}

/// A tree struct for result of parsing (for &str)
#[derive(Debug, Clone)]
pub enum TreeStr {
    /// byte range of single character (terminal symbol) in input sequence
    Terminal(usize, usize),
    /// rule index for reduce, children Trees, byte range of input sequence
    NonTerminal(usize, Vec<TreeStr>, usize, usize),
}

impl TreeStr {
    /// get the slice of input sequence that this tree represents
    pub fn str<'a>(&self, input: &'a str) -> &'a str {
        &input[self.range()]
    }
    /// get the range on input sequence that this tree represents
    pub fn range(&self) -> std::ops::Range<usize> {
        self.begin()..self.end()
    }

    /// get the begin index of input sequence that this tree represents
    pub fn begin(&self) -> usize {
        match self {
            TreeStr::Terminal(beg, _end) => *beg,
            TreeStr::NonTerminal(_rule, _children, beg, _end) => *beg,
        }
    }
    /// get the end index of input sequence that this tree represents
    pub fn end(&self) -> usize {
        match self {
            TreeStr::Terminal(_beg, end) => *end,
            TreeStr::NonTerminal(_rule, children, _beg, end) => {
                children.last().map(|x| x.end()).unwrap_or(*end)
            }
        }
    }
}
