/// Tree represention of single non-terminal token.
/// This tree contains only 2-height nodes.
/// Used for error reporting and conflict resolving in reduce action.
#[derive(Debug, Clone)]
pub struct TreeNonTerminal1 {
    /// index of production rule that this token reduced from
    pub rule: usize,
    /// id that user assigned to this rule
    pub ruleid: usize,
    /// children of this token consumed by reduction
    pub tokens: Vec<Tree0>,
}

/// Tree representation of single token.
/// This tree contains only 2-height nodes.
/// Used for error reporting and conflict resolving in reduce action.
#[derive(Debug, Clone)]
pub enum Tree1 {
    /// A terminal token
    Terminal,
    /// A non-terminal token
    NonTerminal(TreeNonTerminal1),
}
impl Tree1 {
    pub fn to_tree0(&self) -> Tree0 {
        match self {
            Tree1::Terminal => Tree0::Terminal,
            Tree1::NonTerminal(non_terminal) => Tree0::NonTerminal(TreeNonTerminal0 {
                rule: non_terminal.rule,
                ruleid: non_terminal.ruleid,
            }),
        }
    }
}

/// Tree represention of single non-terminal token.
/// This tree contains only 2-height nodes.
/// Used for error reporting and conflict resolving in reduce action.
#[derive(Debug, Clone)]
pub struct TreeNonTerminal0 {
    /// index of production rule that this token reduced from
    pub rule: usize,
    /// id that user assigned to this rule
    pub ruleid: usize,
}

/// Tree representation of single token.
/// This tree contains only 2-height nodes.
/// Used for error reporting and conflict resolving in reduce action.
#[derive(Debug, Clone)]
pub enum Tree0 {
    /// A terminal token
    Terminal,
    /// A non-terminal token
    NonTerminal(TreeNonTerminal0),
}

impl Tree0 {
    pub fn rule_id(&self) -> usize {
        match self {
            Tree0::Terminal => 0,
            Tree0::NonTerminal(non_terminal) => non_terminal.ruleid,
        }
    }
}
