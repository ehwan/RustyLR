/// for resolving shift/reduce conflict
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ReduceType {
    /// reduce to the left, i.e. reduce first
    Left,
    /// reduce to the right, i.e. shift first
    Right,
}
impl std::fmt::Display for ReduceType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ReduceType::Left => write!(f, "Left"),
            ReduceType::Right => write!(f, "Right"),
        }
    }
}
