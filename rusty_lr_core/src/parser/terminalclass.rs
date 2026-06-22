pub trait TerminalClass: Copy {
    type Term;

    const ERROR: Self;
    const EOF: Self;

    /// Gets the pretty name of this terminal class.
    fn as_str(&self) -> &'static str;

    /// Converts this terminal class to a usize
    fn to_usize(&self) -> usize;

    fn from_term(term: &Self::Term) -> Self;

    /// Gets the terminal class for a virtual start branch.
    fn from_virtual_start(_branch_idx: u32) -> Self {
        panic!("from_virtual_start not supported on this terminal class")
    }
}
