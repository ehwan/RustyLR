pub trait TerminalClass: Copy {
    type Term;

    const ERROR: Self;
    const EOF: Self;

    /// Gets the pretty name of this terminal class.
    fn as_str(&self) -> &'static str;

    /// Converts this terminal class to a usize
    fn to_usize(&self) -> usize;

    fn from_term(term: &Self::Term) -> Self;

    fn precedence(&self) -> crate::parser::Precedence;
}
