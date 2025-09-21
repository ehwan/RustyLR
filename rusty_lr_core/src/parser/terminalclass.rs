pub trait TerminalClass: Copy {
    type Term;

    /// Gets the pretty name of this terminal class.
    fn as_str(&self) -> &'static str;

    /// converts this non-terminal to a usize
    fn to_usize(&self) -> usize;

    fn from_term(term: &Self::Term) -> Self;

    fn precedence(&self) -> crate::parser::Precedence;

    fn error() -> Self;
    fn eof() -> Self;
}
