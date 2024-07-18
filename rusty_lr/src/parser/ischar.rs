pub trait IsChar<Term> {
    fn as_term(self) -> Term;
}

impl IsChar<char> for char {
    fn as_term(self) -> char {
        self
    }
}
