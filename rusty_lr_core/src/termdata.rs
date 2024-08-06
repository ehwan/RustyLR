use std::ops::Deref;
use std::ops::DerefMut;

/// type for Terminal data in reduce action
#[derive(Debug, Clone)]
pub struct TermData<'a, Term> {
    /// the terminal symbol
    pub value: &'a Term,
    /// the index of the terminal symbol
    pub index: usize,
}
impl<'a, Term> TermData<'a, Term> {
    pub fn new(value: &'a Term, index: usize) -> Self {
        Self { value, index }
    }
}
impl<'a, Term> Deref for TermData<'a, Term> {
    type Target = &'a Term;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}
impl<'a, Term> DerefMut for TermData<'a, Term> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}
