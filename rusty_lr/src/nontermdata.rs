use std::ops::Deref;
use std::ops::DerefMut;

/// type for NonTerminal data in reduce action
#[derive(Debug, Clone)]
pub struct NonTermData<'a, Term, T> {
    /// the slice of terms that this non-terminal data is reduced from
    pub slice: &'a [Term],
    /// the value of this non-terminal data
    pub value: T,
    /// the range of terms that this non-terminal data is reduced from
    pub range: std::ops::Range<usize>,
}

impl<'a, Term, T> NonTermData<'a, Term, T> {
    pub fn new(slice: &'a [Term], value: T, range: std::ops::Range<usize>) -> Self {
        Self {
            slice,
            value,
            range,
        }
    }
}
impl<'a, Term, T> Deref for NonTermData<'_, Term, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}
impl<'a, Term, T> DerefMut for NonTermData<'_, Term, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}
