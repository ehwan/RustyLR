use std::hash::Hash;

/// trait bound alias for Term
pub trait TermTraitBound: Clone + PartialEq + Eq + PartialOrd + Ord + Hash {}
impl<T: Clone + PartialEq + Eq + PartialOrd + Ord + Hash> TermTraitBound for T {}

/// trait bound alias for NonTerm
pub trait NonTermTraitBound: Clone + PartialEq + Eq + Hash {}
impl<T: Clone + PartialEq + Eq + Hash> NonTermTraitBound for T {}
