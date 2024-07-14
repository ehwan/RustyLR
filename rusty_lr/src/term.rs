/// trait bound alias for Term
pub trait TermTraitBound: Clone + PartialEq + Eq + PartialOrd + Ord {}

impl<T: Clone + PartialEq + Eq + PartialOrd + Ord> TermTraitBound for T {}
