/// trait bound alias for Term
pub trait TermTraitBound: Clone + PartialEq + Eq + PartialOrd + Ord + std::hash::Hash {}

impl<T: Clone + PartialEq + Eq + PartialOrd + Ord + std::hash::Hash> TermTraitBound for T {}
