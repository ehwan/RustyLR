/// Error type for building grammar
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BuildError<Term, NonTerm> {
    RuleNotFound(NonTerm),

    NoAugmented,

    __PhantomData__(Term),
}
