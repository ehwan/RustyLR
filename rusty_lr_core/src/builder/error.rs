/// Error type for building grammar
pub enum BuildError<Term, NonTerm> {
    RuleNotFound(NonTerm),

    NoAugmented,

    __PhantomData__(Term),
}
