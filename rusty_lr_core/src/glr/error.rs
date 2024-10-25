use std::fmt::Debug;
use std::fmt::Display;

/// Error when there is an invalid terminal feeded to the parser.
#[derive(Clone)]
pub struct InvalidTerminalError<Term, NonTerm, ReduceActionError> {
    /// The terminal that feeded to the parser.
    pub term: Term,
    /// The reduce action errors.
    pub reduce_errors: Vec<ReduceActionError>,

    #[cfg(feature = "error")]
    pub(crate) expected: Vec<Term>,
    #[cfg(feature = "error")]
    pub(crate) expected_nonterm: Vec<NonTerm>,
    #[cfg(feature = "error")]
    pub(crate) backtraces: Vec<crate::Backtrace<Term, NonTerm>>,

    #[cfg(not(feature = "error"))]
    pub(crate) _phantom: std::marker::PhantomData<NonTerm>,
}

impl<Term: Display, NonTerm: Display, ReduceActionError: Display> Display
    for InvalidTerminalError<Term, NonTerm, ReduceActionError>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Invalid Terminal: {}", self.term)?;

        for error in &self.reduce_errors {
            write!(f, "\nReduce action error: {}", error)?;
        }

        #[cfg(feature = "error")]
        {
            use std::collections::BTreeSet;
            let expected = self
                .expected
                .iter()
                .map(|term| term.to_string())
                .collect::<BTreeSet<_>>() // sort and dedup first
                .into_iter()
                .collect::<Vec<_>>()
                .join(", "); // and join
            write!(f, "\nExpected: {}", expected)?;

            let expected = self
                .expected_nonterm
                .iter()
                .map(|nonterm| nonterm.to_string())
                .collect::<BTreeSet<_>>() // sort and dedup first
                .into_iter()
                .collect::<Vec<_>>()
                .join(", "); // and join
            write!(f, "\nExpected(NonTerminals): {}", expected)?;

            for (idx, backtrace) in self.backtraces.iter().enumerate() {
                write!(f, "\nBacktrace for path {}: ", idx)?;
                write!(f, "\n{}", backtrace)?;
            }
        }
        Ok(())
    }
}

impl<Term: Debug, NonTerm: Debug, ReduceActionError: Debug> Debug
    for InvalidTerminalError<Term, NonTerm, ReduceActionError>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Invalid Terminal: {:?}", self.term)?;

        for error in &self.reduce_errors {
            write!(f, "\nReduce action error: {:?}", error)?;
        }

        #[cfg(feature = "error")]
        {
            use std::collections::BTreeSet;
            let expected = self
                .expected
                .iter()
                .map(|term| format!("{:?}", term))
                .collect::<BTreeSet<_>>() // sort and dedup first
                .into_iter()
                .collect::<Vec<_>>()
                .join(", "); // and join
            write!(f, "\nExpected: {}", expected)?;

            let expected = self
                .expected_nonterm
                .iter()
                .map(|nonterm| format!("{:?}", nonterm))
                .collect::<BTreeSet<_>>() // sort and dedup first
                .into_iter()
                .collect::<Vec<_>>()
                .join(", "); // and join
            write!(f, "\nExpected(NonTerminals): {}", expected)?;

            for (idx, backtrace) in self.backtraces.iter().enumerate() {
                write!(f, "\nBacktrace for path {}: ", idx)?;
                write!(f, "\n{:?}", backtrace)?;
            }
        }
        Ok(())
    }
}

impl<Term: Display + Debug, NonTerm: Display + Debug, ReduceActionError: Display + Debug>
    std::error::Error for InvalidTerminalError<Term, NonTerm, ReduceActionError>
{
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}

/// Error when there are multiple paths to represent the same string you feeded to the parser.
/// If you want to see the multiple paths, you can use `to_tree_lists` method.
pub struct MultiplePathError<Term, NonTerm> {
    #[cfg(feature = "tree")]
    pub(crate) tree_lists: Vec<crate::TreeList<Term, NonTerm>>,

    #[cfg(not(feature = "tree"))]
    pub(crate) _phantom: std::marker::PhantomData<(Term, NonTerm)>,
}

impl<Term: Display, NonTerm: Display> Display for MultiplePathError<Term, NonTerm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "MultiplePathError")?;

        #[cfg(feature = "tree")]
        {
            for (idx, tree_list) in self.tree_lists.iter().enumerate() {
                write!(f, "\nPath {}: ", idx)?;
                write!(f, "\n{}", tree_list)?;
            }
        }
        Ok(())
    }
}
impl<Term: Debug, NonTerm: Debug> Debug for MultiplePathError<Term, NonTerm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "MultiplePathError")?;

        #[cfg(feature = "tree")]
        {
            for (idx, tree_list) in self.tree_lists.iter().enumerate() {
                write!(f, "\nPath {}: ", idx)?;
                write!(f, "\n{:?}", tree_list)?;
            }
        }
        Ok(())
    }
}

impl<Term: Display + Debug, NonTerm: Display + Debug> std::error::Error
    for MultiplePathError<Term, NonTerm>
{
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}
