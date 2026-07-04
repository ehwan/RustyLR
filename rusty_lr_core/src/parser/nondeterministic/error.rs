use std::fmt::Debug;
use std::fmt::Display;

/// Error type for feed()
#[derive(Clone, Debug)]
pub struct ParseError<Term, Location, ReduceActionError> {
    /// The terminal symbol that caused the error.
    pub term: crate::TerminalSymbol<Term>,
    /// Location of the terminal symbol.
    pub location: Location,
    /// Error from reduce action (from every diverged paths)
    pub reduce_action_errors: Vec<ReduceActionError>,

    /// States when the error occurred (from all diverged paths)
    pub(crate) states: Vec<usize>,
}

/// Successful GLR feed result.
///
/// GLR parsing can keep one branch alive while other branches fail. In that case the feed is a
/// success, but `errors` records the branches that were pruned by `NoAction` or reduce-action
/// failure.
#[derive(Clone, Debug)]
pub struct FeedSuccess<Term, Location, ReduceActionError> {
    /// Branch errors observed while at least one GLR branch survived this feed.
    ///
    /// This is intentionally optional so callers that only care about fatal parse failure can keep
    /// using `context.feed(token)?` or `context.feed(token).unwrap()`.
    pub errors: Option<ParseError<Term, Location, ReduceActionError>>,
}

impl<Term, Location, ReduceActionError> ParseError<Term, Location, ReduceActionError> {
    pub fn location(&self) -> &Location {
        &self.location
    }
    pub fn term(&self) -> &crate::TerminalSymbol<Term> {
        &self.term
    }
    /// States when the error occurred (from all diverged paths)
    pub fn states(&self) -> impl Iterator<Item = usize> + '_ {
        self.states.iter().copied()
    }
}

impl<Term, Location, ReduceActionError> Display for ParseError<Term, Location, ReduceActionError>
where
    Term: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ParseError: {}, States: {:?}", self.term, self.states)
    }
}
