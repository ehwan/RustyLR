use super::Context;
use super::NodeData;
use super::Parser;

use std::hash::Hash;

/// Feed all terminals in `tails` to context, and returns true if all terminals are accepted.
/// Which means, there are one or more path alive after feeding all terminals.
fn feed_tails<P: Parser, Data: NodeData<Term = P::Term, NonTerm = P::NonTerm> + Clone>(
    parser: &P,
    context: &mut Context<Data>,
    tails: impl Iterator<Item = P::Term>,
    userdata: &mut Data::UserData,
) -> bool
where
    P::Term: Hash + Eq + Clone,
    P::NonTerm: Hash + Eq + Clone,
{
    for tail in tails {
        if feed(parser, context, tail, userdata).is_err() {
            return false;
        }
    }
    true
}

/// Perform breadth first search to feed all terminals in `terms` and `tails` to context.
/// This function searches for the shortest path, which can be represented as CurrentState -> Terms^N -> Tails.
/// Returns true if there is a path alive.
pub fn complete<P: Parser, Data: NodeData<Term = P::Term, NonTerm = P::NonTerm> + Clone>(
    parser: &P,
    context: &mut Context<Data>,
    terms: impl Iterator<Item = P::Term> + Clone,
    userdata: &mut Data::UserData,
    tails: impl Iterator<Item = P::Term> + Clone,
    max_depth: usize,
) -> bool
where
    P::Term: Clone + Hash + Eq,
    P::NonTerm: Clone + Hash + Eq,
{
    for _ in 0..max_depth {
        let context0 = context.clone();
        if feed_tails(parser, context, tails.clone(), userdata) {
            return true;
        }

        // breadth first feed all terminals in `terms`
        for term in terms.clone() {
            let mut context_cloned = context0.clone();
            feed(parser, &mut context_cloned, term, userdata).ok();
            context.append(&mut context_cloned);
        }
    }
    feed_tails(parser, context, tails, userdata)
}
