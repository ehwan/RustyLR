use crate::parser::table::ShiftTarget;

/// One decoded parser-table row used by generated code and the grammar builder before converting
/// to a concrete runtime table layout.
pub struct IntermediateState<TermClass, NonTerm> {
    pub shift_goto_map_term: Vec<(TermClass, ShiftTarget<usize>)>, // must be sorted
    pub shift_goto_map_nonterm: Vec<(NonTerm, ShiftTarget<usize>)>, // must be sorted
    pub reduce_map: Vec<(TermClass, Vec<usize>)>,                  // must be sorted
    pub ruleset: Vec<crate::production::LR0ItemRef>,
}
