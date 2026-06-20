use crate::parser::table::ShiftTarget;
use crate::TriState;

/// One decoded parser-table row used by generated code and the grammar builder before converting
/// to a concrete runtime table layout.
pub struct IntermediateState<TermClass, NonTerm, StateIndex, RuleIndex> {
    pub shift_goto_map_term: Vec<(TermClass, ShiftTarget<StateIndex>)>, // must be sorted
    pub shift_goto_map_nonterm: Vec<(NonTerm, ShiftTarget<StateIndex>)>, // must be sorted
    pub reduce_map: Vec<(TermClass, Vec<RuleIndex>)>,                   // must be sorted
    pub ruleset: Vec<crate::rule::ShiftedRuleRef>,
    pub can_accept_error: TriState,
}

/// For state, terminal and class indices, we use the most compact integer type that can hold the maximum value.
/// This trait defines the conversion between {u8, u16, u32, usize} <-> usize.
pub trait Index: Copy {
    fn into_usize(self) -> usize;
    fn from_usize_unchecked(value: usize) -> Self;
}

impl Index for usize {
    fn into_usize(self) -> usize {
        self
    }
    fn from_usize_unchecked(value: usize) -> Self {
        value
    }
}

impl Index for u8 {
    fn into_usize(self) -> usize {
        self as usize
    }
    fn from_usize_unchecked(value: usize) -> Self {
        value as u8
    }
}

impl Index for u16 {
    fn into_usize(self) -> usize {
        self as usize
    }
    fn from_usize_unchecked(value: usize) -> Self {
        value as u16
    }
}

impl Index for u32 {
    fn into_usize(self) -> usize {
        self as usize
    }
    fn from_usize_unchecked(value: usize) -> Self {
        value as u32
    }
}
