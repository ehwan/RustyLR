
// ================================User Codes Begin================================
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ast {
    True,
    And(Box<Ast>, Box<Ast>),
}

// =================================User Codes End=================================
/*
====================================Grammar=====================================

# of terminal classes: 4
# of states: 12

0: Expr -> 't'
1: Expr -> '!'? Expr '&' Expr
2: '!'? -> '!'
3: '!'? -> 
4: Augmented -> VirtualStart(0) Expr eof

*/
// =============================Generated Codes Begin==============================
#[allow(non_camel_case_types, dead_code)]
pub type ExprContext = ::rusty_lr::parser::nondeterministic::Context<
    Parser,
    Data,
    ExprExtracter,
    u8,
    1usize,
>;
#[allow(non_camel_case_types, dead_code)]
pub type Rule = ::rusty_lr::production::Production<TerminalClasses, NonTerminals>;
#[allow(non_camel_case_types, dead_code)]
pub type Tables = ::rusty_lr::parser::table::DenseFlatTables<
    TerminalClasses,
    NonTerminals,
    u8,
    u8,
>;
#[allow(non_camel_case_types, dead_code)]
pub type ParseError = ::rusty_lr::parser::nondeterministic::ParseError<
    char,
    ::rusty_lr::DefaultLocation,
    ::rusty_lr::DefaultReduceActionError,
>;
/// A enum that represents terminal classes
#[allow(non_camel_case_types, dead_code)]
#[derive(
    Clone,
    Copy,
    std::hash::Hash,
    std::cmp::PartialEq,
    std::cmp::Eq,
    std::cmp::PartialOrd,
    std::cmp::Ord
)]
#[repr(usize)]
pub enum TerminalClasses {
    TermClass0,
    TermClass1,
    TermClass2,
    TermClass3,
    error,
    eof,
    VirtualStart0,
}
impl TerminalClasses {
    #[inline]
    pub fn from_usize(value: usize) -> Self {
        debug_assert!(
            value < 7usize, "Terminal class index {} is out of bounds (max {})", value,
            7usize
        );
        unsafe { ::std::mem::transmute(value) }
    }
}
impl ::rusty_lr::parser::terminalclass::TerminalClass for TerminalClasses {
    type Term = char;
    const ERROR: Self = Self::error;
    const EOF: Self = Self::eof;
    fn as_str(&self) -> &'static str {
        match self {
            TerminalClasses::TermClass0 => "'!'",
            TerminalClasses::TermClass1 => "'&'",
            TerminalClasses::TermClass2 => "'t'",
            TerminalClasses::TermClass3 => "<Others>",
            TerminalClasses::error => "error",
            TerminalClasses::eof => "eof",
            TerminalClasses::VirtualStart0 => "virtual_start",
        }
    }
    fn to_usize(&self) -> usize {
        *self as usize
    }
    fn from_term(terminal: &Self::Term) -> Self {
        #[allow(unreachable_patterns, unused_variables)]
        match terminal {
            '!' => TerminalClasses::TermClass0,
            '&' => TerminalClasses::TermClass1,
            't' => TerminalClasses::TermClass2,
            _ => TerminalClasses::TermClass3,
        }
    }
    fn from_virtual_start(branch_idx: u32) -> Self {
        match branch_idx {
            0u32 => Self::VirtualStart0,
            _ => panic!("Invalid virtual start branch index: {}", branch_idx),
        }
    }
}
impl std::fmt::Display for TerminalClasses {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ::rusty_lr::parser::terminalclass::TerminalClass;
        write!(f, "{}", self.as_str())
    }
}
impl std::fmt::Debug for TerminalClasses {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ::rusty_lr::parser::terminalclass::TerminalClass;
        write!(f, "{}", self.as_str())
    }
}
/// An enum that represents non-terminal symbols
#[allow(non_camel_case_types, dead_code)]
#[derive(
    Clone,
    Copy,
    std::hash::Hash,
    std::cmp::PartialEq,
    std::cmp::Eq,
    std::cmp::PartialOrd,
    std::cmp::Ord
)]
#[repr(usize)]
pub enum NonTerminals {
    Expr,
    __LiteralChar0Question1,
    Augmented,
}
impl NonTerminals {
    #[inline]
    pub fn from_usize(value: usize) -> Self {
        debug_assert!(
            value < 3usize, "Non-terminal index {} is out of bounds (max {})", value,
            3usize
        );
        unsafe { ::std::mem::transmute(value) }
    }
}
impl std::fmt::Display for NonTerminals {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ::rusty_lr::parser::nonterminal::NonTerminal;
        write!(f, "{}", self.as_str())
    }
}
impl std::fmt::Debug for NonTerminals {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ::rusty_lr::parser::nonterminal::NonTerminal;
        write!(f, "{}", self.as_str())
    }
}
impl ::rusty_lr::parser::nonterminal::NonTerminal for NonTerminals {
    fn as_str(&self) -> &'static str {
        match self {
            NonTerminals::Expr => "Expr",
            NonTerminals::__LiteralChar0Question1 => "'!'?",
            NonTerminals::Augmented => "Augmented",
        }
    }
    fn nonterm_type(&self) -> Option<::rusty_lr::parser::nonterminal::NonTerminalType> {
        match self {
            NonTerminals::Expr => None,
            NonTerminals::__LiteralChar0Question1 => {
                Some(::rusty_lr::parser::nonterminal::NonTerminalType::Optional)
            }
            NonTerminals::Augmented => {
                Some(::rusty_lr::parser::nonterminal::NonTerminalType::Augmented)
            }
        }
    }
    fn to_usize(&self) -> usize {
        *self as usize
    }
}
/// enum for each non-terminal and terminal symbol, that actually hold data
#[rustfmt::skip]
#[allow(unused_braces, unused_parens, non_snake_case, non_camel_case_types)]
#[doc(hidden)]
#[derive(Clone)]
pub enum __RustyLRData<__RustyLRData0> {
    __variant0(__RustyLRData0),
    Empty,
}
pub type Data = __RustyLRData<Ast>;
impl ::std::fmt::Debug for Data {
    fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
        match self {
            Self::__variant0(..) => f.write_str(stringify!(__variant0)),
            Self::Empty => f.write_str("Empty"),
        }
    }
}
#[doc(hidden)]
#[allow(non_camel_case_types, dead_code)]
pub struct ExprExtracter;
impl ::rusty_lr::parser::semantic_value::StartExtractor<Data> for ExprExtracter {
    type StartType = Ast;
    const BRANCH_INDEX: u32 = 0u32;
    fn extract(value: Data) -> Option<Self::StartType> {
        #[allow(unreachable_patterns, unused_variables)]
        match value {
            Data::__variant0(val) => Some(val),
            _ => None,
        }
    }
}
#[rustfmt::skip]
#[allow(
    unused_braces,
    unused_parens,
    unused_variables,
    non_snake_case,
    unused_mut,
    dead_code,
    unreachable_patterns
)]
impl Data {
    ///Expr -> 't'
    #[inline]
    fn reduce_Expr_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<char>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.pop();
        __data_stack.pop();
        let __res = { Ast::True };
        if __push_data {
            __data_stack.push(Self::__variant0(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///Expr -> '!'? Expr '&' Expr
    #[inline]
    fn reduce_Expr_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<char>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant0(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant0(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 3usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.truncate(__location_stack.len() - 4);
        let mut right = match __data_stack.pop().unwrap() {
            Data::__variant0(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut left = match __data_stack.pop().unwrap() {
            Data::__variant0(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let __res = { Ast::And(Box::new(left), Box::new(right)) };
        if __push_data {
            __data_stack.push(Self::__variant0(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///'!'? ->
    #[inline]
    fn reduce___LiteralChar0Question1_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<char>,
        data: &mut (),
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)] {}
        __data_stack.push(Self::Empty);
        Ok(())
    }
}
#[rustfmt::skip]
#[allow(
    unused_braces,
    unused_parens,
    non_snake_case,
    non_camel_case_types,
    unused_variables
)]
impl ::rusty_lr::parser::semantic_value::SemanticValue for Data {
    type Term = char;
    type NonTerm = NonTerminals;
    type ReduceActionError = ::rusty_lr::DefaultReduceActionError;
    type UserData = ();
    type Location = ::rusty_lr::DefaultLocation;
    fn new_empty() -> Self {
        Self::Empty
    }
    fn new_terminal(term: Self::Term) -> Self {
        Self::Empty
    }
    fn reduce_action(
        data_stack: &mut Vec<Self>,
        location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        push_data: bool,
        rule_index: usize,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Self::Term>,
        user_data: &mut Self::UserData,
        location0: &mut Self::Location,
    ) -> Result<(), Self::ReduceActionError> {
        match rule_index {
            0usize => {
                Self::reduce_Expr_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            1usize => {
                Self::reduce_Expr_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            3usize => {
                Self::reduce___LiteralChar0Question1_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            _ => {
                unreachable!("Invalid Rule: {}", rule_index);
            }
        }
    }
}
/// A lightweight parser struct that references the static parser tables and production rules.
///
/// Since this struct only holds `'static` references to shared, read-only static parser tables,
/// it is extremely cheap to instantiate, copy, or clone, and takes very little space.
#[allow(unused_braces, unused_parens, unused_variables, non_snake_case, unused_mut)]
#[derive(Clone, Copy)]
pub struct Parser;
unsafe impl ::std::marker::Send for Parser {}
unsafe impl ::std::marker::Sync for Parser {}
#[rustfmt::skip]
impl ::rusty_lr::parser::Parser for Parser {
    type Term = char;
    type TermClass = TerminalClasses;
    type NonTerm = NonTerminals;
    type StateIndex = u8;
    type ReduceRules = u8;
    type Tables = Tables;
    const ERROR_USED: bool = false;
    fn get_tables() -> &'static Tables {
        static TABLES: std::sync::OnceLock<Tables> = std::sync::OnceLock::new();
        TABLES
            .get_or_init(|| {
                static RULE_NAMES: &[u32] = &[0, 0, 1, 1, 2];
                static RULE_LENGTHS: &[u32] = &[1, 4, 1, 0, 3];
                static SHIFT_TERM_DATA: &[u32] = &[
                    2147516422, 163840, 65538, 2147614725, 294912, 65538, 229377, 163840,
                    65538, 294912, 65538, 360449, 294912, 65538,
                ];
                static SHIFT_TERM_OFFSETS: &[u32] = &[
                    0, 1, 3, 3, 4, 4, 6, 7, 9, 9, 11, 12, 14,
                ];
                static SHIFT_NONTERM_DATA: &[u32] = &[
                    2147581952, 2147647489, 2147680256, 2147778561, 2147745792,
                    2147647489, 2147811328, 2147778561, 2147745792, 2147778561,
                ];
                static SHIFT_NONTERM_OFFSETS: &[u32] = &[
                    0, 0, 2, 2, 2, 2, 4, 4, 6, 6, 8, 8, 10,
                ];
                static REDUCE_DATA: &[u32] = &[
                    0, 1, 3, 2, 1, 3, 1, 1, 0, 5, 1, 0, 0, 1, 3, 2, 1, 3, 0, 1, 3, 2, 1,
                    3, 1, 1, 1, 5, 1, 1, 0, 1, 3, 2, 1, 3, 0, 1, 3, 2, 1, 3,
                ];
                static REDUCE_OFFSETS: &[u32] = &[
                    0, 0, 6, 12, 12, 12, 18, 18, 24, 30, 36, 36, 42,
                ];
                static CAN_ACCEPT_ERROR: &[u8] = &[0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];
                let num_rules = 5usize;
                let mut rules = Vec::with_capacity(num_rules);
                for i in 0..num_rules {
                    let lhs = NonTerminals::from_usize(RULE_NAMES[i] as usize);
                    rules
                        .push(::rusty_lr::parser::table::RuleInfo {
                            lhs,
                            len: RULE_LENGTHS[i] as usize,
                        });
                }
                let num_states = 12usize;
                let mut state_rows = Vec::with_capacity(num_states);
                for i in 0..num_states {
                    let term_start = SHIFT_TERM_OFFSETS[i] as usize;
                    let term_end = SHIFT_TERM_OFFSETS[i + 1] as usize;
                    let mut shift_goto_map_term = Vec::with_capacity(
                        term_end - term_start,
                    );
                    for idx in term_start..term_end {
                        let val = SHIFT_TERM_DATA[idx];
                        let term_class = TerminalClasses::from_usize(
                            (val & 0x7fff) as usize,
                        );
                        let state = ((val >> 15) & 0xffff) as usize;
                        let push = (val >> 31) != 0;
                        shift_goto_map_term
                            .push((
                                term_class,
                                ::rusty_lr::parser::table::ShiftTarget::new(state, push),
                            ));
                    }
                    let nonterm_start = SHIFT_NONTERM_OFFSETS[i] as usize;
                    let nonterm_end = SHIFT_NONTERM_OFFSETS[i + 1] as usize;
                    let mut shift_goto_map_nonterm = Vec::with_capacity(
                        nonterm_end - nonterm_start,
                    );
                    for idx in nonterm_start..nonterm_end {
                        let val = SHIFT_NONTERM_DATA[idx];
                        let nonterm = NonTerminals::from_usize((val & 0x7fff) as usize);
                        let state = ((val >> 15) & 0xffff) as usize;
                        let push = (val >> 31) != 0;
                        shift_goto_map_nonterm
                            .push((
                                nonterm,
                                ::rusty_lr::parser::table::ShiftTarget::new(state, push),
                            ));
                    }
                    let reduce_start = REDUCE_OFFSETS[i] as usize;
                    let reduce_end = REDUCE_OFFSETS[i + 1] as usize;
                    let mut reduce_map = Vec::new();
                    let mut idx = reduce_start;
                    while idx < reduce_end {
                        let term_val = REDUCE_DATA[idx];
                        let term_class = TerminalClasses::from_usize(term_val as usize);
                        let len = REDUCE_DATA[idx + 1] as usize;
                        let mut rules = Vec::with_capacity(len);
                        for r_idx in 0..len {
                            rules.push(REDUCE_DATA[idx + 2 + r_idx] as usize);
                        }
                        reduce_map.push((term_class, rules));
                        idx += 2 + len;
                    }
                    let can_accept_error = match CAN_ACCEPT_ERROR[i] {
                        0 => ::rusty_lr::TriState::False,
                        1 => ::rusty_lr::TriState::True,
                        2 => ::rusty_lr::TriState::Maybe,
                        _ => unreachable!(),
                    };
                    let intermediate = ::rusty_lr::parser::state::IntermediateState {
                        shift_goto_map_term,
                        shift_goto_map_nonterm,
                        reduce_map,
                        ruleset: Vec::new(),
                        can_accept_error,
                    };
                    state_rows.push(intermediate);
                }
                ::rusty_lr::parser::table::IntermediateTables {
                    state_rows,
                    rules,
                }
                    .into()
            })
    }
    #[doc(hidden)]
    fn __rusty_lr_parser_version() -> (usize, usize, usize) {
        (4, 4, 1)
    }
    #[doc(hidden)]
    fn __rustylr_version() -> (usize, usize, usize) {
        (1, 35, 0)
    }
    #[doc(hidden)]
    fn __rusty_lr_version() -> (usize, usize, usize) {
        (4, 4, 0)
    }
}

// ==============================Generated Codes End===============================
        