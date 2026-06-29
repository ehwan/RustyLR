
// ================================User Codes Begin================================
#[derive(Debug, Clone, Copy)]
pub enum Token {
    Num(i32),
    Plus,
    Star,
    LParen,
    RParen,
}

// =================================User Codes End=================================
/*
====================================Grammar=====================================

# of terminal classes: 6
# of states: 14

0: A -> A plus A
1: A -> M
2: M -> M star M
3: M -> P
4: P -> num
5: P -> lparen E rparen
6: E -> A
7: Augmented -> VirtualStart(0) E eof

*/
// =============================Generated Codes Begin==============================
#[allow(non_camel_case_types, dead_code)]
pub type EContext = ::rusty_lr::parser::deterministic::Context<
    Parser,
    Data,
    EExtracter,
    u8,
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
pub type ParseError = ::rusty_lr::parser::deterministic::ParseError<
    Token,
    ::rusty_lr::DefaultLocation,
    String,
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
    num,
    plus,
    star,
    lparen,
    rparen,
    __rustylr_other_terminals,
    error,
    eof,
    VirtualStart0,
}
impl TerminalClasses {
    #[inline]
    pub fn from_usize(value: usize) -> Self {
        debug_assert!(
            value < 9usize, "Terminal class index {} is out of bounds (max {})", value,
            9usize
        );
        unsafe { ::std::mem::transmute(value) }
    }
}
impl ::rusty_lr::parser::terminalclass::TerminalClass for TerminalClasses {
    type Term = Token;
    const ERROR: Self = Self::error;
    const EOF: Self = Self::eof;
    fn as_str(&self) -> &'static str {
        match self {
            TerminalClasses::num => "num",
            TerminalClasses::plus => "plus",
            TerminalClasses::star => "star",
            TerminalClasses::lparen => "lparen",
            TerminalClasses::rparen => "rparen",
            TerminalClasses::__rustylr_other_terminals => "__rustylr_other_terminals",
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
            Token::Num(_) => TerminalClasses::num,
            Token::Plus => TerminalClasses::plus,
            Token::Star => TerminalClasses::star,
            Token::LParen => TerminalClasses::lparen,
            Token::RParen => TerminalClasses::rparen,
            _ => TerminalClasses::__rustylr_other_terminals,
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
    A,
    M,
    P,
    E,
    Augmented,
}
impl NonTerminals {
    #[inline]
    pub fn from_usize(value: usize) -> Self {
        debug_assert!(
            value < 5usize, "Non-terminal index {} is out of bounds (max {})", value,
            5usize
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
            NonTerminals::A => "A",
            NonTerminals::M => "M",
            NonTerminals::P => "P",
            NonTerminals::E => "E",
            NonTerminals::Augmented => "Augmented",
        }
    }
    fn nonterm_type(&self) -> Option<::rusty_lr::parser::nonterminal::NonTerminalType> {
        match self {
            NonTerminals::A => None,
            NonTerminals::M => None,
            NonTerminals::P => None,
            NonTerminals::E => None,
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
pub enum __RustyLRData<__RustyLRData0, __RustyLRData1> {
    __terminals(__RustyLRData0),
    __variant1(__RustyLRData1),
    Empty,
}
pub type Data = __RustyLRData<Token, i32>;
impl ::std::fmt::Debug for Data {
    fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
        match self {
            Self::__terminals(..) => f.write_str(stringify!(__terminals)),
            Self::__variant1(..) => f.write_str(stringify!(__variant1)),
            Self::Empty => f.write_str("Empty"),
        }
    }
}
#[doc(hidden)]
#[allow(non_camel_case_types, dead_code)]
pub struct EExtracter;
impl ::rusty_lr::parser::semantic_value::StartExtractor<Data> for EExtracter {
    type StartType = i32;
    const BRANCH_INDEX: u32 = 0u32;
    fn extract(value: Data) -> Option<Self::StartType> {
        #[allow(unreachable_patterns, unused_variables)]
        match value {
            Data::__variant1(val) => Some(val),
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
    fn custom_reduce_action_0(
        mut M: i32,
        data: &mut i32,
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<i32, String> {
        Ok({ M * 1 })
    }
    ///A -> A plus A
    #[inline]
    fn reduce_A_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut i32,
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), String> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        let mut a2 = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let mut plus = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        let mut A = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            println!("{:?} {:?} {:?}", A, plus, a2);
            *data += 1;
            A + a2
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///A -> M
    #[inline]
    fn reduce_A_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut i32,
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), String> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.pop();
        let mut M = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = M;
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///M -> M star M
    #[inline]
    fn reduce_M_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut i32,
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), String> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        let mut __rustylr_data_2 = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut __rustylr_data_0 = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __rustylr_data_0 = Self::custom_reduce_action_0(
            __rustylr_data_0,
            data,
            __rustylr_location0,
        )?;
        let mut M_optim = __rustylr_data_0;
        let __rustylr_data_2 = Self::custom_reduce_action_0(
            __rustylr_data_2,
            data,
            __rustylr_location0,
        )?;
        let mut m2 = __rustylr_data_2;
        let __res = { M_optim * m2 };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///P -> num
    #[inline]
    fn reduce_P_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut i32,
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), String> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.pop();
        let mut num = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            if let Token::Num(n) = num {
                n
            } else {
                return Err(format!("{:?}", num));
            }
        };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///P -> lparen E rparen
    #[inline]
    fn reduce_P_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut i32,
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), String> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant1(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        __data_stack.pop();
        let mut E = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let __res = E;
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///E -> A
    #[inline]
    fn reduce_E_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<Token>,
        data: &mut i32,
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), String> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.pop();
        let mut A = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = A;
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
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
    type Term = Token;
    type NonTerm = NonTerminals;
    type ReduceActionError = String;
    type UserData = i32;
    type Location = ::rusty_lr::DefaultLocation;
    fn new_empty() -> Self {
        Self::Empty
    }
    fn new_terminal(term: Self::Term) -> Self {
        Self::__terminals(term)
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
                Self::reduce_A_0(
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
                Self::reduce_A_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            2usize => {
                Self::reduce_M_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            4usize => {
                Self::reduce_P_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            5usize => {
                Self::reduce_P_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            6usize => {
                Self::reduce_E_0(
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
    type Term = Token;
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
                static RULE_NAMES: &[u32] = &[0, 0, 1, 1, 2, 2, 3, 4];
                static RULE_LENGTHS: &[u32] = &[3, 1, 3, 1, 1, 3, 1, 3];
                static SHIFT_TERM_DATA: &[u32] = &[
                    2147516424, 2147549184, 98307, 2147549184, 98307, 2147647489,
                    2147549184, 98307, 262146, 2147549184, 98307, 360452, 2147909639,
                ];
                static SHIFT_TERM_OFFSETS: &[u32] = &[
                    0, 1, 3, 3, 5, 6, 8, 8, 9, 11, 11, 12, 12, 13, 13,
                ];
                static SHIFT_NONTERM_DATA: &[u32] = &[
                    2147614720, 2147713025, 2147713026, 2147876867, 2147614720,
                    2147713025, 2147713026, 2147811331, 2147680256, 2147713025,
                    2147713026, 2147778561, 2147778562,
                ];
                static SHIFT_NONTERM_OFFSETS: &[u32] = &[
                    0, 0, 4, 4, 8, 8, 11, 11, 11, 13, 13, 13, 13, 13, 13,
                ];
                static REDUCE_DATA: &[u32] = &[
                    1, 1, 4, 2, 1, 4, 4, 1, 4, 7, 1, 4, 4, 1, 6, 7, 1, 6, 1, 1, 0, 4, 1,
                    0, 7, 1, 0, 1, 1, 1, 4, 1, 1, 7, 1, 1, 1, 1, 2, 2, 1, 2, 4, 1, 2, 7,
                    1, 2, 1, 1, 5, 2, 1, 5, 4, 1, 5, 7, 1, 5,
                ];
                static REDUCE_OFFSETS: &[u32] = &[
                    0, 0, 0, 12, 12, 18, 18, 27, 36, 36, 48, 48, 60, 60, 60,
                ];
                static CAN_ACCEPT_ERROR: &[u8] = &[
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                ];
                let num_rules = 8usize;
                let mut rules = Vec::with_capacity(num_rules);
                for i in 0..num_rules {
                    let lhs = NonTerminals::from_usize(RULE_NAMES[i] as usize);
                    rules
                        .push(::rusty_lr::parser::table::RuleInfo {
                            lhs,
                            len: RULE_LENGTHS[i] as usize,
                        });
                }
                let num_states = 14usize;
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
        (4, 4, 2)
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
        