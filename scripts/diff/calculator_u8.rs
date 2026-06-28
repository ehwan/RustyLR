
// ================================User Codes Begin================================

// =================================User Codes End=================================
/*
====================================Grammar=====================================

# of terminal classes: 9
# of states: 28

0: Digit -> '0'
1: Digit -> ['1', '2', ..., '6'-'9']
2: Number -> ' '* Digit+ ' '*
3: P -> Number
4: P -> ' '* '(' E ')' ' '*
5: E -> E '+' E
6: E -> E '*' E
7: E -> ' '* '-' E
8: E -> P
9: ' '+ -> ' '
10: ' '+ -> ' '+ ' '
11: ' '* -> ' '+
12: ' '* -> 
13: Digit+ -> Digit
14: Digit+ -> Digit+ Digit
15: Augmented -> VirtualStart(0) E eof

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
    TermClass4,
    TermClass5,
    TermClass6,
    TermClass7,
    TermClass8,
    error,
    eof,
    VirtualStart0,
}
impl TerminalClasses {
    #[inline]
    pub fn from_usize(value: usize) -> Self {
        debug_assert!(
            value < 12usize, "Terminal class index {} is out of bounds (max {})", value,
            12usize
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
            TerminalClasses::TermClass0 => "' '",
            TerminalClasses::TermClass1 => "<Others>",
            TerminalClasses::TermClass2 => "'('",
            TerminalClasses::TermClass3 => "')'",
            TerminalClasses::TermClass4 => "'-'",
            TerminalClasses::TermClass5 => "'0'",
            TerminalClasses::TermClass6 => "['1', '2', ..., '6'-'9']",
            TerminalClasses::TermClass7 => "'*'",
            TerminalClasses::TermClass8 => "'+'",
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
            ' ' => TerminalClasses::TermClass0,
            '(' => TerminalClasses::TermClass2,
            ')' => TerminalClasses::TermClass3,
            '-' => TerminalClasses::TermClass4,
            '0' => TerminalClasses::TermClass5,
            '1'..='9' => TerminalClasses::TermClass6,
            '*' => TerminalClasses::TermClass7,
            '+' => TerminalClasses::TermClass8,
            _ => TerminalClasses::TermClass1,
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
    Digit,
    Number,
    P,
    E,
    __LiteralChar0Plus5,
    __LiteralChar0Star6,
    _DigitPlus8,
    Augmented,
}
impl NonTerminals {
    #[inline]
    pub fn from_usize(value: usize) -> Self {
        debug_assert!(
            value < 8usize, "Non-terminal index {} is out of bounds (max {})", value,
            8usize
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
            NonTerminals::Digit => "Digit",
            NonTerminals::Number => "Number",
            NonTerminals::P => "P",
            NonTerminals::E => "E",
            NonTerminals::__LiteralChar0Plus5 => "' '+",
            NonTerminals::__LiteralChar0Star6 => "' '*",
            NonTerminals::_DigitPlus8 => "Digit+",
            NonTerminals::Augmented => "Augmented",
        }
    }
    fn nonterm_type(&self) -> Option<::rusty_lr::parser::nonterminal::NonTerminalType> {
        match self {
            NonTerminals::Digit => None,
            NonTerminals::Number => None,
            NonTerminals::P => None,
            NonTerminals::E => None,
            NonTerminals::__LiteralChar0Plus5 => {
                Some(::rusty_lr::parser::nonterminal::NonTerminalType::PlusLeft)
            }
            NonTerminals::__LiteralChar0Star6 => {
                Some(::rusty_lr::parser::nonterminal::NonTerminalType::Star)
            }
            NonTerminals::_DigitPlus8 => {
                Some(::rusty_lr::parser::nonterminal::NonTerminalType::PlusLeft)
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
pub enum Data {
    __terminals(char),
    __variant1(i32),
    __variant2(f32),
    __variant3(Vec<char>),
    Empty,
}
impl ::std::fmt::Debug for Data {
    fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
        match self {
            Self::__terminals(..) => f.write_str(stringify!(__terminals)),
            Self::__variant1(..) => f.write_str(stringify!(__variant1)),
            Self::__variant2(..) => f.write_str(stringify!(__variant2)),
            Self::__variant3(..) => f.write_str(stringify!(__variant3)),
            Self::Empty => f.write_str("Empty"),
        }
    }
}
#[doc(hidden)]
#[allow(non_camel_case_types, dead_code)]
pub struct EExtracter;
impl ::rusty_lr::parser::semantic_value::StartExtractor<Data> for EExtracter {
    type StartType = f32;
    const BRANCH_INDEX: u32 = 0u32;
    fn extract(value: Data) -> Option<Self::StartType> {
        #[allow(unreachable_patterns, unused_variables)]
        match value {
            Data::__variant2(val) => Some(val),
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
    ///Digit -> '0'
    #[inline]
    fn reduce_Digit_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<char>,
        data: &mut i32,
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
        let __res = { '0' };
        if __push_data {
            __data_stack.push(Self::__terminals(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///Number -> ' '* Digit+ ' '*
    #[inline]
    fn reduce_Number_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<char>,
        data: &mut i32,
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant3(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        __data_stack.pop();
        let mut Digit = match __data_stack.pop().unwrap() {
            Data::__variant3(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let __res = { Digit.into_iter().collect::<String>().parse().unwrap() };
        if __push_data {
            __data_stack.push(Self::__variant1(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///P -> Number
    #[inline]
    fn reduce_P_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<char>,
        data: &mut i32,
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.pop();
        let mut Number = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => val,
            _ => unreachable!(),
        };
        let __res = { Number as f32 };
        if __push_data {
            __data_stack.push(Self::__variant2(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///P -> ' '* '(' E ')' ' '*
    #[inline]
    fn reduce_P_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<char>,
        data: &mut i32,
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant2(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 3usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 4usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.truncate(__location_stack.len() - 5);
        __data_stack.truncate(__data_stack.len() - 2);
        let mut E = match __data_stack.pop().unwrap() {
            Data::__variant2(val) => val,
            _ => unreachable!(),
        };
        __data_stack.truncate(__data_stack.len() - 2);
        let __res = E;
        if __push_data {
            __data_stack.push(Self::__variant2(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///E -> E '+' E
    #[inline]
    fn reduce_E_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<char>,
        data: &mut i32,
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant2(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant2(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        let mut e2 = match __data_stack.pop().unwrap() {
            Data::__variant2(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut E = match __data_stack.pop().unwrap() {
            Data::__variant2(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            *data += 1;
            println!("{:?} '+' {:?}", E, e2);
            E + e2
        };
        if __push_data {
            __data_stack.push(Self::__variant2(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///E -> E '*' E
    #[inline]
    fn reduce_E_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<char>,
        data: &mut i32,
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant2(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant2(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        let mut e2 = match __data_stack.pop().unwrap() {
            Data::__variant2(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut E = match __data_stack.pop().unwrap() {
            Data::__variant2(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            *data += 1;
            println!("{:?} '*' {:?}", E, e2);
            E * e2
        };
        if __push_data {
            __data_stack.push(Self::__variant2(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///E -> ' '* '-' E
    #[inline]
    fn reduce_E_2(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<char>,
        data: &mut i32,
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant2(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        let mut E = match __data_stack.pop().unwrap() {
            Data::__variant2(val) => val,
            _ => unreachable!(),
        };
        __data_stack.truncate(__data_stack.len() - 2);
        let __res = { -E };
        if __push_data {
            __data_stack.push(Self::__variant2(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///' '+ -> ' '+ ' '
    #[inline]
    fn reduce___LiteralChar0Plus5_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<char>,
        data: &mut i32,
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
        }
        __location_stack.truncate(__location_stack.len() - 2);
        __data_stack.truncate(__data_stack.len() - 2);
        __data_stack.push(Self::Empty);
        Ok(())
    }
    ///' '* -> ' '+
    #[inline]
    fn reduce___LiteralChar0Star6_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<char>,
        data: &mut i32,
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
        __data_stack.push(Self::Empty);
        Ok(())
    }
    ///' '* ->
    #[inline]
    fn reduce___LiteralChar0Star6_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<char>,
        data: &mut i32,
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)] {}
        __data_stack.push(Self::Empty);
        Ok(())
    }
    ///Digit+ -> Digit
    #[inline]
    fn reduce__DigitPlus8_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<char>,
        data: &mut i32,
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.pop();
        let mut A = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        let __res = { vec![A] };
        if __push_data {
            __data_stack.push(Self::__variant3(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///Digit+ -> Digit+ Digit
    #[inline]
    fn reduce__DigitPlus8_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<::rusty_lr::DefaultLocation>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr::TerminalSymbol<char>,
        data: &mut i32,
        __rustylr_location0: &mut ::rusty_lr::DefaultLocation,
    ) -> Result<(), ::rusty_lr::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant3(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 2);
        let mut A = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        let mut Ap = match __data_stack.pop().unwrap() {
            Data::__variant3(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Ap.push(A);
            Ap
        };
        if __push_data {
            __data_stack.push(Self::__variant3(__res));
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
    type Term = char;
    type NonTerm = NonTerminals;
    type ReduceActionError = ::rusty_lr::DefaultReduceActionError;
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
                Self::reduce_Digit_0(
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
                Self::reduce_Number_0(
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
            4usize => {
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
            5usize => {
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
            6usize => {
                Self::reduce_E_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            7usize => {
                Self::reduce_E_2(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            10usize => {
                Self::reduce___LiteralChar0Plus5_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            11usize => {
                Self::reduce___LiteralChar0Star6_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            12usize => {
                Self::reduce___LiteralChar0Star6_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            13usize => {
                Self::reduce__DigitPlus8_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            14usize => {
                Self::reduce__DigitPlus8_1(
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
                static RULE_NAMES: &[u32] = &[
                    0, 0, 1, 2, 2, 3, 3, 3, 3, 4, 4, 5, 5, 6, 6, 7,
                ];
                static RULE_LENGTHS: &[u32] = &[
                    1, 1, 3, 1, 5, 3, 3, 3, 1, 1, 2, 1, 0, 1, 2, 3,
                ];
                static SHIFT_TERM_DATA: &[u32] = &[
                    2147516427, 196608, 131079, 819208, 2148368394, 196608, 229376,
                    294914, 786436, 622597, 2148139014, 196608, 360451, 425991, 491528,
                    196608, 196608, 294914, 557060, 622597, 2148139014, 196608, 425991,
                    196608, 196608, 622597, 2148204550, 196608, 196608, 131079,
                ];
                static SHIFT_TERM_OFFSETS: &[u32] = &[
                    0, 1, 2, 2, 5, 6, 6, 7, 7, 11, 12, 15, 16, 16, 17, 21, 22, 23, 24,
                    24, 24, 24, 27, 27, 27, 28, 29, 30, 30,
                ];
                static SHIFT_NONTERM_DATA: &[u32] = &[
                    2147549185, 2147581954, 2147581955, 2147680260, 2147745797,
                    2147549185, 2147647490, 2147647491, 2147680260, 2147745797,
                    2148139008, 2148171782, 2147549185, 2147811330, 2147811331,
                    2147680260, 2147942405, 2147680260, 2147876869, 2147549185,
                    2147647490, 2147647491, 2147680260, 2147942405, 2148139008,
                    2148171782, 2147549185, 2148007938, 2148007939, 2147680260,
                    2147942405, 2147549185, 2148073474, 2148073475, 2147680260,
                    2147942405, 2148204544, 2147680260, 2148237317, 2147549185,
                    2148073474, 2148073475, 2147680260, 2147745797, 2147549185,
                    2148335618, 2148335619, 2147680260, 2147745797,
                ];
                static SHIFT_NONTERM_OFFSETS: &[u32] = &[
                    0, 0, 5, 5, 5, 10, 10, 10, 10, 12, 17, 17, 19, 19, 24, 26, 31, 31,
                    36, 36, 36, 36, 39, 39, 39, 44, 49, 49, 49,
                ];
                static REDUCE_DATA: &[u32] = &[
                    2, 1, 12, 4, 1, 12, 5, 1, 12, 6, 1, 12, 3, 1, 3, 7, 1, 3, 8, 1, 3,
                    10, 1, 3, 2, 1, 12, 4, 1, 12, 5, 1, 12, 6, 1, 12, 3, 1, 6, 7, 1, 6,
                    8, 1, 6, 10, 1, 6, 2, 1, 11, 3, 1, 11, 4, 1, 11, 5, 1, 11, 6, 1, 11,
                    7, 1, 11, 8, 1, 11, 10, 1, 11, 0, 1, 10, 2, 1, 10, 3, 1, 10, 4, 1,
                    10, 5, 1, 10, 6, 1, 10, 7, 1, 10, 8, 1, 10, 10, 1, 10, 2, 1, 12, 4,
                    1, 12, 5, 1, 12, 6, 1, 12, 3, 1, 12, 7, 1, 12, 8, 1, 12, 10, 1, 12,
                    3, 1, 4, 7, 1, 4, 8, 1, 4, 10, 1, 4, 2, 1, 12, 4, 1, 12, 5, 1, 12, 6,
                    1, 12, 2, 1, 12, 4, 1, 12, 5, 1, 12, 6, 1, 12, 3, 1, 5, 8, 1, 5, 2,
                    1, 12, 4, 1, 12, 5, 1, 12, 6, 1, 12, 3, 1, 7, 7, 1, 7, 8, 1, 7, 10,
                    1, 7, 0, 1, 0, 3, 1, 0, 5, 1, 0, 6, 1, 0, 7, 1, 0, 8, 1, 0, 10, 1, 0,
                    0, 1, 13, 3, 1, 13, 5, 1, 13, 6, 1, 13, 7, 1, 13, 8, 1, 13, 10, 1,
                    13, 3, 1, 12, 7, 1, 12, 8, 1, 12, 10, 1, 12, 0, 1, 14, 3, 1, 14, 5,
                    1, 14, 6, 1, 14, 7, 1, 14, 8, 1, 14, 10, 1, 14, 3, 1, 2, 7, 1, 2, 8,
                    1, 2, 10, 1, 2, 2, 1, 12, 4, 1, 12, 5, 1, 12, 6, 1, 12, 2, 1, 12, 4,
                    1, 12, 5, 1, 12, 6, 1, 12, 8, 1, 5, 10, 1, 5,
                ];
                static REDUCE_OFFSETS: &[u32] = &[
                    0, 0, 12, 24, 24, 36, 48, 72, 99, 99, 111, 111, 123, 135, 147, 147,
                    159, 165, 177, 189, 210, 231, 243, 264, 276, 288, 300, 306, 306,
                ];
                static CAN_ACCEPT_ERROR: &[u8] = &[
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0,
                ];
                let num_rules = 16usize;
                let mut rules = Vec::with_capacity(num_rules);
                for i in 0..num_rules {
                    let lhs = NonTerminals::from_usize(RULE_NAMES[i] as usize);
                    rules
                        .push(::rusty_lr::parser::table::RuleInfo {
                            lhs,
                            len: RULE_LENGTHS[i] as usize,
                        });
                }
                let num_states = 28usize;
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
        (4, 4, 0)
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
        