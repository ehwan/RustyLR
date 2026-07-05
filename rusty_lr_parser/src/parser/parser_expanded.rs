// ================================User Codes Begin================================
use crate::parser::args::AllowTarget;
use crate::parser::args::GrammarArgs;
use crate::parser::args::IdentOrLiteral;
use crate::parser::args::PatternArgs;
use crate::parser::args::PrecDPrecArgs;
use crate::parser::args::RecoveredError;
use crate::parser::args::RuleDefArgs;
use crate::parser::args::RuleLineArgs;
use crate::parser::lexer::Lexed;
use crate::parser::location::Located;
use crate::parser::location::Location;
use crate::terminalset::TerminalSet;
use crate::terminalset::TerminalSetItem;
use proc_macro2::Group;
use proc_macro2::TokenStream;
use quote::ToTokens;
use rusty_lr_core::production::Associativity;
use std::boxed::Box;

// =================================User Codes End=================================
/*
====================================Grammar=====================================

# of terminal classes: 43
# of states: 194

0: Rule -> ident RuleType colon RuleLines semicolon
1: Rule -> ident RuleType colon RuleLines error semicolon
2: RuleType -> parengroup
3: RuleType ->
4: RuleLines -> RuleLines pipe RuleLine
5: RuleLines -> RuleLine
6: RuleLine -> MappedSymbol* PrecDef* Action
7: PrecDef -> percent prec IdentOrLiteral
8: PrecDef -> percent prec error
9: PrecDef -> percent dprec int_literal
10: PrecDef -> percent dprec error
11: PrecDef -> percent error
12: MappedSymbol -> Pattern
13: MappedSymbol -> ident equal Pattern
14: MappedSymbol -> ident equal error
15: TerminalSetItem -> ident
16: TerminalSetItem -> ident minus ident
17: TerminalSetItem -> ident minus error
18: TerminalSetItem -> char_literal
19: TerminalSetItem -> char_literal minus char_literal
20: TerminalSetItem -> char_literal minus error
21: TerminalSetItem -> byte_literal
22: TerminalSetItem -> byte_literal minus byte_literal
23: TerminalSetItem -> byte_literal minus error
24: TerminalSet -> lbracket caret? TerminalSetItem* rbracket
25: TerminalSet -> lbracket caret? error rbracket
26: TerminalSet -> dot
27: Pattern -> ident
28: Pattern -> Pattern plus
29: Pattern -> Pattern star
30: Pattern -> Pattern question
31: Pattern -> Pattern exclamation
32: Pattern -> TerminalSet
33: Pattern -> lparen $sep(Pattern*, pipe, +) rparen
34: Pattern -> lparen error rparen
35: Pattern -> byte_literal
36: Pattern -> byte_str_literal
37: Pattern -> char_literal
38: Pattern -> str_literal
39: Pattern -> Pattern minus Pattern
40: Pattern -> dollar ident lparen Pattern comma Pattern comma? rparen
41: Pattern -> dollar ident lparen Pattern comma Pattern comma plus rparen
42: Pattern -> dollar ident lparen Pattern comma Pattern comma star rparen
43: Pattern -> dollar ident lparen Pattern comma Pattern error rparen
44: Pattern -> dollar ident lparen Pattern comma Pattern comma error rparen
45: Action -> bracegroup
46: Action -> error
47: Action ->
48: IdentOrLiteral -> ident
49: IdentOrLiteral -> byte_literal
50: IdentOrLiteral -> char_literal
51: AllowTarget -> ident
52: AllowTarget -> byte_literal
53: AllowTarget -> char_literal
54: AllowTarget -> char_literal minus char_literal
55: AllowTarget -> byte_literal minus byte_literal
56: AllowTarget -> TerminalSet
57: Directive -> percent token ident [^semicolon]+ semicolon
58: Directive -> percent token ident semicolon
59: Directive -> percent token error semicolon
60: Directive -> percent start ident semicolon
61: Directive -> percent start error semicolon
62: Directive -> percent tokentype [^semicolon]+ semicolon
63: Directive -> percent tokentype semicolon
64: Directive -> percent userdata [^semicolon]+ semicolon
65: Directive -> percent userdata semicolon
66: Directive -> percent left IdentOrLiteral+ semicolon
67: Directive -> percent left error semicolon
68: Directive -> percent right IdentOrLiteral+ semicolon
69: Directive -> percent right error semicolon
70: Directive -> percent precedence IdentOrLiteral+ semicolon
71: Directive -> percent precedence error semicolon
72: Directive -> percent errortype [^semicolon]+ semicolon
73: Directive -> percent errortype semicolon
74: Directive -> percent moduleprefix [^semicolon]+ semicolon
75: Directive -> percent moduleprefix semicolon
76: Directive -> percent glr semicolon
77: Directive -> percent glr error semicolon
78: Directive -> percent lalr semicolon
79: Directive -> percent lalr error semicolon
80: Directive -> percent nooptim semicolon
81: Directive -> percent nooptim error semicolon
82: Directive -> percent location [^semicolon]+ semicolon
83: Directive -> percent location semicolon
84: Directive -> percent allow ident semicolon
85: Directive -> percent allow ident lparen AllowTarget rparen semicolon
86: Directive -> percent allow ident lparen error rparen semicolon
87: Directive -> percent allow error semicolon
88: Directive -> percent error semicolon
89: GrammarLine -> Rule
90: GrammarLine -> Directive
91: Grammar -> GrammarLine+
92: MappedSymbol+ -> MappedSymbol
93: MappedSymbol+ -> MappedSymbol+ MappedSymbol
94: MappedSymbol* -> MappedSymbol+
95: MappedSymbol* ->
96: PrecDef+ -> PrecDef
97: PrecDef+ -> PrecDef+ PrecDef
98: PrecDef* -> PrecDef+
99: PrecDef* ->
100: caret? -> caret
101: caret? ->
102: TerminalSetItem+ -> TerminalSetItem
103: TerminalSetItem+ -> TerminalSetItem+ TerminalSetItem
104: TerminalSetItem* -> TerminalSetItem+
105: TerminalSetItem* ->
106: Pattern+ -> Pattern
107: Pattern+ -> Pattern+ Pattern
108: Pattern* -> Pattern+
109: Pattern* ->
110: $sep(Pattern*, pipe, +) -> Pattern*
111: $sep(Pattern*, pipe, +) -> $sep(Pattern*, pipe, +) pipe Pattern*
112: comma? -> comma
113: comma? ->
114: [^semicolon] -> ident
115: [^semicolon] -> colon
116: [^semicolon] -> pipe
117: [^semicolon] -> percent
118: [^semicolon] -> equal
119: [^semicolon] -> plus
120: [^semicolon] -> star
121: [^semicolon] -> question
122: [^semicolon] -> caret
123: [^semicolon] -> minus
124: [^semicolon] -> exclamation
125: [^semicolon] -> dot
126: [^semicolon] -> dollar
127: [^semicolon] -> comma
128: [^semicolon] -> int_literal
129: [^semicolon] -> byte_literal
130: [^semicolon] -> byte_str_literal
131: [^semicolon] -> char_literal
132: [^semicolon] -> str_literal
133: [^semicolon] -> [other_literal, <Others>]
134: [^semicolon] -> parengroup
135: [^semicolon] -> bracegroup
136: [^semicolon] -> lparen
137: [^semicolon] -> rparen
138: [^semicolon] -> lbracket
139: [^semicolon] -> rbracket
140: [^semicolon] -> left
141: [^semicolon] -> right
142: [^semicolon] -> token
143: [^semicolon] -> start
144: [^semicolon] -> tokentype
145: [^semicolon] -> userdata
146: [^semicolon] -> errortype
147: [^semicolon] -> moduleprefix
148: [^semicolon] -> lalr
149: [^semicolon] -> glr
150: [^semicolon] -> prec
151: [^semicolon] -> precedence
152: [^semicolon] -> nooptim
153: [^semicolon] -> dprec
154: [^semicolon] -> location
155: [^semicolon] -> allow
156: [^semicolon]+ -> [^semicolon]
157: [^semicolon]+ -> [^semicolon]+ [^semicolon]
158: IdentOrLiteral+ -> IdentOrLiteral
159: IdentOrLiteral+ -> IdentOrLiteral+ IdentOrLiteral
160: GrammarLine+ -> GrammarLine
161: GrammarLine+ -> GrammarLine GrammarLine+
162: Augmented -> VirtualStart(0) Grammar eof

*/
// =============================Generated Codes Begin==============================
#[allow(non_camel_case_types, dead_code)]
pub type GrammarContext =
    ::rusty_lr_core::parser::deterministic::Context<Parser, Data, GrammarExtracter, u8>;
#[allow(non_camel_case_types, dead_code)]
pub type Rule = ::rusty_lr_core::production::Production<TerminalClasses, NonTerminals>;
#[allow(non_camel_case_types, dead_code)]
pub type Tables =
    ::rusty_lr_core::parser::table::SparseFlatTables<TerminalClasses, NonTerminals, u8, u8>;
#[allow(non_camel_case_types, dead_code)]
pub type ParseError = ::rusty_lr_core::parser::deterministic::ParseError<
    Lexed,
    Location,
    ::rusty_lr_core::DefaultReduceActionError,
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
    std::cmp::Ord,
)]
#[repr(usize)]
pub enum TerminalClasses {
    ident,
    TermClass1,
    colon,
    pipe,
    percent,
    equal,
    caret,
    dot,
    dollar,
    comma,
    int_literal,
    byte_literal,
    byte_str_literal,
    char_literal,
    str_literal,
    parengroup,
    bracegroup,
    lparen,
    rparen,
    lbracket,
    rbracket,
    left,
    right,
    token,
    start,
    tokentype,
    userdata,
    errortype,
    moduleprefix,
    lalr,
    glr,
    prec,
    precedence,
    nooptim,
    dprec,
    location,
    allow,
    semicolon,
    plus,
    star,
    question,
    exclamation,
    minus,
    error,
    eof,
    VirtualStart0,
}
impl TerminalClasses {
    #[inline]
    pub fn from_usize(value: usize) -> Self {
        debug_assert!(
            value < 46usize,
            "Terminal class index {} is out of bounds (max {})",
            value,
            46usize
        );
        unsafe { ::std::mem::transmute(value) }
    }
}
impl ::rusty_lr_core::parser::terminalclass::TerminalClass for TerminalClasses {
    type Term = Lexed;
    const ERROR: Self = Self::error;
    const EOF: Self = Self::eof;
    fn as_str(&self) -> &'static str {
        match self {
            TerminalClasses::ident => "ident",
            TerminalClasses::TermClass1 => "[other_literal, <Others>]",
            TerminalClasses::colon => "colon",
            TerminalClasses::pipe => "pipe",
            TerminalClasses::percent => "percent",
            TerminalClasses::equal => "equal",
            TerminalClasses::caret => "caret",
            TerminalClasses::dot => "dot",
            TerminalClasses::dollar => "dollar",
            TerminalClasses::comma => "comma",
            TerminalClasses::int_literal => "int_literal",
            TerminalClasses::byte_literal => "byte_literal",
            TerminalClasses::byte_str_literal => "byte_str_literal",
            TerminalClasses::char_literal => "char_literal",
            TerminalClasses::str_literal => "str_literal",
            TerminalClasses::parengroup => "parengroup",
            TerminalClasses::bracegroup => "bracegroup",
            TerminalClasses::lparen => "lparen",
            TerminalClasses::rparen => "rparen",
            TerminalClasses::lbracket => "lbracket",
            TerminalClasses::rbracket => "rbracket",
            TerminalClasses::left => "left",
            TerminalClasses::right => "right",
            TerminalClasses::token => "token",
            TerminalClasses::start => "start",
            TerminalClasses::tokentype => "tokentype",
            TerminalClasses::userdata => "userdata",
            TerminalClasses::errortype => "errortype",
            TerminalClasses::moduleprefix => "moduleprefix",
            TerminalClasses::lalr => "lalr",
            TerminalClasses::glr => "glr",
            TerminalClasses::prec => "prec",
            TerminalClasses::precedence => "precedence",
            TerminalClasses::nooptim => "nooptim",
            TerminalClasses::dprec => "dprec",
            TerminalClasses::location => "location",
            TerminalClasses::allow => "allow",
            TerminalClasses::semicolon => "semicolon",
            TerminalClasses::plus => "plus",
            TerminalClasses::star => "star",
            TerminalClasses::question => "question",
            TerminalClasses::exclamation => "exclamation",
            TerminalClasses::minus => "minus",
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
            Lexed::Ident(ident) => TerminalClasses::ident,
            Lexed::Colon(_) => TerminalClasses::colon,
            Lexed::Pipe(_) => TerminalClasses::pipe,
            Lexed::Percent(_) => TerminalClasses::percent,
            Lexed::Equal(_) => TerminalClasses::equal,
            Lexed::Caret(_) => TerminalClasses::caret,
            Lexed::Dot(_) => TerminalClasses::dot,
            Lexed::Dollar(_) => TerminalClasses::dollar,
            Lexed::Comma(_) => TerminalClasses::comma,
            Lexed::IntLiteral(_) => TerminalClasses::int_literal,
            Lexed::ByteLiteral(_) => TerminalClasses::byte_literal,
            Lexed::ByteStrLiteral(_) => TerminalClasses::byte_str_literal,
            Lexed::CharLiteral(_) => TerminalClasses::char_literal,
            Lexed::StrLiteral(_) => TerminalClasses::str_literal,
            Lexed::ParenGroup(parengroup) => TerminalClasses::parengroup,
            Lexed::BraceGroup(bracegroup) => TerminalClasses::bracegroup,
            Lexed::LParen => TerminalClasses::lparen,
            Lexed::RParen => TerminalClasses::rparen,
            Lexed::LBracket => TerminalClasses::lbracket,
            Lexed::RBracket => TerminalClasses::rbracket,
            Lexed::Left(_) => TerminalClasses::left,
            Lexed::Right(_) => TerminalClasses::right,
            Lexed::Token(_) => TerminalClasses::token,
            Lexed::Start(_) => TerminalClasses::start,
            Lexed::TokenType(_) => TerminalClasses::tokentype,
            Lexed::UserData(_) => TerminalClasses::userdata,
            Lexed::ErrorType(_) => TerminalClasses::errortype,
            Lexed::ModulePrefix(_) => TerminalClasses::moduleprefix,
            Lexed::Lalr(_) => TerminalClasses::lalr,
            Lexed::Glr(_) => TerminalClasses::glr,
            Lexed::Prec(_) => TerminalClasses::prec,
            Lexed::Precedence(_) => TerminalClasses::precedence,
            Lexed::NoOptim(_) => TerminalClasses::nooptim,
            Lexed::DPrec(_) => TerminalClasses::dprec,
            Lexed::Location(_) => TerminalClasses::location,
            Lexed::Allow(_) => TerminalClasses::allow,
            Lexed::Semicolon(_) => TerminalClasses::semicolon,
            Lexed::Plus(_) => TerminalClasses::plus,
            Lexed::Star(_) => TerminalClasses::star,
            Lexed::Question(_) => TerminalClasses::question,
            Lexed::Exclamation(_) => TerminalClasses::exclamation,
            Lexed::Minus(_) => TerminalClasses::minus,
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
        use ::rusty_lr_core::parser::terminalclass::TerminalClass;
        write!(f, "{}", self.as_str())
    }
}
impl std::fmt::Debug for TerminalClasses {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ::rusty_lr_core::parser::terminalclass::TerminalClass;
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
    std::cmp::Ord,
)]
#[repr(usize)]
pub enum NonTerminals {
    Rule,
    RuleType,
    RuleLines,
    RuleLine,
    PrecDef,
    MappedSymbol,
    TerminalSetItem,
    TerminalSet,
    Pattern,
    Action,
    IdentOrLiteral,
    AllowTarget,
    Directive,
    GrammarLine,
    Grammar,
    _MappedSymbolPlus16,
    _MappedSymbolStar17,
    _PrecDefPlus18,
    _PrecDefStar19,
    _caretQuestion20,
    _TerminalSetItemPlus21,
    _TerminalSetItemStar22,
    _PatternPlus23,
    _PatternStar24,
    __PatternStar24SepPlus25,
    _commaQuestion26,
    _TermSet27,
    __TermSet27Plus28,
    _IdentOrLiteralPlus29,
    _GrammarLinePlus30,
    Augmented,
}
impl NonTerminals {
    #[inline]
    pub fn from_usize(value: usize) -> Self {
        debug_assert!(
            value < 31usize,
            "Non-terminal index {} is out of bounds (max {})",
            value,
            31usize
        );
        unsafe { ::std::mem::transmute(value) }
    }
}
impl std::fmt::Display for NonTerminals {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ::rusty_lr_core::parser::nonterminal::NonTerminal;
        write!(f, "{}", self.as_str())
    }
}
impl std::fmt::Debug for NonTerminals {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ::rusty_lr_core::parser::nonterminal::NonTerminal;
        write!(f, "{}", self.as_str())
    }
}
impl ::rusty_lr_core::parser::nonterminal::NonTerminal for NonTerminals {
    fn as_str(&self) -> &'static str {
        match self {
            NonTerminals::Rule => "Rule",
            NonTerminals::RuleType => "RuleType",
            NonTerminals::RuleLines => "RuleLines",
            NonTerminals::RuleLine => "RuleLine",
            NonTerminals::PrecDef => "PrecDef",
            NonTerminals::MappedSymbol => "MappedSymbol",
            NonTerminals::TerminalSetItem => "TerminalSetItem",
            NonTerminals::TerminalSet => "TerminalSet",
            NonTerminals::Pattern => "Pattern",
            NonTerminals::Action => "Action",
            NonTerminals::IdentOrLiteral => "IdentOrLiteral",
            NonTerminals::AllowTarget => "AllowTarget",
            NonTerminals::Directive => "Directive",
            NonTerminals::GrammarLine => "GrammarLine",
            NonTerminals::Grammar => "Grammar",
            NonTerminals::_MappedSymbolPlus16 => "MappedSymbol+",
            NonTerminals::_MappedSymbolStar17 => "MappedSymbol*",
            NonTerminals::_PrecDefPlus18 => "PrecDef+",
            NonTerminals::_PrecDefStar19 => "PrecDef*",
            NonTerminals::_caretQuestion20 => "caret?",
            NonTerminals::_TerminalSetItemPlus21 => "TerminalSetItem+",
            NonTerminals::_TerminalSetItemStar22 => "TerminalSetItem*",
            NonTerminals::_PatternPlus23 => "Pattern+",
            NonTerminals::_PatternStar24 => "Pattern*",
            NonTerminals::__PatternStar24SepPlus25 => "$sep(Pattern*, pipe, +)",
            NonTerminals::_commaQuestion26 => "comma?",
            NonTerminals::_TermSet27 => "[^semicolon]",
            NonTerminals::__TermSet27Plus28 => "[^semicolon]+",
            NonTerminals::_IdentOrLiteralPlus29 => "IdentOrLiteral+",
            NonTerminals::_GrammarLinePlus30 => "GrammarLine+",
            NonTerminals::Augmented => "Augmented",
        }
    }
    fn nonterm_type(&self) -> Option<::rusty_lr_core::parser::nonterminal::NonTerminalType> {
        match self {
            NonTerminals::Rule => None,
            NonTerminals::RuleType => None,
            NonTerminals::RuleLines => None,
            NonTerminals::RuleLine => None,
            NonTerminals::PrecDef => None,
            NonTerminals::MappedSymbol => None,
            NonTerminals::TerminalSetItem => None,
            NonTerminals::TerminalSet => None,
            NonTerminals::Pattern => None,
            NonTerminals::Action => None,
            NonTerminals::IdentOrLiteral => None,
            NonTerminals::AllowTarget => None,
            NonTerminals::Directive => None,
            NonTerminals::GrammarLine => None,
            NonTerminals::Grammar => None,
            NonTerminals::_MappedSymbolPlus16 => {
                Some(::rusty_lr_core::parser::nonterminal::NonTerminalType::PlusLeft)
            }
            NonTerminals::_MappedSymbolStar17 => {
                Some(::rusty_lr_core::parser::nonterminal::NonTerminalType::Star)
            }
            NonTerminals::_PrecDefPlus18 => {
                Some(::rusty_lr_core::parser::nonterminal::NonTerminalType::PlusLeft)
            }
            NonTerminals::_PrecDefStar19 => {
                Some(::rusty_lr_core::parser::nonterminal::NonTerminalType::Star)
            }
            NonTerminals::_caretQuestion20 => {
                Some(::rusty_lr_core::parser::nonterminal::NonTerminalType::Optional)
            }
            NonTerminals::_TerminalSetItemPlus21 => {
                Some(::rusty_lr_core::parser::nonterminal::NonTerminalType::PlusLeft)
            }
            NonTerminals::_TerminalSetItemStar22 => {
                Some(::rusty_lr_core::parser::nonterminal::NonTerminalType::Star)
            }
            NonTerminals::_PatternPlus23 => {
                Some(::rusty_lr_core::parser::nonterminal::NonTerminalType::PlusLeft)
            }
            NonTerminals::_PatternStar24 => {
                Some(::rusty_lr_core::parser::nonterminal::NonTerminalType::Star)
            }
            NonTerminals::__PatternStar24SepPlus25 => {
                Some(::rusty_lr_core::parser::nonterminal::NonTerminalType::PlusLeft)
            }
            NonTerminals::_commaQuestion26 => {
                Some(::rusty_lr_core::parser::nonterminal::NonTerminalType::Optional)
            }
            NonTerminals::_TermSet27 => {
                Some(::rusty_lr_core::parser::nonterminal::NonTerminalType::TerminalSet)
            }
            NonTerminals::__TermSet27Plus28 => {
                Some(::rusty_lr_core::parser::nonterminal::NonTerminalType::PlusLeft)
            }
            NonTerminals::_IdentOrLiteralPlus29 => {
                Some(::rusty_lr_core::parser::nonterminal::NonTerminalType::PlusLeft)
            }
            NonTerminals::_GrammarLinePlus30 => {
                Some(::rusty_lr_core::parser::nonterminal::NonTerminalType::PlusRight)
            }
            NonTerminals::Augmented => {
                Some(::rusty_lr_core::parser::nonterminal::NonTerminalType::Augmented)
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
pub enum __RustyLRData<
    __RustyLRData0,
    __RustyLRData1,
    __RustyLRData2,
    __RustyLRData3,
    __RustyLRData4,
    __RustyLRData5,
    __RustyLRData6,
    __RustyLRData7,
    __RustyLRData8,
    __RustyLRData9,
    __RustyLRData10,
    __RustyLRData11,
    __RustyLRData12,
    __RustyLRData13,
    __RustyLRData14,
    __RustyLRData15,
    __RustyLRData16,
    __RustyLRData17,
    __RustyLRData18,
    __RustyLRData19,
> {
    __terminals(__RustyLRData0),
    __variant1(__RustyLRData1),
    __variant2(__RustyLRData2),
    __variant3(__RustyLRData3),
    __variant4(__RustyLRData4),
    __variant5(__RustyLRData5),
    __variant6(__RustyLRData6),
    __variant7(__RustyLRData7),
    __variant8(__RustyLRData8),
    __variant9(__RustyLRData9),
    __variant10(__RustyLRData10),
    __variant11(__RustyLRData11),
    __variant12(__RustyLRData12),
    __variant13(__RustyLRData13),
    __variant14(__RustyLRData14),
    __variant15(__RustyLRData15),
    __variant16(__RustyLRData16),
    __variant17(__RustyLRData17),
    __variant18(__RustyLRData18),
    __variant19(__RustyLRData19),
    Empty,
}
pub type Data = __RustyLRData<
    Lexed,
    ::std::boxed::Box<RuleDefArgs>,
    Option<Group>,
    Vec<RuleLineArgs>,
    ::std::boxed::Box<RuleLineArgs>,
    ::std::boxed::Box<PrecDPrecArgs>,
    ::std::boxed::Box<(Option<Located<String>>, PatternArgs)>,
    TerminalSetItem,
    TerminalSet,
    PatternArgs,
    IdentOrLiteral,
    AllowTarget,
    Vec<(Option<Located<String>>, PatternArgs)>,
    Vec<PrecDPrecArgs>,
    Option<Lexed>,
    Vec<TerminalSetItem>,
    Vec<PatternArgs>,
    Vec<Vec<PatternArgs>>,
    Vec<Lexed>,
    Vec<IdentOrLiteral>,
>;
impl ::std::fmt::Debug for Data {
    fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
        match self {
            Self::__terminals(..) => f.write_str(stringify!(__terminals)),
            Self::__variant1(..) => f.write_str(stringify!(__variant1)),
            Self::__variant2(..) => f.write_str(stringify!(__variant2)),
            Self::__variant3(..) => f.write_str(stringify!(__variant3)),
            Self::__variant4(..) => f.write_str(stringify!(__variant4)),
            Self::__variant5(..) => f.write_str(stringify!(__variant5)),
            Self::__variant6(..) => f.write_str(stringify!(__variant6)),
            Self::__variant7(..) => f.write_str(stringify!(__variant7)),
            Self::__variant8(..) => f.write_str(stringify!(__variant8)),
            Self::__variant9(..) => f.write_str(stringify!(__variant9)),
            Self::__variant10(..) => f.write_str(stringify!(__variant10)),
            Self::__variant11(..) => f.write_str(stringify!(__variant11)),
            Self::__variant12(..) => f.write_str(stringify!(__variant12)),
            Self::__variant13(..) => f.write_str(stringify!(__variant13)),
            Self::__variant14(..) => f.write_str(stringify!(__variant14)),
            Self::__variant15(..) => f.write_str(stringify!(__variant15)),
            Self::__variant16(..) => f.write_str(stringify!(__variant16)),
            Self::__variant17(..) => f.write_str(stringify!(__variant17)),
            Self::__variant18(..) => f.write_str(stringify!(__variant18)),
            Self::__variant19(..) => f.write_str(stringify!(__variant19)),
            Self::Empty => f.write_str("Empty"),
        }
    }
}
#[doc(hidden)]
#[allow(non_camel_case_types, dead_code)]
pub struct GrammarExtracter;
impl ::rusty_lr_core::parser::semantic_value::StartExtractor<Data> for GrammarExtracter {
    type StartType = ();
    const BRANCH_INDEX: u32 = 0u32;
    fn extract(value: Data) -> Option<Self::StartType> {
        #[allow(unreachable_patterns, unused_variables)]
        match value {
            Data::Empty => Some(()),
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
        mut t: Vec<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<TokenStream, ::rusty_lr_core::DefaultReduceActionError> {
        Ok({
            let mut tokens = TokenStream::new();
            for token in t.into_iter() {
                token.append_to_stream(&mut tokens);
            }
            tokens
        })
    }
    ///Rule -> ident RuleType colon RuleLines semicolon
    #[inline]
    fn reduce_Rule_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
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
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 3usize), Some(&
                Data::__variant2(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 4usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 2);
        let mut __rustylr_location_colon = __location_stack.pop().unwrap();
        __location_stack.pop();
        let mut __rustylr_location_ident = __location_stack.pop().unwrap();
        __data_stack.pop();
        let mut RuleLines = match __data_stack.pop().unwrap() {
            Data::__variant3(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut RuleType = match __data_stack.pop().unwrap() {
            Data::__variant2(val) => val,
            _ => unreachable!(),
        };
        let mut ident = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            let Lexed::Ident(ident) = ident else {
                unreachable!("Rule-Ident");
            };
            if let Some(first) = RuleLines.first_mut() {
                first.separator_location = __rustylr_location_colon;
            }
            RuleDefArgs {
                name: Located::new(ident.to_string(), __rustylr_location_ident),
                typename: RuleType.map(|t| t.stream()),
                rule_lines: RuleLines,
            }
        };
        if __push_data {
            __data_stack.push(Self::__variant1(::std::boxed::Box::new(__res)));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///Rule -> ident RuleType colon RuleLines error semicolon
    #[inline]
    fn reduce_Rule_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
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
                Data::__variant3(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 3usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 4usize), Some(&
                Data::__variant2(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 5usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.pop();
        let mut __rustylr_location_error = __location_stack.pop().unwrap();
        __location_stack.pop();
        let mut __rustylr_location_colon = __location_stack.pop().unwrap();
        __location_stack.pop();
        let mut __rustylr_location_ident = __location_stack.pop().unwrap();
        __data_stack.truncate(__data_stack.len() - 2);
        let mut RuleLines = match __data_stack.pop().unwrap() {
            Data::__variant3(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut RuleType = match __data_stack.pop().unwrap() {
            Data::__variant2(val) => val,
            _ => unreachable!(),
        };
        let mut ident = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            let Lexed::Ident(ident) = ident else {
                unreachable!("Rule-Ident");
            };
            data.error_recovered
                .push(RecoveredError {
                    message: "Expected semicolon or rule alternative".to_string(),
                    link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#production-rules"
                        .to_string(),
                    location: __rustylr_location_error,
                });
            if let Some(first) = RuleLines.first_mut() {
                first.separator_location = __rustylr_location_colon;
            }
            RuleDefArgs {
                name: Located::new(ident.to_string(), __rustylr_location_ident),
                typename: RuleType.map(|t| t.stream()),
                rule_lines: RuleLines,
            }
        };
        if __push_data {
            __data_stack.push(Self::__variant1(::std::boxed::Box::new(__res)));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///RuleType -> parengroup
    #[inline]
    fn reduce_RuleType_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.pop();
        let mut parengroup = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            let Lexed::ParenGroup(parengroup) = parengroup else {
                unreachable!("RuleType - Group");
            };
            Some(parengroup)
        };
        if __push_data {
            __data_stack.push(Self::__variant2(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///RuleType ->
    #[inline]
    fn reduce_RuleType_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)] {}
        let __res = { None };
        if __push_data {
            __data_stack.push(Self::__variant2(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///RuleLines -> RuleLines pipe RuleLine
    #[inline]
    fn reduce_RuleLines_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant4(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant3(_)))
            );
        }
        __location_stack.pop();
        let mut __rustylr_location_pipe = __location_stack.pop().unwrap();
        __location_stack.pop();
        let mut RuleLine = match __data_stack.pop().unwrap() {
            Data::__variant4(val) => *val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut RuleLines = match __data_stack.pop().unwrap() {
            Data::__variant3(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            RuleLine.separator_location = __rustylr_location_pipe;
            RuleLines.push(RuleLine);
            RuleLines
        };
        if __push_data {
            __data_stack.push(Self::__variant3(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///RuleLines -> RuleLine
    #[inline]
    fn reduce_RuleLines_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant4(_)))
            );
        }
        __location_stack.pop();
        let mut RuleLine = match __data_stack.pop().unwrap() {
            Data::__variant4(val) => *val,
            _ => unreachable!(),
        };
        let __res = { vec![RuleLine] };
        if __push_data {
            __data_stack.push(Self::__variant3(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///RuleLine -> MappedSymbol* PrecDef* Action
    #[inline]
    fn reduce_RuleLine_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant2(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant13(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant12(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        let mut Action = match __data_stack.pop().unwrap() {
            Data::__variant2(val) => val,
            _ => unreachable!(),
        };
        let mut PrecDef = match __data_stack.pop().unwrap() {
            Data::__variant13(val) => val,
            _ => unreachable!(),
        };
        let mut MappedSymbol = match __data_stack.pop().unwrap() {
            Data::__variant12(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            RuleLineArgs {
                tokens: MappedSymbol,
                reduce_action: Action.map(|action| action.to_token_stream()),
                separator_location: Location::default(),
                precs: PrecDef,
            }
        };
        if __push_data {
            __data_stack.push(Self::__variant4(::std::boxed::Box::new(__res)));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///PrecDef -> percent prec IdentOrLiteral
    #[inline]
    fn reduce_PrecDef_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant10(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        let mut IdentOrLiteral = match __data_stack.pop().unwrap() {
            Data::__variant10(val) => val,
            _ => unreachable!(),
        };
        __data_stack.truncate(__data_stack.len() - 2);
        let __res = { PrecDPrecArgs::Prec(IdentOrLiteral) };
        if __push_data {
            __data_stack.push(Self::__variant5(::std::boxed::Box::new(__res)));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///PrecDef -> percent prec error
    #[inline]
    fn reduce_PrecDef_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__terminals(_)))
            );
        }
        let mut __rustylr_location_error = __location_stack.pop().unwrap();
        __location_stack.truncate(__location_stack.len() - 2);
        __data_stack.truncate(__data_stack.len() - 3);
        let __res = {
            data.error_recovered
                .push(RecoveredError {
                    message: "Expected <ident> to token or <literal>".to_string(),
                    link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#operator-precedence"
                        .to_string(),
                    location: __rustylr_location_error,
                });
            PrecDPrecArgs::None
        };
        if __push_data {
            __data_stack.push(Self::__variant5(::std::boxed::Box::new(__res)));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///PrecDef -> percent dprec int_literal
    #[inline]
    fn reduce_PrecDef_2(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__terminals(_)))
            );
        }
        let mut __rustylr_location_int_literal = __location_stack.pop().unwrap();
        __location_stack.truncate(__location_stack.len() - 2);
        let mut int_literal = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        __data_stack.truncate(__data_stack.len() - 2);
        let __res = {
            let Lexed::IntLiteral(i) = int_literal else {
                unreachable!("PrecDPrecArgs-DPrec");
            };
            PrecDPrecArgs::DPrec(Located::new(i, __rustylr_location_int_literal))
        };
        if __push_data {
            __data_stack.push(Self::__variant5(::std::boxed::Box::new(__res)));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///PrecDef -> percent dprec error
    #[inline]
    fn reduce_PrecDef_3(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__terminals(_)))
            );
        }
        let mut __rustylr_location_error = __location_stack.pop().unwrap();
        __location_stack.truncate(__location_stack.len() - 2);
        __data_stack.truncate(__data_stack.len() - 3);
        let __res = {
            data.error_recovered
                .push(RecoveredError {
                    message: "Expected integer literal".to_string(),
                    link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#rule-priority"
                        .to_string(),
                    location: __rustylr_location_error,
                });
            PrecDPrecArgs::None
        };
        if __push_data {
            __data_stack.push(Self::__variant5(::std::boxed::Box::new(__res)));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///PrecDef -> percent error
    #[inline]
    fn reduce_PrecDef_4(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__terminals(_)))
            );
        }
        let mut __rustylr_location_error = __location_stack.pop().unwrap();
        __location_stack.pop();
        __data_stack.truncate(__data_stack.len() - 2);
        let __res = {
            data.error_recovered
                .push(RecoveredError {
                    message: "Expected %prec or %dprec".to_string(),
                    link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#operator-precedence"
                        .to_string(),
                    location: __rustylr_location_error,
                });
            PrecDPrecArgs::None
        };
        if __push_data {
            __data_stack.push(Self::__variant5(::std::boxed::Box::new(__res)));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///MappedSymbol -> Pattern
    #[inline]
    fn reduce_MappedSymbol_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant9(_)))
            );
        }
        __location_stack.pop();
        let mut Pattern = match __data_stack.pop().unwrap() {
            Data::__variant9(val) => val,
            _ => unreachable!(),
        };
        let __res = { (None, Pattern) };
        if __push_data {
            __data_stack.push(Self::__variant6(::std::boxed::Box::new(__res)));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///MappedSymbol -> ident equal Pattern
    #[inline]
    fn reduce_MappedSymbol_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant9(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 2);
        let mut __rustylr_location_ident = __location_stack.pop().unwrap();
        let mut Pattern = match __data_stack.pop().unwrap() {
            Data::__variant9(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut ident = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            let Lexed::Ident(ident) = ident else {
                unreachable!("Token-Ident");
            };
            (Some(Located::new(ident.to_string(), __rustylr_location_ident)), Pattern)
        };
        if __push_data {
            __data_stack.push(Self::__variant6(::std::boxed::Box::new(__res)));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///MappedSymbol -> ident equal error
    #[inline]
    fn reduce_MappedSymbol_2(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__terminals(_)))
            );
        }
        let mut __rustylr_location_error = __location_stack.pop().unwrap();
        __location_stack.pop();
        let mut __rustylr_location_ident = __location_stack.pop().unwrap();
        __data_stack.truncate(__data_stack.len() - 2);
        let mut ident = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            let Lexed::Ident(ident) = ident else {
                unreachable!("Token-Ident");
            };
            data.error_recovered
                .push(RecoveredError {
                    message: "Expected pattern after symbol binding".to_string(),
                    link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#patterns"
                        .to_string(),
                    location: __rustylr_location_error,
                });
            (
                Some(Located::new(ident.to_string(), __rustylr_location_ident)),
                PatternArgs::Ident(Default::default()),
            )
        };
        if __push_data {
            __data_stack.push(Self::__variant6(::std::boxed::Box::new(__res)));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///TerminalSetItem -> ident
    #[inline]
    fn reduce_TerminalSetItem_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
        }
        let mut __rustylr_location_ident = __location_stack.pop().unwrap();
        let mut ident = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            let Lexed::Ident(ident) = ident else {
                unreachable!("TerminalSetItem-Range1");
            };
            TerminalSetItem::Terminal(
                Located::new(ident.to_string(), __rustylr_location_ident),
            )
        };
        if __push_data {
            __data_stack.push(Self::__variant7(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///TerminalSetItem -> ident minus ident
    #[inline]
    fn reduce_TerminalSetItem_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__terminals(_)))
            );
        }
        let mut __rustylr_location_last = __location_stack.pop().unwrap();
        __location_stack.pop();
        let mut __rustylr_location_first = __location_stack.pop().unwrap();
        let mut last = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut first = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            let Lexed::Ident(first) = first else {
                unreachable!("TerminalSetItem-Range1");
            };
            let Lexed::Ident(last) = last else {
                unreachable!("TerminalSetItem-Range3");
            };
            TerminalSetItem::Range(
                Located::new(first.to_string(), __rustylr_location_first),
                Located::new(last.to_string(), __rustylr_location_last),
            )
        };
        if __push_data {
            __data_stack.push(Self::__variant7(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///TerminalSetItem -> ident minus error
    #[inline]
    fn reduce_TerminalSetItem_2(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__terminals(_)))
            );
        }
        let mut __rustylr_location_error = __location_stack.pop().unwrap();
        __location_stack.truncate(__location_stack.len() - 2);
        __data_stack.truncate(__data_stack.len() - 3);
        let __res = {
            data.error_recovered
                .push(RecoveredError {
                    message: "Expected ident for terminal set".to_string(),
                    link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#patterns"
                        .to_string(),
                    location: __rustylr_location_error,
                });
            TerminalSetItem::Terminal(Default::default())
        };
        if __push_data {
            __data_stack.push(Self::__variant7(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///TerminalSetItem -> char_literal
    #[inline]
    fn reduce_TerminalSetItem_3(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
        }
        let mut __rustylr_location_char_literal = __location_stack.pop().unwrap();
        let mut char_literal = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            let Lexed::CharLiteral(ch) = char_literal else {
                unreachable!("TerminalSetItem-CharLiteral1");
            };
            TerminalSetItem::Char(
                Located::new(ch.value(), __rustylr_location_char_literal),
            )
        };
        if __push_data {
            __data_stack.push(Self::__variant7(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///TerminalSetItem -> char_literal minus char_literal
    #[inline]
    fn reduce_TerminalSetItem_4(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__terminals(_)))
            );
        }
        let mut __rustylr_location_last = __location_stack.pop().unwrap();
        __location_stack.pop();
        let mut __rustylr_location_first = __location_stack.pop().unwrap();
        let mut last = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut first = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            let Lexed::CharLiteral(first) = first else {
                unreachable!("TerminalSetItem-CharLiteral2");
            };
            let Lexed::CharLiteral(last) = last else {
                unreachable!("TerminalSetItem-CharLiteral3");
            };
            TerminalSetItem::CharRange(
                Located::new(first.value(), __rustylr_location_first),
                Located::new(last.value(), __rustylr_location_last),
            )
        };
        if __push_data {
            __data_stack.push(Self::__variant7(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///TerminalSetItem -> char_literal minus error
    #[inline]
    fn reduce_TerminalSetItem_5(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__terminals(_)))
            );
        }
        let mut __rustylr_location_error = __location_stack.pop().unwrap();
        __location_stack.truncate(__location_stack.len() - 2);
        __data_stack.truncate(__data_stack.len() - 3);
        let __res = {
            data.error_recovered
                .push(RecoveredError {
                    message: "Expected char literal for terminal set".to_string(),
                    link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#patterns"
                        .to_string(),
                    location: __rustylr_location_error,
                });
            TerminalSetItem::Terminal(Default::default())
        };
        if __push_data {
            __data_stack.push(Self::__variant7(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///TerminalSetItem -> byte_literal
    #[inline]
    fn reduce_TerminalSetItem_6(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
        }
        let mut __rustylr_location_byte_literal = __location_stack.pop().unwrap();
        let mut byte_literal = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            let Lexed::ByteLiteral(b) = byte_literal else {
                unreachable!("TerminalSetItem-ByteLiteral1");
            };
            TerminalSetItem::Byte(
                Located::new(b.value(), __rustylr_location_byte_literal),
            )
        };
        if __push_data {
            __data_stack.push(Self::__variant7(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///TerminalSetItem -> byte_literal minus byte_literal
    #[inline]
    fn reduce_TerminalSetItem_7(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__terminals(_)))
            );
        }
        let mut __rustylr_location_last = __location_stack.pop().unwrap();
        __location_stack.pop();
        let mut __rustylr_location_first = __location_stack.pop().unwrap();
        let mut last = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut first = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            let Lexed::ByteLiteral(first) = first else {
                unreachable!("TerminalSetItem-ByteLiteral2");
            };
            let Lexed::ByteLiteral(last) = last else {
                unreachable!("TerminalSetItem-ByteLiteral3");
            };
            TerminalSetItem::ByteRange(
                Located::new(first.value(), __rustylr_location_first),
                Located::new(last.value(), __rustylr_location_last),
            )
        };
        if __push_data {
            __data_stack.push(Self::__variant7(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///TerminalSetItem -> byte_literal minus error
    #[inline]
    fn reduce_TerminalSetItem_8(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__terminals(_)))
            );
        }
        let mut __rustylr_location_error = __location_stack.pop().unwrap();
        __location_stack.truncate(__location_stack.len() - 2);
        __data_stack.truncate(__data_stack.len() - 3);
        let __res = {
            data.error_recovered
                .push(RecoveredError {
                    message: "Expected byte literal for terminal set".to_string(),
                    link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#patterns"
                        .to_string(),
                    location: __rustylr_location_error,
                });
            TerminalSetItem::Terminal(Default::default())
        };
        if __push_data {
            __data_stack.push(Self::__variant7(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///TerminalSet -> lbracket caret? TerminalSetItem* rbracket
    #[inline]
    fn reduce_TerminalSet_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant15(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant14(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 3usize), Some(&
                Data::__terminals(_)))
            );
        }
        let mut __rustylr_location_rbracket = __location_stack.pop().unwrap();
        __location_stack.truncate(__location_stack.len() - 2);
        let mut __rustylr_location_lbracket = __location_stack.pop().unwrap();
        __data_stack.pop();
        let mut TerminalSetItem = match __data_stack.pop().unwrap() {
            Data::__variant15(val) => val,
            _ => unreachable!(),
        };
        let mut caret = match __data_stack.pop().unwrap() {
            Data::__variant14(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let __res = {
            TerminalSet {
                negate: caret.is_some(),
                items: TerminalSetItem,
                open_location: __rustylr_location_lbracket,
                close_location: __rustylr_location_rbracket,
            }
        };
        if __push_data {
            __data_stack.push(Self::__variant8(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///TerminalSet -> lbracket caret? error rbracket
    #[inline]
    fn reduce_TerminalSet_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant14(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 3usize), Some(&
                Data::__terminals(_)))
            );
        }
        let mut __rustylr_location_rbracket = __location_stack.pop().unwrap();
        let mut __rustylr_location_error = __location_stack.pop().unwrap();
        __location_stack.pop();
        let mut __rustylr_location_lbracket = __location_stack.pop().unwrap();
        __data_stack.truncate(__data_stack.len() - 2);
        let mut caret = match __data_stack.pop().unwrap() {
            Data::__variant14(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let __res = {
            data.error_recovered
                .push(RecoveredError {
                    message: "Expected terminal set item".to_string(),
                    link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#patterns"
                        .to_string(),
                    location: __rustylr_location_error,
                });
            TerminalSet {
                negate: caret.is_some(),
                items: vec![],
                open_location: __rustylr_location_lbracket,
                close_location: __rustylr_location_rbracket,
            }
        };
        if __push_data {
            __data_stack.push(Self::__variant8(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///TerminalSet -> dot
    #[inline]
    fn reduce_TerminalSet_2(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
        }
        let mut __rustylr_location_dot = __location_stack.pop().unwrap();
        __data_stack.pop();
        let __res = {
            let span = __rustylr_location_dot;
            TerminalSet {
                negate: true,
                items: vec![],
                open_location: span.clone(),
                close_location: span,
            }
        };
        if __push_data {
            __data_stack.push(Self::__variant8(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///Pattern -> ident
    #[inline]
    fn reduce_Pattern_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
        }
        let mut __rustylr_location_ident = __location_stack.pop().unwrap();
        let mut ident = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            let Lexed::Ident(ident) = ident else {
                unreachable!("Pattern-Ident");
            };
            PatternArgs::Ident(Located::new(ident.to_string(), __rustylr_location_ident))
        };
        if __push_data {
            __data_stack.push(Self::__variant9(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///Pattern -> Pattern plus
    #[inline]
    fn reduce_Pattern_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant9(_)))
            );
        }
        let mut __rustylr_location_plus = __location_stack.pop().unwrap();
        __location_stack.pop();
        let mut plus = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        let mut Pattern = match __data_stack.pop().unwrap() {
            Data::__variant9(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            let Lexed::Plus(plus) = plus else {
                unreachable!("Pattern-Plus");
            };
            PatternArgs::Plus {
                base: Box::new(Pattern),
                op_location: __rustylr_location_plus,
            }
        };
        if __push_data {
            __data_stack.push(Self::__variant9(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///Pattern -> Pattern star
    #[inline]
    fn reduce_Pattern_2(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant9(_)))
            );
        }
        let mut __rustylr_location_star = __location_stack.pop().unwrap();
        __location_stack.pop();
        __data_stack.pop();
        let mut Pattern = match __data_stack.pop().unwrap() {
            Data::__variant9(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            PatternArgs::Star {
                base: Box::new(Pattern),
                op_location: __rustylr_location_star,
            }
        };
        if __push_data {
            __data_stack.push(Self::__variant9(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///Pattern -> Pattern question
    #[inline]
    fn reduce_Pattern_3(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant9(_)))
            );
        }
        let mut __rustylr_location_question = __location_stack.pop().unwrap();
        __location_stack.pop();
        __data_stack.pop();
        let mut Pattern = match __data_stack.pop().unwrap() {
            Data::__variant9(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            PatternArgs::Question {
                base: Box::new(Pattern),
                op_location: __rustylr_location_question,
            }
        };
        if __push_data {
            __data_stack.push(Self::__variant9(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///Pattern -> Pattern exclamation
    #[inline]
    fn reduce_Pattern_4(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant9(_)))
            );
        }
        let mut __rustylr_location_exclamation = __location_stack.pop().unwrap();
        __location_stack.pop();
        __data_stack.pop();
        let mut Pattern = match __data_stack.pop().unwrap() {
            Data::__variant9(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            PatternArgs::Exclamation {
                base: Box::new(Pattern),
                op_location: __rustylr_location_exclamation,
            }
        };
        if __push_data {
            __data_stack.push(Self::__variant9(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///Pattern -> TerminalSet
    #[inline]
    fn reduce_Pattern_5(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant8(_)))
            );
        }
        __location_stack.pop();
        let mut TerminalSet = match __data_stack.pop().unwrap() {
            Data::__variant8(val) => val,
            _ => unreachable!(),
        };
        let __res = { PatternArgs::TerminalSet(TerminalSet) };
        if __push_data {
            __data_stack.push(Self::__variant9(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///Pattern -> lparen $sep(Pattern*, pipe, +) rparen
    #[inline]
    fn reduce_Pattern_6(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant17(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__terminals(_)))
            );
        }
        let mut __rustylr_location_rparen = __location_stack.pop().unwrap();
        __location_stack.pop();
        let mut __rustylr_location_lparen = __location_stack.pop().unwrap();
        __data_stack.pop();
        let mut Pattern = match __data_stack.pop().unwrap() {
            Data::__variant17(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let __res = {
            PatternArgs::Group {
                alternatives: Pattern,
                open_location: __rustylr_location_lparen,
                close_location: __rustylr_location_rparen,
            }
        };
        if __push_data {
            __data_stack.push(Self::__variant9(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///Pattern -> lparen error rparen
    #[inline]
    fn reduce_Pattern_7(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.pop();
        let mut __rustylr_location_error = __location_stack.pop().unwrap();
        __location_stack.pop();
        __data_stack.truncate(__data_stack.len() - 3);
        let __res = {
            data.error_recovered
                .push(RecoveredError {
                    message: "syntax error when parsing GROUP".to_string(),
                    link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#patterns"
                        .to_string(),
                    location: __rustylr_location_error,
                });
            PatternArgs::Ident(Default::default())
        };
        if __push_data {
            __data_stack.push(Self::__variant9(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///Pattern -> byte_literal
    #[inline]
    fn reduce_Pattern_8(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
        }
        let mut __rustylr_location_byte_literal = __location_stack.pop().unwrap();
        let mut byte_literal = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            let Lexed::ByteLiteral(b) = byte_literal else {
                unreachable!("Pattern-ByteLiteral");
            };
            PatternArgs::Byte(Located::new(b.value(), __rustylr_location_byte_literal))
        };
        if __push_data {
            __data_stack.push(Self::__variant9(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///Pattern -> byte_str_literal
    #[inline]
    fn reduce_Pattern_9(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
        }
        let mut __rustylr_location_byte_str_literal = __location_stack.pop().unwrap();
        let mut byte_str_literal = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            let Lexed::ByteStrLiteral(b) = byte_str_literal else {
                unreachable!("Pattern-ByteStringLiteral");
            };
            PatternArgs::ByteString(
                Located::new(b.value(), __rustylr_location_byte_str_literal),
            )
        };
        if __push_data {
            __data_stack.push(Self::__variant9(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///Pattern -> char_literal
    #[inline]
    fn reduce_Pattern_10(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
        }
        let mut __rustylr_location_char_literal = __location_stack.pop().unwrap();
        let mut char_literal = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            let Lexed::CharLiteral(c) = char_literal else {
                unreachable!("Pattern-CharLiteral");
            };
            PatternArgs::Char(Located::new(c.value(), __rustylr_location_char_literal))
        };
        if __push_data {
            __data_stack.push(Self::__variant9(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///Pattern -> str_literal
    #[inline]
    fn reduce_Pattern_11(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
        }
        let mut __rustylr_location_str_literal = __location_stack.pop().unwrap();
        let mut str_literal = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            let Lexed::StrLiteral(s) = str_literal else {
                unreachable!("Pattern-StringLiteral");
            };
            PatternArgs::String(Located::new(s.value(), __rustylr_location_str_literal))
        };
        if __push_data {
            __data_stack.push(Self::__variant9(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///Pattern -> Pattern minus Pattern
    #[inline]
    fn reduce_Pattern_12(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant9(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant9(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        let mut p2 = match __data_stack.pop().unwrap() {
            Data::__variant9(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut p1 = match __data_stack.pop().unwrap() {
            Data::__variant9(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            PatternArgs::Minus {
                base: Box::new(p1),
                exclude: Box::new(p2),
            }
        };
        if __push_data {
            __data_stack.push(Self::__variant9(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///Pattern -> dollar ident lparen Pattern comma Pattern comma? rparen
    #[inline]
    fn reduce_Pattern_13(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant9(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 3usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 4usize), Some(&
                Data::__variant9(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 5usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 6usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 7usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 6);
        let mut __rustylr_location_ident = __location_stack.pop().unwrap();
        __location_stack.pop();
        __data_stack.truncate(__data_stack.len() - 2);
        let mut del = match __data_stack.pop().unwrap() {
            Data::__variant9(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut base = match __data_stack.pop().unwrap() {
            Data::__variant9(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut ident = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let __res = {
            let Lexed::Ident(ident) = ident else {
                unreachable!("Pattern-Sep-Ident");
            };
            if ident != "sep" {
                data.error_recovered
                    .push(RecoveredError {
                        message: "Expected $sep".to_string(),
                        link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#patterns"
                            .to_string(),
                        location: __rustylr_location_ident,
                    });
            }
            PatternArgs::Sep {
                base: Box::new(base),
                delimiter: Box::new(del),
                at_least_one: false,
                location: *__rustylr_location0,
            }
        };
        if __push_data {
            __data_stack.push(Self::__variant9(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///Pattern -> dollar ident lparen Pattern comma Pattern comma plus rparen
    #[inline]
    fn reduce_Pattern_14(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 3usize), Some(&
                Data::__variant9(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 4usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 5usize), Some(&
                Data::__variant9(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 6usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 7usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 8usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 7);
        let mut __rustylr_location_ident = __location_stack.pop().unwrap();
        __location_stack.pop();
        __data_stack.truncate(__data_stack.len() - 3);
        let mut del = match __data_stack.pop().unwrap() {
            Data::__variant9(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut base = match __data_stack.pop().unwrap() {
            Data::__variant9(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut ident = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let __res = {
            let Lexed::Ident(ident) = ident else {
                unreachable!("Pattern-Sep-Ident");
            };
            if ident != "sep" {
                data.error_recovered
                    .push(RecoveredError {
                        message: "Expected $sep".to_string(),
                        link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#patterns"
                            .to_string(),
                        location: __rustylr_location_ident,
                    });
            }
            PatternArgs::Sep {
                base: Box::new(base),
                delimiter: Box::new(del),
                at_least_one: true,
                location: *__rustylr_location0,
            }
        };
        if __push_data {
            __data_stack.push(Self::__variant9(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///Pattern -> dollar ident lparen Pattern comma Pattern comma star rparen
    #[inline]
    fn reduce_Pattern_15(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 3usize), Some(&
                Data::__variant9(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 4usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 5usize), Some(&
                Data::__variant9(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 6usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 7usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 8usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 7);
        let mut __rustylr_location_ident = __location_stack.pop().unwrap();
        __location_stack.pop();
        __data_stack.truncate(__data_stack.len() - 3);
        let mut del = match __data_stack.pop().unwrap() {
            Data::__variant9(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut base = match __data_stack.pop().unwrap() {
            Data::__variant9(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut ident = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let __res = {
            let Lexed::Ident(ident) = ident else {
                unreachable!("Pattern-Sep-Ident");
            };
            if ident != "sep" {
                data.error_recovered
                    .push(RecoveredError {
                        message: "Expected $sep".to_string(),
                        link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#patterns"
                            .to_string(),
                        location: __rustylr_location_ident,
                    });
            }
            PatternArgs::Sep {
                base: Box::new(base),
                delimiter: Box::new(del),
                at_least_one: false,
                location: *__rustylr_location0,
            }
        };
        if __push_data {
            __data_stack.push(Self::__variant9(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///Pattern -> dollar ident lparen Pattern comma Pattern error rparen
    #[inline]
    fn reduce_Pattern_16(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant9(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 3usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 4usize), Some(&
                Data::__variant9(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 5usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 6usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 7usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.pop();
        let mut __rustylr_location_error = __location_stack.pop().unwrap();
        __location_stack.truncate(__location_stack.len() - 4);
        let mut __rustylr_location_ident = __location_stack.pop().unwrap();
        __location_stack.pop();
        __data_stack.truncate(__data_stack.len() - 2);
        let mut del = match __data_stack.pop().unwrap() {
            Data::__variant9(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut base = match __data_stack.pop().unwrap() {
            Data::__variant9(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut ident = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let __res = {
            let Lexed::Ident(ident) = ident else {
                unreachable!("Pattern-Sep-Ident");
            };
            if ident != "sep" {
                data.error_recovered
                    .push(RecoveredError {
                        message: "Expected $sep".to_string(),
                        link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#patterns"
                            .to_string(),
                        location: __rustylr_location_ident,
                    });
            }
            data.error_recovered
                .push(RecoveredError {
                    message: "Unexpected $sep arguments".to_string(),
                    link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#patterns"
                        .to_string(),
                    location: __rustylr_location_error,
                });
            PatternArgs::Sep {
                base: Box::new(base),
                delimiter: Box::new(del),
                at_least_one: false,
                location: *__rustylr_location0,
            }
        };
        if __push_data {
            __data_stack.push(Self::__variant9(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///Pattern -> dollar ident lparen Pattern comma Pattern comma error rparen
    #[inline]
    fn reduce_Pattern_17(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 3usize), Some(&
                Data::__variant9(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 4usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 5usize), Some(&
                Data::__variant9(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 6usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 7usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 8usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.pop();
        let mut __rustylr_location_error = __location_stack.pop().unwrap();
        __location_stack.truncate(__location_stack.len() - 5);
        let mut __rustylr_location_ident = __location_stack.pop().unwrap();
        __location_stack.pop();
        __data_stack.truncate(__data_stack.len() - 3);
        let mut del = match __data_stack.pop().unwrap() {
            Data::__variant9(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut base = match __data_stack.pop().unwrap() {
            Data::__variant9(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut ident = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let __res = {
            let Lexed::Ident(ident) = ident else {
                unreachable!("Pattern-Sep-Ident");
            };
            if ident != "sep" {
                data.error_recovered
                    .push(RecoveredError {
                        message: "Expected $sep".to_string(),
                        link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#patterns"
                            .to_string(),
                        location: __rustylr_location_ident,
                    });
            }
            data.error_recovered
                .push(RecoveredError {
                    message: "Expected '+' or '*' repetition".to_string(),
                    link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#patterns"
                        .to_string(),
                    location: __rustylr_location_error,
                });
            PatternArgs::Sep {
                base: Box::new(base),
                delimiter: Box::new(del),
                at_least_one: false,
                location: *__rustylr_location0,
            }
        };
        if __push_data {
            __data_stack.push(Self::__variant9(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///Action -> bracegroup
    #[inline]
    fn reduce_Action_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.pop();
        let mut bracegroup = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            let Lexed::BraceGroup(bracegroup) = bracegroup else {
                unreachable!("Action0");
            };
            Some(bracegroup)
        };
        if __push_data {
            __data_stack.push(Self::__variant2(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///Action -> error
    #[inline]
    fn reduce_Action_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
        }
        let mut __rustylr_location_error = __location_stack.pop().unwrap();
        __data_stack.pop();
        let __res = {
            data.error_recovered
                .push(RecoveredError {
                    message: "Expected reduce action block or rule terminator"
                        .to_string(),
                    link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#reduceaction-optional"
                        .to_string(),
                    location: __rustylr_location_error,
                });
            None
        };
        if __push_data {
            __data_stack.push(Self::__variant2(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///Action ->
    #[inline]
    fn reduce_Action_2(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)] {}
        let __res = { None };
        if __push_data {
            __data_stack.push(Self::__variant2(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///IdentOrLiteral -> ident
    #[inline]
    fn reduce_IdentOrLiteral_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
        }
        let mut __rustylr_location_ident = __location_stack.pop().unwrap();
        let mut ident = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            let Lexed::Ident(ident) = ident else {
                unreachable!("IdentOrLiteral-Ident");
            };
            IdentOrLiteral::Ident(
                Located::new(ident.to_string(), __rustylr_location_ident),
            )
        };
        if __push_data {
            __data_stack.push(Self::__variant10(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///IdentOrLiteral -> byte_literal
    #[inline]
    fn reduce_IdentOrLiteral_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
        }
        let mut __rustylr_location_byte_literal = __location_stack.pop().unwrap();
        let mut byte_literal = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            let Lexed::ByteLiteral(b) = byte_literal else {
                unreachable!("IdentOrLiteral-ByteLiteral");
            };
            IdentOrLiteral::Byte(
                Located::new(b.value(), __rustylr_location_byte_literal),
            )
        };
        if __push_data {
            __data_stack.push(Self::__variant10(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///IdentOrLiteral -> char_literal
    #[inline]
    fn reduce_IdentOrLiteral_2(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
        }
        let mut __rustylr_location_char_literal = __location_stack.pop().unwrap();
        let mut char_literal = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            let Lexed::CharLiteral(c) = char_literal else {
                unreachable!("IdentOrLiteral-CharLiteral");
            };
            IdentOrLiteral::Char(
                Located::new(c.value(), __rustylr_location_char_literal),
            )
        };
        if __push_data {
            __data_stack.push(Self::__variant10(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///AllowTarget -> ident
    #[inline]
    fn reduce_AllowTarget_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
        }
        let mut __rustylr_location_ident = __location_stack.pop().unwrap();
        let mut ident = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            let Lexed::Ident(ident) = ident else {
                unreachable!("AllowTarget-Ident");
            };
            AllowTarget::Ident(Located::new(ident.to_string(), __rustylr_location_ident))
        };
        if __push_data {
            __data_stack.push(Self::__variant11(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///AllowTarget -> byte_literal
    #[inline]
    fn reduce_AllowTarget_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
        }
        let mut __rustylr_location_byte_literal = __location_stack.pop().unwrap();
        let mut byte_literal = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            let Lexed::ByteLiteral(b) = byte_literal else {
                unreachable!("AllowTarget-ByteLiteral");
            };
            AllowTarget::Byte(Located::new(b.value(), __rustylr_location_byte_literal))
        };
        if __push_data {
            __data_stack.push(Self::__variant11(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///AllowTarget -> char_literal
    #[inline]
    fn reduce_AllowTarget_2(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
        }
        let mut __rustylr_location_char_literal = __location_stack.pop().unwrap();
        let mut char_literal = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            let Lexed::CharLiteral(c) = char_literal else {
                unreachable!("AllowTarget-CharLiteral");
            };
            AllowTarget::Char(Located::new(c.value(), __rustylr_location_char_literal))
        };
        if __push_data {
            __data_stack.push(Self::__variant11(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///AllowTarget -> char_literal minus char_literal
    #[inline]
    fn reduce_AllowTarget_3(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__terminals(_)))
            );
        }
        let mut __rustylr_location_last = __location_stack.pop().unwrap();
        __location_stack.pop();
        let mut __rustylr_location_first = __location_stack.pop().unwrap();
        let mut last = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut first = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            let Lexed::CharLiteral(first) = first else {
                unreachable!("AllowTarget-CharLiteralRange1");
            };
            let Lexed::CharLiteral(last) = last else {
                unreachable!("AllowTarget-CharLiteralRange2");
            };
            AllowTarget::CharRange(
                Located::new(first.value(), __rustylr_location_first),
                Located::new(last.value(), __rustylr_location_last),
            )
        };
        if __push_data {
            __data_stack.push(Self::__variant11(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///AllowTarget -> byte_literal minus byte_literal
    #[inline]
    fn reduce_AllowTarget_4(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__terminals(_)))
            );
        }
        let mut __rustylr_location_last = __location_stack.pop().unwrap();
        __location_stack.pop();
        let mut __rustylr_location_first = __location_stack.pop().unwrap();
        let mut last = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut first = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            let Lexed::ByteLiteral(first) = first else {
                unreachable!("AllowTarget-ByteLiteralRange1");
            };
            let Lexed::ByteLiteral(last) = last else {
                unreachable!("AllowTarget-ByteLiteralRange2");
            };
            AllowTarget::ByteRange(
                Located::new(first.value(), __rustylr_location_first),
                Located::new(last.value(), __rustylr_location_last),
            )
        };
        if __push_data {
            __data_stack.push(Self::__variant11(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///AllowTarget -> TerminalSet
    #[inline]
    fn reduce_AllowTarget_5(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant8(_)))
            );
        }
        __location_stack.pop();
        let mut TerminalSet = match __data_stack.pop().unwrap() {
            Data::__variant8(val) => val,
            _ => unreachable!(),
        };
        let __res = { AllowTarget::TerminalSet(TerminalSet) };
        if __push_data {
            __data_stack.push(Self::__variant11(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///Directive -> percent token ident [^semicolon]+ semicolon
    #[inline]
    fn reduce_Directive_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant18(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 3usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 4usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 2);
        let mut __rustylr_location_ident = __location_stack.pop().unwrap();
        __location_stack.truncate(__location_stack.len() - 2);
        __data_stack.pop();
        let mut __rustylr_data_3 = match __data_stack.pop().unwrap() {
            Data::__variant18(val) => val,
            _ => unreachable!(),
        };
        let mut ident = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        __data_stack.truncate(__data_stack.len() - 2);
        let __rustylr_data_3 = Self::custom_reduce_action_0(
            __rustylr_data_3,
            data,
            __rustylr_location0,
        )?;
        let mut RustCode = __rustylr_data_3;
        {
            let Lexed::Ident(ident) = ident else {
                unreachable!("TokenDef-Ident");
            };
            data.terminals
                .push((
                    Located::new(ident.to_string(), __rustylr_location_ident),
                    RustCode,
                ));
        };
        __data_stack.push(Self::Empty);
        Ok(())
    }
    ///Directive -> percent token ident semicolon
    #[inline]
    fn reduce_Directive_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 3usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.pop();
        let mut __rustylr_location_ident = __location_stack.pop().unwrap();
        __location_stack.truncate(__location_stack.len() - 2);
        __data_stack.truncate(__data_stack.len() - 4);
        {
            data.error_recovered
                .push(RecoveredError {
                    message: "Expected token definition".to_string(),
                    link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#token-definition-must-defined"
                        .to_string(),
                    location: __rustylr_location_ident,
                });
        };
        __data_stack.push(Self::Empty);
        Ok(())
    }
    ///Directive -> percent token error semicolon
    #[inline]
    fn reduce_Directive_2(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
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
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 3usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.pop();
        let mut __rustylr_location_error = __location_stack.pop().unwrap();
        __location_stack.truncate(__location_stack.len() - 2);
        __data_stack.truncate(__data_stack.len() - 4);
        {
            data.error_recovered
                .push(RecoveredError {
                    message: "Expected token name".to_string(),
                    link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#token-definition-must-defined"
                        .to_string(),
                    location: __rustylr_location_error,
                });
        };
        __data_stack.push(Self::Empty);
        Ok(())
    }
    ///Directive -> percent start ident semicolon
    #[inline]
    fn reduce_Directive_3(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 3usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.pop();
        let mut __rustylr_location_ident = __location_stack.pop().unwrap();
        __location_stack.truncate(__location_stack.len() - 2);
        __data_stack.pop();
        let mut ident = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        __data_stack.truncate(__data_stack.len() - 2);
        {
            let Lexed::Ident(ident) = ident else {
                unreachable!("StartDef-Ident");
            };
            data.start_rule_name
                .push(Located::new(ident.to_string(), __rustylr_location_ident));
        };
        __data_stack.push(Self::Empty);
        Ok(())
    }
    ///Directive -> percent start error semicolon
    #[inline]
    fn reduce_Directive_4(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
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
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 3usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.pop();
        let mut __rustylr_location_error = __location_stack.pop().unwrap();
        __location_stack.truncate(__location_stack.len() - 2);
        __data_stack.truncate(__data_stack.len() - 4);
        {
            data.error_recovered
                .push(RecoveredError {
                    message: "Expected start rule name".to_string(),
                    link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#start-symbol-must-defined"
                        .to_string(),
                    location: __rustylr_location_error,
                });
        };
        __data_stack.push(Self::Empty);
        Ok(())
    }
    ///Directive -> percent tokentype [^semicolon]+ semicolon
    #[inline]
    fn reduce_Directive_5(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant18(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 3usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 2);
        let mut __rustylr_location_tokentype = __location_stack.pop().unwrap();
        __location_stack.pop();
        __data_stack.pop();
        let mut __rustylr_data_2 = match __data_stack.pop().unwrap() {
            Data::__variant18(val) => val,
            _ => unreachable!(),
        };
        __data_stack.truncate(__data_stack.len() - 2);
        let __rustylr_data_2 = Self::custom_reduce_action_0(
            __rustylr_data_2,
            data,
            __rustylr_location0,
        )?;
        let mut RustCode = __rustylr_data_2;
        {
            data.token_typename.push((__rustylr_location_tokentype, RustCode));
        };
        __data_stack.push(Self::Empty);
        Ok(())
    }
    ///Directive -> percent tokentype semicolon
    #[inline]
    fn reduce_Directive_6(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.pop();
        let mut __rustylr_location_tokentype = __location_stack.pop().unwrap();
        __location_stack.pop();
        __data_stack.truncate(__data_stack.len() - 3);
        {
            data.error_recovered
                .push(RecoveredError {
                    message: "Expected token type definition".to_string(),
                    link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#token-type-must-defined"
                        .to_string(),
                    location: __rustylr_location_tokentype,
                });
        };
        __data_stack.push(Self::Empty);
        Ok(())
    }
    ///Directive -> percent userdata [^semicolon]+ semicolon
    #[inline]
    fn reduce_Directive_7(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant18(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 3usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 2);
        let mut __rustylr_location_userdata = __location_stack.pop().unwrap();
        __location_stack.pop();
        __data_stack.pop();
        let mut __rustylr_data_2 = match __data_stack.pop().unwrap() {
            Data::__variant18(val) => val,
            _ => unreachable!(),
        };
        __data_stack.truncate(__data_stack.len() - 2);
        let __rustylr_data_2 = Self::custom_reduce_action_0(
            __rustylr_data_2,
            data,
            __rustylr_location0,
        )?;
        let mut RustCode = __rustylr_data_2;
        {
            data.userdata_typename.push((__rustylr_location_userdata, RustCode));
        };
        __data_stack.push(Self::Empty);
        Ok(())
    }
    ///Directive -> percent userdata semicolon
    #[inline]
    fn reduce_Directive_8(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.pop();
        let mut __rustylr_location_userdata = __location_stack.pop().unwrap();
        __location_stack.pop();
        __data_stack.truncate(__data_stack.len() - 3);
        {
            data.error_recovered
                .push(RecoveredError {
                    message: "Expected userdata definition".to_string(),
                    link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#userdata-type-optional"
                        .to_string(),
                    location: __rustylr_location_userdata,
                });
        };
        __data_stack.push(Self::Empty);
        Ok(())
    }
    ///Directive -> percent left IdentOrLiteral+ semicolon
    #[inline]
    fn reduce_Directive_9(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant19(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 3usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 2);
        let mut __rustylr_location_left = __location_stack.pop().unwrap();
        __location_stack.pop();
        __data_stack.pop();
        let mut IdentOrLiteral = match __data_stack.pop().unwrap() {
            Data::__variant19(val) => val,
            _ => unreachable!(),
        };
        __data_stack.truncate(__data_stack.len() - 2);
        {
            data.precedences
                .push((
                    __rustylr_location_left,
                    Some(Associativity::Left),
                    IdentOrLiteral,
                ));
        };
        __data_stack.push(Self::Empty);
        Ok(())
    }
    ///Directive -> percent left error semicolon
    #[inline]
    fn reduce_Directive_10(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
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
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 3usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.pop();
        let mut __rustylr_location_error = __location_stack.pop().unwrap();
        __location_stack.truncate(__location_stack.len() - 2);
        __data_stack.truncate(__data_stack.len() - 4);
        {
            data.error_recovered
                .push(RecoveredError {
                    message: "Expected <ident> to token or <literal>".to_string(),
                    link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#operator-precedence"
                        .to_string(),
                    location: __rustylr_location_error,
                });
        };
        __data_stack.push(Self::Empty);
        Ok(())
    }
    ///Directive -> percent right IdentOrLiteral+ semicolon
    #[inline]
    fn reduce_Directive_11(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant19(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 3usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 2);
        let mut __rustylr_location_right = __location_stack.pop().unwrap();
        __location_stack.pop();
        __data_stack.pop();
        let mut IdentOrLiteral = match __data_stack.pop().unwrap() {
            Data::__variant19(val) => val,
            _ => unreachable!(),
        };
        __data_stack.truncate(__data_stack.len() - 2);
        {
            data.precedences
                .push((
                    __rustylr_location_right,
                    Some(Associativity::Right),
                    IdentOrLiteral,
                ));
        };
        __data_stack.push(Self::Empty);
        Ok(())
    }
    ///Directive -> percent right error semicolon
    #[inline]
    fn reduce_Directive_12(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
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
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 3usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.pop();
        let mut __rustylr_location_error = __location_stack.pop().unwrap();
        __location_stack.truncate(__location_stack.len() - 2);
        __data_stack.truncate(__data_stack.len() - 4);
        {
            data.error_recovered
                .push(RecoveredError {
                    message: "Expected <ident> to token or <literal>".to_string(),
                    link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#operator-precedence"
                        .to_string(),
                    location: __rustylr_location_error,
                });
        };
        __data_stack.push(Self::Empty);
        Ok(())
    }
    ///Directive -> percent precedence IdentOrLiteral+ semicolon
    #[inline]
    fn reduce_Directive_13(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant19(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 3usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 2);
        let mut __rustylr_location_precedence = __location_stack.pop().unwrap();
        __location_stack.pop();
        __data_stack.pop();
        let mut IdentOrLiteral = match __data_stack.pop().unwrap() {
            Data::__variant19(val) => val,
            _ => unreachable!(),
        };
        __data_stack.truncate(__data_stack.len() - 2);
        {
            data.precedences.push((__rustylr_location_precedence, None, IdentOrLiteral));
        };
        __data_stack.push(Self::Empty);
        Ok(())
    }
    ///Directive -> percent precedence error semicolon
    #[inline]
    fn reduce_Directive_14(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
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
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 3usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.pop();
        let mut __rustylr_location_error = __location_stack.pop().unwrap();
        __location_stack.truncate(__location_stack.len() - 2);
        __data_stack.truncate(__data_stack.len() - 4);
        {
            data.error_recovered
                .push(RecoveredError {
                    message: "Expected <ident> to token or <literal>".to_string(),
                    link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#operator-precedence"
                        .to_string(),
                    location: __rustylr_location_error,
                });
        };
        __data_stack.push(Self::Empty);
        Ok(())
    }
    ///Directive -> percent errortype [^semicolon]+ semicolon
    #[inline]
    fn reduce_Directive_15(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant18(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 3usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 2);
        let mut __rustylr_location_errortype = __location_stack.pop().unwrap();
        __location_stack.pop();
        __data_stack.pop();
        let mut __rustylr_data_2 = match __data_stack.pop().unwrap() {
            Data::__variant18(val) => val,
            _ => unreachable!(),
        };
        __data_stack.truncate(__data_stack.len() - 2);
        let __rustylr_data_2 = Self::custom_reduce_action_0(
            __rustylr_data_2,
            data,
            __rustylr_location0,
        )?;
        let mut RustCode = __rustylr_data_2;
        {
            data.error_typename.push((__rustylr_location_errortype, RustCode));
        };
        __data_stack.push(Self::Empty);
        Ok(())
    }
    ///Directive -> percent errortype semicolon
    #[inline]
    fn reduce_Directive_16(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.pop();
        let mut __rustylr_location_errortype = __location_stack.pop().unwrap();
        __location_stack.pop();
        __data_stack.truncate(__data_stack.len() - 3);
        {
            data.error_recovered
                .push(RecoveredError {
                    message: "Expected error type definition".to_string(),
                    link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#error-type-optional"
                        .to_string(),
                    location: __rustylr_location_errortype,
                });
        };
        __data_stack.push(Self::Empty);
        Ok(())
    }
    ///Directive -> percent moduleprefix [^semicolon]+ semicolon
    #[inline]
    fn reduce_Directive_17(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant18(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 3usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 2);
        let mut __rustylr_location_moduleprefix = __location_stack.pop().unwrap();
        __location_stack.pop();
        __data_stack.pop();
        let mut __rustylr_data_2 = match __data_stack.pop().unwrap() {
            Data::__variant18(val) => val,
            _ => unreachable!(),
        };
        __data_stack.truncate(__data_stack.len() - 2);
        let __rustylr_data_2 = Self::custom_reduce_action_0(
            __rustylr_data_2,
            data,
            __rustylr_location0,
        )?;
        let mut RustCode = __rustylr_data_2;
        {
            data.module_prefix.push((__rustylr_location_moduleprefix, RustCode));
        };
        __data_stack.push(Self::Empty);
        Ok(())
    }
    ///Directive -> percent moduleprefix semicolon
    #[inline]
    fn reduce_Directive_18(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.pop();
        let mut __rustylr_location_moduleprefix = __location_stack.pop().unwrap();
        __location_stack.pop();
        __data_stack.truncate(__data_stack.len() - 3);
        {
            data.error_recovered
                .push(RecoveredError {
                    message: "Expected moduleprefix definition".to_string(),
                    link: "This is hidden directive, user must not use this explicitly"
                        .to_string(),
                    location: __rustylr_location_moduleprefix,
                });
        };
        __data_stack.push(Self::Empty);
        Ok(())
    }
    ///Directive -> percent glr semicolon
    #[inline]
    fn reduce_Directive_19(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        __data_stack.pop();
        let mut glr = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        {
            data.glr = true;
        };
        __data_stack.push(Self::Empty);
        Ok(())
    }
    ///Directive -> percent glr error semicolon
    #[inline]
    fn reduce_Directive_20(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
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
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 3usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.pop();
        let mut __rustylr_location_error = __location_stack.pop().unwrap();
        __location_stack.truncate(__location_stack.len() - 2);
        __data_stack.truncate(__data_stack.len() - 4);
        {
            data.error_recovered
                .push(RecoveredError {
                    message: "Expected semicolon".to_string(),
                    link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#glr-parser-generation"
                        .to_string(),
                    location: __rustylr_location_error,
                });
        };
        __data_stack.push(Self::Empty);
        Ok(())
    }
    ///Directive -> percent lalr semicolon
    #[inline]
    fn reduce_Directive_21(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        __data_stack.pop();
        let mut lalr = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        {
            data.lalr = true;
        };
        __data_stack.push(Self::Empty);
        Ok(())
    }
    ///Directive -> percent lalr error semicolon
    #[inline]
    fn reduce_Directive_22(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
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
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 3usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.pop();
        let mut __rustylr_location_error = __location_stack.pop().unwrap();
        __location_stack.truncate(__location_stack.len() - 2);
        __data_stack.truncate(__data_stack.len() - 4);
        {
            data.error_recovered
                .push(RecoveredError {
                    message: "Expected semicolon".to_string(),
                    link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#lalr-parser-generation"
                        .to_string(),
                    location: __rustylr_location_error,
                });
        };
        __data_stack.push(Self::Empty);
        Ok(())
    }
    ///Directive -> percent nooptim semicolon
    #[inline]
    fn reduce_Directive_23(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        __data_stack.truncate(__data_stack.len() - 3);
        {
            data.no_optim = true;
        };
        __data_stack.push(Self::Empty);
        Ok(())
    }
    ///Directive -> percent nooptim error semicolon
    #[inline]
    fn reduce_Directive_24(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
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
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 3usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.pop();
        let mut __rustylr_location_error = __location_stack.pop().unwrap();
        __location_stack.truncate(__location_stack.len() - 2);
        __data_stack.truncate(__data_stack.len() - 4);
        {
            data.error_recovered
                .push(RecoveredError {
                    message: "Expected semicolon".to_string(),
                    link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#no-optimization"
                        .to_string(),
                    location: __rustylr_location_error,
                });
        };
        __data_stack.push(Self::Empty);
        Ok(())
    }
    ///Directive -> percent location [^semicolon]+ semicolon
    #[inline]
    fn reduce_Directive_25(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant18(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 3usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 2);
        let mut __rustylr_location_location = __location_stack.pop().unwrap();
        __location_stack.pop();
        __data_stack.pop();
        let mut __rustylr_data_2 = match __data_stack.pop().unwrap() {
            Data::__variant18(val) => val,
            _ => unreachable!(),
        };
        __data_stack.truncate(__data_stack.len() - 2);
        let __rustylr_data_2 = Self::custom_reduce_action_0(
            __rustylr_data_2,
            data,
            __rustylr_location0,
        )?;
        let mut RustCode = __rustylr_data_2;
        {
            data.location_typename.push((__rustylr_location_location, RustCode));
        };
        __data_stack.push(Self::Empty);
        Ok(())
    }
    ///Directive -> percent location semicolon
    #[inline]
    fn reduce_Directive_26(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.pop();
        let mut __rustylr_location_location = __location_stack.pop().unwrap();
        __location_stack.pop();
        __data_stack.pop();
        let mut location = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        {
            data.error_recovered
                .push(RecoveredError {
                    message: "Expected location type definition".to_string(),
                    link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#location-tracking"
                        .to_string(),
                    location: __rustylr_location_location,
                });
        };
        __data_stack.push(Self::Empty);
        Ok(())
    }
    ///Directive -> percent allow ident semicolon
    #[inline]
    fn reduce_Directive_27(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 3usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.pop();
        let mut __rustylr_location_ident = __location_stack.pop().unwrap();
        __location_stack.truncate(__location_stack.len() - 2);
        __data_stack.pop();
        let mut ident = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        __data_stack.truncate(__data_stack.len() - 2);
        {
            let Lexed::Ident(ident) = ident else {
                unreachable!("AllowDef-Ident");
            };
            data.allowed_diagnostics
                .push((Located::new(ident.to_string(), __rustylr_location_ident), None));
        };
        __data_stack.push(Self::Empty);
        Ok(())
    }
    ///Directive -> percent allow ident lparen AllowTarget rparen semicolon
    #[inline]
    fn reduce_Directive_28(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant11(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 3usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 4usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 5usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 6usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 4);
        let mut __rustylr_location_ident = __location_stack.pop().unwrap();
        __location_stack.truncate(__location_stack.len() - 2);
        __data_stack.truncate(__data_stack.len() - 2);
        let mut AllowTarget = match __data_stack.pop().unwrap() {
            Data::__variant11(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut ident = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        __data_stack.truncate(__data_stack.len() - 2);
        {
            let Lexed::Ident(ident) = ident else {
                unreachable!("AllowDef-Ident");
            };
            data.allowed_diagnostics
                .push((
                    Located::new(ident.to_string(), __rustylr_location_ident),
                    Some(AllowTarget),
                ));
        };
        __data_stack.push(Self::Empty);
        Ok(())
    }
    ///Directive -> percent allow ident lparen error rparen semicolon
    #[inline]
    fn reduce_Directive_29(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::Empty))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 3usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 4usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 5usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 6usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 2);
        let mut __rustylr_location_error = __location_stack.pop().unwrap();
        __location_stack.truncate(__location_stack.len() - 4);
        __data_stack.truncate(__data_stack.len() - 7);
        {
            data.error_recovered
                .push(RecoveredError {
                    message: "Expected diagnostic suppression target".to_string(),
                    link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#diagnostic-suppression"
                        .to_string(),
                    location: __rustylr_location_error,
                });
        };
        __data_stack.push(Self::Empty);
        Ok(())
    }
    ///Directive -> percent allow error semicolon
    #[inline]
    fn reduce_Directive_30(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
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
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 3usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.pop();
        let mut __rustylr_location_error = __location_stack.pop().unwrap();
        __location_stack.truncate(__location_stack.len() - 2);
        __data_stack.truncate(__data_stack.len() - 4);
        {
            data.error_recovered
                .push(RecoveredError {
                    message: "Expected diagnostic name".to_string(),
                    link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#diagnostic-suppression"
                        .to_string(),
                    location: __rustylr_location_error,
                });
        };
        __data_stack.push(Self::Empty);
        Ok(())
    }
    ///Directive -> percent error semicolon
    #[inline]
    fn reduce_Directive_31(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
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
                Data::__terminals(_)))
            );
        }
        __location_stack.pop();
        let mut __rustylr_location_error = __location_stack.pop().unwrap();
        __location_stack.pop();
        __data_stack.truncate(__data_stack.len() - 3);
        {
            data.error_recovered
                .push(RecoveredError {
                    message: "Expected directive, e.g. %token, %start, ...".to_string(),
                    link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#syntax"
                        .to_string(),
                    location: __rustylr_location_error,
                });
        };
        __data_stack.push(Self::Empty);
        Ok(())
    }
    ///GrammarLine -> Rule
    #[inline]
    fn reduce_GrammarLine_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant1(_)))
            );
        }
        __location_stack.pop();
        let mut Rule = match __data_stack.pop().unwrap() {
            Data::__variant1(val) => *val,
            _ => unreachable!(),
        };
        {
            data.rules.push(Rule);
        };
        __data_stack.push(Self::Empty);
        Ok(())
    }
    ///MappedSymbol+ -> MappedSymbol
    #[inline]
    fn reduce__MappedSymbolPlus16_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant6(_)))
            );
        }
        __location_stack.pop();
        let mut A = match __data_stack.pop().unwrap() {
            Data::__variant6(val) => *val,
            _ => unreachable!(),
        };
        let __res = { vec![A] };
        if __push_data {
            __data_stack.push(Self::__variant12(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///MappedSymbol+ -> MappedSymbol+ MappedSymbol
    #[inline]
    fn reduce__MappedSymbolPlus16_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant6(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant12(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 2);
        let mut A = match __data_stack.pop().unwrap() {
            Data::__variant6(val) => *val,
            _ => unreachable!(),
        };
        let mut Ap = match __data_stack.pop().unwrap() {
            Data::__variant12(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Ap.push(A);
            Ap
        };
        if __push_data {
            __data_stack.push(Self::__variant12(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///MappedSymbol* -> MappedSymbol+
    #[inline]
    fn reduce__MappedSymbolStar17_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant12(_)))
            );
        }
        __location_stack.pop();
        let mut __token0 = match __data_stack.pop().unwrap() {
            Data::__variant12(val) => val,
            _ => unreachable!(),
        };
        let __res = __token0;
        if __push_data {
            __data_stack.push(Self::__variant12(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///MappedSymbol* ->
    #[inline]
    fn reduce__MappedSymbolStar17_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)] {}
        let __res = { vec![] };
        if __push_data {
            __data_stack.push(Self::__variant12(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///PrecDef+ -> PrecDef
    #[inline]
    fn reduce__PrecDefPlus18_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant5(_)))
            );
        }
        __location_stack.pop();
        let mut A = match __data_stack.pop().unwrap() {
            Data::__variant5(val) => *val,
            _ => unreachable!(),
        };
        let __res = { vec![A] };
        if __push_data {
            __data_stack.push(Self::__variant13(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///PrecDef+ -> PrecDef+ PrecDef
    #[inline]
    fn reduce__PrecDefPlus18_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant5(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant13(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 2);
        let mut A = match __data_stack.pop().unwrap() {
            Data::__variant5(val) => *val,
            _ => unreachable!(),
        };
        let mut Ap = match __data_stack.pop().unwrap() {
            Data::__variant13(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Ap.push(A);
            Ap
        };
        if __push_data {
            __data_stack.push(Self::__variant13(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///PrecDef* -> PrecDef+
    #[inline]
    fn reduce__PrecDefStar19_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant13(_)))
            );
        }
        __location_stack.pop();
        let mut __token0 = match __data_stack.pop().unwrap() {
            Data::__variant13(val) => val,
            _ => unreachable!(),
        };
        let __res = __token0;
        if __push_data {
            __data_stack.push(Self::__variant13(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///PrecDef* ->
    #[inline]
    fn reduce__PrecDefStar19_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)] {}
        let __res = { vec![] };
        if __push_data {
            __data_stack.push(Self::__variant13(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///caret? -> caret
    #[inline]
    fn reduce__caretQuestion20_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
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
        let __res = Some(A);
        if __push_data {
            __data_stack.push(Self::__variant14(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///caret? ->
    #[inline]
    fn reduce__caretQuestion20_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)] {}
        let __res = { None };
        if __push_data {
            __data_stack.push(Self::__variant14(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///TerminalSetItem+ -> TerminalSetItem
    #[inline]
    fn reduce__TerminalSetItemPlus21_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant7(_)))
            );
        }
        __location_stack.pop();
        let mut A = match __data_stack.pop().unwrap() {
            Data::__variant7(val) => val,
            _ => unreachable!(),
        };
        let __res = { vec![A] };
        if __push_data {
            __data_stack.push(Self::__variant15(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///TerminalSetItem+ -> TerminalSetItem+ TerminalSetItem
    #[inline]
    fn reduce__TerminalSetItemPlus21_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant7(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant15(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 2);
        let mut A = match __data_stack.pop().unwrap() {
            Data::__variant7(val) => val,
            _ => unreachable!(),
        };
        let mut Ap = match __data_stack.pop().unwrap() {
            Data::__variant15(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Ap.push(A);
            Ap
        };
        if __push_data {
            __data_stack.push(Self::__variant15(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///TerminalSetItem* -> TerminalSetItem+
    #[inline]
    fn reduce__TerminalSetItemStar22_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant15(_)))
            );
        }
        __location_stack.pop();
        let mut __token0 = match __data_stack.pop().unwrap() {
            Data::__variant15(val) => val,
            _ => unreachable!(),
        };
        let __res = __token0;
        if __push_data {
            __data_stack.push(Self::__variant15(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///TerminalSetItem* ->
    #[inline]
    fn reduce__TerminalSetItemStar22_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)] {}
        let __res = { vec![] };
        if __push_data {
            __data_stack.push(Self::__variant15(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///Pattern+ -> Pattern
    #[inline]
    fn reduce__PatternPlus23_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant9(_)))
            );
        }
        __location_stack.pop();
        let mut A = match __data_stack.pop().unwrap() {
            Data::__variant9(val) => val,
            _ => unreachable!(),
        };
        let __res = { vec![A] };
        if __push_data {
            __data_stack.push(Self::__variant16(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///Pattern+ -> Pattern+ Pattern
    #[inline]
    fn reduce__PatternPlus23_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant9(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant16(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 2);
        let mut A = match __data_stack.pop().unwrap() {
            Data::__variant9(val) => val,
            _ => unreachable!(),
        };
        let mut Ap = match __data_stack.pop().unwrap() {
            Data::__variant16(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Ap.push(A);
            Ap
        };
        if __push_data {
            __data_stack.push(Self::__variant16(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///Pattern* -> Pattern+
    #[inline]
    fn reduce__PatternStar24_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant16(_)))
            );
        }
        __location_stack.pop();
        let mut __token0 = match __data_stack.pop().unwrap() {
            Data::__variant16(val) => val,
            _ => unreachable!(),
        };
        let __res = __token0;
        if __push_data {
            __data_stack.push(Self::__variant16(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///Pattern* ->
    #[inline]
    fn reduce__PatternStar24_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)] {}
        let __res = { vec![] };
        if __push_data {
            __data_stack.push(Self::__variant16(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///$sep(Pattern*, pipe, +) -> Pattern*
    #[inline]
    fn reduce___PatternStar24SepPlus25_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant16(_)))
            );
        }
        __location_stack.pop();
        let mut __token0 = match __data_stack.pop().unwrap() {
            Data::__variant16(val) => val,
            _ => unreachable!(),
        };
        let __res = { vec![__token0] };
        if __push_data {
            __data_stack.push(Self::__variant17(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///$sep(Pattern*, pipe, +) -> $sep(Pattern*, pipe, +) pipe Pattern*
    #[inline]
    fn reduce___PatternStar24SepPlus25_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant16(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 2usize), Some(&
                Data::__variant17(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 3);
        let mut __token1 = match __data_stack.pop().unwrap() {
            Data::__variant16(val) => val,
            _ => unreachable!(),
        };
        __data_stack.pop();
        let mut __token0 = match __data_stack.pop().unwrap() {
            Data::__variant17(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            __token0.push(__token1);
            __token0
        };
        if __push_data {
            __data_stack.push(Self::__variant17(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///comma? -> comma
    #[inline]
    fn reduce__commaQuestion26_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
        }
        __location_stack.pop();
        __data_stack.pop();
        __data_stack.push(Self::Empty);
        Ok(())
    }
    ///comma? ->
    #[inline]
    fn reduce__commaQuestion26_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)] {}
        __data_stack.push(Self::Empty);
        Ok(())
    }
    ///[^semicolon]+ -> [^semicolon]
    #[inline]
    fn reduce___TermSet27Plus28_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
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
            __data_stack.push(Self::__variant18(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///[^semicolon]+ -> [^semicolon]+ [^semicolon]
    #[inline]
    fn reduce___TermSet27Plus28_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__terminals(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant18(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 2);
        let mut A = match __data_stack.pop().unwrap() {
            Data::__terminals(val) => val,
            _ => unreachable!(),
        };
        let mut Ap = match __data_stack.pop().unwrap() {
            Data::__variant18(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Ap.push(A);
            Ap
        };
        if __push_data {
            __data_stack.push(Self::__variant18(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///IdentOrLiteral+ -> IdentOrLiteral
    #[inline]
    fn reduce__IdentOrLiteralPlus29_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant10(_)))
            );
        }
        __location_stack.pop();
        let mut A = match __data_stack.pop().unwrap() {
            Data::__variant10(val) => val,
            _ => unreachable!(),
        };
        let __res = { vec![A] };
        if __push_data {
            __data_stack.push(Self::__variant19(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///IdentOrLiteral+ -> IdentOrLiteral+ IdentOrLiteral
    #[inline]
    fn reduce__IdentOrLiteralPlus29_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 0usize), Some(&
                Data::__variant10(_)))
            );
            debug_assert!(
                matches!(__data_stack.get(__data_stack.len() - 1 - 1usize), Some(&
                Data::__variant19(_)))
            );
        }
        __location_stack.truncate(__location_stack.len() - 2);
        let mut A = match __data_stack.pop().unwrap() {
            Data::__variant10(val) => val,
            _ => unreachable!(),
        };
        let mut Ap = match __data_stack.pop().unwrap() {
            Data::__variant19(val) => val,
            _ => unreachable!(),
        };
        let __res = {
            Ap.push(A);
            Ap
        };
        if __push_data {
            __data_stack.push(Self::__variant19(__res));
        } else {
            __data_stack.push(Self::Empty);
        }
        Ok(())
    }
    ///GrammarLine+ -> GrammarLine
    #[inline]
    fn reduce__GrammarLinePlus30_0(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
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
    ///GrammarLine+ -> GrammarLine GrammarLine+
    #[inline]
    fn reduce__GrammarLinePlus30_1(
        __data_stack: &mut Vec<Self>,
        __location_stack: &mut Vec<Location>,
        __push_data: bool,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Lexed>,
        data: &mut GrammarArgs,
        __rustylr_location0: &mut Location,
    ) -> Result<(), ::rusty_lr_core::DefaultReduceActionError> {
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
}
#[rustfmt::skip]
#[allow(
    unused_braces,
    unused_parens,
    non_snake_case,
    non_camel_case_types,
    unused_variables
)]
impl ::rusty_lr_core::parser::semantic_value::SemanticValue for Data {
    type Term = Lexed;
    type NonTerm = NonTerminals;
    type ReduceActionError = ::rusty_lr_core::DefaultReduceActionError;
    type UserData = GrammarArgs;
    type Location = Location;
    fn new_empty() -> Self {
        Self::Empty
    }
    fn new_terminal(term: Self::Term) -> Self {
        Self::__terminals(term)
    }
    fn reduce_action(
        data_stack: &mut Vec<Self>,
        location_stack: &mut Vec<Location>,
        push_data: bool,
        rule_index: usize,
        shift: &mut bool,
        lookahead: &::rusty_lr_core::TerminalSymbol<Self::Term>,
        user_data: &mut Self::UserData,
        location0: &mut Self::Location,
    ) -> Result<(), Self::ReduceActionError> {
        match rule_index {
            0usize => {
                Self::reduce_Rule_0(
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
                Self::reduce_Rule_1(
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
                Self::reduce_RuleType_0(
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
                Self::reduce_RuleType_1(
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
                Self::reduce_RuleLines_0(
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
                Self::reduce_RuleLines_1(
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
                Self::reduce_RuleLine_0(
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
                Self::reduce_PrecDef_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            8usize => {
                Self::reduce_PrecDef_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            9usize => {
                Self::reduce_PrecDef_2(
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
                Self::reduce_PrecDef_3(
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
                Self::reduce_PrecDef_4(
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
                Self::reduce_MappedSymbol_0(
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
                Self::reduce_MappedSymbol_1(
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
                Self::reduce_MappedSymbol_2(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            15usize => {
                Self::reduce_TerminalSetItem_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            16usize => {
                Self::reduce_TerminalSetItem_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            17usize => {
                Self::reduce_TerminalSetItem_2(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            18usize => {
                Self::reduce_TerminalSetItem_3(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            19usize => {
                Self::reduce_TerminalSetItem_4(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            20usize => {
                Self::reduce_TerminalSetItem_5(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            21usize => {
                Self::reduce_TerminalSetItem_6(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            22usize => {
                Self::reduce_TerminalSetItem_7(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            23usize => {
                Self::reduce_TerminalSetItem_8(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            24usize => {
                Self::reduce_TerminalSet_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            25usize => {
                Self::reduce_TerminalSet_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            26usize => {
                Self::reduce_TerminalSet_2(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            27usize => {
                Self::reduce_Pattern_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            28usize => {
                Self::reduce_Pattern_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            29usize => {
                Self::reduce_Pattern_2(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            30usize => {
                Self::reduce_Pattern_3(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            31usize => {
                Self::reduce_Pattern_4(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            32usize => {
                Self::reduce_Pattern_5(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            33usize => {
                Self::reduce_Pattern_6(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            34usize => {
                Self::reduce_Pattern_7(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            35usize => {
                Self::reduce_Pattern_8(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            36usize => {
                Self::reduce_Pattern_9(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            37usize => {
                Self::reduce_Pattern_10(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            38usize => {
                Self::reduce_Pattern_11(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            39usize => {
                Self::reduce_Pattern_12(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            40usize => {
                Self::reduce_Pattern_13(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            41usize => {
                Self::reduce_Pattern_14(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            42usize => {
                Self::reduce_Pattern_15(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            43usize => {
                Self::reduce_Pattern_16(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            44usize => {
                Self::reduce_Pattern_17(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            45usize => {
                Self::reduce_Action_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            46usize => {
                Self::reduce_Action_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            47usize => {
                Self::reduce_Action_2(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            48usize => {
                Self::reduce_IdentOrLiteral_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            49usize => {
                Self::reduce_IdentOrLiteral_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            50usize => {
                Self::reduce_IdentOrLiteral_2(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            51usize => {
                Self::reduce_AllowTarget_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            52usize => {
                Self::reduce_AllowTarget_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            53usize => {
                Self::reduce_AllowTarget_2(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            54usize => {
                Self::reduce_AllowTarget_3(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            55usize => {
                Self::reduce_AllowTarget_4(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            56usize => {
                Self::reduce_AllowTarget_5(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            57usize => {
                Self::reduce_Directive_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            58usize => {
                Self::reduce_Directive_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            59usize => {
                Self::reduce_Directive_2(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            60usize => {
                Self::reduce_Directive_3(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            61usize => {
                Self::reduce_Directive_4(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            62usize => {
                Self::reduce_Directive_5(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            63usize => {
                Self::reduce_Directive_6(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            64usize => {
                Self::reduce_Directive_7(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            65usize => {
                Self::reduce_Directive_8(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            66usize => {
                Self::reduce_Directive_9(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            67usize => {
                Self::reduce_Directive_10(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            68usize => {
                Self::reduce_Directive_11(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            69usize => {
                Self::reduce_Directive_12(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            70usize => {
                Self::reduce_Directive_13(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            71usize => {
                Self::reduce_Directive_14(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            72usize => {
                Self::reduce_Directive_15(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            73usize => {
                Self::reduce_Directive_16(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            74usize => {
                Self::reduce_Directive_17(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            75usize => {
                Self::reduce_Directive_18(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            76usize => {
                Self::reduce_Directive_19(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            77usize => {
                Self::reduce_Directive_20(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            78usize => {
                Self::reduce_Directive_21(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            79usize => {
                Self::reduce_Directive_22(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            80usize => {
                Self::reduce_Directive_23(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            81usize => {
                Self::reduce_Directive_24(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            82usize => {
                Self::reduce_Directive_25(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            83usize => {
                Self::reduce_Directive_26(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            84usize => {
                Self::reduce_Directive_27(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            85usize => {
                Self::reduce_Directive_28(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            86usize => {
                Self::reduce_Directive_29(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            87usize => {
                Self::reduce_Directive_30(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            88usize => {
                Self::reduce_Directive_31(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            89usize => {
                Self::reduce_GrammarLine_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            92usize => {
                Self::reduce__MappedSymbolPlus16_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            93usize => {
                Self::reduce__MappedSymbolPlus16_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            94usize => {
                Self::reduce__MappedSymbolStar17_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            95usize => {
                Self::reduce__MappedSymbolStar17_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            96usize => {
                Self::reduce__PrecDefPlus18_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            97usize => {
                Self::reduce__PrecDefPlus18_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            98usize => {
                Self::reduce__PrecDefStar19_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            99usize => {
                Self::reduce__PrecDefStar19_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            100usize => {
                Self::reduce__caretQuestion20_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            101usize => {
                Self::reduce__caretQuestion20_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            102usize => {
                Self::reduce__TerminalSetItemPlus21_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            103usize => {
                Self::reduce__TerminalSetItemPlus21_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            104usize => {
                Self::reduce__TerminalSetItemStar22_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            105usize => {
                Self::reduce__TerminalSetItemStar22_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            106usize => {
                Self::reduce__PatternPlus23_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            107usize => {
                Self::reduce__PatternPlus23_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            108usize => {
                Self::reduce__PatternStar24_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            109usize => {
                Self::reduce__PatternStar24_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            110usize => {
                Self::reduce___PatternStar24SepPlus25_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            111usize => {
                Self::reduce___PatternStar24SepPlus25_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            112usize => {
                Self::reduce__commaQuestion26_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            113usize => {
                Self::reduce__commaQuestion26_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            156usize => {
                Self::reduce___TermSet27Plus28_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            157usize => {
                Self::reduce___TermSet27Plus28_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            158usize => {
                Self::reduce__IdentOrLiteralPlus29_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            159usize => {
                Self::reduce__IdentOrLiteralPlus29_1(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            160usize => {
                Self::reduce__GrammarLinePlus30_0(
                    data_stack,
                    location_stack,
                    push_data,
                    shift,
                    lookahead,
                    user_data,
                    location0,
                )
            }
            161usize => {
                Self::reduce__GrammarLinePlus30_1(
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
#[allow(
    unused_braces,
    unused_parens,
    unused_variables,
    non_snake_case,
    unused_mut
)]
#[derive(Clone, Copy)]
pub struct Parser;
unsafe impl ::std::marker::Send for Parser {}
unsafe impl ::std::marker::Sync for Parser {}
#[rustfmt::skip]
impl ::rusty_lr_core::parser::Parser for Parser {
    type Term = Lexed;
    type TermClass = TerminalClasses;
    type NonTerm = NonTerminals;
    type StateIndex = u8;
    type ReduceRules = u8;
    type Tables = Tables;
    const ERROR_USED: bool = true;
    fn get_tables() -> &'static Tables {
        static TABLES: std::sync::OnceLock<Tables> = std::sync::OnceLock::new();
        TABLES
            .get_or_init(|| {
                static RULE_NAMES: &[u32] = &[
                    0, 0, 1, 1, 2, 2, 3, 4, 4, 4, 4, 4, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6,
                    6, 7, 7, 7, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 9,
                    9, 9, 10, 10, 10, 11, 11, 11, 11, 11, 11, 12, 12, 12, 12, 12, 12, 12,
                    12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12,
                    12, 12, 12, 12, 12, 12, 12, 12, 13, 13, 14, 15, 15, 16, 16, 17, 17,
                    18, 18, 19, 19, 20, 20, 21, 21, 22, 22, 23, 23, 24, 24, 25, 25, 26,
                    26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26,
                    26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26,
                    26, 26, 26, 26, 26, 26, 26, 27, 27, 28, 28, 29, 29, 30,
                ];
                static RULE_LENGTHS: &[u32] = &[
                    5, 6, 1, 0, 3, 1, 3, 3, 3, 3, 3, 2, 1, 3, 3, 1, 3, 3, 1, 3, 3, 1, 3,
                    3, 4, 4, 1, 1, 2, 2, 2, 2, 1, 3, 3, 1, 1, 1, 1, 3, 8, 9, 9, 8, 9, 1,
                    1, 0, 1, 1, 1, 1, 1, 1, 3, 3, 1, 5, 4, 4, 4, 4, 4, 3, 4, 3, 4, 4, 4,
                    4, 4, 4, 4, 3, 4, 3, 3, 4, 3, 4, 3, 4, 4, 3, 4, 7, 7, 4, 3, 1, 1, 1,
                    1, 2, 1, 0, 1, 2, 1, 0, 1, 0, 1, 2, 1, 0, 1, 2, 1, 0, 1, 3, 1, 0, 1,
                    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 2, 1,
                    2, 3,
                ];
                static SHIFT_TERM_DATA: &[u32] = &[
                    2147516461, 2147549184, 2150858756, 2147581967, 2147647490,
                    2147680256, 2147778567, 2147811336, 2147909643, 2147942412,
                    2147975181, 2148007950, 2148040721, 2148073491, 2147713029,
                    2147745792, 2147778567, 2147811336, 2147909643, 2147942412,
                    2147975181, 2148007950, 2148040721, 2148073491, 2149810219,
                    2147844096, 2147876881, 2147745792, 2147778567, 2147811336,
                    2147909643, 2147942412, 2147975181, 2148007950, 2148040721,
                    2148073491, 2147745792, 2147778567, 2147811336, 2147909643,
                    2147942412, 2147975181, 2148007950, 2148040721, 2148073491,
                    2148892715, 2148106246, 2148171776, 2148302859, 2148433933,
                    2148565035, 2148204586, 2148237312, 2148270123, 2148335658,
                    2148368395, 2148401195, 2148466730, 2148499469, 2148532267,
                    2148597780, 2148171776, 2148302859, 2148433933, 2148761620,
                    2148859913, 2148991014, 2149023783, 2149056552, 2149089321,
                    2149122090, 2147745792, 2147778567, 2147811336, 2147909643,
                    2147942412, 2147975181, 2148007950, 2148040721, 2148073491,
                    2148925458, 2148991014, 2149023783, 2149056552, 2149089321,
                    2149122090, 2147745792, 2147778567, 2147811336, 2147909643,
                    2147942412, 2147975181, 2148007950, 2148040721, 2148073491,
                    2148991014, 2149023783, 2149056552, 2149089321, 2147745792,
                    2147778567, 2147811336, 2147909643, 2147942412, 2147975181,
                    2148007950, 2148040721, 2148073491, 2148991014, 2149023783,
                    2149056552, 2149089321, 2149122090, 2149318659, 2149384210,
                    2147745792, 2147778567, 2147811336, 2147909643, 2147942412,
                    2147975181, 2148007950, 2148040721, 2148073491, 2149449737,
                    2148991014, 2149023783, 2149056552, 2149089321, 2149122090,
                    2149679147, 2149482534, 2149548071, 2149613611, 2149515282,
                    2149580818, 2149646354, 2149711890, 2149777426, 2148991014,
                    2149023783, 2149056552, 2149089321, 2149122090, 2149908483, 3244069,
                    2150760491, 2147680256, 2147778567, 2147811336, 2147909643,
                    2147942412, 2147975181, 2148007950, 2148040721, 2148073491,
                    2148991014, 2149023783, 2149056552, 2149089321, 2149122090,
                    2147680256, 2147778567, 2147811336, 2147909643, 2147942412,
                    2147975181, 2148007950, 2148040721, 2148073491, 2150137860,
                    2150170655, 2150367266, 2150465579, 2150203392, 2150236171,
                    2150268941, 2150301739, 2150400010, 2150432811, 2150137860,
                    2150629392, 2150662187, 3309605, 2150891541, 2151120918, 2151284759,
                    2151579672, 2151743513, 2151874586, 2152005659, 2152136732,
                    2152267805, 2152398878, 2152529952, 2152693793, 2152824867,
                    2152955940, 2153611307, 2150203392, 2150236171, 2150268941,
                    2150924331, 3473445, 2150203392, 2150236171, 2150268941, 3571749,
                    2150203392, 2150236171, 2150268941, 2151153707, 3702821, 2150203392,
                    2150236171, 2150268941, 3768357, 2151317504, 2151514155, 2151383040,
                    2151383041, 2151383042, 2151383043, 2151383044, 2151383045,
                    2151383046, 2151383047, 2151383048, 2151383049, 2151383050,
                    2151383051, 2151383052, 2151383053, 2151383054, 2151383055,
                    2151383056, 2151383057, 2151383058, 2151383059, 2151383060,
                    2151383061, 2151383062, 2151383063, 2151383064, 2151383065,
                    2151383066, 2151383067, 2151383068, 2151383069, 2151383070,
                    2151383071, 2151383072, 2151383073, 2151383074, 2151383075,
                    2151383076, 3866661, 2151383078, 2151383079, 2151383080, 2151383081,
                    2151383082, 2151481344, 2151481345, 2151481346, 2151481347,
                    2151481348, 2151481349, 2151481350, 2151481351, 2151481352,
                    2151481353, 2151481354, 2151481355, 2151481356, 2151481357,
                    2151481358, 2151481359, 2151481360, 2151481361, 2151481362,
                    2151481363, 2151481364, 2151481365, 2151481366, 2151481367,
                    2151481368, 2151481369, 2151481370, 2151481371, 2151481372,
                    2151481373, 2151481374, 2151481375, 2151481376, 2151481377,
                    2151481378, 2151481379, 2151481380, 3964965, 2151481382, 2151481383,
                    2151481384, 2151481385, 2151481386, 4063269, 2151612416, 2151677995,
                    4161573, 4227109, 2151383040, 2151383041, 2151383042, 2151383043,
                    2151383044, 2151383045, 2151383046, 2151383047, 2151383048,
                    2151383049, 2151383050, 2151383051, 2151383052, 2151383053,
                    2151383054, 2151383055, 2151383056, 2151383057, 2151383058,
                    2151383059, 2151383060, 2151383061, 2151383062, 2151383063,
                    2151383064, 2151383065, 2151383066, 2151383067, 2151383068,
                    2151383069, 2151383070, 2151383071, 2151383072, 2151383073,
                    2151383074, 2151383075, 2151383076, 4292645, 2151383078, 2151383079,
                    2151383080, 2151383081, 2151383082, 2151481344, 2151481345,
                    2151481346, 2151481347, 2151481348, 2151481349, 2151481350,
                    2151481351, 2151481352, 2151481353, 2151481354, 2151481355,
                    2151481356, 2151481357, 2151481358, 2151481359, 2151481360,
                    2151481361, 2151481362, 2151481363, 2151481364, 2151481365,
                    2151481366, 2151481367, 2151481368, 2151481369, 2151481370,
                    2151481371, 2151481372, 2151481373, 2151481374, 2151481375,
                    2151481376, 2151481377, 2151481378, 2151481379, 2151481380, 4358181,
                    2151481382, 2151481383, 2151481384, 2151481385, 2151481386,
                    2151383040, 2151383041, 2151383042, 2151383043, 2151383044,
                    2151383045, 2151383046, 2151383047, 2151383048, 2151383049,
                    2151383050, 2151383051, 2151383052, 2151383053, 2151383054,
                    2151383055, 2151383056, 2151383057, 2151383058, 2151383059,
                    2151383060, 2151383061, 2151383062, 2151383063, 2151383064,
                    2151383065, 2151383066, 2151383067, 2151383068, 2151383069,
                    2151383070, 2151383071, 2151383072, 2151383073, 2151383074,
                    2151383075, 2151383076, 4423717, 2151383078, 2151383079, 2151383080,
                    2151383081, 2151383082, 2151481344, 2151481345, 2151481346,
                    2151481347, 2151481348, 2151481349, 2151481350, 2151481351,
                    2151481352, 2151481353, 2151481354, 2151481355, 2151481356,
                    2151481357, 2151481358, 2151481359, 2151481360, 2151481361,
                    2151481362, 2151481363, 2151481364, 2151481365, 2151481366,
                    2151481367, 2151481368, 2151481369, 2151481370, 2151481371,
                    2151481372, 2151481373, 2151481374, 2151481375, 2151481376,
                    2151481377, 2151481378, 2151481379, 2151481380, 4489253, 2151481382,
                    2151481383, 2151481384, 2151481385, 2151481386, 2151383040,
                    2151383041, 2151383042, 2151383043, 2151383044, 2151383045,
                    2151383046, 2151383047, 2151383048, 2151383049, 2151383050,
                    2151383051, 2151383052, 2151383053, 2151383054, 2151383055,
                    2151383056, 2151383057, 2151383058, 2151383059, 2151383060,
                    2151383061, 2151383062, 2151383063, 2151383064, 2151383065,
                    2151383066, 2151383067, 2151383068, 2151383069, 2151383070,
                    2151383071, 2151383072, 2151383073, 2151383074, 2151383075,
                    2151383076, 4554789, 2151383078, 2151383079, 2151383080, 2151383081,
                    2151383082, 2151481344, 2151481345, 2151481346, 2151481347,
                    2151481348, 2151481349, 2151481350, 2151481351, 2151481352,
                    2151481353, 2151481354, 2151481355, 2151481356, 2151481357,
                    2151481358, 2151481359, 2151481360, 2151481361, 2151481362,
                    2151481363, 2151481364, 2151481365, 2151481366, 2151481367,
                    2151481368, 2151481369, 2151481370, 2151481371, 2151481372,
                    2151481373, 2151481374, 2151481375, 2151481376, 2151481377,
                    2151481378, 2151481379, 2151481380, 4620325, 2151481382, 2151481383,
                    2151481384, 2151481385, 2151481386, 2151383040, 2151383041,
                    2151383042, 2151383043, 2151383044, 2151383045, 2151383046,
                    2151383047, 2151383048, 2151383049, 2151383050, 2151383051,
                    2151383052, 2151383053, 2151383054, 2151383055, 2151383056,
                    2151383057, 2151383058, 2151383059, 2151383060, 2151383061,
                    2151383062, 2151383063, 2151383064, 2151383065, 2151383066,
                    2151383067, 2151383068, 2151383069, 2151383070, 2151383071,
                    2151383072, 2151383073, 2151383074, 2151383075, 2151383076, 4685861,
                    2151383078, 2151383079, 2151383080, 2151383081, 2151383082,
                    2151481344, 2151481345, 2151481346, 2151481347, 2151481348,
                    2151481349, 2151481350, 2151481351, 2151481352, 2151481353,
                    2151481354, 2151481355, 2151481356, 2151481357, 2151481358,
                    2151481359, 2151481360, 2151481361, 2151481362, 2151481363,
                    2151481364, 2151481365, 2151481366, 2151481367, 2151481368,
                    2151481369, 2151481370, 2151481371, 2151481372, 2151481373,
                    2151481374, 2151481375, 2151481376, 2151481377, 2151481378,
                    2151481379, 2151481380, 4751397, 2151481382, 2151481383, 2151481384,
                    2151481385, 2151481386, 4816933, 2152333355, 4882469, 4948005,
                    2152464427, 5013541, 2150203392, 2150236171, 2150268941, 2152562731,
                    5111845, 2150203392, 2150236171, 2150268941, 5177381, 5242917,
                    2152759339, 5308453, 2151383040, 2151383041, 2151383042, 2151383043,
                    2151383044, 2151383045, 2151383046, 2151383047, 2151383048,
                    2151383049, 2151383050, 2151383051, 2151383052, 2151383053,
                    2151383054, 2151383055, 2151383056, 2151383057, 2151383058,
                    2151383059, 2151383060, 2151383061, 2151383062, 2151383063,
                    2151383064, 2151383065, 2151383066, 2151383067, 2151383068,
                    2151383069, 2151383070, 2151383071, 2151383072, 2151383073,
                    2151383074, 2151383075, 2151383076, 5373989, 2151383078, 2151383079,
                    2151383080, 2151383081, 2151383082, 2151481344, 2151481345,
                    2151481346, 2151481347, 2151481348, 2151481349, 2151481350,
                    2151481351, 2151481352, 2151481353, 2151481354, 2151481355,
                    2151481356, 2151481357, 2151481358, 2151481359, 2151481360,
                    2151481361, 2151481362, 2151481363, 2151481364, 2151481365,
                    2151481366, 2151481367, 2151481368, 2151481369, 2151481370,
                    2151481371, 2151481372, 2151481373, 2151481374, 2151481375,
                    2151481376, 2151481377, 2151481378, 2151481379, 2151481380, 5439525,
                    2151481382, 2151481383, 2151481384, 2151481385, 2151481386,
                    2152988672, 2153545771, 2153021457, 6029349, 2153054208, 2147778567,
                    2153086987, 2153185293, 2148073491, 2153283627, 2153119786,
                    2153152523, 2153218090, 2153250829, 2153316370, 5865509, 2153447442,
                    5996581, 6094885, 6160421, 2147549184, 2150858756, 2153807916,
                ];
                static SHIFT_TERM_OFFSETS: &[u32] = &[
                    0, 1, 3, 4, 4, 5, 14, 15, 25, 25, 25, 26, 27, 36, 36, 36, 36, 36, 46,
                    47, 47, 51, 52, 54, 54, 54, 55, 57, 57, 57, 58, 60, 60, 60, 61, 61,
                    61, 64, 64, 65, 65, 65, 71, 80, 81, 81, 86, 86, 86, 86, 86, 95, 99,
                    108, 113, 113, 115, 124, 124, 124, 131, 134, 135, 135, 136, 136, 137,
                    137, 138, 138, 139, 139, 139, 144, 147, 156, 156, 156, 161, 170, 170,
                    171, 174, 178, 178, 178, 178, 178, 178, 180, 180, 180, 180, 180, 181,
                    181, 183, 183, 183, 183, 183, 184, 184, 184, 199, 203, 204, 204, 204,
                    208, 208, 208, 212, 213, 213, 217, 217, 219, 262, 262, 262, 305, 305,
                    305, 306, 306, 308, 309, 309, 310, 310, 353, 353, 396, 396, 439, 439,
                    482, 482, 525, 525, 568, 568, 611, 611, 654, 654, 656, 656, 657, 657,
                    659, 659, 660, 660, 664, 665, 665, 669, 669, 671, 671, 672, 672, 715,
                    715, 758, 758, 760, 762, 768, 768, 769, 770, 770, 771, 772, 772, 773,
                    774, 774, 774, 775, 776, 776, 776, 777, 777, 778, 778, 778, 780, 780,
                    781, 781,
                ];
                static SHIFT_NONTERM_DATA: &[u32] = &[
                    2153676800, 6225932, 2153709581, 2153775118, 6291485, 2147614721,
                    2149875714, 2150825987, 2149974021, 2148794375, 2150006792,
                    2150039567, 2150105104, 2148794375, 2149842952, 2148794375,
                    2148827144, 2148794375, 2148958216, 2149187606, 2149253143,
                    2149285912, 2148139027, 2148630534, 2148663316, 2148728853,
                    2148696070, 2148794375, 2149416968, 2148794375, 2149154824,
                    2148794375, 2149220360, 2148794375, 2148958216, 2149187606,
                    2149351447, 2149744665, 2149941251, 2149974021, 2148794375,
                    2150006792, 2150039567, 2150105104, 2150072325, 2148794375,
                    2150006792, 2150498308, 2150531089, 2150596626, 2150334474,
                    2150563844, 2150694921, 2150989834, 2151022620, 2151088138,
                    2150989834, 2151219228, 2151088138, 2151383066, 2151415835,
                    2151481370, 2151383066, 2151809051, 2151481370, 2151383066,
                    2151940123, 2151481370, 2151383066, 2152071195, 2151481370,
                    2151383066, 2152202267, 2151481370, 2150989834, 2152628252,
                    2151088138, 2151383066, 2152890395, 2151481370, 2153381895,
                    2153414667, 2153676800, 6225932, 2153709581, 2153742365,
                ];
                static SHIFT_NONTERM_OFFSETS: &[u32] = &[
                    0, 0, 5, 6, 6, 6, 13, 13, 15, 15, 15, 15, 15, 17, 17, 17, 17, 17, 22,
                    23, 23, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26,
                    26, 27, 27, 27, 27, 27, 27, 29, 29, 29, 29, 29, 29, 29, 29, 31, 31,
                    33, 33, 33, 33, 37, 37, 37, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
                    38, 38, 38, 38, 38, 44, 44, 44, 44, 47, 47, 50, 50, 51, 51, 51, 51,
                    51, 51, 51, 51, 51, 51, 51, 52, 52, 53, 53, 53, 53, 53, 53, 53, 53,
                    53, 55, 55, 55, 55, 56, 56, 56, 58, 58, 58, 59, 59, 59, 61, 61, 61,
                    62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 64, 64, 65, 65, 67, 67, 68,
                    68, 70, 70, 71, 71, 73, 73, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74,
                    76, 76, 76, 77, 77, 77, 77, 77, 77, 79, 79, 80, 80, 80, 80, 82, 82,
                    82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82,
                    82, 82, 86, 86, 86, 86,
                ];
                static REDUCE_DATA: &[u32] = &[
                    2, 1, 3, 2, 1, 2, 3, 1, 95, 4, 1, 95, 16, 1, 95, 37, 1, 95, 43, 1,
                    95, 0, 1, 27, 3, 1, 27, 4, 1, 27, 7, 1, 27, 8, 1, 27, 11, 1, 27, 12,
                    1, 27, 13, 1, 27, 14, 1, 27, 16, 1, 27, 17, 1, 27, 19, 1, 27, 37, 1,
                    27, 38, 1, 27, 39, 1, 27, 40, 1, 27, 41, 1, 27, 42, 1, 27, 43, 1, 27,
                    0, 1, 27, 3, 1, 27, 4, 1, 27, 7, 1, 27, 8, 1, 27, 9, 1, 27, 11, 1,
                    27, 12, 1, 27, 13, 1, 27, 14, 1, 27, 16, 1, 27, 17, 1, 27, 18, 1, 27,
                    19, 1, 27, 37, 1, 27, 38, 1, 27, 39, 1, 27, 40, 1, 27, 41, 1, 27, 42,
                    1, 27, 43, 1, 27, 0, 1, 26, 3, 1, 26, 4, 1, 26, 7, 1, 26, 8, 1, 26,
                    9, 1, 26, 11, 1, 26, 12, 1, 26, 13, 1, 26, 14, 1, 26, 16, 1, 26, 17,
                    1, 26, 18, 1, 26, 19, 1, 26, 37, 1, 26, 38, 1, 26, 39, 1, 26, 40, 1,
                    26, 41, 1, 26, 42, 1, 26, 43, 1, 26, 0, 1, 35, 3, 1, 35, 4, 1, 35, 7,
                    1, 35, 8, 1, 35, 9, 1, 35, 11, 1, 35, 12, 1, 35, 13, 1, 35, 14, 1,
                    35, 16, 1, 35, 17, 1, 35, 18, 1, 35, 19, 1, 35, 37, 1, 35, 38, 1, 35,
                    39, 1, 35, 40, 1, 35, 41, 1, 35, 42, 1, 35, 43, 1, 35, 0, 1, 36, 3,
                    1, 36, 4, 1, 36, 7, 1, 36, 8, 1, 36, 9, 1, 36, 11, 1, 36, 12, 1, 36,
                    13, 1, 36, 14, 1, 36, 16, 1, 36, 17, 1, 36, 18, 1, 36, 19, 1, 36, 37,
                    1, 36, 38, 1, 36, 39, 1, 36, 40, 1, 36, 41, 1, 36, 42, 1, 36, 43, 1,
                    36, 0, 1, 37, 3, 1, 37, 4, 1, 37, 7, 1, 37, 8, 1, 37, 9, 1, 37, 11,
                    1, 37, 12, 1, 37, 13, 1, 37, 14, 1, 37, 16, 1, 37, 17, 1, 37, 18, 1,
                    37, 19, 1, 37, 37, 1, 37, 38, 1, 37, 39, 1, 37, 40, 1, 37, 41, 1, 37,
                    42, 1, 37, 43, 1, 37, 0, 1, 38, 3, 1, 38, 4, 1, 38, 7, 1, 38, 8, 1,
                    38, 9, 1, 38, 11, 1, 38, 12, 1, 38, 13, 1, 38, 14, 1, 38, 16, 1, 38,
                    17, 1, 38, 18, 1, 38, 19, 1, 38, 37, 1, 38, 38, 1, 38, 39, 1, 38, 40,
                    1, 38, 41, 1, 38, 42, 1, 38, 43, 1, 38, 3, 1, 109, 18, 1, 109, 0, 1,
                    101, 11, 1, 101, 13, 1, 101, 20, 1, 101, 43, 1, 101, 0, 1, 100, 11,
                    1, 100, 13, 1, 100, 20, 1, 100, 43, 1, 100, 20, 1, 105, 0, 1, 15, 11,
                    1, 15, 13, 1, 15, 20, 1, 15, 0, 1, 16, 11, 1, 16, 13, 1, 16, 20, 1,
                    16, 0, 1, 17, 11, 1, 17, 13, 1, 17, 20, 1, 17, 0, 1, 21, 11, 1, 21,
                    13, 1, 21, 20, 1, 21, 0, 1, 22, 11, 1, 22, 13, 1, 22, 20, 1, 22, 0,
                    1, 23, 11, 1, 23, 13, 1, 23, 20, 1, 23, 0, 1, 18, 11, 1, 18, 13, 1,
                    18, 20, 1, 18, 0, 1, 19, 11, 1, 19, 13, 1, 19, 20, 1, 19, 0, 1, 20,
                    11, 1, 20, 13, 1, 20, 20, 1, 20, 0, 1, 25, 3, 1, 25, 4, 1, 25, 7, 1,
                    25, 8, 1, 25, 9, 1, 25, 11, 1, 25, 12, 1, 25, 13, 1, 25, 14, 1, 25,
                    16, 1, 25, 17, 1, 25, 18, 1, 25, 19, 1, 25, 37, 1, 25, 38, 1, 25, 39,
                    1, 25, 40, 1, 25, 41, 1, 25, 42, 1, 25, 43, 1, 25, 0, 1, 102, 11, 1,
                    102, 13, 1, 102, 20, 1, 102, 20, 1, 104, 0, 1, 103, 11, 1, 103, 13,
                    1, 103, 20, 1, 103, 0, 1, 24, 3, 1, 24, 4, 1, 24, 7, 1, 24, 8, 1, 24,
                    9, 1, 24, 11, 1, 24, 12, 1, 24, 13, 1, 24, 14, 1, 24, 16, 1, 24, 17,
                    1, 24, 18, 1, 24, 19, 1, 24, 37, 1, 24, 38, 1, 24, 39, 1, 24, 40, 1,
                    24, 41, 1, 24, 42, 1, 24, 43, 1, 24, 0, 1, 32, 3, 1, 32, 4, 1, 32, 7,
                    1, 32, 8, 1, 32, 9, 1, 32, 11, 1, 32, 12, 1, 32, 13, 1, 32, 14, 1,
                    32, 16, 1, 32, 17, 1, 32, 18, 1, 32, 19, 1, 32, 37, 1, 32, 38, 1, 32,
                    39, 1, 32, 40, 1, 32, 41, 1, 32, 42, 1, 32, 43, 1, 32, 0, 1, 34, 3,
                    1, 34, 4, 1, 34, 7, 1, 34, 8, 1, 34, 9, 1, 34, 11, 1, 34, 12, 1, 34,
                    13, 1, 34, 14, 1, 34, 16, 1, 34, 17, 1, 34, 18, 1, 34, 19, 1, 34, 37,
                    1, 34, 38, 1, 34, 39, 1, 34, 40, 1, 34, 41, 1, 34, 42, 1, 34, 43, 1,
                    34, 0, 1, 106, 3, 1, 106, 7, 1, 106, 8, 1, 106, 11, 1, 106, 12, 1,
                    106, 13, 1, 106, 14, 1, 106, 17, 1, 106, 18, 1, 106, 19, 1, 106, 0,
                    1, 28, 3, 1, 28, 4, 1, 28, 7, 1, 28, 8, 1, 28, 9, 1, 28, 11, 1, 28,
                    12, 1, 28, 13, 1, 28, 14, 1, 28, 16, 1, 28, 17, 1, 28, 18, 1, 28, 19,
                    1, 28, 37, 1, 28, 38, 1, 28, 39, 1, 28, 40, 1, 28, 41, 1, 28, 42, 1,
                    28, 43, 1, 28, 0, 1, 29, 3, 1, 29, 4, 1, 29, 7, 1, 29, 8, 1, 29, 9,
                    1, 29, 11, 1, 29, 12, 1, 29, 13, 1, 29, 14, 1, 29, 16, 1, 29, 17, 1,
                    29, 18, 1, 29, 19, 1, 29, 37, 1, 29, 38, 1, 29, 39, 1, 29, 40, 1, 29,
                    41, 1, 29, 42, 1, 29, 43, 1, 29, 0, 1, 30, 3, 1, 30, 4, 1, 30, 7, 1,
                    30, 8, 1, 30, 9, 1, 30, 11, 1, 30, 12, 1, 30, 13, 1, 30, 14, 1, 30,
                    16, 1, 30, 17, 1, 30, 18, 1, 30, 19, 1, 30, 37, 1, 30, 38, 1, 30, 39,
                    1, 30, 40, 1, 30, 41, 1, 30, 42, 1, 30, 43, 1, 30, 0, 1, 31, 3, 1,
                    31, 4, 1, 31, 7, 1, 31, 8, 1, 31, 9, 1, 31, 11, 1, 31, 12, 1, 31, 13,
                    1, 31, 14, 1, 31, 16, 1, 31, 17, 1, 31, 18, 1, 31, 19, 1, 31, 37, 1,
                    31, 38, 1, 31, 39, 1, 31, 40, 1, 31, 41, 1, 31, 42, 1, 31, 43, 1, 31,
                    0, 1, 39, 3, 1, 39, 4, 1, 39, 7, 1, 39, 8, 1, 39, 9, 1, 39, 11, 1,
                    39, 12, 1, 39, 13, 1, 39, 14, 1, 39, 16, 1, 39, 17, 1, 39, 18, 1, 39,
                    19, 1, 39, 37, 1, 39, 42, 1, 39, 43, 1, 39, 3, 1, 108, 18, 1, 108, 0,
                    1, 107, 3, 1, 107, 7, 1, 107, 8, 1, 107, 11, 1, 107, 12, 1, 107, 13,
                    1, 107, 14, 1, 107, 17, 1, 107, 18, 1, 107, 19, 1, 107, 3, 1, 110,
                    18, 1, 110, 3, 1, 109, 18, 1, 109, 3, 1, 111, 18, 1, 111, 0, 1, 33,
                    3, 1, 33, 4, 1, 33, 7, 1, 33, 8, 1, 33, 9, 1, 33, 11, 1, 33, 12, 1,
                    33, 13, 1, 33, 14, 1, 33, 16, 1, 33, 17, 1, 33, 18, 1, 33, 19, 1, 33,
                    37, 1, 33, 38, 1, 33, 39, 1, 33, 40, 1, 33, 41, 1, 33, 42, 1, 33, 43,
                    1, 33, 18, 1, 113, 18, 1, 112, 0, 1, 41, 3, 1, 41, 4, 1, 41, 7, 1,
                    41, 8, 1, 41, 9, 1, 41, 11, 1, 41, 12, 1, 41, 13, 1, 41, 14, 1, 41,
                    16, 1, 41, 17, 1, 41, 18, 1, 41, 19, 1, 41, 37, 1, 41, 38, 1, 41, 39,
                    1, 41, 40, 1, 41, 41, 1, 41, 42, 1, 41, 43, 1, 41, 0, 1, 42, 3, 1,
                    42, 4, 1, 42, 7, 1, 42, 8, 1, 42, 9, 1, 42, 11, 1, 42, 12, 1, 42, 13,
                    1, 42, 14, 1, 42, 16, 1, 42, 17, 1, 42, 18, 1, 42, 19, 1, 42, 37, 1,
                    42, 38, 1, 42, 39, 1, 42, 40, 1, 42, 41, 1, 42, 42, 1, 42, 43, 1, 42,
                    0, 1, 44, 3, 1, 44, 4, 1, 44, 7, 1, 44, 8, 1, 44, 9, 1, 44, 11, 1,
                    44, 12, 1, 44, 13, 1, 44, 14, 1, 44, 16, 1, 44, 17, 1, 44, 18, 1, 44,
                    19, 1, 44, 37, 1, 44, 38, 1, 44, 39, 1, 44, 40, 1, 44, 41, 1, 44, 42,
                    1, 44, 43, 1, 44, 0, 1, 43, 3, 1, 43, 4, 1, 43, 7, 1, 43, 8, 1, 43,
                    9, 1, 43, 11, 1, 43, 12, 1, 43, 13, 1, 43, 14, 1, 43, 16, 1, 43, 17,
                    1, 43, 18, 1, 43, 19, 1, 43, 37, 1, 43, 38, 1, 43, 39, 1, 43, 40, 1,
                    43, 41, 1, 43, 42, 1, 43, 43, 1, 43, 0, 1, 40, 3, 1, 40, 4, 1, 40, 7,
                    1, 40, 8, 1, 40, 9, 1, 40, 11, 1, 40, 12, 1, 40, 13, 1, 40, 14, 1,
                    40, 16, 1, 40, 17, 1, 40, 18, 1, 40, 19, 1, 40, 37, 1, 40, 38, 1, 40,
                    39, 1, 40, 40, 1, 40, 41, 1, 40, 42, 1, 40, 43, 1, 40, 0, 1, 14, 3,
                    1, 14, 4, 1, 14, 7, 1, 14, 8, 1, 14, 11, 1, 14, 12, 1, 14, 13, 1, 14,
                    14, 1, 14, 16, 1, 14, 17, 1, 14, 19, 1, 14, 37, 1, 14, 43, 1, 14, 0,
                    1, 13, 3, 1, 13, 4, 1, 13, 7, 1, 13, 8, 1, 13, 11, 1, 13, 12, 1, 13,
                    13, 1, 13, 14, 1, 13, 16, 1, 13, 17, 1, 13, 19, 1, 13, 37, 1, 13, 43,
                    1, 13, 3, 1, 95, 4, 1, 95, 16, 1, 95, 37, 1, 95, 43, 1, 95, 3, 1, 4,
                    37, 1, 4, 43, 1, 4, 0, 1, 92, 3, 1, 92, 4, 1, 92, 7, 1, 92, 8, 1, 92,
                    11, 1, 92, 12, 1, 92, 13, 1, 92, 14, 1, 92, 16, 1, 92, 17, 1, 92, 19,
                    1, 92, 37, 1, 92, 43, 1, 92, 0, 1, 12, 3, 1, 12, 4, 1, 12, 7, 1, 12,
                    8, 1, 12, 11, 1, 12, 12, 1, 12, 13, 1, 12, 14, 1, 12, 16, 1, 12, 17,
                    1, 12, 19, 1, 12, 37, 1, 12, 43, 1, 12, 3, 1, 94, 4, 1, 94, 16, 1,
                    94, 37, 1, 94, 43, 1, 94, 0, 1, 93, 3, 1, 93, 4, 1, 93, 7, 1, 93, 8,
                    1, 93, 11, 1, 93, 12, 1, 93, 13, 1, 93, 14, 1, 93, 16, 1, 93, 17, 1,
                    93, 19, 1, 93, 37, 1, 93, 43, 1, 93, 3, 1, 99, 16, 1, 99, 37, 1, 99,
                    43, 1, 99, 0, 1, 48, 3, 1, 48, 4, 1, 48, 11, 1, 48, 13, 1, 48, 16, 1,
                    48, 37, 1, 48, 43, 1, 48, 0, 1, 49, 3, 1, 49, 4, 1, 49, 11, 1, 49,
                    13, 1, 49, 16, 1, 49, 37, 1, 49, 43, 1, 49, 0, 1, 50, 3, 1, 50, 4, 1,
                    50, 11, 1, 50, 13, 1, 50, 16, 1, 50, 37, 1, 50, 43, 1, 50, 3, 1, 8,
                    4, 1, 8, 16, 1, 8, 37, 1, 8, 43, 1, 8, 3, 1, 7, 4, 1, 7, 16, 1, 7,
                    37, 1, 7, 43, 1, 7, 3, 1, 9, 4, 1, 9, 16, 1, 9, 37, 1, 9, 43, 1, 9,
                    3, 1, 10, 4, 1, 10, 16, 1, 10, 37, 1, 10, 43, 1, 10, 3, 1, 11, 4, 1,
                    11, 16, 1, 11, 37, 1, 11, 43, 1, 11, 3, 1, 96, 4, 1, 96, 16, 1, 96,
                    37, 1, 96, 43, 1, 96, 3, 1, 98, 16, 1, 98, 37, 1, 98, 43, 1, 98, 3,
                    1, 97, 4, 1, 97, 16, 1, 97, 37, 1, 97, 43, 1, 97, 3, 1, 47, 37, 1,
                    47, 3, 1, 45, 37, 1, 45, 43, 1, 45, 3, 1, 46, 37, 1, 46, 43, 1, 46,
                    3, 1, 6, 37, 1, 6, 43, 1, 6, 0, 1, 0, 4, 1, 0, 44, 1, 0, 0, 1, 1, 4,
                    1, 1, 44, 1, 1, 3, 1, 5, 37, 1, 5, 43, 1, 5, 0, 1, 67, 4, 1, 67, 44,
                    1, 67, 0, 1, 158, 11, 1, 158, 13, 1, 158, 37, 1, 158, 0, 1, 66, 4, 1,
                    66, 44, 1, 66, 0, 1, 159, 11, 1, 159, 13, 1, 159, 37, 1, 159, 0, 1,
                    69, 4, 1, 69, 44, 1, 69, 0, 1, 68, 4, 1, 68, 44, 1, 68, 0, 1, 58, 4,
                    1, 58, 44, 1, 58, 0, 1, 156, 1, 1, 156, 2, 1, 156, 3, 1, 156, 4, 1,
                    156, 5, 1, 156, 6, 1, 156, 7, 1, 156, 8, 1, 156, 9, 1, 156, 10, 1,
                    156, 11, 1, 156, 12, 1, 156, 13, 1, 156, 14, 1, 156, 15, 1, 156, 16,
                    1, 156, 17, 1, 156, 18, 1, 156, 19, 1, 156, 20, 1, 156, 21, 1, 156,
                    22, 1, 156, 23, 1, 156, 24, 1, 156, 25, 1, 156, 26, 1, 156, 27, 1,
                    156, 28, 1, 156, 29, 1, 156, 30, 1, 156, 31, 1, 156, 32, 1, 156, 33,
                    1, 156, 34, 1, 156, 35, 1, 156, 36, 1, 156, 37, 1, 156, 38, 1, 156,
                    39, 1, 156, 40, 1, 156, 41, 1, 156, 42, 1, 156, 0, 1, 57, 4, 1, 57,
                    44, 1, 57, 0, 1, 157, 1, 1, 157, 2, 1, 157, 3, 1, 157, 4, 1, 157, 5,
                    1, 157, 6, 1, 157, 7, 1, 157, 8, 1, 157, 9, 1, 157, 10, 1, 157, 11,
                    1, 157, 12, 1, 157, 13, 1, 157, 14, 1, 157, 15, 1, 157, 16, 1, 157,
                    17, 1, 157, 18, 1, 157, 19, 1, 157, 20, 1, 157, 21, 1, 157, 22, 1,
                    157, 23, 1, 157, 24, 1, 157, 25, 1, 157, 26, 1, 157, 27, 1, 157, 28,
                    1, 157, 29, 1, 157, 30, 1, 157, 31, 1, 157, 32, 1, 157, 33, 1, 157,
                    34, 1, 157, 35, 1, 157, 36, 1, 157, 37, 1, 157, 38, 1, 157, 39, 1,
                    157, 40, 1, 157, 41, 1, 157, 42, 1, 157, 0, 1, 59, 4, 1, 59, 44, 1,
                    59, 0, 1, 60, 4, 1, 60, 44, 1, 60, 0, 1, 61, 4, 1, 61, 44, 1, 61, 0,
                    1, 63, 4, 1, 63, 44, 1, 63, 0, 1, 62, 4, 1, 62, 44, 1, 62, 0, 1, 65,
                    4, 1, 65, 44, 1, 65, 0, 1, 64, 4, 1, 64, 44, 1, 64, 0, 1, 73, 4, 1,
                    73, 44, 1, 73, 0, 1, 72, 4, 1, 72, 44, 1, 72, 0, 1, 75, 4, 1, 75, 44,
                    1, 75, 0, 1, 74, 4, 1, 74, 44, 1, 74, 0, 1, 78, 4, 1, 78, 44, 1, 78,
                    0, 1, 79, 4, 1, 79, 44, 1, 79, 0, 1, 76, 4, 1, 76, 44, 1, 76, 0, 1,
                    77, 4, 1, 77, 44, 1, 77, 0, 1, 71, 4, 1, 71, 44, 1, 71, 0, 1, 70, 4,
                    1, 70, 44, 1, 70, 0, 1, 80, 4, 1, 80, 44, 1, 80, 0, 1, 81, 4, 1, 81,
                    44, 1, 81, 0, 1, 83, 4, 1, 83, 44, 1, 83, 0, 1, 82, 4, 1, 82, 44, 1,
                    82, 18, 1, 51, 18, 1, 52, 18, 1, 55, 18, 1, 53, 18, 1, 54, 0, 1, 86,
                    4, 1, 86, 44, 1, 86, 18, 1, 56, 0, 1, 85, 4, 1, 85, 44, 1, 85, 0, 1,
                    84, 4, 1, 84, 44, 1, 84, 0, 1, 87, 4, 1, 87, 44, 1, 87, 0, 1, 88, 4,
                    1, 88, 44, 1, 88, 0, 1, 89, 4, 1, 89, 44, 1, 89, 44, 1, 160, 44, 1,
                    161,
                ];
                static REDUCE_OFFSETS: &[u32] = &[
                    0, 0, 0, 3, 6, 6, 21, 78, 78, 141, 204, 204, 204, 204, 267, 330, 393,
                    456, 462, 477, 492, 495, 507, 507, 519, 531, 543, 543, 555, 567, 579,
                    579, 591, 603, 603, 666, 678, 681, 693, 693, 756, 819, 819, 819, 819,
                    882, 915, 978, 1041, 1104, 1167, 1167, 1218, 1224, 1257, 1263, 1263,
                    1269, 1275, 1338, 1341, 1344, 1344, 1407, 1407, 1470, 1470, 1533,
                    1533, 1596, 1596, 1659, 1701, 1743, 1743, 1758, 1767, 1809, 1851,
                    1866, 1908, 1920, 1920, 1920, 1944, 1968, 1992, 2007, 2022, 2022,
                    2037, 2052, 2067, 2082, 2094, 2109, 2115, 2124, 2133, 2142, 2151,
                    2151, 2160, 2169, 2169, 2169, 2169, 2178, 2190, 2190, 2199, 2211,
                    2211, 2211, 2220, 2220, 2229, 2229, 2229, 2238, 2367, 2367, 2376,
                    2505, 2505, 2514, 2514, 2514, 2523, 2523, 2532, 2532, 2541, 2541,
                    2550, 2550, 2559, 2559, 2568, 2568, 2577, 2577, 2586, 2586, 2595,
                    2595, 2604, 2604, 2613, 2613, 2622, 2622, 2631, 2631, 2640, 2640,
                    2640, 2649, 2649, 2658, 2658, 2667, 2667, 2676, 2676, 2685, 2685,
                    2694, 2694, 2694, 2694, 2697, 2700, 2700, 2703, 2706, 2706, 2709,
                    2709, 2709, 2718, 2721, 2721, 2721, 2730, 2739, 2739, 2748, 2748,
                    2757, 2766, 2769, 2772, 2772, 2772,
                ];
                let num_rules = 163usize;
                let mut rules = Vec::with_capacity(num_rules);
                for i in 0..num_rules {
                    let lhs = NonTerminals::from_usize(RULE_NAMES[i] as usize);
                    rules
                        .push(::rusty_lr_core::parser::table::RuleInfo {
                            lhs,
                            len: RULE_LENGTHS[i] as usize,
                        });
                }
                let num_states = 194usize;
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
                                ::rusty_lr_core::parser::table::ShiftTarget::new(
                                    state,
                                    push,
                                ),
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
                                ::rusty_lr_core::parser::table::ShiftTarget::new(
                                    state,
                                    push,
                                ),
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
                    let intermediate = ::rusty_lr_core::parser::state::IntermediateState {
                        shift_goto_map_term,
                        shift_goto_map_nonterm,
                        reduce_map,
                        ruleset: Vec::new(),
                    };
                    state_rows.push(intermediate);
                }
                ::rusty_lr_core::parser::table::IntermediateTables {
                    state_rows,
                    rules,
                }
                    .into()
            })
    }
    #[doc(hidden)]
    fn __rusty_lr_parser_version() -> (usize, usize, usize) {
        (4, 5, 0)
    }
    #[doc(hidden)]
    fn __rustylr_version() -> (usize, usize, usize) {
        (1, 36, 0)
    }
    #[doc(hidden)]
    fn __rusty_lr_version() -> (usize, usize, usize) {
        (4, 5, 0)
    }
}

// ==============================Generated Codes End===============================
