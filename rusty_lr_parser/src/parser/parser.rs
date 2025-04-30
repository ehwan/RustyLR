use crate::parser::args::PatternArgs;
use crate::parser::args::GrammarArgs;
use crate::parser::args::RuleDefArgs;
use crate::parser::args::RuleLineArgs;
use crate::parser::lexer::Lexed;
use crate::parser::args::TerminalOrTerminalSet;
use crate::parser::args::IdentOrLiteral;
use crate::terminalset::TerminalSet;
use crate::terminalset::TerminalSetItem;

use proc_macro2::Group;
use proc_macro2::Ident;
use proc_macro2::Span;
use proc_macro2::TokenStream;
use proc_macro2::Literal;
use proc_macro2::Punct;
use proc_macro2::Spacing;
use quote::ToTokens;

use std::boxed::Box;

use rusty_lr_core::ReduceType;

// bootstrap the parser for the grammar
// this define the actual parser for proc-macro line parsing
// This should be changed to GLR parser in the future

macro_rules! punct(
    ($l:literal) => {
        Punct::new($l, Spacing::Alone)
    };
);

%%

%moduleprefix ::rusty_lr_core;

%userdata GrammarArgs;

%tokentype Lexed;
%token ident Lexed::Ident(Ident::new("id", Span::call_site()));
%token colon Lexed::Colon(punct!(':'));
%token semicolon Lexed::Semicolon(punct!(';'));
%token pipe Lexed::Pipe(punct!('|'));
%token percent Lexed::Percent(punct!('%'));
%token equal Lexed::Equal(punct!('='));
%token plus Lexed::Plus(punct!('+'));
%token star Lexed::Star(punct!('*'));
%token question Lexed::Question(punct!('?'));
%token caret Lexed::Caret(punct!('^'));
%token minus Lexed::Minus(punct!('-'));
%token exclamation Lexed::Exclamation(punct!('!'));
%token slash Lexed::Slash(punct!('/'));
// %token otherpunct Lexed::OtherPunct(punct!('.'));

%token literal Lexed::Literal(Literal::usize_suffixed(0));

%token parengroup Lexed::ParenGroup(None);
%token bracegroup Lexed::BraceGroup(None);
// %token bracketgroup Lexed::BracketGroup(None);
// %token nonegroup Lexed::NoneGroup(None);

%token lparen Lexed::LParen(Span::call_site());
%token rparen Lexed::RParen(Span::call_site());
// %token lbrace Lexed::LBrace(Span::call_site());
// %token rbrace Lexed::RBrace(Span::call_site());
%token lbracket Lexed::LBracket(Span::call_site());
%token rbracket Lexed::RBracket(Span::call_site());

%token left Lexed::Left(punct!('%'),Ident::new("id", Span::call_site()));
%token right Lexed::Right(punct!('%'),Ident::new("id", Span::call_site()));
%token token Lexed::Token(punct!('%'),Ident::new("id", Span::call_site()));
%token start Lexed::Start(punct!('%'),Ident::new("id", Span::call_site()));
%token eofdef Lexed::EofDef(punct!('%'),Ident::new("id", Span::call_site()));
%token tokentype Lexed::TokenType(punct!('%'),Ident::new("id", Span::call_site()));
%token userdata Lexed::UserData(punct!('%'),Ident::new("id", Span::call_site()));
%token errortype Lexed::ErrorType(punct!('%'),Ident::new("id", Span::call_site()));
%token moduleprefix Lexed::ModulePrefix(punct!('%'),Ident::new("id", Span::call_site()));
%token lalr Lexed::Lalr(punct!('%'),Ident::new("id", Span::call_site()));
%token glr Lexed::Glr(punct!('%'),Ident::new("id", Span::call_site()));
%token prec Lexed::Prec(punct!('%'),Ident::new("id", Span::call_site()));
%token precedence Lexed::Precedence(punct!('%'),Ident::new("id", Span::call_site()));
%token nooptim Lexed::NoOptim(punct!('%'),Ident::new("id", Span::call_site()));
%token dense Lexed::Dense(punct!('%'),Ident::new("id", Span::call_site()));

%eof Lexed::Eof;

%start Grammar;

Rule(RuleDefArgs) : ident RuleType colon RuleLines semicolon {
    let ident = if let Lexed::Ident(ident) = ident {
        ident
    } else {
        unreachable!( "Rule-Ident" );
    };
    if let Lexed::Colon(colon) = colon {
        let span = colon.span();
        if let Some(fisrt) = RuleLines.first_mut() {
            fisrt.separator_span = span;
        }
    }else {
        unreachable!( "Rule-Colon2" );
    }
    RuleDefArgs {
        name: ident,
        typename: RuleType.map(|t| t.stream()),
        rule_lines: RuleLines
    }
}
;

RuleType(Option<Group>): parengroup {
    if let Lexed::ParenGroup(group) = parengroup {
        group
    }else{
        unreachable!( "RuleType - Group" );
    }
}
| {
    None
}
;

RuleLines(Vec<RuleLineArgs>): RuleLines pipe RuleLine {
    if let Lexed::Pipe(punct) = pipe {
        RuleLine.separator_span = punct.span();
        RuleLines.push( RuleLine );
    }
    RuleLines
}
| RuleLine {
    vec![ RuleLine ]
}
;

RuleLine(RuleLineArgs): TokenMapped* PrecDef? Action
{
    RuleLineArgs {
        tokens: TokenMapped,
        reduce_action: Action.map(|action| action.to_token_stream()),
        separator_span: Span::call_site(),
        precedence: PrecDef,
    }
}
;

PrecDef(IdentOrLiteral): prec! IdentOrLiteral;

TokenMapped((Option<Ident>, PatternArgs)): Pattern {
    ( None, Pattern )
}
| ident equal Pattern {
    if let Lexed::Ident(ident) = ident {
        ( Some(ident), Pattern )
    }else {
        unreachable!( "Token-Ident" );
    }
}
;

TerminalSetItem(TerminalSetItem): ident {
    let ident = if let Lexed::Ident(ident) = ident {
        ident
    }else {
        unreachable!( "TerminalSetItem-Range1" );
    };
    TerminalSetItem::Terminal( ident )
}
| first=ident minus last=ident {
    let first = if let Lexed::Ident(first) = first {
        first
    }else {
        unreachable!( "TerminalSetItem-Range1" );
    };
    let last = if let Lexed::Ident(last) = last {
        last
    }else {
        unreachable!( "TerminalSetItem-Range3" );
    };

    TerminalSetItem::Range( first, last )
}
| literal {
    let Lexed::Literal(literal) = literal else {
        unreachable!( "TerminalSetItem-Literal" );
    };
    TerminalSetItem::Literal(literal)
}
| first=literal minus last=literal {
    let Lexed::Literal(first) = first else {
        unreachable!( "TerminalSetItem-Range1" );
    };
    let Lexed::Literal(last) = last else {
        unreachable!( "TerminalSetItem-Range3" );
    };
    TerminalSetItem::LiteralRange( first, last )
}
;

TerminalSet(TerminalSet): lbracket caret? TerminalSetItem* rbracket {
    let open_span = if let Lexed::LBracket(lbracket) = lbracket {
        lbracket
    } else {
        unreachable!( "TerminalSet-Open" );
    };
    let close_span = if let Lexed::RBracket(rbracket) = rbracket {
        rbracket
    } else {
        unreachable!( "TerminalSet-Close" );
    };
    TerminalSet {
      negate: caret.is_some(),
      items: TerminalSetItem,
      open_span,
      close_span,
    }
}
;

%left minus;
%left slash;
%left star plus question exclamation;

Pattern(PatternArgs): ident {
    if let Lexed::Ident(ident) = ident {
        PatternArgs::Ident( ident )
    }else {
        unreachable!( "Pattern-Ident" );
    }
}
| Pattern plus {
    if let Lexed::Plus(plus) = plus {
        PatternArgs::Plus( Box::new(Pattern), plus.span() )
    }else {
        unreachable!( "Pattern-Plus" );
    }
}
| Pattern star {
    if let Lexed::Star(star) = star {
        PatternArgs::Star( Box::new(Pattern), star.span() )
    }else {
        unreachable!( "Pattern-Star" );
    }
}
| Pattern question {
    if let Lexed::Question(question) = question {
        PatternArgs::Question( Box::new(Pattern), question.span() )
    }else {
        unreachable!( "Pattern-Question" );
    }
}
| Pattern exclamation {
    if let Lexed::Exclamation(exclamation) = exclamation {
        PatternArgs::Exclamation( Box::new(Pattern), exclamation.span() )
    }else {
        unreachable!( "Pattern-Exclamation" );
    }
}
| TerminalSet {
    PatternArgs::TerminalSet( TerminalSet )
}
| p1=Pattern slash lh=Pattern {
    PatternArgs::Lookaheads( Box::new(p1), Box::new(lh) )
}
| lparen Pattern+ rparen {
    let open = if let Lexed::LParen(lparen) = lparen {
        lparen
    } else {
        unreachable!( "Pattern-Group-Open" );
    };
    let close = if let Lexed::RParen(rparen) = rparen {
        rparen
    } else {
        unreachable!( "Pattern-Group-Close" );
    };
    PatternArgs::Group(Pattern, open, close)
}
| literal {
    let Lexed::Literal(literal) = literal else {
        unreachable!( "Pattern-Literal" );
    };
    PatternArgs::Literal(literal)
}
| p1=Pattern minus p2=Pattern {
    PatternArgs::Minus( Box::new(p1), Box::new(p2) )
}
;


Action(Option<Group>): bracegroup {
    if let Lexed::BraceGroup(group) = bracegroup {
        group
    }else {
        unreachable!( "Action0" );
    }
}
| { None }
;

TokenDef((Ident, TokenStream)): token ident RustCode semicolon
{
    if let Lexed::Ident(ident) = ident {
        ( ident, RustCode )
    }else {
        unreachable!( "TokenDef-Ident" );
    }
}
;

RustCode(TokenStream): t=[^semicolon]+ {
    let mut tokens = TokenStream::new();
    for token in t.into_iter() {
        token.append_to_stream(&mut tokens);
    }
    tokens
};

StartDef(Ident): start ident semicolon {
    if let Lexed::Ident(ident) = ident {
        ident
    }else {
        unreachable!( "StartDef-Ident" );
    }
}
;
EofDef((Span,TokenStream)): eofdef RustCode semicolon { (eofdef.span(), RustCode) }
;
TokenTypeDef((Span,TokenStream)): tokentype RustCode semicolon { (tokentype.span(), RustCode) }
;
UserDataDef((Span,TokenStream)): userdata RustCode semicolon { (userdata.span(),RustCode) }
;

TerminalOrTerminalSet(TerminalOrTerminalSet): TerminalSet { TerminalOrTerminalSet::TerminalSet( TerminalSet ) }
| ident {
    if let Lexed::Ident(ident) = ident {
        TerminalOrTerminalSet::Ident( ident )
    }else {
        unreachable!( "TerminalOrTerminalSet-Ident" );
    }
}
| literal {
    let Lexed::Literal(literal) = literal else {
        unreachable!( "TerminalOrTerminalSet-Literal" );
    };
    TerminalOrTerminalSet::Literal( literal )
}
;

IdentOrLiteral(IdentOrLiteral): ident {
    let Lexed::Ident(ident) = ident else {
        unreachable!( "IdentOrLiteral-Ident" );
    };
    IdentOrLiteral::Ident( ident )
}
| literal {
    let Lexed::Literal(literal) = literal else {
        unreachable!( "IdentOrLiteral-Literal" );
    };
    IdentOrLiteral::Literal( literal )
};

ReduceType(ReduceType): left { ReduceType::Left }
| right { ReduceType::Right }
;

ReduceDef((ReduceType, Vec<IdentOrLiteral>)): reducetype=ReduceType IdentOrLiteral+ semicolon {
    ( reducetype, IdentOrLiteral )
}
;

ErrorDef((Span,TokenStream)): errortype RustCode semicolon { (errortype.span(), RustCode) }
;

ModulePrefixDef((Span,TokenStream)): moduleprefix RustCode semicolon { (moduleprefix.span(), RustCode) };

Glr: glr semicolon;

Lalr: lalr semicolon;

Precedence(Vec<IdentOrLiteral>): precedence IdentOrLiteral+ semicolon {
    IdentOrLiteral
}
;

NoOptim: nooptim semicolon;

Dense: dense semicolon;

GrammarLine : Rule { data.rules.push(Rule); }
| TokenDef  { data.terminals.push(TokenDef); }
| StartDef  { data.start_rule_name.push(StartDef); }
| EofDef    { data.eof.push(EofDef); }
| TokenTypeDef  { data.token_typename.push(TokenTypeDef); }
| UserDataDef  { data.userdata_typename.push(UserDataDef); }
| ReduceDef  { data.precedences.push(ReduceDef.1.clone()); data.reduce_types.push(ReduceDef); }
| ErrorDef   { data.error_typename.push(ErrorDef); }
| ModulePrefixDef { data.module_prefix.push(ModulePrefixDef); }
| Lalr { data.lalr = true; }
| Glr { data.glr = true; }
| Precedence { data.precedences.push(Precedence); }
| NoOptim { data.no_optim = true; }
| Dense { data.dense = true; }
;

Grammar: GrammarLine+;