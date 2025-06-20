use crate::parser::args::PatternArgs;
use crate::parser::args::GrammarArgs;
use crate::parser::args::RuleDefArgs;
use crate::parser::args::RuleLineArgs;
use crate::parser::args::IdentOrLiteral;
use crate::parser::args::PrecDPrecArgs;
use crate::parser::lexer::Lexed;
use crate::terminalset::TerminalSet;
use crate::terminalset::TerminalSetItem;

use proc_macro2::Group;
use proc_macro2::Ident;
use proc_macro2::Span;
use proc_macro2::TokenStream;
use quote::ToTokens;

use std::boxed::Box;

use rusty_lr_core::ReduceType;

// bootstrap the parser for the grammar
// this define the actual parser for proc-macro line parsing
// This should be changed to GLR parser in the future

%%

%moduleprefix ::rusty_lr_core;

%userdata GrammarArgs;

%tokentype Lexed;
%token ident Lexed::Ident(_);
%token colon Lexed::Colon(_);
%token semicolon Lexed::Semicolon(_);
%token pipe Lexed::Pipe(_);
%token percent Lexed::Percent(_);
%token equal Lexed::Equal(_);
%token plus Lexed::Plus(_);
%token star Lexed::Star(_);
%token question Lexed::Question(_);
%token caret Lexed::Caret(_);
%token minus Lexed::Minus(_);
%token exclamation Lexed::Exclamation(_);
%token slash Lexed::Slash(_);
%token dot Lexed::Dot(_);

%token literal Lexed::Literal(_);

%token parengroup Lexed::ParenGroup(_);
%token bracegroup Lexed::BraceGroup(_);

%token lparen Lexed::LParen(_);
%token rparen Lexed::RParen(_);
%token lbracket Lexed::LBracket(_);
%token rbracket Lexed::RBracket(_);

%token left Lexed::Left(_,_);
%token right Lexed::Right(_,_);
%token token Lexed::Token(_,_);
%token start Lexed::Start(_,_);
%token eofdef Lexed::EofDef(_,_);
%token tokentype Lexed::TokenType(_,_);
%token userdata Lexed::UserData(_,_);
%token errortype Lexed::ErrorType(_,_);
%token moduleprefix Lexed::ModulePrefix(_,_);
%token lalr Lexed::Lalr(_,_);
%token glr Lexed::Glr(_,_);
%token prec Lexed::Prec(_,_);
%token precedence Lexed::Precedence(_,_);
%token nooptim Lexed::NoOptim(_,_);
%token dense Lexed::Dense(_,_);
%token trace Lexed::Trace(_,_);
%token dprec Lexed::DPrec(_,_);
%token filter Lexed::Filter(_,_);

%eof Lexed::Eof;

%start Grammar;

Rule(RuleDefArgs) : ident RuleType colon RuleLines semicolon {
    let Lexed::Ident(ident) = ident else {
        unreachable!( "Rule-Ident" );
    };
    let Lexed::Colon(colon) = colon else {
        unreachable!( "Rule-Colon2" );
    };
    let span = colon.span();
    if let Some(fisrt) = RuleLines.first_mut() {
        fisrt.separator_span = span;
    }
    RuleDefArgs {
        name: ident,
        typename: RuleType.map(|t| t.stream()),
        rule_lines: RuleLines,
    }
}
;

RuleType(Option<Group>): parengroup {
    let Lexed::ParenGroup(group) = parengroup else {
        unreachable!( "RuleType - Group" );
    };
    Some(group)
}
| {
    None
}
;

RuleLines(Vec<RuleLineArgs>): RuleLines pipe RuleLine {
    let Lexed::Pipe(punct) = pipe else {
        unreachable!( "RuleLines-Pipe" );
    };
    RuleLine.separator_span = punct.span();
    RuleLines.push( RuleLine );
    RuleLines
}
| RuleLine {
    vec![ RuleLine ]
}
;

RuleLine(RuleLineArgs): TokenMapped* PrecDef* Action
{
    RuleLineArgs {
        tokens: TokenMapped,
        reduce_action: Action.map(|action| action.to_token_stream()),
        separator_span: Span::call_site(),
        precs: PrecDef,
        prec: None,
        dprec: None,
    }
}
;

PrecDef(PrecDPrecArgs): prec! IdentOrLiteral { PrecDPrecArgs::Prec(IdentOrLiteral) }
| dprec! literal { 
    let Lexed::Literal(literal) = literal else {
        unreachable!( "PrecDPrecArgs-DPrec" );
    };
    PrecDPrecArgs::DPrec(literal) 
}
;

TokenMapped((Option<Ident>, PatternArgs)): Pattern {
    ( None, Pattern )
}
| ident equal Pattern {
    let Lexed::Ident(ident) = ident else {
        unreachable!( "Token-Ident" );
    };
    ( Some(ident), Pattern )
}
;

TerminalSetItem(TerminalSetItem): ident {
    let Lexed::Ident(ident) = ident else {
        unreachable!( "TerminalSetItem-Range1" );
    };
    TerminalSetItem::Terminal( ident )
}
| first=ident minus last=ident {
    let Lexed::Ident(first) = first else {
        unreachable!( "TerminalSetItem-Range1" );
    };
    let Lexed::Ident(last) = last else {
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
    let Lexed::LBracket(open_span) = lbracket else {
        unreachable!( "TerminalSet-Open" );
    };
    let Lexed::RBracket(close_span) = rbracket else {
        unreachable!( "TerminalSet-Close" );
    };
    TerminalSet {
      negate: caret.is_some(),
      items: TerminalSetItem,
      open_span,
      close_span,
    }
}
| dot {
    let span = dot.span();
    TerminalSet {
        negate: true,
        items: vec![],
        open_span: span,
        close_span: span,
    }
}
;

%left minus;
%left slash;
%left star plus question exclamation;

Pattern(PatternArgs): ident {
    let Lexed::Ident(ident) = ident else {
        unreachable!( "Pattern-Ident" );
    };
    PatternArgs::Ident( ident )
}
| Pattern plus {
    let Lexed::Plus(plus) = plus else {
        unreachable!( "Pattern-Plus" );
    };
    PatternArgs::Plus( Box::new(Pattern), plus.span() )
}
| Pattern star {
    let Lexed::Star(star) = star else {
        unreachable!( "Pattern-Star" );
    };
    PatternArgs::Star( Box::new(Pattern), star.span() )
}
| Pattern question {
    let Lexed::Question(question) = question else {
        unreachable!( "Pattern-Question" );
    };
    PatternArgs::Question( Box::new(Pattern), question.span() )
}
| Pattern exclamation {
    let Lexed::Exclamation(exclamation) = exclamation else {
        unreachable!( "Pattern-Exclamation" );
    };
    PatternArgs::Exclamation( Box::new(Pattern), exclamation.span() )
}
| TerminalSet {
    PatternArgs::TerminalSet( TerminalSet )
}
| p1=Pattern slash lh=Pattern {
    PatternArgs::Lookaheads( Box::new(p1), Box::new(lh) )
}
| lparen Pattern+ rparen {
    let Lexed::LParen(open) = lparen else {
        unreachable!( "Pattern-Group-Open" );
    };
    let Lexed::RParen(close) = rparen else {
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
    let Lexed::BraceGroup(group) = bracegroup else {
        unreachable!( "Action0" );
    };
    Some(group)
}
| { None }
;

TokenDef((Ident, TokenStream)): token ident RustCode semicolon
{
    let Lexed::Ident(ident) = ident else {
        unreachable!( "TokenDef-Ident" );
    };
    ( ident, RustCode )
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
    let Lexed::Ident(ident) = ident else {
        unreachable!( "StartDef-Ident" );
    };
    ident
}
;
EofDef((Span,TokenStream)): eofdef RustCode semicolon { (eofdef.span(), RustCode) }
;
TokenTypeDef((Span,TokenStream)): tokentype RustCode semicolon { (tokentype.span(), RustCode) }
;
UserDataDef((Span,TokenStream)): userdata RustCode semicolon { (userdata.span(),RustCode) }
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

Precedence(Vec<IdentOrLiteral>): precedence! IdentOrLiteral+ semicolon!;

NoOptim: nooptim semicolon;

Dense: dense semicolon;

Trace(Vec<Ident>): trace ident* semicolon {
    ident.into_iter().map(|t| {
        let Lexed::Ident(ident) = t else {
            unreachable!( "Trace-Ident" );
        };
        ident
    }).collect()
}
;

Filter(TokenStream): filter! RustCode semicolon! ;

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
| Trace { data.traces.extend(Trace); }
| Filter { data.filter = Some(Filter); }
;

Grammar: GrammarLine+;