use crate::parser::args::PatternArgs;
use crate::parser::args::GrammarArgs;
use crate::parser::args::RuleDefArgs;
use crate::parser::args::RuleLineArgs;
use crate::parser::lexer::Lexed;
use crate::parser::args::ReduceTypeArgs;
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

// rusty_lr_expand parser.rs parser_expanded.rs

%%

%moduleprefix ::rusty_lr_core;

%tokentype Lexed;
%token ident Lexed::Ident(None);
%token colon Lexed::Colon(None);
%token semicolon Lexed::Semicolon(None);
%token pipe Lexed::Pipe(None);
%token percent Lexed::Percent(None);
%token literal Lexed::Literal(None);
%token equal Lexed::Equal(None);
%token plus Lexed::Plus(None);
%token star Lexed::Star(None);
%token question Lexed::Question(None);
%token caret Lexed::Caret(None);
%token minus Lexed::Minus(None);
%token otherpunct Lexed::OtherPunct(None);

%token parengroup Lexed::ParenGroup(None);
%token bracegroup Lexed::BraceGroup(None);
%token bracketgroup Lexed::BracketGroup(None);
%token nonegroup Lexed::NoneGroup(None);

%token lparen Lexed::LParen(None);
%token rparen Lexed::RParen(None);
%token lbrace Lexed::LBrace(None);
%token rbrace Lexed::RBrace(None);
%token lbracket Lexed::LBracket(None);
%token rbracket Lexed::RBracket(None);

%token left Lexed::Left(None);
%token right Lexed::Right(None);
%token token Lexed::Token(None);
%token start Lexed::Start(None);
%token eofdef Lexed::EofDef(None);
%token tokentype Lexed::TokenType(None);
%token userdata Lexed::UserData(None);
%token errortype Lexed::ErrorType(None);
%token moduleprefix Lexed::ModulePrefix(None);

%eof Lexed::Eof;

%start Grammar;

Rule(RuleDefArgs) : ident RuleType colon RuleLines semicolon {
    let ident = if let Lexed::Ident(ident) = ident {
        ident.expect("Rule-Ident")
    } else {
        unreachable!( "Rule-Ident" );
    };
    if let Lexed::Colon(colon) = colon {
        let span = colon.expect( "Rule-Colon" ).span();
        if let Some(fisrt) = RuleLines.first_mut() {
            fisrt.separator_span = span;
        }
    }else {
        unreachable!( "Rule-Colon2" );
    }
    RuleDefArgs {
        name: ident,
        typename: RuleType.map(|t| t.to_token_stream()),
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
        RuleLine.separator_span = punct.expect( "RuleLines-Pipe" ).span();
        RuleLines.push( RuleLine );
    }
    RuleLines
}
| RuleLine {
    vec![ RuleLine ]
}
;

RuleLine(RuleLineArgs): TokenMapped* Action
{
    RuleLineArgs {
        tokens: TokenMapped,
        reduce_action: Action.map(|action| action.to_token_stream()),
        separator_span: Span::call_site(),
    }
}
;

TokenMapped((Option<Ident>, PatternArgs)): Pattern {
    ( None, Pattern )
}
| ident equal Pattern {
    if let Lexed::Ident(ident) = ident {
        ( ident, Pattern )
    }else {
        unreachable!( "Token-Ident" );
    }
}
;

TerminalSetItem(TerminalSetItem): ident {
    let ident = if let Lexed::Ident(ident) = ident {
        ident.expect("TerminalSetItem-Range0")
    }else {
        unreachable!( "TerminalSetItem-Range1" );
    };
    TerminalSetItem::Terminal( ident )
}
| first=ident minus last=ident {
    let first = if let Lexed::Ident(first) = first {
        first.expect("TerminalSetItem-Range0")
    }else {
        unreachable!( "TerminalSetItem-Range1" );
    };
    let last = if let Lexed::Ident(last) = last {
        last.expect("TerminalSetItem-Range2")
    }else {
        unreachable!( "TerminalSetItem-Range3" );
    };

    TerminalSetItem::Range( first, last )
}
;

TerminalSet(TerminalSet): lbracket caret? TerminalSetItem* rbracket {
    let open_span = if let Lexed::LBracket(lbracket) = lbracket {
        lbracket.expect("TerminalSet-Open")
    } else {
        unreachable!( "TerminalSet-Open" );
    };
    let close_span = if let Lexed::RBracket(rbracket) = rbracket {
        rbracket.expect("TerminalSet-Close")
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

Pattern(PatternArgs): ident {
    if let Lexed::Ident(ident) = ident {
        let ident = ident.expect("Pattern-Ident");
        let span = ident.span();
        PatternArgs::Ident( ident, span )
    }else {
        unreachable!( "Pattern-Ident" );
    }
}
| Pattern plus {
    if let Lexed::Plus(plus) = plus {
        PatternArgs::Plus( Box::new(Pattern), plus.expect("Pattern-Plus0").span() )
    }else {
        unreachable!( "Pattern-Plus" );
    }
}
| Pattern star {
    if let Lexed::Star(star) = star {
        PatternArgs::Star( Box::new(Pattern), star.expect("Pattern-Star0").span() )
    }else {
        unreachable!( "Pattern-Star" );
    }
}
| Pattern question {
    if let Lexed::Question(question) = question {
        PatternArgs::Question( Box::new(Pattern), question.expect("Pattern-Question0").span() )
    }else {
        unreachable!( "Pattern-Question" );
    }
}
| TerminalSet {
    PatternArgs::TerminalSet( TerminalSet )
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
        ( ident.expect("TokenDef"), RustCode )
    }else {
        unreachable!( "TokenDef-Ident" );
    }
}
;

RustCode(TokenStream): t=[^semicolon lparen-moduleprefix ]+ {
    let mut tokens = TokenStream::new();
    for token in t.into_iter() {
        tokens.extend( token.stream() );
    }
    tokens
};

StartDef(Ident): start ident semicolon {
    if let Lexed::Ident(ident) = ident {
        ident.expect("StartDef")
    }else {
        unreachable!( "StartDef-Ident" );
    }
}
;
EofDef((Span,TokenStream)): eofdef RustCode semicolon { (eofdef.span().expect("EofDef"), RustCode) }
;
TokenTypeDef((Span,TokenStream)): tokentype RustCode semicolon { (tokentype.span().expect("TokenTypedef"), RustCode) }
;
UserDataDef((Span,TokenStream)): userdata RustCode semicolon { (userdata.span().expect("UserDataDef"),RustCode) }
;

ReduceType(ReduceType): left { ReduceType::Left }
| right { ReduceType::Right }
;

ReduceDef((ReduceTypeArgs, ReduceType)): reducetype=ReduceType ident semicolon {
    if let Lexed::Ident(ident) = ident {
        ( ReduceTypeArgs::Ident(ident.expect("ReduceDef-Ident")), reducetype )
    }else {
        unreachable!( "ReduceDef-Ident (Left)" );
    }
}
| reducetype=ReduceType TerminalSet semicolon {
    ( ReduceTypeArgs::TerminalSet( TerminalSet ), reducetype )
}
;

ErrorDef((Span,TokenStream)): errortype RustCode semicolon { (errortype.span().expect("ErrorDef"), RustCode) }
;

ModulePrefixDef((Span,TokenStream)): moduleprefix RustCode semicolon { (moduleprefix.span().expect("ModulePrefixDef"), RustCode) };

Grammar(GrammarArgs): Grammar Rule {
    Grammar.rules.push( Rule );
    Grammar 
}
| Rule {
    let mut g:GrammarArgs = Default::default();
    g.rules.push( Rule );
    g
}
| Grammar TokenDef {
    Grammar.terminals.push( TokenDef );
    Grammar
}
| TokenDef {
    let mut g:GrammarArgs = Default::default();
    g.terminals.push( TokenDef );
    g
}
| Grammar StartDef {
    Grammar.start_rule_name.push(StartDef);
    Grammar
}
| StartDef {
    let mut g:GrammarArgs = Default::default();
    g.start_rule_name.push(StartDef);
    g
}
| Grammar EofDef {
    Grammar.eof.push(EofDef);
    Grammar
}
| EofDef {
    let mut g:GrammarArgs = Default::default();
    g.eof.push(EofDef);
    g
}
| Grammar TokenTypeDef {
    Grammar.token_typename.push(TokenTypeDef);
    Grammar
}
| TokenTypeDef {
    let mut g:GrammarArgs = Default::default();
    g.token_typename.push(TokenTypeDef);
    g
}
| Grammar UserDataDef {
    Grammar.userdata_typename.push(UserDataDef);
    Grammar
}
| UserDataDef {
    let mut g:GrammarArgs = Default::default();
    g.userdata_typename.push(UserDataDef);
    g
}
| Grammar ReduceDef {
    Grammar.reduce_types.push( ReduceDef );
    Grammar
}
| ReduceDef {
    let mut g:GrammarArgs = Default::default();
    g.reduce_types.push( ReduceDef );
    g
}
| Grammar ErrorDef {
    Grammar.error_typename.push(ErrorDef);
    Grammar
}
| ErrorDef {
    let mut g:GrammarArgs = Default::default();
    g.error_typename.push(ErrorDef);
    g
}
| Grammar ModulePrefixDef {
    Grammar.module_prefix.push(ModulePrefixDef);
    Grammar
}
| ModulePrefixDef {
    let mut g:GrammarArgs = Default::default();
    g.module_prefix.push(ModulePrefixDef);
    g
}
;
