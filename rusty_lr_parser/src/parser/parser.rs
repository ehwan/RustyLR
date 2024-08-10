use crate::error::ParseError;
use crate::parser::args::PatternArgs;
use crate::parser::args::GrammarArgs;
use crate::parser::args::RuleDefArgs;
use crate::parser::args::RuleLineArgs;
use crate::parser::lexer::Lexed;
use crate::parser::args::ReduceTypeArgs;

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
%error ParseError;

%tokentype Lexed;
%token ident Lexed::Ident(None);
%token colon Lexed::Colon(None);
%token semicolon Lexed::Semicolon(None);
%token pipe Lexed::Pipe(None);
%token percent Lexed::Percent(None);
%token left Lexed::Left(None);
%token right Lexed::Right(None);
%token token Lexed::Token(None);
%token start Lexed::Start(None);
%token eofdef Lexed::EofDef(None);
%token tokentype Lexed::TokenType(None);
%token userdata Lexed::UserData(None);
%token errortype Lexed::ErrorType(None);
%token parengroup Lexed::ParenGroup(None);
%token bracegroup Lexed::BraceGroup(None);
%token bracketgroup Lexed::BracketGroup(None);
%token othergroup Lexed::OtherGroup(None);
%token literal Lexed::Literal(None);
%token equal Lexed::Equal(None);
%token plus Lexed::Plus(None);
%token star Lexed::Star(None);
%token question Lexed::Question(None);
%token caret Lexed::Caret(None);
%token minus Lexed::Minus(None);
%token otherpunct Lexed::OtherPunct(None);
%token moduleprefix Lexed::ModulePrefix(None);
%eof Lexed::Eof;

%start Grammar;

Rule(RuleDefArgs) : ident RuleType colon RuleLines semicolon {
    let ident = if let Lexed::Ident(ident) = ident {
        ident.expect("Rule-Ident")
    } else {
        unreachable!( "Rule-Ident" );
    };
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
    RuleLines.push( RuleLine );
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
        reduce_action: Action.map(|action| action.to_token_stream())
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

Pattern(PatternArgs): ident {
    if let Lexed::Ident(ident) = ident {
        PatternArgs::Ident( ident.expect("Pattern-Ident") )
    }else {
        unreachable!( "Pattern-Ident" );
    }
}
| Pattern plus {
    PatternArgs::Plus( Box::new(Pattern) )
}
| Pattern star {
    PatternArgs::Star( Box::new(Pattern) )
}
| Pattern question {
    PatternArgs::Question( Box::new(Pattern) )
}
| bracketgroup {
    if let Lexed::BracketGroup(group) = bracketgroup {
        PatternArgs::TerminalSet( group.expect("Pattern-BracketGroup0") )
    }else {
        unreachable!( "Pattern-BracketGroup1" );
    }
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

RustCode(TokenStream): t=[^semicolon]+ {
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
| reducetype=ReduceType bracketgroup semicolon {
    if let Lexed::BracketGroup(group) = bracketgroup {
        ( ReduceTypeArgs::TerminalSet(group.expect("ReduceDef-BracketGroup")), reducetype )
    }else {
        unreachable!( "Pattern-BracketGroup1" );
    }
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
    if let Some(old) = Grammar.start_rule_name {
        return Err( ParseError::MultipleStartDefinition(StartDef.span(), old, StartDef) );
    }
    Grammar.start_rule_name = Some(StartDef);
    Grammar
}
| StartDef {
    let mut g:GrammarArgs = Default::default();
    g.start_rule_name = Some(StartDef);
    g
}
| Grammar EofDef {
    let (span,eof) = EofDef;
    if let Some(old) = Grammar.eof {
        return Err( ParseError::MultipleEofDefinition(span,old, eof) );
    }
    Grammar.eof = Some(eof);
    Grammar
}
| EofDef {
    let mut g:GrammarArgs = Default::default();
    let (span,eof) = EofDef;
    g.eof = Some(eof);
    g
}
| Grammar TokenTypeDef {
    let (span,token_type) = TokenTypeDef;
    if let Some(old) = Grammar.token_typename {
        return Err( ParseError::MultipleTokenTypeDefinition(span,old, token_type) );
    }
    Grammar.token_typename = Some(token_type);
    Grammar
}
| TokenTypeDef {
    let mut g:GrammarArgs = Default::default();
    let (span,token_type) = TokenTypeDef;
    g.token_typename = Some(token_type);
    g
}
| Grammar UserDataDef {
    let (span,user_data) = UserDataDef;
    if let Some(old) = Grammar.userdata_typename {
        return Err( ParseError::MultipleUserDataDefinition(span,old, user_data) );
    }
    Grammar.userdata_typename = Some(user_data);
    Grammar
}
| UserDataDef {
    let mut g:GrammarArgs = Default::default();
    let (span,user_data) = UserDataDef;
    g.userdata_typename = Some(user_data);
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
    let (span,error) = ErrorDef;
    if let Some(old) = Grammar.error_typename {
        return Err( ParseError::MultipleErrorDefinition(span, old, error) );
    }
    Grammar.error_typename = Some(error);
    Grammar
}
| ErrorDef {
    let mut g:GrammarArgs = Default::default();
    let (span,error) = ErrorDef;
    g.error_typename = Some(error);
    g
}
| Grammar ModulePrefixDef {
    let (span,module_prefix) = ModulePrefixDef;
    // no multiple definition check for module prefix
    Grammar.module_prefix = Some(module_prefix);
    Grammar
}
| ModulePrefixDef {
    let mut g:GrammarArgs = Default::default();
    let (span,module_prefix) = ModulePrefixDef;
    g.module_prefix = Some(module_prefix);
    g
}
;
