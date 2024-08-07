use crate::error::ParseError;
use crate::grammar::Grammar;
use crate::rule::RuleLine;
use crate::rule::RuleLines;
use crate::term::TermType;
use crate::token::Token;
use crate::token::TokenMapped;

use proc_macro2::Group;
use proc_macro2::Ident;
use proc_macro2::Span;
use proc_macro2::TokenStream;
use quote::ToTokens;

use rusty_lr_core::ReduceType;

// bootstrap the parser for the grammar
// this define the actual parser for proc-macro line parsing

// rusty_lr_expand parser.rs parser_expanded.rs

%%

%moduleprefix ::rusty_lr_core;
%error ParseError;

%tokentype TermType;
%token ident TermType::Ident(None);
%token colon TermType::Colon(None);
%token semicolon TermType::Semicolon(None);
%token pipe TermType::Pipe(None);
%token percent TermType::Percent(None);
%token left TermType::Left(None);
%token right TermType::Right(None);
%token token TermType::Token(None);
%token start TermType::Start(None);
%token eofdef TermType::EofDef(None);
%token tokentype TermType::TokenType(None);
%token userdata TermType::UserData(None);
%token errortype TermType::ErrorType(None);
%token group TermType::Group(None);
%token literal TermType::Literal(None);
%token equal TermType::Equal(None);
%token plus TermType::Plus(None);
%token star TermType::Star(None);
%token question TermType::Question(None);
%token otherpunct TermType::OtherPunct(None);
%token moduleprefix TermType::ModulePrefix(None);
%eof TermType::Eof;

%start Grammar;

Rule((Ident, Option<TokenStream>, RuleLines)) : ident RuleType colon RuleLines semicolon {
    let ident = if let TermType::Ident(ident) = ident {
        ident.expect("Rule-Ident")
    } else {
        unreachable!( "Rule-Ident" );
    };
    ( ident, RuleType.map(|t| t.to_token_stream()), RuleLines{rule_lines: RuleLines} )
}
;

RuleType(Option<Group>): group {
    if let TermType::Group(group) = group {
        if let Some(group) = group {
            if group.delimiter() != proc_macro2::Delimiter::Parenthesis {
                return Err( ParseError::InvalidRuletypeDelimiter(group.span()) );
            }
            Some(group)
        }else {
            unreachable!( "RuleType - Some" );
        }
    }else {
        unreachable!( "RuleType - Group" );
    }
}
| {
    None
}
;

RuleLines(Vec<RuleLine>): RuleLines pipe RuleLine {
    RuleLines.push( RuleLine );
    RuleLines
}
| RuleLine {
    vec![ RuleLine ]
}
;

RuleLine(RuleLine): RuleDef Action
{
    RuleLine {
        tokens: RuleDef,
        reduce_action: Action.map(|action| action.to_token_stream())
    }
}
;

RuleDef(Vec<TokenMapped>): TokenMapped* ;

SymbolPattern(Token): ident {
    if let TermType::Ident(ident) = ident {
        Token::NonTerm( ident.expect("SymbolPattern-Ident0") )
    }else {
        unreachable!( "SymbolPattern-Ident" );
    }
}
| ident star {
    if let TermType::Ident(ident) = ident {
        let ident = ident.expect("SymbolPattern-Ident1");
        Token::Star( ident )
    }else {
        unreachable!( "SymbolPattern-Star" );
    }
}
| ident plus {
    if let TermType::Ident(ident) = ident {
        let ident = ident.expect("SymbolPattern-Plus");
        Token::Plus( ident )
    }else {
        unreachable!( "SymbolPattern-Plus" );
    }
}
| ident question {
    if let TermType::Ident(ident) = ident {
        let ident = ident.expect("SymbolPattern-Question");
        Token::Question( ident )
    }else {
        unreachable!( "SymbolPattern-Question" );
    }
}
;

TokenMapped(TokenMapped): SymbolPattern {
    let mapto = SymbolPattern.ident();
    TokenMapped {
        token: SymbolPattern,
        mapto
    }
}
| ident equal SymbolPattern {
    if let TermType::Ident(ident) = ident {
        let ident = ident.expect("TokenMapped-Ident");
        TokenMapped {
            token: SymbolPattern,
            mapto: ident,
        }
    }else {
        unreachable!( "Token-Ident" );
    }
}
;

Action(Option<Group>): group {
    if let TermType::Group(group) = group {
        if let Some(action) = group {
            if action.delimiter() != proc_macro2::Delimiter::Brace {
                return Err( ParseError::InvalidReduceActionDelimiter(action.span()) );
            }
            Some(action)
        } else {
            unreachable!( "Action1" );
        }
    }else {
        unreachable!( "Action0" );
    }
}
| { None }
;

TokenDef((Ident, TokenStream)): token ident RustCode semicolon
{
    if let TermType::Ident(ident) = ident {
        ( ident.expect("TokenDef"), RustCode )
    }else {
        unreachable!( "TokenDef-Ident" );
    }
}
;

AnyTokenNoSemi(TermType):
    ident
    | colon
    | pipe
    | percent
    | left
    | right
    | token
    | start
    | eofdef
    | tokentype
    | userdata
    | errortype
    | group
    | literal
    | equal
    | plus
    | star
    | question
    | otherpunct
    | moduleprefix
    ;

RustCode(TokenStream): AnyTokenNoSemi+ {
    let mut tokens = TokenStream::new();
    for token in AnyTokenNoSemi.into_iter() {
        tokens.extend( token.stream() );
    }
    tokens
};

StartDef(Ident): start ident semicolon {
    if let TermType::Ident(ident) = ident {
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
ReduceDef((Ident, ReduceType)): left ident semicolon {
    if let TermType::Ident(ident) = ident {
        ( ident.expect("ReduceDef-Left"), ReduceType::Left )
    }else {
        unreachable!( "ReduceDef-Ident (Left)" );
    }

    }
| right ident semicolon {
    if let TermType::Ident(ident) = ident {
        ( ident.expect("ReduceDef-Right"), ReduceType::Right )
    }else {
        unreachable!( "ReduceDef-Ident (Right)" );
    }
}
;

ErrorDef((Span,TokenStream)): errortype RustCode semicolon { (errortype.span().expect("ErrorDef"), RustCode) }
;

ModulePrefixDef((Span,TokenStream)): moduleprefix RustCode semicolon { (moduleprefix.span().expect("ModulePrefixDef"), RustCode) };

Grammar(Grammar): Rule Grammar {
    let mut g = Grammar;
    let r = Rule;
    let name = r.0.to_string();
    let span = r.0.span();
    if let Some(old) = g.rules.insert( name.clone(), r ) {
        return Err( ParseError::MultipleRuleDefinition(span, name) );
    }
    g
}
| Rule {
    let mut g = Grammar::new();
    let r = Rule;
    g.rules.insert( r.0.to_string(), r );
    g
}
| TokenDef Grammar {
    let mut g = Grammar;
    let t = TokenDef;
    let ident = t.0.clone();
    let stream = t.1.clone();
    if let Some(old) = g.terminals.insert( t.0.to_string(), t ) {
        return Err( ParseError::MultipleTokenDefinition(ident.span(), ident, old.1, stream) );
    }
    g
}
| TokenDef {
    let mut g = Grammar::new();
    let t = TokenDef;
    g.terminals.insert( t.0.to_string(), t );
    g
}
| StartDef Grammar {
    let mut g = Grammar;
    let start = StartDef;
    let span = start.span();
    if let Some(old) = g.start_rule_name {
        return Err( ParseError::MultipleStartDefinition(span, old, start) );
    }
    g.start_rule_name = Some(start);
    g
}
| StartDef {
    let mut g = Grammar::new();
    let start = StartDef;
    g.start_rule_name = Some(start);
    g
}
| EofDef Grammar {
    let mut g = Grammar;
    let (span,eof) = EofDef;
    if let Some(old) = g.eof {
        return Err( ParseError::MultipleEofDefinition(span,old, eof) );
    }
    g.eof = Some(eof);
    g
}
| EofDef {
    let mut g = Grammar::new();
    let (span,eof) = EofDef;
    g.eof = Some(eof);
    g
}
| TokenTypeDef Grammar {
    let mut g = Grammar;
    let (span,token_type) = TokenTypeDef;
    if let Some(old) = g.token_typename {
        return Err( ParseError::MultipleTokenTypeDefinition(span,old, token_type) );
    }
    g.token_typename = Some(token_type);
    g
}
| TokenTypeDef {
    let mut g = Grammar::new();
    let (span,token_type) = TokenTypeDef;
    g.token_typename = Some(token_type);
    g
}
| UserDataDef Grammar {
    let mut g = Grammar;
    let (span,user_data) = UserDataDef;
    if let Some(old) = g.userdata_typename {
        return Err( ParseError::MultipleUserDataDefinition(span,old, user_data) );
    }
    g.userdata_typename = Some(user_data);
    g
}
| UserDataDef {
    let mut g = Grammar::new();
    let (span,user_data) = UserDataDef;
    g.userdata_typename = Some(user_data);
    g
}
| ReduceDef Grammar {
    let mut g = Grammar;
    let reduce = ReduceDef;
    let span = reduce.0.span();
    let name = reduce.0.to_string();
    if let Some((ident, ReduceType)) = g.reduce_types.insert(name.clone(), reduce) {
        return Err( ParseError::MultipleReduceDefinition(span, name) );
    }
    g
}
| ReduceDef {
    let mut g = Grammar::new();
    let reduce = ReduceDef;
    g.reduce_types.insert(reduce.0.to_string(), reduce);
    g
}
| ErrorDef Grammar {
    let mut g = Grammar;
    let (span,error) = ErrorDef;
    if let Some(old) = g.error_typename {
        return Err( ParseError::MultipleErrorDefinition(span, old, error) );
    }
    g.error_typename = Some(error);
    g
}
| ErrorDef {
    let mut g = Grammar::new();
    let (span,error) = ErrorDef;
    g.error_typename = Some(error);
    g
}
| ModulePrefixDef Grammar {
    let mut g = Grammar;
    let (span,module_prefix) = ModulePrefixDef;
    // no multiple definition check for module prefix
    g.module_prefix = Some(module_prefix);
    g
}
| ModulePrefixDef {
    let mut g = Grammar::new();
    let (span,module_prefix) = ModulePrefixDef;
    g.module_prefix = Some(module_prefix);
    g
}
;
