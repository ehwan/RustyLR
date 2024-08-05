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

use rusty_lr::lalr1;
use rusty_lr::ReduceType;
// use rusty_lr::lr1;

// bootstrap the parser for the grammar; with rusty_lr 0.9.0
// this define the actual parser for proc-macro line parsing
lalr1! {
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
    %token otherpunct TermType::OtherPunct(None);
    %token moduleprefix TermType::ModulePrefix(None);
    %eof TermType::Eof;

    %start Grammar;

    Rule((Ident, Option<TokenStream>, RuleLines)) : ident RuleType colon RuleLines semicolon {
        let ident = if let TermType::Ident(ident) = ident.value {
            ident.as_ref().unwrap().clone()
        } else {
            unreachable!( "Rule-Ident" );
        };
        ( ident, RuleType.value.map(|t| t.to_token_stream()), RuleLines{rule_lines: RuleLines.value} )
    }
    ;

    RuleType(Option<Group>): group {
        if let TermType::Group(group) = *group {
            if let Some(group) = group {
                if group.delimiter() != proc_macro2::Delimiter::Parenthesis {
                    return Err( ParseError::InvalidRuletypeDelimiter(group.span()) );
                }
                Some(group.clone())
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
        let mut v = RuleLines.value;
        v.push( RuleLine.value );
        v
    }
    | RuleLine {
        vec![ RuleLine.value ]
    }
    ;

    RuleLine(RuleLine): RuleDef Action
    {
        RuleLine {
           tokens: RuleDef.value,
           reduce_action: Action.value.map(|action| action.to_token_stream())
        }
    }
    ;

    RuleDef(Vec<TokenMapped>): Tokens { Tokens.value };

    Tokens(Vec<TokenMapped>): TokensOne  { TokensOne.value }
          | { vec![] }
          ;
    TokensOne(Vec<TokenMapped>): TokensOne Token {
        let mut v = TokensOne.value;
        v.push( Token.value );
        v
    }
    | Token { vec![Token.value] }
    ;

    Token(TokenMapped): ident {
        if let TermType::Ident(ident) = *ident {
            TokenMapped {
                token: Token::NonTerm( ident.as_ref().unwrap().clone() ),
                mapped: None
            }
        }else {
            unreachable!( "Token-Ident" );
        }
    }
    | mapto=ident equal ident {
        let ident = if let TermType::Ident(ident) = *ident {
            ident.as_ref().unwrap().clone()
        }else {
            unreachable!( "Token-Ident2" );
        };
        let mapto = if let TermType::Ident(mapto) = *mapto {
            mapto.as_ref().unwrap().clone()
        }else {
            unreachable!( "Token-Ident3" );
        };

        TokenMapped {
            token: Token::NonTerm( ident ),
            mapped: Some(mapto)
        }
    }
         ;

    Action(Option<Group>): group {
        if let TermType::Group(group) = *group {
            if let Some(action) = group {
                if action.delimiter() != proc_macro2::Delimiter::Brace {
                    return Err( ParseError::InvalidReduceActionDelimiter(action.span()) );
                }
                Some(action.clone())
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
        if let TermType::Ident(ident) = *ident {
            ( ident.as_ref().unwrap().clone(), RustCode.value )
        }else {
            unreachable!( "TokenDef-Ident" );
        }
    }
    ;

    AnyTokenNoSemi:
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
        | otherpunct
        | moduleprefix
        ;

    AnyTokens: AnyTokenNoSemi AnyTokens
             | AnyTokenNoSemi
             ;

    RustCode(TokenStream): AnyTokens {
        let mut tokens = TokenStream::new();
        for token in AnyTokens.slice.iter() {
            tokens.extend( token.clone().stream() );
        }
        tokens
    };

    StartDef(Ident): start ident semicolon {
        if let TermType::Ident(ident) = *ident {
            ident.as_ref().unwrap().clone()
        }else {
            unreachable!( "StartDef-Ident" );
        }
    }
    ;
    EofDef((Span,TokenStream)): eofdef RustCode semicolon { (eofdef.value.span().unwrap(), RustCode.value) }
    ;
    TokenTypeDef((Span,TokenStream)): tokentype RustCode semicolon { (tokentype.value.span().unwrap(),RustCode.value) }
    ;
    UserDataDef((Span,TokenStream)): userdata RustCode semicolon { (userdata.value.span().unwrap(),RustCode.value) }
    ;
    ReduceDef((Ident, ReduceType)): left ident semicolon {
        if let TermType::Ident(ident) = *ident {
            ( ident.as_ref().unwrap().clone(), ReduceType::Left )
        }else {
            unreachable!( "ReduceDef-Ident (Left)" );
        }

     }
    | right ident semicolon {
        if let TermType::Ident(ident) = *ident {
            ( ident.as_ref().unwrap().clone(), ReduceType::Right )
        }else {
            unreachable!( "ReduceDef-Ident (Right)" );
        }
    }
    ;

    ErrorDef((Span,TokenStream)): errortype RustCode semicolon { (errortype.value.span().unwrap(), RustCode.value) }
    ;

    ModulePrefixDef((Span,TokenStream)): moduleprefix RustCode semicolon { (moduleprefix.value.span().unwrap(), RustCode.value) };

    Grammar(Grammar): Rule Grammar {
        let mut g = Grammar.value;
        let r = Rule.value;
        let name = r.0.to_string();
        let span = r.0.span();
        if let Some(old) = g.rules.insert( name.clone(), r ) {
            return Err( ParseError::MultipleRuleDefinition(span, name) );
        }
        g
    }
    | Rule {
        let mut g = Grammar::new();
        let r = Rule.value;
        g.rules.insert( r.0.to_string(), r );
        g
    }
    | TokenDef Grammar {
        let mut g = Grammar.value;
        let t = TokenDef.value;
        let ident = t.0.clone();
        let stream = t.1.clone();
        if let Some(old) = g.terminals.insert( t.0.to_string(), t ) {
            return Err( ParseError::MultipleTokenDefinition(ident.span(), ident, old.1, stream) );
        }
        g
    }
    | TokenDef {
        let mut g = Grammar::new();
        let t = TokenDef.value;
        g.terminals.insert( t.0.to_string(), t );
        g
    }
    | StartDef Grammar {
        let mut g = Grammar.value;
        let start = StartDef.value;
        let span = start.span();
        if let Some(old) = g.start_rule_name {
            return Err( ParseError::MultipleStartDefinition(span, old, start) );
        }
        g.start_rule_name = Some(start);
        g
    }
    | StartDef {
        let mut g = Grammar::new();
        let start = StartDef.value;
        g.start_rule_name = Some(start);
        g
    }
    | EofDef Grammar {
        let mut g = Grammar.value;
        let (span,eof) = EofDef.value;
        if let Some(old) = g.eof {
            return Err( ParseError::MultipleEofDefinition(span,old, eof) );
        }
        g.eof = Some(eof);
        g
    }
    | EofDef {
        let mut g = Grammar::new();
        let (span,eof) = EofDef.value;
        g.eof = Some(eof);
        g
    }
    | TokenTypeDef Grammar {
        let mut g = Grammar.value;
        let (span,token_type) = TokenTypeDef.value;
        if let Some(old) = g.token_typename {
            return Err( ParseError::MultipleTokenTypeDefinition(span,old, token_type) );
        }
        g.token_typename = Some(token_type);
        g
    }
    | TokenTypeDef {
        let mut g = Grammar::new();
        let (span,token_type) = TokenTypeDef.value;
        g.token_typename = Some(token_type);
        g
    }
    | UserDataDef Grammar {
        let mut g = Grammar.value;
        let (span,user_data) = UserDataDef.value;
        if let Some(old) = g.userdata_typename {
            return Err( ParseError::MultipleUserDataDefinition(span,old, user_data) );
        }
        g.userdata_typename = Some(user_data);
        g
    }
    | UserDataDef {
        let mut g = Grammar::new();
        let (span,user_data) = UserDataDef.value;
        g.userdata_typename = Some(user_data);
        g
    }
    | ReduceDef Grammar {
        let mut g = Grammar.value;
        let reduce = ReduceDef.value;
        let span = reduce.0.span();
        let name = reduce.0.to_string();
        if let Some((ident, ReduceType)) = g.reduce_types.insert(name.clone(), reduce) {
            return Err( ParseError::MultipleReduceDefinition(span, name) );
        }
        g
    }
    | ReduceDef {
        let mut g = Grammar::new();
        let reduce = ReduceDef.value;
        g.reduce_types.insert(reduce.0.to_string(), reduce);
        g
    }
    | ErrorDef Grammar {
        let mut g = Grammar.value;
        let (span,error) = ErrorDef.value;
        if let Some(old) = g.error_typename {
            return Err( ParseError::MultipleErrorDefinition(span, old, error) );
        }
        g.error_typename = Some(error);
        g
    }
    | ErrorDef {
        let mut g = Grammar::new();
        let (span,error) = ErrorDef.value;
        g.error_typename = Some(error);
        g
    }
    | ModulePrefixDef Grammar {
        let mut g = Grammar.value;
        let (span,module_prefix) = ModulePrefixDef.value;
        // no multiple definition check for module prefix
        g.module_prefix = Some(module_prefix);
        g
    }
    | ModulePrefixDef {
        let mut g = Grammar::new();
        let (span,module_prefix) = ModulePrefixDef.value;
        g.module_prefix = Some(module_prefix);
        g
    }
    ;
}
