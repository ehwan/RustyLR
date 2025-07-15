use crate::parser::args::PatternArgs;
use crate::parser::args::GrammarArgs;
use crate::parser::args::RuleDefArgs;
use crate::parser::args::RuleLineArgs;
use crate::parser::args::IdentOrLiteral;
use crate::parser::args::PrecDPrecArgs;
use crate::parser::args::RecoveredError;
use crate::parser::lexer::Lexed;
use crate::parser::span_pair::SpanPair;
use crate::terminalset::TerminalSet;
use crate::terminalset::TerminalSetItem;

use proc_macro2::Group;
use proc_macro2::Ident;
use proc_macro2::Span;
use proc_macro2::TokenStream;
use quote::ToTokens;
use quote::format_ident;

use std::boxed::Box;

use rusty_lr_core::builder::ReduceType;

// bootstrap the parser for the grammar
// this define the actual parser for proc-macro line parsing
// This should be changed to GLR parser in the future

%%

%moduleprefix ::rusty_lr_core;

%userdata GrammarArgs;

%location SpanPair;

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
%token dollar Lexed::Dollar(_);
%token comma Lexed::Comma(_);

%token literal Lexed::Literal(_);

%token parengroup Lexed::ParenGroup(_);
%token bracegroup Lexed::BraceGroup(_);

%token lparen Lexed::LParen;
%token rparen Lexed::RParen;
%token lbracket Lexed::LBracket;
%token rbracket Lexed::RBracket;

%token left Lexed::Left(_);
%token right Lexed::Right(_);
%token token Lexed::Token(_);
%token start Lexed::Start(_);
%token eofdef Lexed::EofDef(_);
%token tokentype Lexed::TokenType(_);
%token userdata Lexed::UserData(_);
%token errortype Lexed::ErrorType(_);
%token moduleprefix Lexed::ModulePrefix(_);
%token lalr Lexed::Lalr(_);
%token glr Lexed::Glr(_);
%token prec Lexed::Prec(_);
%token precedence Lexed::Precedence(_);
%token nooptim Lexed::NoOptim(_);
%token dense Lexed::Dense(_);
%token trace Lexed::Trace(_);
%token dprec Lexed::DPrec(_);
%token filter Lexed::Filter(_);
%token runtime Lexed::Runtime(_);
%token location Lexed::Location(_);

%eof Lexed::Eof;

%start Grammar;

Rule(RuleDefArgs) : ident RuleType colon RuleLines semicolon {
    let Lexed::Ident(ident) = ident else {
        unreachable!( "Rule-Ident" );
    };
    let span = @colon.span();
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
    RuleLine.separator_span = @pipe.span();
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

PrecDef(PrecDPrecArgs)
    : percent! prec! IdentOrLiteral {
        PrecDPrecArgs::Prec(IdentOrLiteral)
    }
    | percent! prec! error {
        data.error_recovered.push( RecoveredError {
            message: "Expected <ident> to token or <literal>".to_string(),
            link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#operator-precedence".to_string(),
            span: @error,
        });
        PrecDPrecArgs::None
    }
    | percent! dprec! literal {
        let Lexed::Literal(literal) = literal else {
            unreachable!( "PrecDPrecArgs-DPrec" );
        };
        PrecDPrecArgs::DPrec(literal) 
    }
    | percent! dprec! error {
        data.error_recovered.push( RecoveredError {
            message: "Expected integer literal".to_string(),
            link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#rule-priority".to_string(),
            span: @error,
        });
        PrecDPrecArgs::None
    }
    | percent! error {
        data.error_recovered.push( RecoveredError {
            message: "Expected %prec or %dprec".to_string(),
            link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#operator-precedence".to_string(),
            span: @error,
        });
        PrecDPrecArgs::None
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
| ident minus error {
    data.error_recovered.push( RecoveredError {
        message: "Expected ident for terminal set".to_string(),
        link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#patterns".to_string(),
        span: @error,
    });
    TerminalSetItem::Terminal( format_ident!("dummy") )
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
| literal minus error {
    data.error_recovered.push( RecoveredError {
        message: "Expected literal for terminal set".to_string(),
        link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#patterns".to_string(),
        span: @error,
    });
    TerminalSetItem::Terminal( format_ident!("dummy") )
}
;

TerminalSet(TerminalSet): lbracket caret? TerminalSetItem* rbracket {
    TerminalSet {
        negate: caret.is_some(),
        items: TerminalSetItem,
        open_span: @lbracket.span(),
        close_span: @rbracket.span(),
    }
}
| dot {
    let span = @dot.span();
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
    PatternArgs::Plus( Box::new(Pattern), @plus.span() )
}
| Pattern star {
    PatternArgs::Star( Box::new(Pattern), @star.span() )
}
| Pattern question {
    PatternArgs::Question( Box::new(Pattern), @question.span() )
}
| Pattern exclamation {
    PatternArgs::Exclamation( Box::new(Pattern), @exclamation.span() )
}
| TerminalSet {
    PatternArgs::TerminalSet( TerminalSet )
}
| p1=Pattern slash lh=Pattern {
    PatternArgs::Lookaheads( Box::new(p1), Box::new(lh) )
}
| lparen Pattern+ rparen {
    PatternArgs::Group(Pattern, @lparen.span(), @rparen.span())
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
| dollar ident lparen base=Pattern comma del=Pattern comma? rparen {
    let Lexed::Ident(ident) = ident else {
        unreachable!( "Pattern-Sep-Ident" );
    };
    if ident != "sep" {
        data.error_recovered.push( RecoveredError {
            message: "Expected $sep".to_string(),
            link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#patterns".to_string(),
            span: @ident,
        });
    }
    PatternArgs::Sep(
        Box::new(base),
        Box::new(del),
        false, // default is '*'
        *@$
    )
}
| dollar ident lparen base=Pattern comma del=Pattern comma plus rparen {
    let Lexed::Ident(ident) = ident else {
        unreachable!( "Pattern-Sep-Ident" );
    };
    if ident != "sep" {
        data.error_recovered.push( RecoveredError {
            message: "Expected $sep".to_string(),
            link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#patterns".to_string(),
            span: @ident,
        });
    }
    PatternArgs::Sep(
        Box::new(base),
        Box::new(del),
        true,
        *@$
    )
}
| dollar ident lparen base=Pattern comma del=Pattern comma star rparen {
    let Lexed::Ident(ident) = ident else {
        unreachable!( "Pattern-Sep-Ident" );
    };
    if ident != "sep" {
        data.error_recovered.push( RecoveredError {
            message: "Expected $sep".to_string(),
            link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#patterns".to_string(),
            span: @ident,
        });
    }
    PatternArgs::Sep(
        Box::new(base),
        Box::new(del),
        false,
        *@$
    )
}
| dollar ident lparen base=Pattern comma del=Pattern error rparen {
    let Lexed::Ident(ident) = ident else {
        unreachable!( "Pattern-Sep-Ident" );
    };
    if ident != "sep" {
        data.error_recovered.push( RecoveredError {
            message: "Expected $sep".to_string(),
            link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#patterns".to_string(),
            span: @ident,
        });
    }
    data.error_recovered.push( RecoveredError {
        message: "Unexpected $sep arguments".to_string(),
        link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#patterns".to_string(),
        span: @error
    });

    PatternArgs::Sep(
        Box::new(base),
        Box::new(del),
        false,
        *@$
    )
}
| dollar ident lparen base=Pattern comma del=Pattern comma error rparen {
    let Lexed::Ident(ident) = ident else {
        unreachable!( "Pattern-Sep-Ident" );
    };
    if ident != "sep" {
        data.error_recovered.push( RecoveredError {
            message: "Expected $sep".to_string(),
            link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#patterns".to_string(),
            span: @ident,
        });
    }
    data.error_recovered.push( RecoveredError {
        message: "Expected '+' or '*' repetition".to_string(),
        link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#patterns".to_string(),
        span: @error
    });

    PatternArgs::Sep(
        Box::new(base),
        Box::new(del),
        false,
        *@$
    )
}
// | Pattern error {
//     data.error_recovered.push( RecoveredError {
//         message: "Wrong pattern combination".to_string(),
//         link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#patterns".to_string(),
//         span: @error,
//     });
//     Pattern
// }
;

Action(Option<Group>): bracegroup {
    let Lexed::BraceGroup(group) = bracegroup else {
        unreachable!( "Action0" );
    };
    Some(group)
}
| { None }
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

RustCode(TokenStream): t=[^semicolon]+ {
    let mut tokens = TokenStream::new();
    for token in t.into_iter() {
        token.append_to_stream(&mut tokens);
    }
    tokens
};

Directive
    : percent token ident RustCode semicolon {
        let Lexed::Ident(ident) = ident else {
            unreachable!( "TokenDef-Ident" );
        };
        data.terminals.push( (ident, RustCode) );
    }
    | percent token ident semicolon {
        data.error_recovered.push( RecoveredError {
            message: "Expected token definition".to_string(),
            link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#token-definition-must-defined".to_string(),
            span: @ident
        });
    }
    | percent token error semicolon {
        data.error_recovered.push( RecoveredError {
            message: "Expected token name".to_string(),
            link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#token-definition-must-defined".to_string(),
            span: @error
        });
    }
    | percent start ident semicolon {
        let Lexed::Ident(ident) = ident else {
            unreachable!( "StartDef-Ident" );
        };
        data.start_rule_name.push(ident);
    }
    | percent start error semicolon {
        data.error_recovered.push( RecoveredError {
            message: "Expected start rule name".to_string(),
            link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#start-symbol-must-defined".to_string(),
            span: @error
        });
    }
    | percent eofdef RustCode semicolon {
        data.eof.push( (@eofdef.span(), RustCode) );
    }
    | percent eofdef semicolon {
        data.error_recovered.push( RecoveredError {
            message: "Expected EOF definition".to_string(),
            link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#eof-symbol-must-defined".to_string(),
            span: @eofdef
        });
    }
    | percent tokentype RustCode semicolon {
        data.token_typename.push( (@tokentype.span(), RustCode) );
    }
    | percent tokentype semicolon {
        data.error_recovered.push( RecoveredError {
            message: "Expected token type definition".to_string(),
            link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#token-type-must-defined".to_string(),
            span: @tokentype
        });
    }
    | percent userdata RustCode semicolon {
        data.userdata_typename.push( (@userdata.span(),RustCode) );
    }
    | percent userdata semicolon {
        data.error_recovered.push( RecoveredError {
            message: "Expected userdata definition".to_string(),
            link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#userdata-type-optional".to_string(),
            span: @userdata
        });
    }
    | percent left IdentOrLiteral+ semicolon {
        data.precedences.push( (@left.span(), Some(ReduceType::Left), IdentOrLiteral) );
    }
    | percent left error semicolon {
        data.error_recovered.push( RecoveredError {
            message: "Expected <ident> to token or <literal>".to_string(),
            link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#operator-precedence".to_string(),
            span: @error
        });
    }
    | percent right IdentOrLiteral+ semicolon {
        data.precedences.push( (@right.span(), Some(ReduceType::Right), IdentOrLiteral) );
    }
    | percent right error semicolon {
        data.error_recovered.push( RecoveredError {
            message: "Expected <ident> to token or <literal>".to_string(),
            link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#operator-precedence".to_string(),
            span: @error
        });
    }
    | percent precedence IdentOrLiteral+ semicolon {
        data.precedences.push( (@precedence.span(), None, IdentOrLiteral) );
    }
    | percent precedence error semicolon {
        data.error_recovered.push( RecoveredError {
            message: "Expected <ident> to token or <literal>".to_string(),
            link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#operator-precedence".to_string(),
            span: @error
        });
    }
    | percent errortype RustCode semicolon {
        data.error_typename.push( (@errortype.span(), RustCode) );
    }
    | percent errortype semicolon {
        data.error_recovered.push( RecoveredError {
            message: "Expected error type definition".to_string(),
            link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#error-type-optional".to_string(),
            span: @errortype
        });
    }
    | percent moduleprefix RustCode semicolon {
        data.module_prefix.push( (@moduleprefix.span(), RustCode) );
    }
    | percent moduleprefix semicolon {
        data.error_recovered.push( RecoveredError {
            message: "Expected moduleprefix definition".to_string(),
            link: "This is hidden directive, user must not use this explicitly".to_string(),
            span: @moduleprefix
        });
    }
    | percent glr semicolon {
        data.glr = true;
    }
    | percent glr error semicolon {
        data.error_recovered.push( RecoveredError {
            message: "Expected semicolon".to_string(),
            link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#glr-parser-generation".to_string(),
            span: @error,
        });
    }
    | percent lalr semicolon {
        data.lalr = true;
    }
    | percent lalr error semicolon {
        data.error_recovered.push( RecoveredError {
            message: "Expected semicolon".to_string(),
            link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#lalr-parser-generation".to_string(),
            span: @error,
        });
    }
    | percent nooptim semicolon {
        data.no_optim = true;
    }
    | percent nooptim error semicolon {
        data.error_recovered.push( RecoveredError {
            message: "Expected semicolon".to_string(),
            link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#no-optimization".to_string(),
            span: @error,
        });
    }
    | percent dense semicolon {
        data.dense = true;
    }
    | percent dense error semicolon {
        data.error_recovered.push( RecoveredError {
            message: "Expected semicolon".to_string(),
            link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#dense-parser-table".to_string(),
            span: @error,
        });
    }
    | percent trace ident* semicolon {
        let idents = ident.into_iter().map(|t| {
            let Lexed::Ident(ident) = t else {
                unreachable!( "Trace-Ident" );
            };
            ident
        });
        data.traces.extend( idents );
    }
    | percent trace error semicolon {
        data.error_recovered.push( RecoveredError {
            message: "Expected ident".to_string(),
            link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#tracing-non-terminals".to_string(),
            span: @error,
        });
    }
    | percent filter! RustCode semicolon! {
        data.filter = Some(RustCode);
    }
    | percent filter! semicolon! {
        data.error_recovered.push( RecoveredError {
            message: "Expected filter definition".to_string(),
            link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#filter-directive".to_string(),
            span: @filter,
        });
    }
    | percent runtime semicolon {
        data.compiled = false;
    }
    | percent runtime error semicolon {
        data.error_recovered.push( RecoveredError {
            message: "Expected semicolon".to_string(),
            link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#runtime-table-calculation".to_string(),
            span: @error,
        });
    }
    | percent location! RustCode semicolon! {
        data.location_typename = Some(RustCode);
    }
    | percent location! semicolon! {
        data.error_recovered.push( RecoveredError {
            message: "Expected location type definition".to_string(),
            link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#location-tracking".to_string(),
            span: @location,
        });
    }
    | percent error semicolon {
        data.error_recovered.push( RecoveredError {
            message: "Expected directive, e.g. %token, %start, %eof, ...".to_string(),
            link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#syntax".to_string(),
            span: @error,
        });
    }
    ;



GrammarLine : Rule { data.rules.push(Rule); }
    | Directive
    ;

Grammar: GrammarLine+;