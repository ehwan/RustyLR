use crate::parser::args::PatternArgs;
use crate::parser::args::GrammarArgs;
use crate::parser::args::RuleDefArgs;
use crate::parser::args::RuleLineArgs;
use crate::parser::args::IdentOrLiteral;
use crate::parser::args::AllowTarget;
use crate::parser::args::PrecDPrecArgs;
use crate::parser::args::RecoveredError;
use crate::parser::lexer::Lexed;
use crate::parser::location::Location;
use crate::parser::location::Located;
use crate::terminalset::TerminalSet;
use crate::terminalset::TerminalSetItem;

use proc_macro2::Group;
use proc_macro2::TokenStream;
use quote::ToTokens;

use std::boxed::Box;

use rusty_lr_core::production::Associativity;

// bootstrap the parser for the grammar
// this define the actual parser for proc-macro line parsing
// This should be changed to GLR parser in the future

%%

%moduleprefix ::rusty_lr_core;

%userdata GrammarArgs;

%location Location;

%tokentype Lexed;
%token ident Lexed::Ident(ident);
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
%token dot Lexed::Dot(_);
%token dollar Lexed::Dollar(_);
%token comma Lexed::Comma(_);

%token int_literal Lexed::IntLiteral(_);
%token byte_literal Lexed::ByteLiteral(_);
%token byte_str_literal Lexed::ByteStrLiteral(_);
%token char_literal Lexed::CharLiteral(_);
%token str_literal Lexed::StrLiteral(_);
%token other_literal Lexed::OtherLiteral(_);


%token parengroup Lexed::ParenGroup(parengroup);
%token bracegroup Lexed::BraceGroup(bracegroup);

%token lparen Lexed::LParen;
%token rparen Lexed::RParen;
%token lbracket Lexed::LBracket;
%token rbracket Lexed::RBracket;

%token left Lexed::Left(_);
%token right Lexed::Right(_);
%token token Lexed::Token(_);
%token start Lexed::Start(_);
%token tokentype Lexed::TokenType(_);
%token userdata Lexed::UserData(_);
%token errortype Lexed::ErrorType(_);
%token moduleprefix Lexed::ModulePrefix(_);
%token lalr Lexed::Lalr(_);
%token glr Lexed::Glr(_);
%token prec Lexed::Prec(_);
%token precedence Lexed::Precedence(_);
%token nooptim Lexed::NoOptim(_);
%token dprec Lexed::DPrec(_);
%token location Lexed::Location(_);
%token allow Lexed::Allow(_);

%start Grammar;

Rule(box RuleDefArgs) : ident RuleType colon RuleLines semicolon {
    let $ident = ident else { // "$ident" replaced with "$ident" in the macro expansion
        unreachable!( "Rule-Ident" );
    };
    if let Some(fisrt) = RuleLines.first_mut() {
        fisrt.separator_location = @colon;
    }
    RuleDefArgs {
        name: Located::new(ident.to_string(), @ident),
        typename: RuleType.map(|t| t.stream()),
        rule_lines: RuleLines,
    }
}
| ident RuleType colon RuleLines error semicolon {
    let $ident = ident else {
        unreachable!( "Rule-Ident" );
    };
    data.error_recovered.push( RecoveredError {
        message: "Expected semicolon or rule alternative".to_string(),
        link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#production-rules".to_string(),
        location: @error,
    });
    if let Some(fisrt) = RuleLines.first_mut() {
        fisrt.separator_location = @colon;
    }
    RuleDefArgs {
        name: Located::new(ident.to_string(), @ident),
        typename: RuleType.map(|t| t.stream()),
        rule_lines: RuleLines,
    }
}
;

RuleType(Option<Group>): parengroup {
    let $parengroup = parengroup else {
        unreachable!( "RuleType - Group" );
    };
    Some(parengroup)
}
| {
    None
}
;

RuleLines(Vec<RuleLineArgs>): RuleLines pipe RuleLine {
    RuleLine.separator_location = @pipe;
    RuleLines.push( RuleLine );
    RuleLines
}
| RuleLine {
    vec![ RuleLine ]
}
;

RuleLine(box RuleLineArgs): MappedSymbol* PrecDef* Action
{
    RuleLineArgs {
        tokens: MappedSymbol,
        reduce_action: Action.map(|action| action.to_token_stream()),
        separator_location: Location::default(), // will be set later in RuleLines
        precs: PrecDef,
    }
}
;

PrecDef(box PrecDPrecArgs)
    : percent! prec! IdentOrLiteral {
        PrecDPrecArgs::Prec(IdentOrLiteral)
    }
    | percent! prec! error {
        data.error_recovered.push( RecoveredError {
            message: "Expected <ident> to token or <literal>".to_string(),
            link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#operator-precedence".to_string(),
            location: @error,
        });
        PrecDPrecArgs::None
    }
    | percent! dprec! int_literal {
        let Lexed::IntLiteral(i) = int_literal else {
            unreachable!( "PrecDPrecArgs-DPrec" );
        };
        PrecDPrecArgs::DPrec(Located::new(i, @int_literal)) 
    }
    | percent! dprec! error {
        data.error_recovered.push( RecoveredError {
            message: "Expected integer literal".to_string(),
            link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#rule-priority".to_string(),
            location: @error,
        });
        PrecDPrecArgs::None
    }
    | percent! error {
        data.error_recovered.push( RecoveredError {
            message: "Expected %prec or %dprec".to_string(),
            link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#operator-precedence".to_string(),
            location: @error,
        });
        PrecDPrecArgs::None
    }
    ;

MappedSymbol(box (Option<Located<String>>, PatternArgs)): Pattern {
    ( None, Pattern )
}
| ident equal Pattern {
    let $ident = ident else {
        unreachable!( "Token-Ident" );
    };
    ( Some(Located::new(ident.to_string(), @ident)), Pattern )
}
| ident equal error {
    let $ident = ident else {
        unreachable!( "Token-Ident" );
    };
    data.error_recovered.push( RecoveredError {
        message: "Expected pattern after symbol binding".to_string(),
        link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#patterns".to_string(),
        location: @error,
    });
    ( Some(Located::new(ident.to_string(), @ident)), PatternArgs::Ident(Default::default()) )
}
;

TerminalSetItem(TerminalSetItem): ident {
    let $ident = ident else {
        unreachable!( "TerminalSetItem-Range1" );
    };
    TerminalSetItem::Terminal( Located::new(ident.to_string(), @ident) )
}
| first=ident minus last=ident {
    let Lexed::Ident(first) = first else {
        unreachable!( "TerminalSetItem-Range1" );
    };
    let Lexed::Ident(last) = last else {
        unreachable!( "TerminalSetItem-Range3" );
    };

    TerminalSetItem::Range( Located::new(first.to_string(), @first), Located::new(last.to_string(), @last) )
}
| ident minus error {
    data.error_recovered.push( RecoveredError {
        message: "Expected ident for terminal set".to_string(),
        link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#patterns".to_string(),
        location: @error,
    });
    TerminalSetItem::Terminal( Default::default() )
}
| char_literal {
    let Lexed::CharLiteral(ch) = char_literal else {
        unreachable!( "TerminalSetItem-CharLiteral1" );
    };
    TerminalSetItem::Char(Located::new(ch.value(), @char_literal))
}
| first=char_literal minus last=char_literal {
    let Lexed::CharLiteral(first) = first else {
        unreachable!( "TerminalSetItem-CharLiteral2" );
    };
    let Lexed::CharLiteral(last) = last else {
        unreachable!( "TerminalSetItem-CharLiteral3" );
    };
    TerminalSetItem::CharRange(Located::new(first.value(), @first), Located::new(last.value(), @last))
}
| char_literal minus error {
    data.error_recovered.push( RecoveredError {
        message: "Expected char literal for terminal set".to_string(),
        link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#patterns".to_string(),
        location: @error,
    });
    TerminalSetItem::Terminal( Default::default() )
}
| byte_literal {
    let Lexed::ByteLiteral(b) = byte_literal else {
        unreachable!( "TerminalSetItem-ByteLiteral1" );
    };
    TerminalSetItem::Byte(Located::new(b.value(), @byte_literal))
}
| first=byte_literal minus last=byte_literal {
    let Lexed::ByteLiteral(first) = first else {
        unreachable!( "TerminalSetItem-ByteLiteral2" );
    };
    let Lexed::ByteLiteral(last) = last else {
        unreachable!( "TerminalSetItem-ByteLiteral3" );
    };
    TerminalSetItem::ByteRange(Located::new(first.value(), @first), Located::new(last.value(), @last))
}
| byte_literal minus error {
    data.error_recovered.push( RecoveredError {
        message: "Expected byte literal for terminal set".to_string(),
        link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#patterns".to_string(),
        location: @error,
    });
    TerminalSetItem::Terminal( Default::default() )
}
;

TerminalSet(TerminalSet): lbracket caret? TerminalSetItem* rbracket {
    TerminalSet {
        negate: caret.is_some(),
        items: TerminalSetItem,
        open_location: @lbracket,
        close_location: @rbracket,
    }
}
| lbracket caret? error rbracket {
    data.error_recovered.push( RecoveredError {
        message: "Expected terminal set item".to_string(),
        link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#patterns".to_string(),
        location: @error,
    });
    TerminalSet {
        negate: caret.is_some(),
        items: vec![],
        open_location: @lbracket,
        close_location: @rbracket,
    }
}
| dot {
    let span = @dot;
    TerminalSet {
        negate: true,
        items: vec![],
        open_location: span.clone(),
        close_location: span,
    }
}
;

%left minus;
%left star plus question exclamation;
%precedence empty_action;
%precedence error;

Pattern(PatternArgs): ident {
    let $ident = ident else {
        unreachable!( "Pattern-Ident" );
    };
    PatternArgs::Ident( Located::new(ident.to_string(), @ident) )
}
| Pattern plus {
    let Lexed::Plus(plus) = plus else {
        unreachable!( "Pattern-Plus" );
    };
    PatternArgs::Plus { base: Box::new(Pattern), op_location: @plus }
}
| Pattern star {
    PatternArgs::Star { base: Box::new(Pattern), op_location: @star }
}
| Pattern question {
    PatternArgs::Question { base: Box::new(Pattern), op_location: @question }
}
| Pattern exclamation {
    PatternArgs::Exclamation { base: Box::new(Pattern), op_location: @exclamation }
}
| TerminalSet {
    PatternArgs::TerminalSet( TerminalSet )
}
| lparen $sep( Pattern*, pipe, + ) rparen {
    PatternArgs::Group { alternatives: Pattern, open_location: @lparen, close_location: @rparen }
}
| lparen error rparen {
    data.error_recovered.push( RecoveredError {
        message: "syntax error when parsing GROUP".to_string(),
        link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#patterns".to_string(),
        location: @error,
    });
    PatternArgs::Ident( Default::default() )
}
| byte_literal {
    let Lexed::ByteLiteral(b) = byte_literal else {
        unreachable!( "Pattern-ByteLiteral" );
    };
    PatternArgs::Byte(Located::new(b.value(), @byte_literal))
}
| byte_str_literal {
    let Lexed::ByteStrLiteral(b) = byte_str_literal else {
        unreachable!( "Pattern-ByteStringLiteral" );
    };
    PatternArgs::ByteString(Located::new(b.value(), @byte_str_literal))
}
| char_literal {
    let Lexed::CharLiteral(c) = char_literal else {
        unreachable!( "Pattern-CharLiteral" );
    };
    PatternArgs::Char(Located::new(c.value(), @char_literal))
}
| str_literal {
    let Lexed::StrLiteral(s) = str_literal else {
        unreachable!( "Pattern-StringLiteral" );
    };
    PatternArgs::String(Located::new(s.value(), @str_literal))
}
| p1=Pattern minus p2=Pattern {
    PatternArgs::Minus { base: Box::new(p1), exclude: Box::new(p2) }
}
| dollar ident lparen base=Pattern comma del=Pattern comma? rparen {
    let $ident = ident else {
        unreachable!( "Pattern-Sep-Ident" );
    };
    if ident != "sep" {
        data.error_recovered.push( RecoveredError {
            message: "Expected $sep".to_string(),
            link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#patterns".to_string(),
            location: @ident,
        });
    }
    PatternArgs::Sep {
        base: Box::new(base),
        delimiter: Box::new(del),
        at_least_one: false, // default is '*'
        location: *@$
    }
}
| dollar ident lparen base=Pattern comma del=Pattern comma plus rparen {
    let $ident = ident else {
        unreachable!( "Pattern-Sep-Ident" );
    };
    if ident != "sep" {
        data.error_recovered.push( RecoveredError {
            message: "Expected $sep".to_string(),
            link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#patterns".to_string(),
            location: @ident,
        });
    }
    PatternArgs::Sep {
        base: Box::new(base),
        delimiter: Box::new(del),
        at_least_one: true,
        location: *@$
    }
}
| dollar ident lparen base=Pattern comma del=Pattern comma star rparen {
    let $ident = ident else {
        unreachable!( "Pattern-Sep-Ident" );
    };
    if ident != "sep" {
        data.error_recovered.push( RecoveredError {
            message: "Expected $sep".to_string(),
            link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#patterns".to_string(),
            location: @ident,
        });
    }
    PatternArgs::Sep {
        base: Box::new(base),
        delimiter: Box::new(del),
        at_least_one: false,
        location: *@$
    }
}
| dollar ident lparen base=Pattern comma del=Pattern error rparen {
    let $ident = ident else {
        unreachable!( "Pattern-Sep-Ident" );
    };
    if ident != "sep" {
        data.error_recovered.push( RecoveredError {
            message: "Expected $sep".to_string(),
            link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#patterns".to_string(),
            location: @ident,
        });
    }
    data.error_recovered.push( RecoveredError {
        message: "Unexpected $sep arguments".to_string(),
        link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#patterns".to_string(),
        location: @error
    });

    PatternArgs::Sep {
        base: Box::new(base),
        delimiter: Box::new(del),
        at_least_one: false,
        location: *@$
    }
}
| dollar ident lparen base=Pattern comma del=Pattern comma error rparen {
    let $ident = ident else {
        unreachable!( "Pattern-Sep-Ident" );
    };
    if ident != "sep" {
        data.error_recovered.push( RecoveredError {
            message: "Expected $sep".to_string(),
            link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#patterns".to_string(),
            location: @ident,
        });
    }
    data.error_recovered.push( RecoveredError {
        message: "Expected '+' or '*' repetition".to_string(),
        link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#patterns".to_string(),
        location: @error
    });

    PatternArgs::Sep {
        base: Box::new(base),
        delimiter: Box::new(del),
        at_least_one: false,
        location: *@$
    }
}
// | Pattern error {
//     data.error_recovered.push( RecoveredError {
//         message: "Wrong pattern combination".to_string(),
//         link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#patterns".to_string(),
//         location: @error,
//     });
//     Pattern
// }
;

Action(Option<Group>): bracegroup {
    let $bracegroup = bracegroup else {
        unreachable!( "Action0" );
    };
    Some(bracegroup)
}
| error {
    data.error_recovered.push( RecoveredError {
        message: "Expected reduce action block or rule terminator".to_string(),
        link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#reduceaction-optional".to_string(),
        location: @error,
    });
    None
}
| %prec empty_action { None }
;

IdentOrLiteral(IdentOrLiteral): ident {
    let $ident = ident else {
        unreachable!( "IdentOrLiteral-Ident" );
    };
    IdentOrLiteral::Ident( Located::new(ident.to_string(), @ident) )
}
| byte_literal {
    let Lexed::ByteLiteral(b) = byte_literal else {
        unreachable!( "IdentOrLiteral-ByteLiteral" );
    };
    IdentOrLiteral::Byte(Located::new(b.value(), @byte_literal))
}
| char_literal {
    let Lexed::CharLiteral(c) = char_literal else {
        unreachable!( "IdentOrLiteral-CharLiteral" );
    };
    IdentOrLiteral::Char(Located::new(c.value(), @char_literal))
}
;

AllowTarget(AllowTarget): ident {
    let Lexed::Ident(ident) = ident else {
        unreachable!( "AllowTarget-Ident" );
    };
    AllowTarget::Ident( Located::new(ident.to_string(), @ident) )
}
| byte_literal {
    let Lexed::ByteLiteral(b) = byte_literal else {
        unreachable!( "AllowTarget-ByteLiteral" );
    };
    AllowTarget::Byte(Located::new(b.value(), @byte_literal))
}
| char_literal {
    let Lexed::CharLiteral(c) = char_literal else {
        unreachable!( "AllowTarget-CharLiteral" );
    };
    AllowTarget::Char(Located::new(c.value(), @char_literal))
}
| first=char_literal minus last=char_literal {
    let Lexed::CharLiteral(first) = first else {
        unreachable!( "AllowTarget-CharLiteralRange1" );
    };
    let Lexed::CharLiteral(last) = last else {
        unreachable!( "AllowTarget-CharLiteralRange2" );
    };
    AllowTarget::CharRange(Located::new(first.value(), @first), Located::new(last.value(), @last))
}
| first=byte_literal minus last=byte_literal {
    let Lexed::ByteLiteral(first) = first else {
        unreachable!( "AllowTarget-ByteLiteralRange1" );
    };
    let Lexed::ByteLiteral(last) = last else {
        unreachable!( "AllowTarget-ByteLiteralRange2" );
    };
    AllowTarget::ByteRange(Located::new(first.value(), @first), Located::new(last.value(), @last))
}
| TerminalSet {
    AllowTarget::TerminalSet(TerminalSet)
}
;

RustCode(TokenStream): t=[^semicolon]+ {
    let mut tokens = TokenStream::new();
    for token in t.into_iter() {
        token.append_to_stream(&mut tokens);
    }
    tokens
};

Directive
    : percent token ident RustCode semicolon {
        let $ident = ident else {
            unreachable!( "TokenDef-Ident" );
        };
        data.terminals.push( (Located::new(ident.to_string(), @ident), RustCode) );
    }
    | percent token ident semicolon {
        data.error_recovered.push( RecoveredError {
            message: "Expected token definition".to_string(),
            link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#token-definition-must-defined".to_string(),
            location: @ident
        });
    }
    | percent token error semicolon {
        data.error_recovered.push( RecoveredError {
            message: "Expected token name".to_string(),
            link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#token-definition-must-defined".to_string(),
            location: @error
        });
    }
    | percent start ident semicolon {
        let $ident = ident else {
            unreachable!( "StartDef-Ident" );
        };
        data.start_rule_name.push(Located::new(ident.to_string(), @ident));
    }
    | percent start error semicolon {
        data.error_recovered.push( RecoveredError {
            message: "Expected start rule name".to_string(),
            link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#start-symbol-must-defined".to_string(),
            location: @error
        });
    }
    | percent tokentype RustCode semicolon {
        data.token_typename.push( (@tokentype, RustCode) );
    }
    | percent tokentype semicolon {
        data.error_recovered.push( RecoveredError {
            message: "Expected token type definition".to_string(),
            link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#token-type-must-defined".to_string(),
            location: @tokentype
        });
    }
    | percent userdata RustCode semicolon {
        data.userdata_typename.push( (@userdata, RustCode) );
    }
    | percent userdata semicolon {
        data.error_recovered.push( RecoveredError {
            message: "Expected userdata definition".to_string(),
            link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#userdata-type-optional".to_string(),
            location: @userdata
        });
    }
    | percent left IdentOrLiteral+ semicolon {
        data.precedences.push( (@left, Some(Associativity::Left), IdentOrLiteral) );
    }
    | percent left error semicolon {
        data.error_recovered.push( RecoveredError {
            message: "Expected <ident> to token or <literal>".to_string(),
            link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#operator-precedence".to_string(),
            location: @error
        });
    }
    | percent right IdentOrLiteral+ semicolon {
        data.precedences.push( (@right, Some(Associativity::Right), IdentOrLiteral) );
    }
    | percent right error semicolon {
        data.error_recovered.push( RecoveredError {
            message: "Expected <ident> to token or <literal>".to_string(),
            link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#operator-precedence".to_string(),
            location: @error
        });
    }
    | percent precedence IdentOrLiteral+ semicolon {
        data.precedences.push( (@precedence, None, IdentOrLiteral) );
    }
    | percent precedence error semicolon {
        data.error_recovered.push( RecoveredError {
            message: "Expected <ident> to token or <literal>".to_string(),
            link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#operator-precedence".to_string(),
            location: @error
        });
    }
    | percent errortype RustCode semicolon {
        data.error_typename.push( (@errortype, RustCode) );
    }
    | percent errortype semicolon {
        data.error_recovered.push( RecoveredError {
            message: "Expected error type definition".to_string(),
            link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#error-type-optional".to_string(),
            location: @errortype
        });
    }
    | percent moduleprefix RustCode semicolon {
        data.module_prefix.push( (@moduleprefix, RustCode) );
    }
    | percent moduleprefix semicolon {
        data.error_recovered.push( RecoveredError {
            message: "Expected moduleprefix definition".to_string(),
            link: "This is hidden directive, user must not use this explicitly".to_string(),
            location: @moduleprefix
        });
    }
    | percent glr semicolon {
        data.glr = true;
    }
    | percent glr error semicolon {
        data.error_recovered.push( RecoveredError {
            message: "Expected semicolon".to_string(),
            link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#glr-parser-generation".to_string(),
            location: @error,
        });
    }
    | percent lalr semicolon {
        data.lalr = true;
    }
    | percent lalr error semicolon {
        data.error_recovered.push( RecoveredError {
            message: "Expected semicolon".to_string(),
            link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#lalr-parser-generation".to_string(),
            location: @error,
        });
    }
    | percent nooptim semicolon {
        data.no_optim = true;
    }
    | percent nooptim error semicolon {
        data.error_recovered.push( RecoveredError {
            message: "Expected semicolon".to_string(),
            link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#no-optimization".to_string(),
            location: @error,
        });
    }



    | percent location! RustCode semicolon! {
        data.location_typename.push((@location, RustCode));
    }
    | percent location! semicolon! {
        data.error_recovered.push( RecoveredError {
            message: "Expected location type definition".to_string(),
            link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#location-tracking".to_string(),
            location: @location,
        });
    }
    | percent allow ident semicolon {
        let $ident = ident else {
            unreachable!( "AllowDef-Ident" );
        };
        data.allowed_diagnostics.push((Located::new(ident.to_string(), @ident), None));
    }
    | percent allow ident lparen AllowTarget rparen semicolon {
        let $ident = ident else {
            unreachable!( "AllowDef-Ident" );
        };
        data.allowed_diagnostics.push((Located::new(ident.to_string(), @ident), Some(AllowTarget)));
    }
    | percent allow ident lparen error rparen semicolon {
        data.error_recovered.push( RecoveredError {
            message: "Expected diagnostic suppression target".to_string(),
            link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#diagnostic-suppression".to_string(),
            location: @error,
        });
    }
    | percent allow error semicolon {
        data.error_recovered.push( RecoveredError {
            message: "Expected diagnostic name".to_string(),
            link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#diagnostic-suppression".to_string(),
            location: @error,
        });
    }
    | percent error semicolon {
        data.error_recovered.push( RecoveredError {
            message: "Expected directive, e.g. %token, %start, ...".to_string(),
            link: "https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#syntax".to_string(),
            location: @error,
        });
    }
    ;



GrammarLine : Rule { data.rules.push(Rule); }
    | Directive
    ;

Grammar: GrammarLine+;
