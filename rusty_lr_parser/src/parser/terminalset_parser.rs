use crate::terminalset::TerminalSet;
use crate::terminalset::TerminalSetItem;
use crate::parser::lexer::Lexed;

%%
%moduleprefix ::rusty_lr_core;

%tokentype Lexed;
%token ident Lexed::Ident(None);
%token caret Lexed::Caret(None);
%token minus Lexed::Minus(None);

%eof Lexed::Eof;
%start TerminalSet;

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

TerminalSet(TerminalSet): caret? TerminalSetItem+ {
    TerminalSet {
      negate: caret.is_some(),
      items: TerminalSetItem,
    }
}
;