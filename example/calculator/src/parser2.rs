#![allow(unused_imports)]

use rusty_lr_derive::lalr1;
use rusty_lr_derive::lalr1_str;
use rusty_lr_derive::lr1_str;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Token {
    Num,
    Plus,
    Star,
    LParen,
    RParen,
    Eof,
}

lalr1_str! {
    %start Tokens;
    %aug Augmented;

    Digit: '0' %right
         | '1' %right
         | '2' %right
         | '3' %right
         | '4' %right
         | '5' %right
         | '6' %right
         | '7' %right
         | '8' %right
         | '9' %right
         ;

    Num: Digit Num %right
       | Digit %right
       ;

    WS: " " WS %right
      | " " %right
      ;

    WS0: WS
       |
       ;

    Tokens: WS0 Token Tokens
          | WS0 Token
          ;

    Token: Num
         | '+'
         | '*'
         | '('
         | ')'
         ;

    Augmented: Tokens WS0 '\0';
}

lalr1! {
    %tokentype Token;
    %start E;
    %aug Augmented;

    %token num Token::Num;
    %token plus Token::Plus;
    %token star Token::Star;
    %token lparen Token::LParen;
    %token rparen Token::RParen;
    %token eof Token::Eof;

    A : A plus A %left
      | M
      ;

    M : M star M %left
      | P
      ;

    P : num
      | lparen E rparen
      ;

    E : A ;

    Augmented : E eof;
}
