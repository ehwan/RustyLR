#![allow(unused_imports)]

use rusty_lr_derive::lalr1;
use rusty_lr_derive::lalr1_str;
use rusty_lr_derive::lr1_str;

#[derive(Debug, Clone, Copy)]
pub enum Token {
    Num(i32),
    Plus,
    Star,
    LParen,
    RParen,
    Ignore,
    Eof,
}
impl Token {
    pub fn enum_index(&self) -> usize {
        match self {
            Token::Num(_) => 0,
            Token::Plus => 1,
            Token::Star => 2,
            Token::LParen => 3,
            Token::RParen => 4,
            Token::Ignore => 5,
            Token::Eof => 6,
        }
    }
}
impl std::hash::Hash for Token {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.enum_index().hash(state);
    }
}
impl std::cmp::PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        self.enum_index() == other.enum_index()
    }
}
impl std::cmp::Eq for Token {}
impl PartialOrd for Token {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.enum_index().partial_cmp(&other.enum_index())
    }
}
impl Ord for Token {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.enum_index().cmp(&other.enum_index())
    }
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

    Tokens(Vec<Token>): Token Tokens { if v0 != Token::Ignore { v1.push(v0);} v1 }
                      | Token { if v0 == Token::Ignore { vec![] } else { vec![v0] } }
                      ;

    Token(Token): Num { Token::Num(s0.parse().unwrap()) }
                | '+' { Token::Plus }
                | '*' { Token::Star }
                | '(' { Token::LParen }
                | ')' { Token::RParen }
                | WS  { Token::Ignore }
                ;

    Augmented: Tokens '\0';
}

lalr1! {
    %tokentype Token;
    %start E;
    %aug Augmented;

    %token num Token::Num(0);
    %token plus Token::Plus;
    %token star Token::Star;
    %token lparen Token::LParen;
    %token rparen Token::RParen;
    %token eof Token::Eof;

    A(i32) : A plus A %left { v0 + v2 }
      | M { v0 }
      ;

    M(i32) : M star M %left { v0 * v2 }
      | P { v0 }
      ;

    P(i32) : num { if let Token::Num(n) = v0 { *n } else { unreachable!(); } }
      | lparen E rparen { v1 }
      ;

    E(i32) : A  { v0 } ;

    Augmented : E eof;
}
