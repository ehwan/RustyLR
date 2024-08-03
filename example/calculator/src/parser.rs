#![allow(unused_imports)]

use rusty_lr::lalr1;
use rusty_lr::lr1;

#[derive(Debug, Clone, Copy)]
pub enum Token {
    Num(i32),
    Plus,
    Star,
    LParen,
    RParen,
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
            Token::Eof => 5,
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

// this define struct `EParser`
// where 'E' is the start symbol
lalr1! {
    // type of userdata
    %userdata i32;
    // type of token ( as Terminal symbol )
    %tokentype Token;

    // start symbol
    %start E;
    // eof symbol; for augmented rule generation
    %eof Token::Eof;

    // define tokens
    %token num Token::Num(0); // `num` maps to `Token::Num(0)`
    %token plus Token::Plus;
    %token star Token::Star;
    %token lparen Token::LParen;
    %token rparen Token::RParen;

    // resolving shift/reduce conflict
    %left plus;
    %left star;

    // s{N} is slice of shifted terminal symbols captured by N'th symbol
    // v{N} is value of N'th symbol ( if it has value )
    // s is slice of shifted terminal symbols captured by current rule
    // userdata canbe accessed by `data` ( &mut i32, for current situation )
    A(i32) : A plus A {
            println!("{:?} {:?} {:?}", s0, s1, s2 );
            //                         ^   ^   ^
            //                         |   |   |- slice of 2nd 'A'
            //                         |   |- slice of 'plus'
            //                         |- slice of 1st 'A'
            println!( "{:?}", s );
            *data += 1;
            v0 + v2 // --> this will be new value of current 'A'
        //  ^    ^
        //  |    |- value of 2nd 'A'
        //  |- value of 1st 'A'
        }
      | M { v0 }
      ;

    M(i32) : M star M { v0 * v2 }
      | P { v0 }
      ;

    P(i32) : num {
        if let Token::Num(n) = v0 { *n }
        else { return Err(format!("{:?}", s0)); }
        //            ^^^^^^^^^ reduce action returns Result<(), String>
    }
      | lparen E rparen { v1 }
      ;

    E(i32) : A  { v0 };
}
