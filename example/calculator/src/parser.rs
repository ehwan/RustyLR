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

%%

// this define struct `EParser`
// where 'E' is the start symbol

// type of userdata
%userdata i32;
// type of token ( as Terminal symbol )
%tokentype Token;

// start symbol
%start E;
// eof symbol; for augmented rule generation
%eof Token::Eof;

// error type
%err String;

// define tokens
%token num Token::Num(0); // `num` maps to `Token::Num(0)`
%token plus Token::Plus;
%token star Token::Star;
%token lparen Token::LParen;
%token rparen Token::RParen;

// resolving shift/reduce conflict
%left plus;
%left star;

// data that each token holds can be accessed by its name
// s is slice of shifted terminal symbols captured by current rule
// userdata can be accessed by `data` ( &mut i32, for this situation )
A(i32) : A plus a2=A {
        println!("{:?} {:?} {:?}", A, plus, a2 );
        //                         ^    ^    ^
        //                         |    |    |- value of 2nd 'A'
        //                         |    |- Token
        //                         |- value of 1st 'A'
        *data += 1;
        A + a2 // --> this will be new value of current 'A'
    //  ^    ^
    //  |    |- value of 2nd 'A'
    //  |- value of 1st 'A'
    }
    | M
    ;

M(i32) : M star m2=M { M * m2 }
    | P
    ;

P(i32) : num {
    if let Token::Num(n) = num { n }
    else { return Err(format!("{:?}", num)); }
    //            ^^^^^^^^^^^^^^^^^^^^^^^^^^
    //             reduce action returns Result<(), String>
}
    | lparen E rparen { E }
    ;

E(i32) : A;
