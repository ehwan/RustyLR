use rusty_lr::lr1;

lr1! {
    %userdata i32;
    %tokentype char;
    %start E;
    %eof '\0';

    %token zero '0';
    %token one '1';
    %token two '2';
    %token three '3';
    %token four '4';
    %token five '5';
    %token six '6';
    %token seven '7';
    %token eight '8';
    %token nine '9';
    %token plus '+';
    %token star '*';
    %token lparen '(';
    %token rparen ')';
    %token space ' ';

    %derive Clone, Debug;

    %left plus;
    %left star;

    WS0: space*;

    Digit(char): [zero-nine];

    Number(i32): WS0 Digit+ WS0 { Digit.into_iter().collect::<String>().parse().unwrap() };

    A(f32): A plus a2=A {
        *data += 1; // access userdata by `data`
        println!( "{:?} {:?} {:?}", A, plus, a2 );
        A + a2
    }
    | M
    ;

    M(f32): M star m2=M { M * m2 }
    | P
    ;

    P(f32): Number { Number as f32 }
    | WS0 lparen E rparen WS0 { E }
    ;

    E(f32) : A ;

}
