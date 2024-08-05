use rusty_lr::lr1;

lr1! {
    %tokentype u8;
    %start E;
    %eof b'\0';

    %token zero b'0';
    %token one b'1';
    %token two b'2';
    %token three b'3';
    %token four b'4';
    %token five b'5';
    %token six b'6';
    %token seven b'7';
    %token eight b'8';
    %token nine b'9';
    %token plus b'+';
    %token star b'*';
    %token lparen b'(';
    %token rparen b')';
    %token space b' ';

    %left plus;
    %left star;

    WS : space | space WS;
    WS0: WS | ;

    Digit: zero | one | two | three | four | five | six | seven | eight | nine;
    Digits: Digit | Digit Digits;

    Number(i32): WS0 Digits WS0 { std::str::from_utf8(Digits.slice).unwrap().parse().unwrap() };

    A(f32): A plus a2=A {
        println!( "{:?} {:?} {:?}", A.slice, *plus, a2.slice );
        *A + *a2
    }
    | M { *M }
    ;

    M(f32): M star m2=M { *M * *m2 }
    | P { *P }
    ;

    P(f32): Number { *Number as f32 }
    | WS0 lparen E rparen WS0 { *E }
    ;

    E(f32) : A { *A };

}
