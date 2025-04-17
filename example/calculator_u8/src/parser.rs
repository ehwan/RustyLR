use rusty_lr::lr1;

lr1! {
    %userdata i32;
    %tokentype char;
    %start E;
    %eof '\0';

    %left '+';
    %left '*';

    WS0: ' '*;

    Digit(char): ['6'-'9'] | "0" {'0'} | '1' | '2' | '3' | '4' | '5';

    Number(i32): WS0 Digit+ WS0 { Digit.into_iter().collect::<String>().parse().unwrap() };

    A(f32): A '+' a2=A {
        *data += 1; // access userdata by `data`
        println!( "{:?} {:?}", A, a2 );
        A + a2
    }
    | M
    ;

    M(f32): M '*' m2=M { M * m2 }
    | P
    ;

    P(f32): Number { Number as f32 }
    | WS0 '(' E ')' WS0 { E }
    ;

    E(f32) : A ;

}
