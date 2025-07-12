use rusty_lr::lr1;

lr1! {
    %userdata i32;
    %tokentype char;
    %start E;
    %eof '\0';

    %left '+';
    %left '*';
    %precedence UMINUS;

    WS0: ' '*;

    Digit(char): ['6'-'9'] | "0" {'0'} | '1' | '2' | '3' | '4' | '5';

    Number(i32): WS0 Digit+ WS0 { Digit.into_iter().collect::<String>().parse().unwrap() };

    P(f32): Number { Number as f32 }
    | WS0 '(' E ')' WS0 { E }
    ;

    E(f32) : E Op e2=E %prec Op {
        *data += 1; // access userdata by `data`
        println!( "{:?} {:?} {:?}", E, Op, e2 );
        match Op {
            '+' => E + e2,
            '*' => E * e2,
            _ => panic!("Unknown operator: {:?}", Op),
        }
    }
    | WS0 '-' E %prec UMINUS {
        -E
    }
    | P
    ;

    Op(char): '+' | '*' ;
}
