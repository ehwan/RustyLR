use rusty_lr::lr1;

lr1! {
    %err String;
    %glr;
    %tokentype char;
    %start E;

    WS0: ' '*;

    Digit(char): ch=['0'-'9'] { ch };

    Number(i32): WS0 Digit+ WS0 { Digit.into_iter().collect::<String>().parse().unwrap() };

    E(i32): E '+' e2=E {
        match lookahead.to_term() {
            Some('*') => {
                return Err("".to_string());
            }
            _ => {
                *shift = false;
                E + e2
            }
        }
    }
    | E '*' e2=E {
        *shift = false;
        E * e2
    }
    | Number
    ;
}
