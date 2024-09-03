use rusty_lr::lr1;

lr1! {
    %err String;
    %glr;
    %tokentype char;
    %start E;
    %eof '\0';

    %token plus '+';
    %token star '*';
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
    %token space ' ';

    WS0: space*;

    Digit(char): ch=[zero-nine] { ch };

    Number(i32): WS0 Digit+ WS0 { Digit.into_iter().collect::<String>().parse().unwrap() };

    E(i32) : E plus e2=E
            {
                match *lookahead {
                    '*' => {
                        return Err("".to_string());
                    }
                    _ => {
                        // *shift = false;
                        E + e2
                    }

                }
            }
           | E star e2=E
            {
                *shift = false;
                E * e2
            }
           | Number
           ;

}
