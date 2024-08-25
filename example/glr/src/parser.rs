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

    Digit(char): ch=[zero-nine] { ch };

    E(i32) : E plus e2=E  %1
        {
            if e2_rule == 1
            { return Err("".to_string()); }
            else {
                E + e2
            }
        }
           | E star e2=E  %2 {  E * e2 }
           | Digit %3 { Digit as i32 - '0' as i32 }
           ;

}
