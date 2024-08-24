use rusty_lr::lr1;

lr1! {
    %glr;
    %tokentype char;
    %start E;
    %eof '\0';

    %left [plus star];

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

    Digit(char): ch=[zero-nine] { println!("Digit: {}", ch); ch };

    E(i32) : E plus e2=E  {  println!("Plus"); E + e2 }
           | E star e2=E  {  println!("Star"); E * e2 }
           | Digit { println!("D2E"); Digit as i32 - '0' as i32 }
           ;

}
