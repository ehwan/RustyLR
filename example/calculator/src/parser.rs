use rusty_lr_derive::lr1_str;

// this define struct `EParser`
// where 'E' is the start symbol
lr1_str! {
    // define type of user data
    %userdata i32;

    // define terminal symbols
    // the TokenStream will be copied to the generated code
    %token add '+';
    %token mul '*';
    %token lparen '(';
    %token rparen ')';
    %token eof '\0';
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
    %token ws ' ';

    // start symbol ( for final reduction )
    %start E;

    // set augmented production rule
    %augmented Augmented;

    // v{N} is the value of the N-th symbol in the production rule
    // s{N} is the &str(or &[Term]) of the N-th symbol
    // (%left|%reduce|%right|%shift) to resolve shift/reduce conflict
    // reduce action must be evaluated into type (`i32` in this case) you provided
    A(i32): A add A %left { println!("{:?}+{:?}={:?}", s0, s2, s); v0 + v2 }
          | M { v0 }
          ;
    M(i32): M mul M %left { v0 * v2 }
          | P { v0 }
          ;
    P(i32): Num { v0 }
          | WS lparen E rparen WS { v2 }
          ;
    Num(i32): WS Num0 WS { *data += 1; s1.parse().unwrap() }; // user data can be accessed by `data`
    Num0: Digit Num0
       | Digit
       ;
    Digit : zero | one | two | three | four | five | six | seven | eight | nine
          ;
    E(i32): A { v0 }
          ;

    WS1: ws WS1
      | ws
      ;
    WS: WS1
      |
      ;
    Augmented : E eof
              ;
}
