use rusty_lr_derive::lr1_str;

// this define struct `EParser`
lr1_str! {
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

    // start symbol ( for final reduction )
    %start E;

    // set augmented production rule
    %augmented Augmented;

    // v{N} is the value of the N-th symbol in the production rule
    // s{N} is the &str(or &[Term]) of the N-th symbol
    A(i32): A add A %left { println!("{:?}+{:?}={:?}", s0, s2, s); v0 + v2 }
    //              |||||       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ reduce action
    //              ^^^^^ left reduction
          | M { v0 }
          ;
    M(i32): M mul M %left { v0 * v2 }
          | P { v0 }
          ;
    P(i32): Num { s0.parse().unwrap() }
          | lparen E rparen { v1 }
          ;
    Num: Digit Num
       | Digit
       ;
    Digit : zero | one | two | three | four | five | six | seven | eight | nine
          ;
    E(i32): A { v0 }
          ;
    Augmented(i32) : E eof { v0 }
                   ;
}
