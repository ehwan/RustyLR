#![allow(unused_imports)]

use rusty_lr_derive::lalr1_str;
use rusty_lr_derive::lr1_str;

// this define struct `EParser`
// where 'E' is the start symbol
lalr1_str! {
    // define type of user data
    %userdata i32;

    // start symbol ( for final reduction )
    %start E;

    // set augmented production rule
    %augmented Augmented;

    // v{N} is the value of the N-th symbol in the production rule
    // s{N} is the &str(or &[Term]) of the N-th symbol
    // (%left|%reduce|%right|%shift) to resolve shift/reduce conflict
    // reduce action must be evaluated into type (`i32` in this case) you provided
    A(i32): A r"+" A %left { println!("A: {:?}+{:?}={:?}", s0, s2, s); v0 + v2 }
          | M { v0 }
          ;
    M(i32): M '*' M %left { v0 * v2 }
          | P { v0 }
          ;
    P(i32): Num { v0 }
          | WS '(' E ')' WS { v2 }
          ;
    Num(i32): WS Num0 WS { *data += 1; s1.parse().unwrap() }; // user data can be accessed by `data`
    Num0: Digit Num0
       | Digit
       ;
    Digit : '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
          ;
    E(i32): A { v0 }
          ;

    WS1: " " WS1
      | b" "
      ;
    WS: WS1
      |
      ;
    Augmented : E '\0'
              ;
}
