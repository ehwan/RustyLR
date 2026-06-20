%%

%userdata i32;
%tokentype char;
%start E;

%left '+';
%left '*';
%precedence UMINUS;

WS0: ' '*;

Digit(_): ['6'-'9'] | "0" {'0'} | '1' | '2' | '3' | '4' | '5';

Number(i32): WS0 Digit+ WS0 { Digit.into_iter().collect::<String>().parse().unwrap() };

P(f32): Number { Number as f32 }
| WS0 '(' E ')' WS0 { E }
;

E(_) : E '+' e2=E {
    *data += 1; // access userdata by `data`
    println!( "{:?} '+' {:?}", E, e2 );
    E + e2
}
| E '*' e2=E {
    *data += 1; // access userdata by `data`
    println!( "{:?} '*' {:?}", E, e2 );
    E * e2
}
| WS0 '-' E %prec UMINUS {
    -E
}
| P
;
