# Syntax
## Quick Reference
 - [Production rules](#production-rules)
 - [Regex pattern](#regex-pattern)
 - [RuleType](#ruletype-optional)
 - [ReduceAction](#reduceaction-optional)
 - [Accessing token data in ReduceAction](#accessing-token-data-in-reduceaction)
 - [Exclamation mark `!`](#exclamation-mark-)
 - [`%tokentype`](#token-type-must-defined)
 - [`%token`](#token-definition-must-defined)
 - [`%start`](#start-symbol-must-defined)
 - [`%eof`](#eof-symbol-must-defined)
 - [`%userdata`](#userdata-type-optional)
 - [`%left`, `%right`](#reduce-type-optional)
 - [`%err`, `%error`](#error-type-optional)
 - [`%glr`](#glr-parser-generation)
 - [`%lalr`](#lalr-parser-generation)


## Overview
RustyLR's grammar syntax is inspired by parser generators like Yacc and Bison.
Grammars are defined using a combination of directives, token definitions, and production rules.

In procedural macros, the grammar is defined using the `lr1!` macro.
In build script files, the grammar section is separated from Rust code using `%%`. Everything before `%%` is treated as regular Rust code and is copied as-is to the generated output.


## Production rules
Each production rule defines how a non-terminal symbol can be derived from a sequence of patterns.
```
NonTerminalName
    : Pattern1 Pattern2 ... PatternN { ReduceAction }
    | Pattern1 Pattern2 ... PatternN { ReduceAction }
   ...
    ;
```
 - **NonTerminalName:** The name of the non-terminal symbol being defined.​
 - **PatternX:** A terminal or non-terminal symbol, or a pattern as defined below.​
 - **ReduceAction:** Optional Rust code executed when the rule is reduced.​

## Patterns
Patterns define the structure of the input that matches a production rule.

 - `name` : Non-terminal or terminal symbol `name` defined in the grammar.
 - `[term1 term_start-term_last]`, `[^term1 term_start-term_last]` : Set of terminal symbols. [`eof`](#eof-symbol-must-defined) will be automatically removed from the terminal set.
 - `P*` : Zero or more repetition of `P`.
 - `P+` : One or more repetition of `P`.
 - `P?` : Zero or one repetition of `P`.
 - `(P1 P2 P3)` : Grouping of patterns.
 - `'a'` or `b'a'`: Single character literal or byte literal. This is only supported if the `%tokentype` is `char` or `u8`.
 - `"abcd"` or `b"abcd"`: String literal or byte string literal. This is only supported if the `%tokentype` is `char` or `u8`.

Note: When using range patterns like [first-last],
the range is determined by the order of %token directives,
not by the actual values of the tokens.

If you define tokens in the following order:
```
%token one '1';
%token two '2';
...
%token zero '0';
%token nine '9';
```
The range `[zero-nine]` will be `['0', '9']`, not `['0'-'9']`.



## RuleType <sub><sup>(optional)</sup></sub>
Assigning a type to a non-terminal allows the parser to carry semantic information.
```
E(MyType): ... ;
```
- `MyType`: The Rust type associated with the non-terminal E.

The actual value of E is evaluated by the result of the ReduceAction.


## ReduceAction <sub><sup>(optional)</sup></sub>
A ReduceAction is Rust code executed when a production rule is reduced.​

- If a `RuleType` is defined, the ReduceAction must evaluate to that type.​
- If no `RuleType` is defined and only one token holds a value, the ReduceAction can be omitted.​
- Reduce action can return Result<(), ErrorType> to handle errors during parsing.
- Reduce action can be written in Rust code. It is executed when the rule is matched and reduced.

```rust
%err String;

E(i32): A div a2=A {
    if a2 == 0 {
        return Err("Division by zero".to_string());
    }
    A / a2 // new value of E
};
```

## Accessing token data in ReduceAction
Within a ReduceAction, you can access the data associated with tokens and non-terminals:

- **Named Patterns:** Assign names to patterns to access their values.
```rust
E(i32): left=A '+' right=A { left + right };
```
- Or using their default names if obvious.
```rust
E(i32): A '+' right=A { A + right }; // use A directly
```
- **User Data:** Access mutable user-defined data passed to the parser.
```rust
E(i32): A '+' right=A { 
    *data += 1; // data: &mut UserData
    A + right 
};
```
- **Lookahead Token:** Inspect the next token without consuming it.
```rust
match *lookahead { // lookahead: &TerminalType
    '+' => { /* ... */ },
    _ => { /* ... */ },
}
```
- Shift Control: Control whether to perform a shift operation. (for GLR parser)
```rust
*shift = false; // Prevent shift action
```

For some regex pattern, the type of variable will be modified as follows:
 - `P*` : `Vec<P>`
 - `P+` : `Vec<P>`
 - `P?` : `Option<P>`

You can still access the `Vec` or `Option` by using the base name of the pattern.
```rust
E(i32) : A* {
    println!( "Value of A: {:?}", A ); // Vec<A>
};
```

For terminal set `[term1 term_start-term_end]`, `[^term1 term_start-term_end]`, there is no predefined variable name. You must explicitly define the variable name.
```rust
E: digit=[zero-nine] {
    println!( "Value of digit: {:?}", digit ); // %tokentype
};
```

For group `(P1 P2 P3)`:
 - If none of the patterns hold value, the group itself will not hold any value.
 - If only one of the patterns holds value, the group will hold the value of the very pattern. And the variable name will be same as the pattern.
 (i.e. If `P1` holds value, and others don't, then `(P1 P2 P3)` will hold the value of `P1`, and can be accessed via name `P1`)
 - If there are multiple patterns holding value, the group will hold `Tuple` of the values. There is no default variable name for the group, you must define the variable name explicitly by `=` operator.

 ```rust
 NoRuleType: ... ;

 I(i32): ... ;

 // I will be chosen
 A: (NoRuleType I NoRuleType) {
     println!( "Value of I: {:?}", I ); // can access by 'I'
     I
 };

 // ( i32, i32 )
 B: i2=( I NoRuleType I ) {
     println!( "Value of I: {:?}", i2 ); // must explicitly define the variable name
 };

 ```

## Exclamation mark `!`
An exclamation mark `!` can be used right after the token to ignore the value of the token.
The token will be treated as if it is not holding any value.

```rust
A(i32) : ... ;

// A in the middle will be chosen, since other A's are ignored
E(i32) : A! A A!;
```


## Token type <sub><sup>(must defined)</sup></sub>
```
%tokentype <RustType> ;
```
Define the type of terminal symbols.
`<RustType>` must be accessible at the point where the macro is called.

```rust
enum MyTokenType<Generic> {
    Digit,
    Ident,
    ...
    VariantWithGeneric<Generic>
}

lr! {
...
%tokentype MyTokenType<i32>;
}
```


## Token definition <sub><sup>(must defined)</sup></sub>
```
%token name <RustExpr> ;
```
Map terminal symbol `name` to the actual value `<RustExpr>`.
`<RustExpr>` must be accessible at the point where the macro is called.

```rust
%tokentype MyToken;

%token zero MyToken::Zero;
%token one MyToken::One;

...

// 'zero' and 'one' will be replaced by b'0' and b'1' respectively
E: zero one;
```
Note: If `%tokentype` is either `char` or `u8`, you can't use this directive. You must use literal value in the grammar directly.

## Start symbol <sub><sup>(must defined)</sup></sub>
```
%start NonTerminalName ;
```
Set the start symbol of the grammar as `NonTerminalName`.

```rust
%start E;
// this internally generate augmented rule <Augmented> -> E eof

E: ... ;
```



## Eof symbol <sub><sup>(must defined)</sup></sub>
```
%eof <RustExpr> ;
```
Define the `eof` terminal symbol.
`<RustExpr>` must be accessible at the point where the macro is called.
'eof' terminal symbol will be automatically added to the grammar.


```rust
%eof b'\0';
// you can access eof terminal symbol by 'eof' in the grammar
// without %token eof ...;
```


## Userdata type <sub><sup>(optional)</sup></sub>
```
%userdata <RustType> ;
```
Define the type of userdata passed to `feed()` function.


```rust
struct MyUserData { ... }

...

%userdata MyUserData;

...

fn main() {
    ...
    let mut userdata = MyUserData { ... };
    parser.feed( ..., token, &mut userdata); // <-- userdata feed here
}
```



## Reduce type <sub><sup>(optional)</sup></sub>
```
// reduce first
%left term1 ;
%left [term1 term_start-term_last] ;

// shift first
%right term1 ;
%right [term1 term_start-term_last] ;
```
Set the shift/reduce precedence for terminal symbols.
`%left` can be abbreviated as `%reduce` or `%l`, and `%right` can be abbreviated as `%shift` or `%r`.

```rust
// reduce first for token 'plus'
%left '+';

// shift first for token 'hat'
%right '^';
```



## Error type <sub><sup>(optional)</sup></sub>
```
%err <RustType> ;
%error <RustType> ;
```
Define the type of `Err` variant in `Result<(), Err>` returned from [`ReduceAction`](#reduceaction-optional). If not defined, `DefaultReduceActionError` will be used.


```rust
enum MyErrorType<T> {
    ErrVar1,
    ErrVar2,
    ErrVar3(T),
}

...


%err MyErrorType<GenericType> ;

...

match parser.feed( ... ) {
    Ok(_) => {}
    Err(err) => {
        match err {
            ParseError::ReduceAction( err ) => {
                // do something with err
            }
            _ => {}
        }
    }
}
```


## LALR parser generation
```
%lalr;
```
Switch generated parser table to LALR parser.



## GLR parser generation
```
%glr;
```
Swith to GLR parser generation.

If you want to generate GLR parser, add `%glr;` directive in the grammar.
With this directive, any Shift/Reduce, Reduce/Reduce conflicts will not be treated as errors.

See [GLR Parser](#glr-parser) section for more details.