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


---


## Production rules
Every production rules have the base form:
```
NonTerminalName
    : Pattern1 Pattern2 ... PatternN { ReduceAction }
    | Pattern1 Pattern2 ... PatternN { ReduceAction }
   ...
    ;
```

Each `Pattern` follows the syntax:
 - `name` : Non-terminal or terminal symbol `name` defined in the grammar.
 - `[term1 term_start-term_last]`, `[^term1 term_start-term_last]` : Set of terminal symbols. [`eof`](#eof-symbol-must-defined) will be automatically removed from the terminal set.
 - `P*` : Zero or more repetition of `P`.
 - `P+` : One or more repetition of `P`.
 - `P?` : Zero or one repetition of `P`.
 - `(P1 P2 P3)` : Grouping of patterns.
 - `P / term`, `P / [term1 term_start-term_last]`, `P / [^term1 term_start-term_last]` :
 Lookaheads; `P` followed by one of given terminal set. Lookaheads are not consumed.

### Notes
When using range pattern `[first-last]`,
the range is constructed by the order of the [`%token`](#token-definition-must-defined) directives,
not by the actual value of the token.
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
You can assign a value for each non-terminal symbol.
In [reduce action](#reduceaction-optional),
you can access the value of each pattern holds,
and can assign new value to current non-terminal symbol.
Please refer to the [ReduceAction](#reduceaction-optional) and [Accessing token data in ReduceAction](#accessing-token-data-in-reduceaction) section below.
At the end of parsing, the value of the start symbol will be the result of the parsing.
By default, terminal symbols hold the value of [`%tokentype`](#token-type-must-defined) passed by `feed()` function.

```rust
struct MyType<T> {
    ...
}
```
```
E(MyType<i32>) : ... Patterns ... { <This will be new value of E> } ;
```



## ReduceAction <sub><sup>(optional)</sup></sub>
Reduce action can be written in Rust code. It is executed when the rule is matched and reduced.

- If [`RuleType`](#ruletype-optional) is defined for current non-terminal symbol, `ReduceAction` itself must be the value of [`RuleType`](#ruletype-optional) (i.e. no semicolon at the end of the statement).

- `ReduceAction` can be omitted if:
  - [`RuleType`](#ruletype-optional) is not defined.
  - Only one token is holding value in the production rule.

- `Result<(),Error>` can be returned from `ReduceAction`.
  - Returned `Error` will be delivered to the caller of `feed()` function.
  - `ErrorType` can be defined by [`%err`](#error-type-optional) or [`%error`](#error-type-optional) directive.


```rust
NoRuleType: ... ;

RuleTypeI32(i32): ... { 0 } ;

// RuleTypeI32 will be chosen
E(i32): NoRuleType NoRuleType RuleTypeI32 NoRuleType;
```

```rust
// set Err variant type to String
%err String;

%token div '/';

E(i32): A div a2=A {
    if a2 == 0 {
        return Err("Division by zero".to_string());
    }

    A / a2
};

A(i32): ... ;
```


## Accessing token data in ReduceAction

**predefined variables** can be used in `ReduceAction`:
 - `data` ( `&mut UserData` ) : userdata passed to the `feed()` function.
 - `lookahead` ( `&Term` ) : lookahead token that caused the reduce action.
 - `shift` ( `&mut bool` ) : revoke the shift action if set to `false`. See [Resolving Ambiguities](#resolving-ambiguities) section.

To access the data of each token, you can directly use the name of the token as a variable.
 - For non-terminal symbols, the type of variable is `RuleType`.
 - For terminal symbols, the type of variable is [`%tokentype`](#token-type-must-defined).
 - If multiple variables are defined with the same name, the variable on the front-most will be used.
 - You can remap the variable name by using `=` operator.

```rust
E(i32) : A plus a2=A {
    println!("Value of A: {:?}", A);
    println!("Value of plus: {:?}", plus);
    println!("Value of a2: {:?}", a2);

    A + a2 // new value of E
};
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
%tokentype u8;

%token zero b'0';
%token one b'1';

...

// 'zero' and 'one' will be replaced by b'0' and b'1' respectively
E: zero one;
```


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
// define tokens
%token plus '+';
%token hat '^';


// reduce first for token 'plus'
%left plus;

// shift first for token 'hat'
%right hat;
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
Switch to LALR parser generation.



## GLR parser generation
```
%glr;
```
Swith to GLR parser generation.

If you want to generate GLR parser, add `%glr;` directive in the grammar.
With this directive, any Shift/Reduce, Reduce/Reduce conflicts will not be treated as errors.

See [GLR Parser](#glr-parser) section for more details.