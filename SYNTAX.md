# Syntax
## Quick Reference
 - [Production rules](#production-rules)
 - [Regex pattern](#regex-pattern)
 - [RuleType](#ruletype-optional)
 - [ReduceAction](#reduceaction-optional)
 - [Accessing token data in ReduceAction](#accessing-token-data-in-reduceaction)
 - [Exclamation mark `!`](#exclamation-mark-)
 - [Tracable Non-Terminals - `%trace`](#tracing-non-terminals)
 - [Token type - `%tokentype`](#token-type-must-defined)
 - [Defining tokens - `%token`](#token-definition-must-defined)
 - [Start symbol - `%start`](#start-symbol-must-defined)
 - [Eof token - `%eof`](#eof-symbol-must-defined)
 - [UserData type - `%userdata`](#userdata-type-optional)
 - [Resolving Conflicts](#resolving-conflicts)
    - [Panic Mode Error Recovery - `error`](#panic-mode-error-recovery)
    - [Shift/Reduce conflicts - `%left`, `%right`, `%precedence`, `%prec`](#operator-precedence)
    - [Reduce/Reduce conflicts - `%dprec`](#rule-priority)
 - [Error variants - `%err`, `%error`](#error-type-optional)
 - [GLR parser - `%glr`](#glr-parser-generation)
 - [LALR parser - `%lalr`](#lalr-parser-generation)
 - [Disable Optimization - `%nooptim`](#no-optimization)
 - [Make dense parser table - `%dense`](#dense-parser-table)


## Overview
RustyLR's grammar syntax is inspired by parser generators like Yacc and Bison.
Grammars are defined using a combination of directives, token definitions, and production rules.

In procedural macros, the grammar is defined using the `lr1!` macro.
In build script files, the grammar section is separated from Rust code using `%%`. Everything before `%%` is treated as regular Rust code and is copied as-is to the generated output.


## Production rules
Each production rule defines how a non-terminal symbol can be derived from a sequence of patterns.
```
NonTerminalName
    : Pattern1 Pattern2 ... PatternN %prec OpName { ReduceAction }
    | Pattern1 Pattern2 ... PatternN { ReduceAction }
   ...
    ;
```
 - **NonTerminalName:** The name of the non-terminal symbol being defined.​
 - **PatternX:** A terminal or non-terminal symbol, or a pattern as defined below.​
 - **ReduceAction:** Optional Rust code executed when the rule is reduced.​
 - **OpName:** Use this symbol as an operator for this production rule. `OpName` could be defined `%token` or literal, or any unique identifier just for this rule. See [ReduceType](#reduce-type-optional) for more details.

## Patterns
Patterns define the structure of the input that matches a production rule.

 - `.` : Any single terminal symbol.
 - `name` : Non-terminal or terminal symbol `name` defined in the grammar.
 - `[term1 term_start-term_last]`, `[^term1 term_start-term_last]` : Set of terminal symbols. [`eof`](#eof-symbol-must-defined) will be automatically removed from the terminal set.
 - `P*` : Zero or more repetition of `P`.
 - `P+` : One or more repetition of `P`.
 - `P?` : Zero or one repetition of `P`.
 - `(P1 P2 P3)` : Grouping of patterns.
 - `P / term` or `P / [term1 term_start-term_last]`: Pattern `P` followed by a lookaheads. lookaheads will not be consumed.
 - `'a'` or `b'a'`: Single character literal or byte literal. This is only supported if the `%tokentype` is `char` or `u8`.
 - `"abcd"` or `b"abcd"`: String literal or byte string literal. This is only supported if the `%tokentype` is `char` or `u8`.
 - `P - TerminalSet`: `P` must be a subset of terminal symbols. This pattern matches `P` but not any of the terminal symbols in `TerminalSet`.

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

If you are using `char` or `u8` as `%tokentype`, you can use the range pattern like this:
```
['0'-'9']
```
This exactly matches the range of characters from '0' to '9'.



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

## Tracing Non-Terminals
Putting non-terminals in `%trace` directive will enable tracing for that non-terminal.
By calling `context.trace(): HashSet<NonTerminals>`, you can get the set of tracing non-terminals
that current context is trying to parse.

 - Tracing non-terminals will not be automatically removed from the grammar by the optimization.

```
%trace NonTerm1, NonTerm2, ...;
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

Note: This directive is not for defining the *Whole Token Space*. Any token not defined here could also be captured by [^ term1 ...]-like negation pattern.

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


## Resolving Conflicts

### Panic Mode Error Recovery
```
JsonObject: '{' JsonKeyValue* '}'
          | '{' error '}'          { println!("recovering with '}}'"); }
          ;
```
The `error` token is a reserved non-terminal symbol that can be matched with **any tokens**.
In the above example, if the parser encounters an invalid token while parsing a JSON object, it will enter panic mode and discard all tokens until it finds a closing brace `}`.

When an invalid token is encountered,
the parser enters panic mode and starts discarding symbols from the parsing stack until it finds a point where the special `error` token is allowed by the grammar.
At that point, it shifts the invalid fed token as the `error` token,
respectively trying to complete the rule that contains the `error` token.

- The `error` token does not have any value, no associated rule-type.
- In the GLR parsing, the `error` path will be ignored if there are any other valid paths. In other words, it enters panic-mode only if there is no other way to feed the terminal symbol.

### Operator Precedence
```
// reduce first
%left term1 term2 term3 ...;

// shift first
%right term4 term5 term6 ... ;

// only precedence
%precedence term7 term8 term9 ... ;
```
For shift/reduce conflicts, the `%left`, `%right`, and `%precedence` directives are used to resolve the conflicts.
These directives define the associativity and precedence of operators.
As in `bison`, the order of precedence is determined by the order in which `%left`, `%right`, or `%precedence` directives appear.

When a conflict occurs, the parser will compare the precedence of the shift terminal and the *operator* in the reduce rule. If both precedences are defined, either the shift or reduce will be chosen based on the precedence of the operator.
 - If the shift terminal has a higher precedence than the reduce operator, the shift will be chosen.
 - If the reduce operator has a higher precedence than the shift terminal, the reduce will be chosen.
 - If both have the same precedence, the `%left` or `%right` directive will be used to determine the resolving process.

The *operator* of the reduce rule is the rightmost terminal symbol in the production rule, that has a precedence defined by `%left`, `%right`, or `%precedence` directive. In other way, the operator of the reduce rule can be defined explicitly by using `%prec` directive.

```rust
// left reduction for binary operator '+'
%left '+';

// right reduction for binary operator '^'
%right '^';
```

```rust
%left '+';
%left '*';
%left UnaryMinus; // << highest priority

E: E '+' E { E + E }
 | E '*' E { E * E }
 | '-' E %prec UnaryMinus { -E } // make operator for this production rule `UnaryMinus`
 ;
```

### Rule priority
```
E:
    P1 P2 P3 %dprec 2
  | P4 P5 P6 %dprec 1
  ;
```
For reduce/reduce conflicts, if every rules those are in conflict have priority set with `%dprec`, the rule with the highest priority will be chosen.


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

## No optimization
```
%nooptim;
```
Disable grammar optimization.

## Dense parser table
```
%dense;
```

Normally, the generated code will use `HashMap` to store the parser table.
This directive will force the parser to use `Vec` instead of `HashMap`.
Be careful, this could increase the size of the memory usage a lot.