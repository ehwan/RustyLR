# Syntax
## Quick Reference
 - [Token type - `%tokentype`](#token-type-must-defined)
 - [Defining tokens - `%token`](#token-definition-must-defined)
 - [`%filter`](#filter-directive)
 - [Production rules](#production-rules)
 - [Regex patterns](#regex-pattern)
 - [RuleType](#ruletype-optional)
 - [ReduceAction](#reduceaction-optional)
 - [Accessing token data in ReduceAction](#accessing-token-data-in-reduceaction)
 - [Exclamation mark `!`](#exclamation-mark-)
 - [Traceable Non-Terminals - `%trace`](#tracing-non-terminals)
 - [Start symbol - `%start`](#start-symbol-must-defined)
 - [User data type - `%userdata`](#userdata-type-optional)
 - [Resolving Conflicts](#resolving-conflicts)
    - [Panic Mode Error Recovery - `error`](#panic-mode-error-recovery)
    - [Shift/Reduce conflicts - `%left`, `%right`, `%precedence`, `%prec`](#operator-precedence)
    - [Reduce/Reduce conflicts - `%dprec`](#rule-priority)
 - [Error variants - `%err`, `%error`](#error-type-optional)
 - [GLR parser - `%glr`](#glr-parser-generation)
 - [LALR parser - `%lalr`](#lalr-parser-generation)
 - [Disable Optimization - `%nooptim`](#no-optimization)
 - [Make dense parser table - `%dense`](#dense-parser-table)
 - [Runtime table calculation - `%runtime`](#runtime-table-calculation)
 - [Location tracking - `%location`](#location-tracking)


## Overview
RustyLR's grammar syntax is inspired by parser generators like Yacc and Bison.
Grammars are defined using a combination of directives, token definitions, and production rules.

In procedural macros, the grammar is defined using the `lr1!` macro.
In build script files, the grammar section is separated from Rust code using `%%`. Everything before `%%` is treated as regular Rust code and is copied as-is to the generated output.

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
%token name <MatchPattern> ;
```
Defines the terminal symbol `name` for further use in the grammar,
and `<MatchPattern>` will be used in the terminal classification `match` statement:
```rust
match terminal_symbol {
    <MatchPattern> => { classification }
    ...
}
```

**Example:**
```rust
%tokentype MyToken;

%token num MyToken::Num(_);
%token plus MyToken::Punct('+');
%token minus MyToken::Punct('-');
...

E: num plus num 
 | num minus num 
 ;
```

**Notes:**
- If `%tokentype` is either `char` or `u8`, you can't use this directive. You must use literal values in the grammar directly.
- This directive is not for defining the *complete token space*. Any token not defined here can also be captured by `[^ term1 ...]`-like negation patterns.


## `%filter` directive
For `%tokentype` that cannot be used in `match` statement directly,
you can define a filter function using the `%filter` directive.
```rust
%filter ::my::filter_fn ;
```
Now the `match` statement will be generated as follows:
```rust
match ::my::filter_fn(terminal_symbol) {
    <MatchPattern> => { classification }
    ...
}
```

The signature of the filter function must be `fn (&Terminal) -> MatchType`.


## Production Rules
Each production rule defines how a non-terminal symbol can be derived from a sequence of patterns.
```
NonTerminalName
    : Pattern1 Pattern2 ... PatternN %prec OpName { ReduceAction }
    | Pattern1 Pattern2 ... PatternN { ReduceAction }
   ...
    ;
```
**Components:**
 - **NonTerminalName:** The name of the non-terminal symbol being defined
 - **PatternX:** A terminal or non-terminal symbol, or a pattern as defined below
 - **ReduceAction:** Optional Rust code executed when the rule is reduced
 - **OpName:** Use this symbol as an operator for this production rule. `OpName` can be defined with `%token` or literal, or any unique identifier just for this rule. See [RuleType](#ruletype-optional) for more details.

## Patterns
Patterns define the structure of the input that matches a production rule.

 - `.` : Any single terminal symbol
 - `name` : Non-terminal or terminal symbol `name` defined in the grammar
 - `[term1 term_start-term_last]`, `[^term1 term_start-term_last]` : Set of terminal symbols.
 - `P*` : Zero or more repetitions of `P`
 - `P+` : One or more repetitions of `P`
 - `P?` : Zero or one repetition of `P`
 - `$sep( P, P_separator, repetition )`: A repetition of `P` separated by `P_separator`. The `repetition` can be `*`, or `+` to indicate zero or more, or one or more repetitions respectively
 - `(P1 P2 P3 | P4 | P5 P6 ...)` : Grouping of patterns
 - `P / term` or `P / [term1 term_start-term_last]`: Pattern `P` followed by lookaheads. Lookaheads will not be consumed
 - `'a'` or `b'a'`: Single character literal or byte literal. This is only supported if the `%tokentype` is `char` or `u8`
 - `"abcd"` or `b"abcd"`: String literal or byte string literal. This is only supported if the `%tokentype` is `char` or `u8`
 - `P - TerminalSet`: `P` must be a subset of terminal symbols. This pattern matches `P` but not any of the terminal symbols in `TerminalSet`

**Important Note about Range Patterns:**
When using range patterns like `[first-last]`, the range is determined by the order of `%token` directives, not by the actual values of the tokens.

**Example:**
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
- `MyType`: The Rust type associated with the non-terminal `E`

The actual value of `E` is evaluated by the result of the ReduceAction.


## ReduceAction <sub><sup>(optional)</sup></sub>
A ReduceAction is Rust code executed when a production rule is reduced.

**Rules:**
- If a `RuleType` is defined, the ReduceAction must evaluate to that type
- If no `RuleType` is defined and only one token holds a value, the ReduceAction can be omitted
- Reduce actions can return `Result<(), ErrorType>` to handle errors during parsing
- Reduce actions can be written in Rust code. They are executed when the rule is matched and reduced

**Example:**
```rust
%err String;

E(i32): A div a2=A {
    if a2 == 0 {
        return Err("Division by zero".to_string());
    }
    A / a2 // new value of E
};
```

## Accessing Token Data in ReduceAction
Within a ReduceAction, you can access the data associated with tokens and non-terminals:

**Named Patterns:** Assign names to patterns to access their values.
```rust
E(i32): left=A '+' right=A { left + right };
```

**Default Names:** Use default names when obvious.
```rust
E(i32): A '+' right=A { A + right }; // use A directly
```
This is also possible for advanced patterns:
```rust
E(i32): A* { A.iter().sum() }; // sum all values in A
```
Here, `A` is a `Vec<A>` and you can access its values directly.

**User Data:** Access mutable user-defined data passed to the parser.
```rust
E(i32): A '+' right=A { 
    *data += 1; // data: &mut UserData
    A + right 
};
```
Here, `data` is `&mut UserData`, which is defined by the `%userdata` directive.

**Lookahead Token:** Inspect the next token without consuming it.
```rust
match *lookahead.to_term().unwrap() { // lookahead: &TerminalType
    '+' => { /* ... */ },
    _ => { /* ... */ },
}
```
Here, `lookahead` is a `&TerminalSymbol<%TokenType>`, it is either a terminal symbol fed by the user or a special token like `error`.

**Shift Control:** Control whether to perform a shift operation (for GLR parser).
```rust
*shift = false; // Prevent shift action
```

### Variable Types for Regex Patterns
For some regex patterns, the type of variable will be modified as follows:
 - `P*` : `Vec<P>`
 - `P+` : `Vec<P>`
 - `P?` : `Option<P>`
 - `$sep(P, P_separator, repetetion)` : `Vec<P>`

You can still access the `Vec` or `Option` by using the base name of the pattern.
```rust
E(i32) : A* {
    println!("Value of A: {:?}", A); // Vec<A>
};
```

### Terminal Sets
For terminal set `[term1 term_start-term_end]`, `[^term1 term_start-term_end]`, there is no predefined variable name. You must explicitly define the variable name.
```rust
E: digit=[zero-nine] {
    println!("Value of digit: {:?}", digit); // %tokentype
};
```

### Pattern Groups
For group `(P1 P2 P3 | P4 P5 | P6)`:
 - For each line `P1 P2 P3`, `P4 P5`, and `P6`, if every line holds a same type of value `T`, the group will hold a `T` value. Else, the group will not hold any value.
 - Rule type for each line will be determined by following rules:
   - If none of the tokens in the line holds a value, the group will not hold any value
   - If only one of the tokens in the line holds a value, the group will hold that value
   - If there are multiple tokens in the line holding value, the group will hold a `Tuple` of those values
 - There is no default variable name for the group, you must define the variable name explicitly with the `=` operator

 ```rust
 NoRuleType: ... ;

 I(i32): ... ;

 // I will be chosen
 A: i=(NoRuleType I NoRuleType | I) {
     println!("Value of I: {:?}", i); // can access by 'i'
     i
 };

// (I I) and A does not match, so it will not hold any value
 B: ( I NoRuleType I | A ) {
 };

 ```

## Exclamation Mark `!`
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
that the current context is trying to parse.

 - Tracing non-terminals will not be automatically removed from the grammar by optimization.

```
%trace NonTerm1 NonTerm2 ...;
```


## Start symbol <sub><sup>(must defined)</sup></sub>
```
%start NonTerminalName ;
```
Set the start symbol of the grammar as `NonTerminalName`.

```rust
%start E;
// This internally generates augmented rule Augmented -> E eof

E: ... ;
```



## Userdata type <sub><sup>(optional)</sup></sub>
```
%userdata <RustType> ;
```
Define the type of user data passed to the `feed()` function.

**Example:**
```rust
struct MyUserData { ... }

...

%userdata MyUserData;

...

fn main() {
    ...
    let mut userdata = MyUserData { ... };
    parser.feed(..., token, &mut userdata); // <-- userdata fed here
}
```


## Resolving Conflicts

### Panic Mode Error Recovery
```
JsonObject: '{' JsonKeyValue* '}'
          | '{' error '}'          { println!("recovering with '}}' at {}", @error); }
          ;
```
The `error` token is a reserved terminal symbol that can be matched with **any zero or more tokens**.
In the above example, if the parser encounters an invalid token while parsing a JSON object, it will enter panic mode and discard all tokens until it finds a closing brace `}`.

**How it works:**
When an invalid token is encountered, the parser enters panic mode and starts discarding symbols from the parsing stack until it finds a point where the special `error` token is allowed by the grammar.
At that point, it shifts the invalid fed token as the `error` token, then tries to complete the rule that contains the `error` token.

**Important notes:**
- The `error` token does not have any value, no associated rule-type
- `error` token does have location data, which can be accessed in the reduce action by `@error`. The location data is merged from the invalid tokens that consist of the `error` token.
- In GLR parsing, the `error` path will be ignored if there are any other valid paths. In other words, it enters panic-mode only if there is no other way to feed the terminal symbol

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

**Conflict Resolution:**
When a conflict occurs, the parser will compare the precedence of the shift terminal and the *operator* in the reduce rule. If both precedences are defined, either the shift or reduce will be chosen based on the precedence of the operator.
 - If the shift terminal has a higher precedence than the reduce operator, the shift will be chosen
 - If the reduce operator has a higher precedence than the shift terminal, the reduce will be chosen
 - If both have the same precedence, the `%left` or `%right` directive will be used to determine the resolving process

The *operator* of the reduce rule is the rightmost terminal symbol in the production rule that has a precedence defined by `%left`, `%right`, or `%precedence` directive. Alternatively, the operator of the reduce rule can be defined explicitly by using the `%prec` directive.

**Examples:**
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

You can also set the precedence of a rule by referencing a non-terminal symbol with `%prec`.
This is useful when an operator is represented by a non-terminal that produces different actual operators.

**Example:**
```rust
%left '+';
%left '*';

E: E op=BinOp E %prec op { ... }
 | ...
 ;

// The precedence of `BinOp` is determined by the production rule that `BinOp` was derived from.
BinOp: '+'
     | '*'
     ;
```
In this example, the `E: E op=BinOp E` rule's precedence is determined by the `BinOp` non-terminal.
When the parser needs to resolve a conflict involving this rule, it will look at what `BinOp` was reduced from.
If `BinOp` was reduced from `+`, the precedence of `BinOp` will be the precedence of `+`, which is defined by `%left '+'`.
otherwise, if `BinOp` was reduced from `*`, the precedence of `BinOp` will be the precedence of `*`, which is defined by `%left '*'`.

If any non-terminal symbol was referenced in the `%prec` directive,
every production rule in that non-terminal must have operator precedence.


### Rule priority
```
E:
    P1 P2 P3 %dprec 2
  | P4 P5 P6 %dprec 1
  ;
```
For reduce/reduce conflicts, rule with the highest priority will be chosen.
The priority is defined by the `%dprec` directive. Default priority is `0`.


## Error type <sub><sup>(optional)</sup></sub>
```
%err <RustType> ;
%error <RustType> ;
```
Define the type of `Err` variant in `Result<(), Err>` returned from [`ReduceAction`](#reduceaction-optional). If not defined, `DefaultReduceActionError` will be used.

**Example:**
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
By default, the parser will be generated as minimal-LR(1) parser.



## GLR parser generation
```
%glr;
```
Switch to GLR parser generation.

If you want to generate a GLR parser, add the `%glr;` directive to your grammar.
With this directive, any Shift/Reduce, Reduce/Reduce conflicts will not be treated as errors.

See [GLR Parser](GLR.md) section for more details.

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
**Be careful:** this could increase memory usage significantly.

## Runtime table calculation
```
%runtime;
```
RustyLR by default generates the parser table at compile time.
This directive will switch the parser to runtime table calculation mode.

Parser tables are generally extremely large, the generated code will be tens of thousands of lines of code,
and most of them are for initializing the parser table.
This directive will switch the parser table to be calculated at runtime,
which will dramatically reduce the size of the generated code, but will increase the runtime overhead.

## Location tracking
```
%location <Typename for location> ;
```
The location type must implement `rusty_lr::Location` trait.

User must explicitly feed the location data to the parser.
```rust
context.feed_location(parser, terminal, user_data, terminal_location);
context.feed(parser, terminal, user_data); // this is equivalent to using `Location::default()` for the location
```

And the location data can be accessed in the reduce action by `@name` syntax.
```rust
Expr: exp1=Expr '+' exp2=Expr {
    println!("Location of exp1: {:?}", @exp1);
    println!("Location of exp2: {:?}", @exp2);
    println!("Location of this expression: {:?}", @$); // @$ is the location of the non-terminal itself
    exp1 + exp2
};
```

Default location type is `rusty_lr::DefaultLocation`. This does not hold any data.