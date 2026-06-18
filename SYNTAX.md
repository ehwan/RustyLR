# Syntax

This document provides a comprehensive guide to the grammar definition syntax used by RustyLR. The syntax is heavily inspired by parser generators like *Yacc* and *Bison*, but tailored to integrate seamlessly with the Rust programming language.

---

## Quick Reference

- [Token Type (`%tokentype`)](#token-type-must-defined)
- [Token Definition (`%token`)](#token-definition-must-defined)
- [Filter Directive (`%filter`)](#filter-directive)
- [Production Rules](#production-rules)
- [Patterns](#patterns)
- [RuleType (Non-Terminal Types)](#ruletype-optional)
- [Reduce Actions](#reduceaction-optional)
- [Accessing Data in Reduce Actions](#accessing-token-data-in-reduceaction)
- [Exclamation Mark (`!`) Value Discard](#exclamation-mark-)
- [Tracing Non-Terminals (`%trace`)](#tracing-non-terminals)
- [Start Symbol (`%start`)](#start-symbol-must-defined)
- [Userdata Type (`%userdata`)](#userdata-type-optional)
- [Conflict Resolution](#resolving-conflicts)
  - [Panic-Mode Error Recovery (`error`)](#panic-mode-error-recovery)
  - [Operator Precedence (`%left`, `%right`, `%precedence`, `%prec`)](#operator-precedence)
  - [Rule Priority (`%dprec`)](#rule-priority)
- [Error Type (`%err` / `%error`)](#error-type-optional)
- [LALR(1) Parser Generation (`%lalr`)](#lalr-parser-generation)
- [GLR Parser Generation (`%glr`)](#glr-parser-generation)
- [Disabling Table Optimization (`%nooptim`)](#no-optimization)
- [Dense Parser Tables (`%dense`)](#dense-parser-table)
- [Location Tracking (`%location`)](#location-tracking)
- [Variable Substitution](#variable-substitution)

---

## Overview

RustyLR grammars can be defined in two ways:
1. **Procedural Macros:** Using the `lr1!` macro inline in your Rust code.
2. **Build Scripts:** Using a standalone grammar file (e.g., `src/grammar.rs`) processed by the `rustylr` command-line tool. stand-alone files use the `%%` delimiter to separate Rust helper code (imports, custom enums) from the grammar definition. Everything preceding `%%` is copied as-is into the generated parser file.

---

## Token Type (Must Defined)

```
%tokentype <RustType> ;
```

Defines the Rust type representing the input terminal symbols (tokens). The `<RustType>` must be in scope at the place where the parser is generated.

### Example
```rust
#[derive(Debug, Clone)]
pub enum Token {
    Ident(String),
    Num(i32),
    Plus,
    Minus,
}

// In the grammar section:
%tokentype Token;
```

---

## Token Definition (Must Defined)

```
%token name <MatchPattern> ;
```

Binds a grammar terminal symbol (`name`) to a pattern (`<MatchPattern>`) used to classify tokens. Under the hood, RustyLR generates a `match` statement to identify the token:

```rust
match terminal_token {
    <MatchPattern> => { /* Classified as `name` */ },
    ...
}
```

### Example
```rust
%tokentype Token;

%token id Token::Ident(_);   // Matches Token::Ident("foo")
%token num Token::Num(_);     // Matches Token::Num(42)
%token plus Token::Plus;       // Matches Token::Plus
%token minus Token::Minus;     // Matches Token::Minus

Expr
    : id plus num
    | num minus id
    ;
```

### Notes
- **Literal Tokens:** If `%tokentype` is set to `char` or `u8`, you do not need to define tokens using `%token`. Instead, use character literals (`'+'`, `'a'`) or byte literals (`b'+'`, `b'a'`) directly in the grammar.
- **Incomplete Token Space:** You do not need to exhaustively define every single possible enum variant. Any tokens not explicitly matched can still be captured using negation sets (e.g., `[^ plus minus]`).

---

## %filter Directive

```
%filter <Path> ;
```

When your `%tokentype` cannot be matched directly in a simple Rust `match` pattern (e.g., if it contains generic parameters, references, or requires complex validation), you can define a custom filter function.

When specified, the generated classification `match` will wrap the input terminal in the filter function:

```rust
match filter_fn(terminal_token) {
    <MatchPattern> => { /* ... */ }
    ...
}
```

The filter function signature must be: `fn(&Terminal) -> MatchType` or `fn(Terminal) -> MatchType` depending on your wrapper design.

### Example
```rust
fn my_filter(token: &Token) -> TokenKind {
    match token {
        Token::Ident(_) => TokenKind::Ident,
        Token::Num(_) => TokenKind::Num,
        Token::Plus => TokenKind::Plus,
    }
}

// In the grammar:
%filter my_filter;
```

---

## Production Rules

Production rules define how non-terminal symbols are constructed from sequences of other symbols or pattern groups.

```
NonTerminalName
    : Pattern1 Pattern2 ... PatternN %prec OpName { ReduceAction }
    | Pattern1 Pattern2 ... PatternN { ReduceAction }
    ...
    ;
```

### Components
- **`NonTerminalName`**: The name of the non-terminal being defined.
- **`PatternX`**: A symbol (terminal or non-terminal) or a regex-like pattern (see [Patterns](#patterns)).
- **`ReduceAction`**: An optional block of Rust code executed when the production rule is reduced.
- **`%prec OpName`**: Explicitly assigns the operator precedence of the terminal `OpName` to this rule (see [Operator Precedence](#operator-precedence)).

---

## Patterns

RustyLR supports rich regular expression patterns on the right-hand side of production rules:

- **`.`** : Matches any single terminal symbol.
- **`name`** : Matches the terminal or non-terminal symbol `name`.
- **`[term1 term_start-term_last]`** : Matches any terminal symbol in the specified set.
- **`[^term1 term_start-term_last]`** : Negated set. Matches any terminal symbol *not* in the specified set.
- **`P*`** : Matches zero or more repetitions of pattern `P` (binds as a `Vec<P>`).
- **`P+`** : Matches one or more repetitions of pattern `P` (binds as a `Vec<P>`).
- **`P?`** : Matches zero or one occurrence of pattern `P` (binds as an `Option<P>`).
- **`$sep(P, P_separator, repetition)`** : Matches repetitions of `P` separated by `P_separator`. The `repetition` argument can be `*` (zero or more) or `+` (one or more). Binds as a `Vec<P>`.
- **`(P1 P2 | P3)`** : Grouping and alternation.
- **`P / term`** or **`P / [term1 term2]`** : Lookahead assertion. Matches `P` only if followed by the lookahead symbol(s), without consuming them.
- **`'a'` / `b'a'`** : Character/byte literals (only valid if `%tokentype` is `char` or `u8`).
- **`"abcd"` / `b"abcd"`** : String/byte string literals (only valid if `%tokentype` is `char` or `u8`).
- **`P - TerminalSet`** : Matches pattern `P` but excludes any terminal in the `TerminalSet`.

### Range Patterns (`[first-last]`)
When defining ranges of custom tokens (e.g. `[zero-nine]`), the range ordering is determined by the **declaration order of your `%token` directives**, rather than the inherent values of the enum elements.

#### Example
If tokens are declared as:
```rust
%token zero Token::Num(0);
%token one  Token::Num(1);
%token two  Token::Num(2);
// ...
%token nine Token::Num(9);
```
The range `[zero-two]` matches `zero`, `one`, and `two`.

If the `%tokentype` is `char` or `u8`, literal character ranges like `['0'-'9']` or `[b'0'-b'9']` are resolved using standard ASCII values.

---

## RuleType (Optional)

You can assign a semantic return type to any non-terminal symbol.

```
NonTerminal(RustType) : ... ;
```

- **`RustType`**: The Rust type that this non-terminal evaluates to when reduced.

### Type Inference with `_`
If you do not want to specify types manually, you can use the `_` placeholder. RustyLR will inspect the rule's reduce actions and identity transitions to infer the correct type automatically:

```rust
Expr(_): Term;
```

> [!WARNING]
> If a circular dependency exists (e.g., two rules trying to infer their types from one another without a base type), RustyLR will fail with a compilation error.

### Memory Optimization with `Box`
Internally, the generated parser stores all semantic values (the `%tokentype` and all non-terminals' `RuleType`s) in a single unified `enum` representing the parser's data stack.

Because the memory footprint of a Rust `enum` is dictated by its largest variant, if even one `RuleType` is exceptionally large (e.g., a large AST struct), the size of *every* stack slot will inflate. This can result in significant memory waste and performance degradation.

To avoid this, wrap large AST nodes or structures in a `Box` (e.g., `Box<MyLargeNode>`). This ensures the enum variant only takes up the size of a single pointer, optimizing stack memory usage.

---

## ReduceAction (Optional)

A reduce action is Rust code executed when a rule is matched and reduced.

### General Rules
- If the non-terminal has a `RuleType`, the reduce action block must evaluate to that type.
- If no `RuleType` is defined and only one symbol in the rule has a value, the reduce action can be omitted (it automatically forwards that value).
- Actions can return a `Result<RuleType, ErrorType>` to propagate runtime parser errors.

### Named Variables
Assign names to elements on the right-hand side using the `=` syntax to access their values inside the action block:

```rust
Expr(i32)
    : left=Expr '+' right=Term { left + right }
    ;
```

---

## Accessing Token Data in ReduceAction

Within a `ReduceAction` block, you can access values and metadata from the matched symbols:

### 1. Named Bindings
Explicitly bind a pattern to a variable name:
```rust
Expr(i32) : left=Expr '+' right=Term { left + right };
```

### 2. Default Bindings
If a symbol name is a valid identifier, you can reference it directly without an explicit binding:
```rust
Expr(i32) : Expr '+' Term { Expr + Term };
```

For regex repetitions (e.g., `Term*`), referencing the base name `Term` returns a `Vec<Term>`:
```rust
Sum(i32) : Term* { Term.iter().sum() };
```

### 3. Bison-style Positional Variables
You can access values and locations using index numbers corresponding to the position of the symbol on the RHS (1-indexed):
- **`$1`, `$2`, ...**: The semantic value of the 1st, 2nd, etc. symbol.
- **`@1`, `@2`, ...**: The location/span of the 1st, 2nd, etc. symbol.
- **`@$` (or `@0`)**: The location of the entire reduced non-terminal.

```rust
Expr(i32) : Expr '+' Expr { $1 + $3 };
```

### 4. User Data (`data`)
Access mutable user data passed to the `feed()` function. It is exposed in reduce actions as `data` (of type `&mut UserData`):
```rust
Expr(i32) : Term { 
    *data += 1; // Increment a parse counter
    Term 
};
```

### 5. Lookahead Token (`lookahead`)
You can inspect the lookahead token that triggered the current reduction. It is exposed as `lookahead` (of type `&TerminalSymbol<TokenType>`):
```rust
Expr(i32) : Term {
    if let Some(next_token) = lookahead.to_term() {
        println!("Lookahead is: {:?}", next_token);
    }
    Term
};
```

### 6. Shift Control (`shift`)
For GLR parsing, you can dynamically control whether the parser should perform a shift action or prune the branch by modifying the mutable `shift` boolean:
```rust
*shift = false; // Disable shifting the next token, forcing reduction
```

### Variable Types for Patterns
When matching regex patterns, the bindings yield the following Rust types:
- **`P*`** : `Vec<P>`
- **`P+`** : `Vec<P>`
- **`P?`** : `Option<P>`
- **`$sep(P, Sep, Rep)`** : `Vec<P>`

### Terminal Sets
For terminal sets (like `[plus minus]`), no default variable name is generated. You must assign a variable name to capture the token value:
```rust
Op(Token) : op=[plus minus] { op };
```

### Pattern Groups
For groupings like `(P1 P2 | P3)`:
- If every branch in the group evaluates to the same type `T`, the group yields a value of type `T`. Otherwise, it yields no value.
- The type of a branch is:
  - Empty if no symbol yields a value.
  - `T` if exactly one symbol yields a value.
  - A tuple `(T1, T2, ...)` if multiple symbols yield values.
- Groups have no default variable name. You must explicitly bind them:
  ```rust
  Expr(i32) : val=( Term | '+' Term ) {
      match val {
          // ...
      }
  };
  ```

---

## Exclamation Mark (`!`)

An exclamation mark (`!`) appended immediately after a symbol tells the parser to discard its value. This is useful for omitting unneeded tokens (like punctuation or delimiters) from the default variable scope.

```rust
// Only the middle `Expr` is kept; the parentheses values are discarded
Expr(i32) : '('! Expr ')'! ; 
```

---

## Tracing Non-Terminals

```
%trace NonTerm1 NonTerm2 ... ;
```

Registers non-terminals for tracing. When registered, you can query the active parsing goals at runtime using `context.trace(&parser)`, which returns a `HashSet<NonTerminals>`.

Additionally, tracing prevents the optimization engine from merging or optimizing away these non-terminal states.

---

## Start Symbol (Must Defined)

```
%start NonTerminalName ;
```

Defines the entry point of the grammar. RustyLR automatically creates an augmented rule `Augmented -> NonTerminalName eof`.

---

## Userdata Type (Optional)

```
%userdata <RustType> ;
```

Specifies a custom state type passed to the parser's `feed` and `accept` functions. This allows you to thread compilation state, symbol tables, or diagnostics through your reduce actions.

### Example
```rust
struct ParserState {
    errors: Vec<String>,
}

// In the grammar:
%userdata ParserState;

// In your reduce action:
Expr(i32) : Term {
    if Term < 0 {
        data.errors.push("Negative number encountered".to_string());
    }
    Term
};
```

---

## Resolving Conflicts

### Panic-Mode Error Recovery

```rust
Expr: '{' Stmt* '}'
    | '{' error '}' { println!("Recovered from syntax error at: {:?}", @error); }
    ;
```

The reserved terminal `error` is used to implement panic-mode recovery. When the parser encounters an unexpected token:
1. It pops states off the parser stack until it finds a state that can shift the `error` symbol.
2. It shifts the `error` symbol.
3. It discards input tokens until it encounters a token that can legally follow `error` in the grammar (in this case, `}`).
4. It reduces the recovery rule, letting the parser resume normal execution.

#### Notes
- The `error` token does not carry a semantic value.
- You can access the span of all discarded tokens using the location binder `@error`.
- In GLR mode, the parser will prefer non-error paths and only trigger error recovery if all other active branches fail.

### Operator Precedence

```
%left term1 term2 ... ;
%right term3 term4 ... ;
%precedence term5 term6 ... ;
```

Used to resolve shift/reduce conflicts in expressions. The precedence of terminals is defined by the declaration order of the `%left`, `%right`, and `%precedence` directives (lower to higher).

- **`%left`**: Declares left-associative operators (e.g., `a + b + c` parses as `(a + b) + c`).
- **`%right`**: Declares right-associative operators (e.g., `a ^ b ^ c` parses as `a ^ (b ^ c)`).
- **`%precedence`**: Declares precedence without associativity. Banning chained usage of the operator without parentheses.

When a conflict arises between shifting a terminal and reducing a rule:
1. The parser compares the precedence of the lookahead terminal to the precedence of the rule's *operator*.
2. The rule's operator is the rightmost terminal in the rule that has a precedence assigned.
3. If the lookahead terminal has higher precedence, the parser **shifts**.
4. If the rule operator has higher precedence, the parser **reduces**.
5. If precedences are equal:
   - For `%left`, the parser **reduces**.
   - For `%right`, the parser **shifts**.
   - For `%precedence`, the parser reports a syntax error.

#### Explicit Precedence (`%prec`)
You can override a rule's default operator using the `%prec` directive followed by a terminal name:

```rust
%left '+';
%left '*';
%left UnaryMinus;

Expr
    : Expr '+' Expr
    | Expr '*' Expr
    | '-' Expr %prec UnaryMinus // Gives unary minus higher precedence than '*'
    ;
```

You can also use a non-terminal in `%prec` if the operator is determined dynamically:
```rust
Expr : Expr op=BinOp Expr %prec op { ... };

BinOp : '+' | '*';
```
Here, the rule's precedence matches the specific operator that `BinOp` resolved to.

### Rule Priority

```rust
Expr
    : Expr '+' Expr %dprec 2
    | Expr '*' Expr %dprec 1
    ;
```

Assigns static priority numbers to rules to resolve reduce/reduce conflicts. If two rules can be reduced at the same time, the parser resolves the conflict by choosing the rule with the highest `%dprec` value (default is `0`).

---

## Error Type (Optional)

```
%err <RustType> ;
%error <RustType> ;
```

Defines a custom error type returned by reduce actions. If your reduce action returns a `Result`, returning an `Err(custom_error)` will stop execution (or prune the GLR branch) and bubble the error up wrapped in `ParseError::ReduceAction`.

---

## LALR Parser Generation

```
%lalr;
```

Forces RustyLR to generate LALR(1) parsing tables. By default, RustyLR builds minimal-LR(1) (IELR-style) tables, which provide full LR(1) parsing strength while keeping table sizes close to LALR(1).

---

## GLR Parser Generation

```
%glr;
```

Enables Generalized LR (GLR) parser generation. With `%glr;` enabled, shift/reduce and reduce/reduce conflicts are not treated as compiler errors. Instead, the parser splits into parallel execution branches at runtime.

---

## No Optimization

```
%nooptim;
```

Disables optimization passes on the generated table (which merge states and group equivalent terminals). Use this for debugging or to speed up the parser generation compilation phase itself.

---

## Dense Parser Table

```
%dense;
```

Forces the generated parser table to use flat `Vec` indexes instead of `HashMap` lookups. This significantly speeds up token feeding, but can drastically increase the binary size of the generated parser.

---

## Location Tracking

```
%location <RustType> ;
```

Enables location tracking in the parser. The specified `<RustType>` must implement the `rusty_lr::Location` trait:

```rust
pub trait Location: Default + Clone {
    fn merge(&self, other: &Self) -> Self;
}
```

To feed locations, use `feed_location` instead of `feed`:
```rust
context.feed_location(&parser, token, &mut userdata, span);
```

Within reduce actions, access symbol spans using the `@` prefix:
```rust
Expr(i32) : e1=Expr '+' e2=Expr {
    println!("Start/End position: {:?}", @$); // Location of this Expr
    println!("Left operand position: {:?}", @e1);
    e1 + e2
};
```

---

## Variable Substitution

You can use variables prefixed with `$` inside any RustCode block in the grammar. This includes:
- `%tokentype`
- `%location`
- `%userdata`
- `%errortype` (or `%error`)
- `%moduleprefix`
- `%filter`
- `%token` terminal definitions
- Non-terminal rule types
- Reduce actions

### Supported Variables
- `$tokentype` -> Evaluates to the type defined by `%tokentype`.
- `$location` -> Evaluates to the type defined by `%location` (defaults to `$moduleprefix::DefaultLocation`).
- `$userdata` -> Evaluates to the type defined by `%userdata` (defaults to `()`).
- `$error` or `$errortype` -> Evaluates to the type defined by `%errortype` / `%error` (defaults to `$moduleprefix::DefaultReduceActionError`).
- `$moduleprefix` -> Evaluates to the path defined by `%moduleprefix` (defaults to `::rusty_lr`).
- `$filter` -> Evaluates to the filter expression/function defined by `%filter`.
- `$NonTerminalName` -> Evaluates to the `ruletype` defined for `NonTerminalName`.
- `$terminal_name` -> Evaluates to the match pattern/definition of `<terminal_name>`.

### Substitution Errors
- **Circular Dependency**: If you introduce circular dependencies among type/definition variables, a compile-time error (`CircularDependency`) is returned.
- **Max Depth Exceeded**: A maximum recursion limit of `100` is enforced to prevent infinite compilation loops. If exceeded, a compile-time error (`MaxSubstitutionDepthExceeded`) is returned.
- **Filter Not Defined**: If `$filter` is used inside a reduce action block but `%filter` directive is not defined, a compile-time error (`FilterNotDefined`) is returned.

### Example
```rust
%tokentype Token;
%userdata MyUser;
%error MyError;
%token a Token::A;

Expr($tokentype) : a { $tokentype };
Term($location) : a { $location };
Rule($userdata) : a { $userdata };
```

### Extracting Terminal Values (Syntax Sugar)

When defining a terminal token, you typically wrap it inside an enum variant. For example:
```rust
%tokentype Token;
%token ident Token::Ident(ident);
```

Notice that the variable bound inside the pattern `Token::Ident(ident)` has the exact same name as the terminal symbol `ident`.

Inside a reduce action, you can use variable substitution to easily match and extract this inner value into a local variable without writing out the full boilerplate `let Token::Ident(ident) = ...`:

```rust
Rule(ResultType) : ident {
    let $ident = ident else {
        unreachable!("Expected ident token");
    };
    // Now you can use the extracted `ident` value directly!
    println!("Ident value: {}", ident);
}
```

Because `$ident` expands directly to the terminal's pattern stream (`Token::Ident(ident)`), the line `let $ident = ident else { ... };` automatically expands to:

```rust
let Token::Ident(ident) = ident else {
    unreachable!("Expected ident token");
};
```

This syntax sugar is extremely useful for reducing boilerplate code when processing token streams in your compiler's parser rules.