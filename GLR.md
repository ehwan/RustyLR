# GLR Parsing in RustyLR

RustyLR supports Generalized LR (GLR) parsing, enabling it to process ambiguous or non-deterministic grammars that traditional deterministic LR(1) or LALR(1) parsers cannot handle.

When a GLR parser encounters a conflict (such as a shift/reduce or reduce/reduce conflict), it does not fail. Instead, it forks the current parsing state into multiple parallel branches. Each branch represents a different possible interpretation of the input. These branches are processed concurrently, and invalid paths are pruned dynamically as parsing progresses.

---

## Enabling GLR Parsing

To generate a GLR parser in RustyLR, add the `%glr;` directive to your grammar definition. This directive instructs the parser generator to construct a non-deterministic parsing table and runtime structure capable of maintaining multiple parser stacks.

Once the `%glr;` directive is active, grammar conflicts will not be reported as compiler errors. However, you should still be mindful of conflict points in your grammar, as excessive non-determinism increases memory usage and parsing complexity at runtime.

> [!TIP]
> If you are using the `rustylr` CLI, run it with the `--verbose` flag to inspect any grammar conflicts and trace the resulting non-deterministic paths.

---

## Example: Ambiguous Expression Grammar

Below is a simple grammar illustrating shift/reduce conflicts caused by operator ambiguity:

```rust
%glr;
%tokentype char;
%start E;

Digit(char) : ch=['0'-'9'] { ch };

E(i32)
    : E '+' e2=E   { E + e2 } // Shift/Reduce conflict: + vs + or *
    | E '*' e2=E   { E * e2 } // Shift/Reduce conflict: * vs + or *
    | Digit     { Digit.to_digit(10).unwrap() as i32 }
    ;
```

For the input string `1 + 2 * 3 + 4`, this grammar generates multiple valid parse trees due to the absence of operator precedence and associativity:
- `((1 + 2) * 3) + 4`
- `(1 + (2 * 3)) + 4`
- `1 + ((2 * 3) + 4)`
- `1 + (2 * (3 + 4))`
- `(1 + 2) * (3 + 4)`

A GLR parser will execute all branches concurrently, tracking all possible parse trees in a shared parse forest.

---

## Resolving Ambiguities Dynamically

RustyLR allows you to resolve grammar conflicts and prune parsing branches dynamically within your reduce actions.

### 1. Pruning Paths via `Err`
If a reduce action evaluates to a semantic error and returns `Err`, the parser immediately discards that active parsing branch. By checking the lookahead token or parsing context, you can selectively fail unwanted paths.

### 2. Disabling Shift Actions (`*shift = false;`)
You can control lookahead behavior directly by modifying the mutable `shift` flag (exposed in reduce actions as `shift: &mut bool`). Setting `*shift = false;` instructs the parser not to perform a shift action on the current lookahead token, effectively forcing a reduction and pruning the shift branch.

### Example: Enforcing Precedence via Actions
Here is how you can resolve the operator conflicts in the expression grammar dynamically inside the reduce actions:

```rust
E(i32)
    : E '+' e2=E {
        match lookahead.to_term() {
            Some('*') => {
                // If the next token is '*', return an error to prevent reducing
                // this addition first. This prunes the '+' reduction branch and
                // allows '*' to shift first (multiplying 2 * 3 before adding 1).
                return Err("defer addition for multiplication".to_string());
            }
            _ => {
                // Otherwise, prevent shifting further tokens and force the reduction
                *shift = false;
                E + e2
            }
        }
    }
    | E '*' e2=E {
        // Enforce left-associative reduction for multiplication
        *shift = false;
        E * e2
    }
    | Digit { Digit.to_digit(10).unwrap() as i32 }
    ;
```

### Predefined Variables in Reduce Actions
- **`lookahead`**: A reference of type `&TerminalSymbol<TokenType>` pointing to the next token in the input stream (either a user-supplied terminal or a special symbol like `error`).
- **`shift`**: A mutable reference of type `&mut bool` that controls whether the parser is allowed to shift the next token.

---

## Parsing with the GLR Parser

Initialize the state context with initial user data (or `with_default_userdata()` when the user data type implements `Default`), and feed your tokens to it. The GLR parser shares a similar interface to the deterministic parser: `accept()` returns the first successful `(parse_result, userdata)` pair, while `accept_all()` returns an iterator over all successful pairs. You can feed tokens using either `feed` (basic) or `feed_location` (location-aware).

```rust
// Include the generated parser module
mod parser;

fn main() {
    let mut context = parser::EContext::with_default_userdata();

    let input = vec!['1', '+', '2', '*', '3', '+', '4'];

    // Feed tokens to the GLR parser
    for token in input {
        // basic feeding:
        if let Err(e) = context.feed(token) {
            eprintln!("Fatal parse error: {}", e);
            return;
        }

        // or location-aware feeding (if %location is configured in the grammar):
        // let span = MySpan { start: ..., end: ... };
        // if let Err(e) = context.feed_location(token, span) {
        //     eprintln!("Fatal parse error: {}", e);
        //     return;
        // }
    }

    // Retrieve all valid parse tree results
    match context.accept_all() {
        Ok(results) => {
            for (result, userdata) in results {
                println!("Parse tree result: {:?}, userdata: {:?}", result, userdata);
            }
        }
        Err(e) => {
            eprintln!("Parsing failed: {}", e);
        }
    }
}
```

### Key API Components
- **`EContext::new(userdata)`**: Initializes a new GLR state context with initial user data. Use `EContext::with_default_userdata()` when `UserData: Default`.
- **`context.feed(token)`**: Feeds a token into all active parsing stacks.
- **`context.feed_location(token, location)`**: Feeds a token with its location span into all active parsing stacks (requires `%location` in the grammar).
- **`context.accept()`**: Finalizes parsing (feeding the end-of-file symbol) and returns the first successful `(parse_result, userdata)` pair.
- **`context.accept_all()`**: Finalizes parsing and returns an iterator over successful `(parse_result, userdata)` pairs from all active branches. When the GLR parser branches, each branch receives its own cloned user data.
