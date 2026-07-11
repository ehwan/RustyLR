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

## Advanced Dynamic Ambiguity Resolution

RustyLR allows you to resolve grammar conflicts and prune parsing branches dynamically within your reduce actions. This is an advanced GLR escape hatch; for ordinary operator precedence, prefer `%left`, `%right`, `%precedence`, and `%prec`. The reserved `error` terminal can participate in precedence declarations when recovery productions need deterministic conflict resolution.

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

Initialize the generated context with initial user data (or `with_default_userdata()` when the user data type implements `Default`), and feed your terminal symbols (tokens) to it. The GLR parser shares a similar interface to the deterministic parser: `accept()` mutably borrows the context and returns the first successful `(parse_result, userdata)` pair, while `accept_all()` returns an iterator over all successful pairs with the start type already extracted. Successful acceptance moves branch results out and consumes the context; later feeds or accepts return a consumed-context error. You can feed terminal symbols (tokens) using either `feed` (basic) or `feed_location` (location-aware).

In GLR mode, user data is branch-local. When the parser forks because of a conflict, the current semantic stack and user data are cloned, and each resulting branch owns and mutates its own `UserData` value independently. Token values, semantic return values, and user data must therefore implement `Clone` for GLR branch-local parsing. Results returned from `accept_all()` include the final user data for each successful branch.

`NoAction` is the only feed failure that means the CFG cannot consume the lookahead terminal. `ReduceAction` means the terminal was grammatically feedable, but a runtime semantic action failed. GLR recovery is entered only when every active branch fails with `NoAction`; if at least one branch survives, the feed succeeds and any sibling branch failures are returned in `FeedSuccess::errors`.

GLR parse errors keep branch diagnostics grouped. `ParseError::branch_errors` is a list of `ParseErrorBranch` values; each value represents one failed nondeterministic branch for the current lookahead and preserves that branch's parser state, branch-local user data, and reduce-action error when the failure was semantic. When at least one branch survives, the context remains usable and the failed branch user data is available only through the reported branch error. When no branch survives, branches that only saw `NoAction` are restored for reuse and branches that failed with `ReduceAction` are consumed. Later feeds or accepts report a consumed-context error only if no reusable branch remains.

Branch reuse follows the same rule for `feed()`, `feed_location()`, `accept()`, and `accept_all()`:

- A branch that fails with `NoAction` has not run a semantic action for the lookahead, so its stack and user data are unchanged. If no branch succeeds, that branch is restored into the context and can be fed again.
- A branch that fails with `ReduceAction` may have partially mutated semantic state. That branch is consumed, and its user data is moved into `ParseErrorBranch::ReduceAction`.
- If at least one branch succeeds, the successful branches become the next active context. Failed sibling branches are pruned and reported through `FeedSuccess::errors`.
- If no branch succeeds but at least one `NoAction` branch remains reusable, the call returns `Err(ParseError)` while keeping those reusable branches active for the next call.
- If every branch has been consumed, the context itself is consumed. Later `feed()` or `accept()` calls return a consumed-context error.

For example, this ambiguous grammar creates two branches for the same input terminal symbol. Each branch mutates its own cloned `Vec<&'static str>`:

```rust
use rusty_lr::lr1;

lr1! {
    %glr;
    %tokentype char;
    %userdata Vec<&'static str>;
    %start S;

    S(i32): A;
    A(i32): 'a' {
        data.push("left");
        1
    }
    | 'a' {
        data.push("right");
        2
    };
}

let mut context = SContext::new(Vec::new());
context.feed('a').unwrap();

let mut results = context.accept_all().unwrap().collect::<Vec<_>>();
results.sort_by_key(|(value, _)| *value);

assert_eq!(results, vec![
    (1, vec!["left"]),
    (2, vec!["right"]),
]);
```

```rust
// Include the generated parser module
mod parser;

fn main() {
    let mut context = parser::EContext::with_default_userdata();

    let input = vec!['1', '+', '2', '*', '3', '+', '4'];

    // Feed tokens to the GLR parser
    for token in input {
        // basic feeding:
        match context.feed(token) {
            Ok(success) => {
                if let Some(branch_errors) = success.errors {
                    eprintln!("Some GLR branches were pruned: {}", branch_errors);
                }
            }
            Err(e) => {
                eprintln!("Fatal parse error: {}", e);
                return;
            }
        }

        // or location-aware feeding (if %location is configured in the grammar):
        // let span = MySpan { start: ..., end: ... };
        // if let Ok(success) = context.feed_location(token, span) {
        //     if let Some(branch_errors) = success.errors {
        //         eprintln!("Some GLR branches were pruned: {}", branch_errors);
        //     }
        // } else {
        //     eprintln!("Fatal parse error");
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
- **`context.feed(token)`**: Feeds a token into all active parsing stacks and returns `FeedSuccess` when at least one branch survives.
- **`context.feed_location(token, location)`**: Feeds a token with its location span into all active parsing stacks (requires `%location` in the grammar) and returns `FeedSuccess` when at least one branch survives.
- **`FeedSuccess::errors`**: Optional branch-failure information from GLR branches pruned while the feed still succeeded.
- **`ParseError::branch_errors`**: Branch-grouped diagnostics. Each `ParseErrorBranch` is either `NoAction { state, userdata }` or `ReduceAction { state, source, userdata }`.
- **`context.accept()`**: Finalizes parsing (feeding the end-of-file symbol) and returns the first successful `(parse_result, userdata)` pair. Branches that only see `NoAction` remain reusable; success consumes the context.
- **`context.accept_all()`**: Finalizes parsing and returns an iterator over successful `(parse_result, userdata)` pairs from all active branches. Branches that only see `NoAction` remain reusable; success consumes the context.
