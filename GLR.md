# GLR Parsing in RustyLR
RustyLR supports Generalized LR (GLR) parsing, enabling it to handle ambiguous or nondeterministic grammars that traditional LR(1) or LALR(1) parsers cannot process.

When a GLR parser encounters a conflict (such as shift/reduce or reduce/reduce),
it forks the current parsing state into multiple branches,
each representing a different possible interpretation of the input.
These branches are processed in parallel, and invalid paths are pruned as parsing progresses.

## Enabling GLR Parsing
To use GLR parsing in RustyLR, include the `%glr;` directive in your grammar definition.
This directive instructs RustyLR to generate a GLR parser,
which can handle ambiguous grammars by exploring multiple parsing paths.

Once the `%glr` directive is added, any conflicts in the grammar will not be reported as errors.
It's important to be aware of points in your grammar where shift/reduce or reduce/reduce conflicts occur, as each divergence increases computational complexity.

**Tip:** If you are using the `rustylr` executable, you can use the `--verbose` option to see any conflicts in the grammar and their divergent paths.

## Example: Ambiguous Grammar

```rust
%glr;
%tokentype char;
%start E;

Digit(char): ['0'-'9'] ;

E(i32): E '+' E { E + E }
      | E '*' E { E * E }
      | Digit { Digit.to_digit(10).unwrap() as i32 };
```

In this grammar, the expression `1 + 2 * 3 + 4` has multiple valid parse trees due to the ambiguity in operator precedence and associativity:
 - `((1 + 2) * 3) + 4`
 - `(1 + (2 * 3)) + 4`
 - `1 + ((2 * 3) + 4)`
 - `1 + (2 * (3 + 4))`
 - `(1 + 2) * (3 + 4)`

The GLR parser will explore all possible parsing paths to construct the parse forest.

## Resolving Ambiguities
RustyLR allows you to resolve ambiguities dynamically within reduce actions.
Simply returning `Err` from a reduce action will prune the current branch of the parse tree.
By inspecting the lookahead token or other context, you can decide whether to proceed with a particular reduction.

For example, to enforce operator precedence (e.g., `*` has higher precedence than `+`), you can modify the reduce actions as follows:

```rust
E : E '+' E {
      match *lookahead.to_term().unwrap() {
          '*' => {
              // Don't reduce if the next token is '*'
              // This prevents:
              // E + E   /   *
              //             ^ lookahead
              // from becoming:  E *  ...
              //                 ^ (E + E)
              return Err("".to_string());
          }
          _ => {
              // Revoke the shift action
              // This prevents:
              // E + E   /  +
              //            ^ lookahead
              // from becoming: E + E +  ...
              // and enforces only the reduce action:
              // E + ...
              // ^ (E + E)
              *shift = false;
          }
      }
      E + E  // Return the result of the addition
}
```

### Predefined Variables in Reduce Actions
- `lookahead: &TerminalSymbol<TokenType>` - refers to the next token in the input stream. either a terminal fed by the user or an special token like `error`
- `shift: &mut bool` - controls whether a shift action should be performed

### Ambiguity Resolution Rules
- Returning `Err` from the reduce action will discard the current parsing path
- Setting `*shift = false;` prevents the parser from performing a shift action, enforcing the desired reduction

## Parsing with the GLR Parser
RustyLR provides a consistent parsing interface for both deterministic and GLR parsers.
After generating the parser, you can feed tokens to the parser context and retrieve the parsing results.

```rust
let parser = EParser::new();        // Create <StartSymbol>Parser instance
let mut context = EContext::new();  // Create <StartSymbol>Context instance

for token in input_sequence {
    match context.feed(&parser, token) {
        Ok(_) => {}
        Err(e) => {
            println!("Parse error: {}", e);
            return;
        }
    }
}

// Retrieve all possible parse results
for result in context.accept().unwrap() {
    println!("Parse result: {:?}", result);
}
```

### Key Components:
- `EParser::new()` - Creates a new parser instance
- `EContext::new()` - Initializes the parsing context
- `context.feed(&parser, token)` - Feeds tokens to the parser
- `context.accept()` - Returns all possible values of the `%start` symbol from every parse path