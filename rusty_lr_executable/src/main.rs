use clap::Parser;
use proc_macro2::Spacing;
use proc_macro2::TokenStream;
use proc_macro2::TokenTree;
use quote::TokenStreamExt;

use std::collections::BTreeSet;
use std::fs::read;
use std::fs::write;
use std::process::Command;

mod arg;

// split stream by '%%'
fn split_stream(token_stream: TokenStream) -> Result<(TokenStream, TokenStream), &'static str> {
    // input stream
    let mut token_stream = token_stream.into_iter().peekable();

    // before '%%'
    let mut output_stream = TokenStream::new();

    while let Some(token) = token_stream.next() {
        if let TokenTree::Punct(token) = token {
            if token.as_char() == '%' && token.spacing() == Spacing::Joint {
                if let Some(TokenTree::Punct(next)) = token_stream.peek() {
                    if next.as_char() == '%' && next.spacing() == Spacing::Alone {
                        token_stream.next();
                        let macro_stream: TokenStream = token_stream.collect();
                        return Ok((output_stream, macro_stream));
                    } else {
                        output_stream.append(token);
                    }
                } else {
                    output_stream.append(token);
                }
            } else {
                output_stream.append(token);
            }
        } else {
            output_stream.append(token);
        }
    }

    Err("No '%%' found")
}

fn main() {
    let args = match arg::Args::try_parse() {
        Ok(args) => args,
        Err(e) => {
            eprintln!("{}", e);
            return;
        }
    };

    // read file
    let input_bytes = match read(args.input_file.clone()) {
        Ok(bytes) => bytes,
        Err(e) => {
            eprintln!("Error reading file: {}", e);
            return;
        }
    };
    let str = match String::from_utf8(input_bytes) {
        Ok(str) => str,
        Err(e) => {
            eprintln!("Error reading utf-8: {}", e);
            return;
        }
    };
    // lex with proc-macro2
    let token_stream: TokenStream = match str.parse() {
        Ok(token_stream) => token_stream,
        Err(e) => {
            eprintln!("Error lexing token stream: {}", e);
            return;
        }
    };

    // split stream by '%%'
    let (output_stream, macro_stream) = match split_stream(token_stream) {
        Ok((output_stream, macro_stream)) => (output_stream, macro_stream),
        Err(e) => {
            eprintln!("{}", e);
            return;
        }
    };

    // parse lines
    let grammar = match rusty_lr_parser::grammar::Grammar::parse(macro_stream) {
        Ok(grammar) => grammar,
        Err(e) => {
            println!("{}", e);
            return;
        }
    };

    // expand macro
    let expanded_stream = if args.runtime {
        grammar.emit_runtime(args.lalr)
    } else {
        grammar.emit_compiletime(args.lalr)
    };
    let expanded_stream = match expanded_stream {
        Ok(expanded_stream) => expanded_stream,
        Err(e) => {
            eprintln!("{}", e);
            return;
        }
    };

    // this comments will be printed to the output file
    let mut debug_comments = String::new();
    {
        let parser = match grammar.create_grammar() {
            Ok(mut grammar) => {
                debug_comments.push_str(format!("{:=^80}\n", "Grammar").as_str());
                debug_comments.push_str(format!("{}\n", grammar).as_str());
                if args.lalr {
                    match grammar.build_lalr(rusty_lr_parser::utils::AUGMENTED_NAME.to_string()) {
                        Ok(parser) => parser,
                        Err(e) => {
                            eprintln!("{}", e);
                            return;
                        }
                    }
                } else {
                    match grammar.build(rusty_lr_parser::utils::AUGMENTED_NAME.to_string()) {
                        Ok(parser) => parser,
                        Err(e) => {
                            eprintln!("{}", e);
                            return;
                        }
                    }
                }
            }
            Err(e) => {
                eprintln!("{}", e);
                return;
            }
        };

        for (state_id, state) in parser.states.iter().enumerate() {
            let mut reduce_first = BTreeSet::new();
            let mut shift_first = BTreeSet::new();

            for (shifted_rule_ref, lookaheads) in state.ruleset.rules.iter() {
                if shifted_rule_ref.shifted == parser.rules[shifted_rule_ref.rule].rule.len() {
                    for token in lookaheads.iter() {
                        if state.shift_goto_map_term.contains_key(token) {
                            shift_first.insert(token);
                        }
                    }
                }

                if let Some(rusty_lr_core::Token::Term(token)) = parser.rules[shifted_rule_ref.rule]
                    .rule
                    .get(shifted_rule_ref.shifted)
                {
                    if state.reduce_map.contains_key(token) {
                        reduce_first.insert(token);
                    }
                }
            }

            if !reduce_first.is_empty() || !shift_first.is_empty() {
                debug_comments.push_str(format!("{:=^80}\n", "Shift/Reduce Conflict").as_str());
                debug_comments.push_str(format!("State{}:\n", state_id).as_str());
                for (shifted_rule_ref, lookaheads) in state.ruleset.rules.iter() {
                    let shifted_rule = rusty_lr_core::ShiftedRule {
                        rule: parser.rules[shifted_rule_ref.rule].clone(),
                        shifted: shifted_rule_ref.shifted,
                    };
                    let lookahead_rule = rusty_lr_core::LookaheadRule {
                        rule: shifted_rule,
                        lookaheads: lookaheads.clone(),
                    };
                    debug_comments.push_str(format!("{}\n", lookahead_rule).as_str());
                }
                if !reduce_first.is_empty() {
                    debug_comments.push_str("Tokens for reduce: ");
                    for (id, token) in reduce_first.iter().enumerate() {
                        debug_comments.push_str(format!("{}", token).as_str());
                        if id < reduce_first.len() - 1 {
                            debug_comments.push_str(", ");
                        }
                    }
                    debug_comments.push_str("\n");
                }
                if !shift_first.is_empty() {
                    debug_comments.push_str("Tokens for shift: ");
                    for (id, token) in shift_first.iter().enumerate() {
                        debug_comments.push_str(format!("{}", token).as_str());
                        if id < shift_first.len() - 1 {
                            debug_comments.push_str(", ");
                        }
                    }
                    debug_comments.push_str("\n");
                }
            }
        }
    }
    if args.verbose {
        println!("{}", debug_comments);
    }

    let this_name = env!("CARGO_PKG_NAME");
    let this_version = env!("CARGO_PKG_VERSION");
    let output_string = format!(
        r#"
// This file was generated by {} {}
//
// Input file: {}
// Output file: {}
// {:=^80}
{}
// {:=^80}
/*
{}
*/
// {:=^80}
{}
// {:=^80}
        "#,
        this_name,
        this_version,
        args.input_file,
        args.output_file.clone(),
        "User Codes Begin",
        output_stream,
        "User Codes End",
        debug_comments,
        "Generated Codes Begin",
        expanded_stream,
        "Generated Codes End"
    );
    match write(args.output_file.clone(), output_string) {
        Ok(_) => {}
        Err(e) => {
            eprintln!("Error writing output file: {}", e);
            return;
        }
    }

    if !args.no_format {
        let mut child = Command::new("rustfmt")
            .arg(args.output_file)
            .spawn()
            .expect("Failed to run rustfmt");
        child.wait().expect("Failed to wait on rustfmt");
    }
}
