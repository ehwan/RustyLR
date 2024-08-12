use clap::Parser;

use codespan_reporting::diagnostic::Diagnostic;
use codespan_reporting::diagnostic::Label;
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::ColorChoice;
use codespan_reporting::term::termcolor::StandardStream;

use proc_macro2::Ident;
use proc_macro2::Span;
use proc_macro2::TokenStream;

use rusty_lr_parser::error::ParseError;

use std::collections::BTreeSet;
use std::fs::read;
use std::fs::write;
use std::process::Command;

mod arg;
mod split;
mod utils;

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

    let mut files = SimpleFiles::new();
    let file_id = files.add(args.input_file.clone(), str.clone());

    // lex with proc-macro2
    let token_stream: TokenStream = match str.parse() {
        Ok(token_stream) => token_stream,
        Err(e) => {
            let range = e.span().byte_range();
            let diag = Diagnostic::error()
                .with_message("Lexing error")
                .with_labels(vec![
                    Label::primary(file_id, range).with_message(e.to_string())
                ]);
            let writer = StandardStream::stderr(ColorChoice::Auto);
            let config = codespan_reporting::term::Config::default();
            term::emit(&mut writer.lock(), &config, &files, &diag)
                .expect("Failed to write to stderr");
            return;
        }
    };

    // split stream by '%%'
    let (output_stream, macro_stream) = match split::split_stream(token_stream) {
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
            let diag = match e {
                ParseError::MultipleModulePrefixDefinition(
                    (span1, tokenstream1),
                    (span2, tokenstream2),
                ) => {
                    let range1 = utils::span_stream_range(span1, tokenstream1);
                    let range2 = utils::span_stream_range(span2, tokenstream2);

                    Diagnostic::error()
                        .with_message("Multiple %moduleprefix definition")
                        .with_labels(vec![
                            Label::primary(file_id, range1).with_message("First definition"),
                            Label::primary(file_id, range2).with_message("Other definition"),
                        ])
                        .with_notes(vec![
                            "Only one %moduleprefix definition is allowed".to_string()
                        ])
                }
                ParseError::MultipleUserDataDefinition(
                    (span1, tokenstream1),
                    (span2, tokenstream2),
                ) => {
                    let range1 = utils::span_stream_range(span1, tokenstream1);
                    let range2 = utils::span_stream_range(span2, tokenstream2);

                    Diagnostic::error()
                        .with_message("Multiple %userdata definition")
                        .with_labels(vec![
                            Label::primary(file_id, range1).with_message("First definition"),
                            Label::primary(file_id, range2).with_message("Other definition"),
                        ])
                        .with_notes(vec!["Only one %userdata definition is allowed".to_string()])
                }
                ParseError::MultipleErrorDefinition(
                    (span1, tokenstream1),
                    (span2, tokenstream2),
                ) => {
                    let range1 = utils::span_stream_range(span1, tokenstream1);
                    let range2 = utils::span_stream_range(span2, tokenstream2);

                    Diagnostic::error()
                        .with_message("Multiple %error definition")
                        .with_labels(vec![
                            Label::primary(file_id, range1).with_message("First definition"),
                            Label::primary(file_id, range2).with_message("Other definition"),
                        ])
                        .with_notes(vec!["Only one %error definition is allowed".to_string()])
                }
                ParseError::MultipleTokenTypeDefinition(
                    (span1, tokenstream1),
                    (span2, tokenstream2),
                ) => {
                    let range1 = utils::span_stream_range(span1, tokenstream1);
                    let range2 = utils::span_stream_range(span2, tokenstream2);

                    Diagnostic::error()
                        .with_message("Multiple %tokentype definition")
                        .with_labels(vec![
                            Label::primary(file_id, range1).with_message("First definition"),
                            Label::primary(file_id, range2).with_message("Other definition"),
                        ])
                        .with_notes(vec!["Only one %tokentype definition is allowed".to_string()])
                }
                ParseError::MultipleEofDefinition((span1, tokenstream1), (span2, tokenstream2)) => {
                    let range1 = utils::span_stream_range(span1, tokenstream1);
                    let range2 = utils::span_stream_range(span2, tokenstream2);

                    Diagnostic::error()
                        .with_message("Multiple %eof definition")
                        .with_labels(vec![
                            Label::primary(file_id, range1).with_message("First definition"),
                            Label::primary(file_id, range2).with_message("Other definition"),
                        ])
                        .with_notes(vec!["Only one %eof definition is allowed".to_string()])
                }
                ParseError::MultipleStartDefinition(ident1, ident2) => {
                    let range1 = ident1.span().byte_range();
                    let range2 = ident2.span().byte_range();

                    Diagnostic::error()
                        .with_message("Multiple %start definition")
                        .with_labels(vec![
                            Label::primary(file_id, range1).with_message("First definition"),
                            Label::primary(file_id, range2).with_message("Other definition"),
                        ])
                        .with_notes(vec!["Only one %start definition is allowed".to_string()])
                }
                ParseError::MultipleRuleDefinition(ident1, ident2) => {
                    let range1 = ident1.span().byte_range();
                    let range2 = ident2.span().byte_range();

                    Diagnostic::error()
                        .with_message("Multiple rule definition")
                        .with_labels(vec![
                            Label::primary(file_id, range1).with_message("First definition"),
                            Label::primary(file_id, range2).with_message("Other definition"),
                        ])
                        .with_notes(vec!["Rule name must be unique".to_string()])
                }

                ParseError::MultipleReduceDefinition(ident) => {
                    let range = ident.span().byte_range();

                    Diagnostic::error()
                        .with_message("Multiple reduce definition")
                        .with_labels(vec![Label::primary(file_id, range).with_message(
                            "This terminal symbol is defined as both of %left and %right",
                        )])
                        .with_notes(vec![
                            "Reduce type must be unique, either %left or %right".to_string()
                        ])
                }

                ParseError::TermNonTermConflict {
                    name,
                    terminal,
                    non_terminal,
                } => {
                    let range = name.span().byte_range();

                    Diagnostic::error()
                        .with_message("Ambiguous token name")
                        .with_labels(vec![
                            Label::primary(file_id, range).with_message(
                                "This name is used for both terminal and non-terminal",
                            ),
                            Label::secondary(file_id, terminal.span().byte_range())
                                .with_message("Terminal definition here"),
                            Label::secondary(file_id, non_terminal.span().byte_range())
                                .with_message("Non-terminal definition here"),
                        ])
                        .with_notes(vec![
                            "Terminal and non-terminal name must be unique".to_string()
                        ])
                }

                ParseError::InvalidTerminalRange(
                    (first, first_index, first_stream),
                    (last, last_index, last_stream),
                ) => {
                    let range1 = first.span().byte_range();
                    let range2 = last.span().byte_range();
                    let range = range1.start..range2.end;
                    let range1 = utils::tokenstream_range(first_stream);
                    let range2 = utils::tokenstream_range(last_stream);

                    Diagnostic::error()
                        .with_message("Invalid terminal range")
                        .with_labels(vec![
                            Label::primary(file_id, range).with_message("Invalid range here"),
                            Label::secondary(file_id, range1).with_message(format!("First terminal symbol (index {})", first_index)),
                            Label::secondary(file_id, range2).with_message(format!("Last terminal symbol (index {})", last_index)),
                        ]).with_notes(vec![
                            "First terminal symbol has to be less than or equal to the last terminal symbol".to_string()
                        ])
                }

                ParseError::StartNonTerminalNotDefined(ident) => {
                    let range = ident.span().byte_range();

                    Diagnostic::error()
                        .with_message("Start non-terminal not defined")
                        .with_labels(vec![Label::primary(file_id, range)
                            .with_message("This name is given to %start")])
                        .with_notes(vec!["Non-terminal name must be defined".to_string()])
                }

                ParseError::TerminalNotDefined(ident) => {
                    let range = ident.span().byte_range();

                    Diagnostic::error()
                        .with_message("Terminal symbol not defined")
                        .with_labels(vec![Label::primary(file_id, range)
                            .with_message("This terminal symbol is not defined")])
                        .with_notes(vec!["Terminal symbol must be defined".to_string()])
                }

                ParseError::MultipleTokenDefinition(ident1, ident2) => {
                    let range1 = ident1.span().byte_range();
                    let range2 = ident2.span().byte_range();

                    Diagnostic::error()
                        .with_message("Multiple %token definition")
                        .with_labels(vec![
                            Label::primary(file_id, range1).with_message("First definition"),
                            Label::primary(file_id, range2).with_message("Other definition"),
                        ])
                        .with_notes(vec!["Token name must be unique".to_string()])
                }

                ParseError::EofDefined(ident) => {
                    let range = ident.span().byte_range();

                    Diagnostic::error()
                        .with_message("'eof' is reserved name")
                        .with_labels(vec![
                            Label::primary(file_id, range).with_message("This name is reserved")
                        ])
                }
                ParseError::AugmentedDefined(ident) => {
                    let range = ident.span().byte_range();

                    Diagnostic::error()
                        .with_message("'Augmented' is reserved name")
                        .with_labels(vec![
                            Label::primary(file_id, range).with_message("This name is reserved")
                        ])
                }

                ParseError::StartNotDefined => Diagnostic::error()
                    .with_message("%start not defined")
                    .with_labels(vec![])
                    .with_notes(vec![
                        "%start must be defined".to_string(),
                        ">>> %start <non-terminal>".to_string(),
                    ]),
                ParseError::EofNotDefined => Diagnostic::error()
                    .with_message("%eof not defined")
                    .with_labels(vec![])
                    .with_notes(vec![
                        "%eof must be defined".to_string(),
                        ">>> %eof <terminal>".to_string(),
                    ]),
                ParseError::TokenTypeNotDefined => Diagnostic::error()
                    .with_message("%tokentype not defined")
                    .with_labels(vec![])
                    .with_notes(vec![
                        "%tokentype must be defined".to_string(),
                        ">>> %tokentype <TokenType>".to_string(),
                    ]),

                ParseError::MacroLineParse { span, message } => {
                    let range = span.byte_range();

                    Diagnostic::error()
                        .with_message("Parse Failed")
                        .with_labels(vec![
                            Label::primary(file_id, range).with_message("Error here")
                        ])
                        .with_notes(vec![message])
                }
                ParseError::MacroLineParseEnd { message } => Diagnostic::error()
                    .with_message("Parse Failed")
                    .with_notes(vec![message]),

                _ => unreachable!("Unknown error: {:?}", e),
            };

            let writer = StandardStream::stderr(ColorChoice::Auto);
            let config = codespan_reporting::term::Config::default();
            term::emit(&mut writer.lock(), &config, &files, &diag)
                .expect("Failed to write to stderr");
            return;
        }
    };

    // expand macro
    let expanded_stream = match grammar.emit_compiletime(args.lalr) {
        Ok(expanded_stream) => expanded_stream,
        Err(e) => {
            let diag = match e {
                ParseError::RuleTypeDefinedButActionNotDefined {
                    name,
                    rule_local_id,
                } => {
                    let rule_line = &grammar.rules.get(&name).unwrap().rule_lines[rule_local_id];
                    let rule_line_range = if rule_line.tokens.is_empty() {
                        rule_line.separator_span.byte_range()
                    } else {
                        let first = rule_line.separator_span.byte_range().start;
                        let last = rule_line.tokens.last().unwrap().end_span.byte_range().end;
                        first..last
                    };
                    Diagnostic::error()
                        .with_message("Reduce action not defined")
                        .with_labels(vec![
                            Label::secondary(file_id, name.span().byte_range())
                                .with_message("This rule has a type definition"),
                            Label::primary(file_id, rule_line_range)
                                .with_message("This rule line has no reduce action"),
                        ])
                        .with_notes(vec!["".to_string()])
                }

                ParseError::ShiftReduceConflict {
                    term,
                    reduce_rule: (reduceid, reduce_production_rule),
                    shift_rules,
                } => {
                    let mut message = format!(
                        "Shift/Reduce conflict:\nReduce rule:\n>>> {}\nShift rules:",
                        reduce_production_rule
                    );
                    for (_, shifted_rule) in shift_rules.iter() {
                        message.push_str(format!("\n>>> {}", shifted_rule).as_str());
                    }

                    let (name, rules, rule) =
                        grammar.get_rule_by_id(reduceid).expect("Rule not found");
                    let (rule_begin, rule_end) = rules.rule_lines[rule].span_pair();
                    let rule_range = rule_begin.byte_range().start..rule_end.byte_range().end;
                    let mut labels = Vec::new();

                    if !name
                        .to_string()
                        .starts_with(rusty_lr_parser::utils::AUTO_GENERATED_RULE_PREFIX)
                    {
                        labels.push(
                            Label::primary(file_id, name.span().byte_range())
                                .with_message(format!("Reduce rule {} was defined here", name)),
                        );
                        labels.push(
                            Label::secondary(file_id, rule_range)
                                .with_message(format!("in this line")),
                        );
                    } else {
                        let origin_span = grammar
                            .generated_root_span
                            .get(&name)
                            .expect("generated_root_span::rule not found");
                        let origin_range =
                            origin_span.0.byte_range().start..origin_span.1.byte_range().end;
                        labels.push(
                            Label::primary(file_id, origin_range)
                                .with_message(format!("Reduce rule {} was generated here", name)),
                        );
                    }

                    for (shiftid, shift_rule) in shift_rules.into_iter() {
                        let (name, rules, rule) =
                            grammar.get_rule_by_id(shiftid).expect("Rule not found");
                        if !name
                            .to_string()
                            .starts_with(rusty_lr_parser::utils::AUTO_GENERATED_RULE_PREFIX)
                        {
                            let first_shift_token_byte = rules.rule_lines[rule].tokens
                                [shift_rule.shifted]
                                .begin_span
                                .byte_range()
                                .start;
                            let (_, rule_end) = rules.rule_lines[rule].span_pair();
                            let rule_range = first_shift_token_byte..rule_end.byte_range().end;
                            labels.push(
                                Label::primary(file_id, name.span().byte_range())
                                    .with_message(format!("Shift rule {} was defined here", name)),
                            );
                            labels.push(
                                Label::secondary(file_id, rule_range)
                                    .with_message(format!("in this line")),
                            );
                        } else {
                            let origin_span = grammar
                                .generated_root_span
                                .get(&name)
                                .expect("generated_root_span::rule not found");
                            let origin_range =
                                origin_span.0.byte_range().start..origin_span.1.byte_range().end;
                            labels.push(
                                Label::secondary(file_id, origin_range).with_message(format!(
                                    "Shift rule {} was generated here",
                                    name
                                )),
                            );
                        }
                    }
                    Diagnostic::error()
                        .with_message(message)
                        .with_labels(labels)
                        .with_notes(vec![format!("conflict terminal: {}", term)])
                }
                ParseError::ReduceReduceConflict {
                    lookahead,
                    rule1: (ruleid1, production_rule1),
                    rule2: (ruleid2, production_rule2),
                } => {
                    let (name1, rules1, rule1) =
                        grammar.get_rule_by_id(ruleid1).expect("Rule not found 1");
                    let (rule1_begin, rule1_end) = rules1.rule_lines[rule1].span_pair();
                    let rule_range1 = rule1_begin.byte_range().start..rule1_end.byte_range().end;
                    let (name2, rules2, rule2) =
                        grammar.get_rule_by_id(ruleid2).expect("Rule not found 2");
                    let (rule2_begin, rule2_end) = rules2.rule_lines[rule2].span_pair();
                    let rule_range2 = rule2_begin.byte_range().start..rule2_end.byte_range().end;

                    let mut labels = Vec::new();

                    // no byte range for auto generated rules
                    if !name1
                        .to_string()
                        .starts_with(rusty_lr_parser::utils::AUTO_GENERATED_RULE_PREFIX)
                    {
                        labels.push(
                            Label::primary(file_id, name1.span().byte_range())
                                .with_message(format!("{} was defined here", name1)),
                        );
                        labels.push(
                            Label::secondary(file_id, rule_range1)
                                .with_message(format!("in this line")),
                        );
                    } else {
                        let origin_span = grammar
                            .generated_root_span
                            .get(&name1)
                            .expect("generated_root_span::rule not found");
                        let origin_range =
                            origin_span.0.byte_range().start..origin_span.1.byte_range().end;

                        labels.push(
                            Label::primary(file_id, origin_range)
                                .with_message(format!("{} was generated here", name1)),
                        );
                    }
                    if !name2
                        .to_string()
                        .starts_with(rusty_lr_parser::utils::AUTO_GENERATED_RULE_PREFIX)
                    {
                        labels.push(
                            Label::primary(file_id, name2.span().byte_range())
                                .with_message(format!("{} was defined here", name2)),
                        );
                        labels.push(
                            Label::secondary(file_id, rule_range2)
                                .with_message(format!("in this line")),
                        );
                    } else {
                        let origin_span = grammar
                            .generated_root_span
                            .get(&name2)
                            .expect("generated_root_span::rule not found");
                        let origin_range =
                            origin_span.0.byte_range().start..origin_span.1.byte_range().end;

                        labels.push(
                            Label::primary(file_id, origin_range)
                                .with_message(format!("{} was generated here", name2)),
                        );
                    }
                    Diagnostic::error()
                        .with_message(format!(
                            "Reduce/Reduce conflict:\n>>> {}\n>>> {}",
                            production_rule1, production_rule2
                        ))
                        .with_labels(labels)
                        .with_notes(vec![format!("with lookahead {}", lookahead)])
                }

                _ => unreachable!("Unknown error: {:?}", e),
            };

            let writer = StandardStream::stderr(ColorChoice::Auto);
            let config = codespan_reporting::term::Config::default();
            term::emit(&mut writer.lock(), &config, &files, &diag)
                .expect("Failed to write to stderr");
            return;
        }
    };

    // this comments will be printed to the output file
    // build again here whether it was built before
    // since many informations are removed in the rusty_lr_parser output
    let mut debug_comments = String::new();
    {
        let parser = match grammar.create_grammar() {
            Ok(mut grammar) => {
                debug_comments.push_str(format!("{:=^80}\n", "Grammar").as_str());
                debug_comments.push_str(format!("{}\n", grammar).as_str());
                if args.lalr {
                    match grammar.build_lalr(Ident::new(
                        rusty_lr_parser::utils::AUGMENTED_NAME,
                        Span::call_site(),
                    )) {
                        Ok(parser) => parser,
                        Err(e) => {
                            eprintln!("{}", e);
                            return;
                        }
                    }
                } else {
                    match grammar.build(Ident::new(
                        rusty_lr_parser::utils::AUGMENTED_NAME,
                        Span::call_site(),
                    )) {
                        Ok(parser) => parser,
                        Err(e) => {
                            eprintln!("{}", e);
                            return;
                        }
                    }
                }
            }
            Err(e) => {
                unreachable!("Unknown error: {:?}", e);
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
                        debug_comments.push_str(&token.to_string());
                        if id < reduce_first.len() - 1 {
                            debug_comments.push_str(", ");
                        }
                    }
                    debug_comments.push('\n');
                }
                if !shift_first.is_empty() {
                    debug_comments.push_str("Tokens for shift: ");
                    for (id, token) in shift_first.iter().enumerate() {
                        debug_comments.push_str(&token.to_string());
                        if id < shift_first.len() - 1 {
                            debug_comments.push_str(", ");
                        }
                    }
                    debug_comments.push('\n');
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
