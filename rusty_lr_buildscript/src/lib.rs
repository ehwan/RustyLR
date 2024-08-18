//! Build script for rusty_lr
//!
//! This crate is private and not intended to be used directly.
//! Please use the [`rusty_lr`](https://crates.io/crates/rusty_lr) crate instead.
//!
//! ```no_run
//! fn main() {
//!     println!("cargo::rerun-if-changed=src/parser/parser.rs");
//!
//!     let output_dir = std::env::var("OUT_DIR").unwrap();
//!     let output = format!("{}/parser.rs", output_dir);
//!     Builder::new()
//!        .file("src/parser/parser.rs")
//!        .build(&output);
//! }
//!

pub mod output;
mod split;
mod utils;

use codespan_reporting::diagnostic::Diagnostic;
use codespan_reporting::diagnostic::Label;
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::ColorChoice;
use codespan_reporting::term::termcolor::StandardStream;

use proc_macro2::Ident;
use proc_macro2::Span;
use proc_macro2::TokenStream;

use quote::quote;
use rusty_lr_core::ShiftedRule;
use rusty_lr_parser::error::ArgError;
use rusty_lr_parser::error::EmitError;
use rusty_lr_parser::error::ParseArgError;
use rusty_lr_parser::error::ParseError;

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::fs::read;
use std::fs::write;

/// Main entry for the build script
pub struct Builder {
    /// input_file to read
    input_file: Option<String>,

    /// build LALR(1) parser
    lalr: bool,

    /// print debug information.
    ///
    /// print the auto-generated rules, and where they are originated from.
    /// print the shift/reduce conflicts, and the resolving process.
    verbose: bool,
}

impl Builder {
    pub fn new() -> Self {
        Self {
            input_file: None,
            lalr: false,
            verbose: false,
        }
    }

    /// set input file
    pub fn file(&mut self, filename: &str) -> &mut Self {
        self.input_file = Some(filename.to_string());
        self
    }

    /// set to build LALR(1) parser
    pub fn lalr(&mut self) -> &mut Self {
        self.lalr = true;
        self
    }

    /// print debug information to stdout.
    pub fn verbose(&mut self) -> &mut Self {
        self.verbose = true;
        self
    }

    /// build and emit code to output file
    pub fn build(&self, output_file: &str) {
        let output = match self.build_impl() {
            Ok(output) => {
                let stream1 = output.user_stream;
                let stream2 = output.generated_stream;
                quote! {
                    #stream1
                    #stream2
                }
            }
            Err(_) => {
                panic!("build failed");
            }
        };

        write(output_file, output.to_string()).expect("Failed to write to file");
    }

    /// for internal use
    pub fn build_impl(&self) -> Result<output::Output, String> {
        if self.input_file.is_none() {
            eprintln!("Input file not set");
            return Err("Input file not set".to_string());
        }

        let input_file = self.input_file.as_ref().unwrap();
        // read file
        let input_bytes = match read(input_file) {
            Ok(bytes) => bytes,
            Err(e) => {
                let message = format!("Error reading file: {}", e);
                eprintln!("{}", message);
                return Err(message);
            }
        };

        let str = match String::from_utf8(input_bytes) {
            Ok(str) => str,
            Err(e) => {
                let message = format!("Error reading utf-8: {}", e);
                eprintln!("{}", message);
                return Err(message);
            }
        };

        let mut files = SimpleFiles::new();
        let file_id = files.add(input_file, str.clone());

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
                return Err("Lexing error".to_string());
            }
        };

        // split stream by '%%'
        let (output_stream, macro_stream) = match split::split_stream(token_stream) {
            Ok((output_stream, macro_stream)) => (output_stream, macro_stream),
            Err(_) => {
                let diag = Diagnostic::error()
                    .with_message("Cannot find `%%`")
                    .with_notes(vec![
                    "Please put `%%` to separate the code part and the context-free grammar part"
                        .to_string(),
                ]);
                let writer = StandardStream::stderr(ColorChoice::Auto);
                let config = codespan_reporting::term::Config::default();
                term::emit(&mut writer.lock(), &config, &files, &diag)
                    .expect("Failed to write to stderr");
                return Err(diag.message);
            }
        };

        let grammar_args = match rusty_lr_parser::grammar::Grammar::parse_args(macro_stream) {
            Ok(grammar_args) => grammar_args,
            Err(e) => {
                let diag =
                    match e {
                        ParseArgError::MacroLineParse { span, message } => {
                            let range = span.byte_range();

                            Diagnostic::error()
                                .with_message("Parse Failed")
                                .with_labels(vec![
                                    Label::primary(file_id, range).with_message("Error here")
                                ])
                                .with_notes(vec![message])
                        }
                        ParseArgError::MacroLineParseEnd { message } => Diagnostic::error()
                            .with_message("Parse Failed")
                            .with_notes(vec![message]),

                        _ => {
                            let message = e.short_message();
                            let span = e.span().byte_range();
                            Diagnostic::error().with_message(message).with_labels(vec![
                                Label::primary(file_id, span).with_message("occured here"),
                            ])
                        }
                    };

                let writer = StandardStream::stderr(ColorChoice::Auto);
                let config = codespan_reporting::term::Config::default();
                term::emit(&mut writer.lock(), &config, &files, &diag)
                    .expect("Failed to write to stderr");
                return Err(diag.message);
            }
        };
        match rusty_lr_parser::grammar::Grammar::arg_check_error(&grammar_args) {
            Ok(_) => {}
            Err(e) => {
                let diag = match e {
                    ArgError::MultipleModulePrefixDefinition(
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
                    ArgError::MultipleUserDataDefinition(
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
                            .with_notes(
                                vec!["Only one %userdata definition is allowed".to_string()],
                            )
                    }
                    ArgError::MultipleErrorDefinition(
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
                    ArgError::MultipleTokenTypeDefinition(
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
                            .with_notes(vec![
                                "Only one %tokentype definition is allowed".to_string()
                            ])
                    }
                    ArgError::MultipleEofDefinition(
                        (span1, tokenstream1),
                        (span2, tokenstream2),
                    ) => {
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
                    ArgError::MultipleStartDefinition(ident1, ident2) => {
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

                    ArgError::StartNotDefined => Diagnostic::error()
                        .with_message("%start not defined")
                        .with_labels(vec![])
                        .with_notes(vec![
                            "%start must be defined".to_string(),
                            ">>> %start <non-terminal>".to_string(),
                        ]),
                    ArgError::EofNotDefined => Diagnostic::error()
                        .with_message("%eof not defined")
                        .with_labels(vec![])
                        .with_notes(vec![
                            "%eof must be defined".to_string(),
                            ">>> %eof <terminal>".to_string(),
                        ]),
                    ArgError::TokenTypeNotDefined => Diagnostic::error()
                        .with_message("%tokentype not defined")
                        .with_labels(vec![])
                        .with_notes(vec![
                            "%tokentype must be defined".to_string(),
                            ">>> %tokentype <TokenType>".to_string(),
                        ]),
                    _ => {
                        let message = e.short_message();
                        let span = e.span().byte_range();
                        Diagnostic::error()
                            .with_message(message)
                            .with_labels(vec![
                                Label::primary(file_id, span).with_message("occured here")
                            ])
                    }
                };

                let writer = StandardStream::stderr(ColorChoice::Auto);
                let config = codespan_reporting::term::Config::default();
                term::emit(&mut writer.lock(), &config, &files, &diag)
                    .expect("Failed to write to stderr");
                return Err(diag.message);
            }
        }

        // parse lines
        let grammar = match rusty_lr_parser::grammar::Grammar::from_grammar_args(grammar_args) {
            Ok(grammar) => grammar,
            Err(e) => {
                let diag = match e {
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

                    ParseError::MultipleReduceDefinition { terminal, old, new } => {
                        let old_range = old.0.byte_range().start..old.1.byte_range().end;
                        let old_string = match old.2 {
                            rusty_lr_core::ReduceType::Left => "%left",
                            rusty_lr_core::ReduceType::Right => "%right",
                        };
                        let new_range = new.0.byte_range().start..new.1.byte_range().end;
                        let new_string = match new.2 {
                            rusty_lr_core::ReduceType::Left => "%left",
                            rusty_lr_core::ReduceType::Right => "%right",
                        };

                        Diagnostic::error()
                            .with_message("Multiple reduce definition")
                            .with_labels(vec![
                                Label::primary(file_id, terminal.span().byte_range()).with_message(
                                    "This terminal symbol is defined as both of %left and %right",
                                ),
                                Label::secondary(file_id, old_range)
                                    .with_message(format!("was set as {} here", old_string)),
                                Label::secondary(file_id, new_range)
                                    .with_message(format!("was set as {} here", new_string)),
                            ])
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
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message("This name is reserved")])
                    }
                    ParseError::AugmentedDefined(ident) => {
                        let range = ident.span().byte_range();

                        Diagnostic::error()
                            .with_message("'Augmented' is reserved name")
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message("This name is reserved")])
                    }
                    ParseError::ReservedName(ident) => {
                        let range = ident.span().byte_range();

                        Diagnostic::error()
                            .with_message(format!("'{}' is reserved name", ident))
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message("This name is reserved")])
                    }
                    _ => {
                        let message = e.short_message();
                        let span = e.span().byte_range();
                        Diagnostic::error()
                            .with_message(message)
                            .with_labels(vec![
                                Label::primary(file_id, span).with_message("occured here")
                            ])
                    }
                };

                let writer = StandardStream::stderr(ColorChoice::Auto);
                let config = codespan_reporting::term::Config::default();
                term::emit(&mut writer.lock(), &config, &files, &diag)
                    .expect("Failed to write to stderr");

                return Err(diag.message);
            }
        };

        // expand macro
        let expanded_stream = match grammar.emit_compiletime(self.lalr) {
            Ok(expanded_stream) => expanded_stream,
            Err(e) => {
                let diag = match e.as_ref() {
                    EmitError::RuleTypeDefinedButActionNotDefined {
                        name,
                        rule_local_id,
                    } => {
                        // `name` must not be generated rule,
                        // since it is programmically generated, it must have a proper reduce action

                        let rule_line =
                            &grammar.rules.get(name).unwrap().rule_lines[*rule_local_id];
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

                    EmitError::ShiftReduceConflict {
                        term,
                        reduce_rule: (reduceid, reduce_production_rule),
                        shift_rules,
                    } => {
                        let mut message = format!(
                            "Shift/Reduce conflict:\nReduce rule:\n\t>>> {}\nShift rules:",
                            reduce_production_rule
                        );
                        for (_, shifted_rule) in shift_rules.iter() {
                            message.push_str(format!("\n\t>>> {}", shifted_rule).as_str());
                        }

                        let (name, rules, rule) =
                            grammar.get_rule_by_id(*reduceid).expect("Rule not found");
                        let mut labels = Vec::new();

                        if !name
                            .to_string()
                            .starts_with(rusty_lr_parser::utils::AUTO_GENERATED_RULE_PREFIX)
                        {
                            let (rule_begin, rule_end) = rules.rule_lines[rule].span_pair();
                            let rule_range =
                                rule_begin.byte_range().start..rule_end.byte_range().end;

                            labels.push(
                                Label::primary(file_id, name.span().byte_range())
                                    .with_message(format!("Reduce rule {} was defined here", name)),
                            );
                            labels.push(
                                Label::secondary(file_id, rule_range)
                                    .with_message("in this line".to_string()),
                            );
                        } else {
                            let origin_span = grammar
                                .generated_root_span
                                .get(name)
                                .expect("generated_root_span::rule not found");
                            let origin_range =
                                origin_span.0.byte_range().start..origin_span.1.byte_range().end;
                            labels.push(
                                Label::primary(file_id, origin_range).with_message(format!(
                                    "Reduce rule {} was generated here",
                                    name
                                )),
                            );
                        }

                        for (shiftid, shift_rule) in shift_rules.iter() {
                            let (name, rules, rule) =
                                grammar.get_rule_by_id(*shiftid).expect("Rule not found");
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
                                    Label::primary(file_id, name.span().byte_range()).with_message(
                                        format!("Shift rule {} was defined here", name),
                                    ),
                                );
                                labels.push(
                                    Label::secondary(file_id, rule_range)
                                        .with_message("in this line".to_string()),
                                );
                            } else {
                                let origin_span = grammar
                                    .generated_root_span
                                    .get(name)
                                    .expect("generated_root_span::rule not found");
                                let origin_range = origin_span.0.byte_range().start
                                    ..origin_span.1.byte_range().end;
                                labels.push(Label::secondary(file_id, origin_range).with_message(
                                    format!("Shift rule {} was generated here", name),
                                ));
                            }
                        }
                        Diagnostic::error()
                            .with_message(message)
                            .with_labels(labels)
                            .with_notes(vec![
                                format!("conflict terminal: {}", term),
                                format!(
                                "Try to rearrange the rules or resolve conflict by set reduce type"
                            ),
                                format!(">>> %left {}", term),
                                format!(">>> %right {}", term),
                            ])
                    }
                    EmitError::ReduceReduceConflict {
                        lookahead,
                        rule1: (ruleid1, production_rule1),
                        rule2: (ruleid2, production_rule2),
                    } => {
                        let (name1, rules1, rule1) =
                            grammar.get_rule_by_id(*ruleid1).expect("Rule not found 1");
                        let (rule1_begin, rule1_end) = rules1.rule_lines[rule1].span_pair();
                        let rule_range1 =
                            rule1_begin.byte_range().start..rule1_end.byte_range().end;
                        let (name2, rules2, rule2) =
                            grammar.get_rule_by_id(*ruleid2).expect("Rule not found 2");
                        let (rule2_begin, rule2_end) = rules2.rule_lines[rule2].span_pair();
                        let rule_range2 =
                            rule2_begin.byte_range().start..rule2_end.byte_range().end;

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
                                    .with_message("in this line".to_string()),
                            );
                        } else {
                            let origin_span = grammar
                                .generated_root_span
                                .get(name1)
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
                                    .with_message("in this line".to_string()),
                            );
                        } else {
                            let origin_span = grammar
                                .generated_root_span
                                .get(name2)
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

                    _ => {
                        let message = e.short_message();
                        let span = e.span().byte_range();
                        Diagnostic::error()
                            .with_message(message)
                            .with_labels(vec![
                                Label::primary(file_id, span).with_message("occured here")
                            ])
                    }
                };

                let writer = StandardStream::stderr(ColorChoice::Auto);
                let config = codespan_reporting::term::Config::default();
                term::emit(&mut writer.lock(), &config, &files, &diag)
                    .expect("Failed to write to stderr");

                return Err(diag.message);
            }
        };

        // this comments will be printed to the output file
        // build again here whether it was built before
        // since many informations are removed in the rusty_lr_parser output
        let mut debug_comments = String::new();
        {
            let mut builder = grammar.create_grammar();
            debug_comments.push_str(format!("{:=^80}\n", "Grammar").as_str());
            for rule in builder.rules.iter() {
                debug_comments.push_str(format!("{}\n", rule).as_str());
            }
            let parser = if self.lalr {
                match builder.build_lalr(Ident::new(
                    rusty_lr_parser::utils::AUGMENTED_NAME,
                    Span::call_site(),
                )) {
                    Ok(parser) => parser,
                    Err(_) => unreachable!("Grammar building failed"),
                }
            } else {
                match builder.build(Ident::new(
                    rusty_lr_parser::utils::AUGMENTED_NAME,
                    Span::call_site(),
                )) {
                    Ok(parser) => parser,
                    Err(_) => unreachable!("Grammar building failed"),
                }
            };

            // print note about generated rules
            if self.verbose {
                let mut rules_on_same_root = BTreeMap::new();

                // `generated_root_span` contains only auto-generated rules
                for (rule_name, root_span) in grammar.generated_root_span.iter() {
                    let start = root_span.0.byte_range().start;
                    let end = root_span.1.byte_range().end;
                    rules_on_same_root
                        .entry((start, end))
                        .or_insert_with(Vec::new)
                        .push(rule_name);
                }

                for (root_range, rules) in rules_on_same_root.into_iter() {
                    let mut rules_string = String::new();
                    for rule_name in rules.into_iter() {
                        let name_str = rule_name.to_string();
                        let name_len = name_str.len();
                        let front_padding = " ".repeat(name_len);
                        let rule_lines = grammar.rules.get(rule_name).expect("Rule not found");

                        for (idx, rule_line) in rule_lines.rule_lines.iter().enumerate() {
                            let mut line_string = String::new();
                            for (idx, token) in rule_line.tokens.iter().enumerate() {
                                line_string.push_str(token.token.to_string().as_str());
                                if idx < rule_line.tokens.len() - 1 {
                                    line_string.push(' ');
                                }
                            }

                            if idx == 0 {
                                rules_string.push_str(
                                    format!("\n{} -> {}", name_str, line_string).as_str(),
                                );
                            } else {
                                rules_string.push_str(
                                    format!("\n{}  | {}", front_padding, line_string).as_str(),
                                );
                            }
                        }
                        rules_string.push_str(format!("\n{}  ;", front_padding).as_str());
                    }

                    let message = format!("Auto-generated rules:{}", rules_string);
                    let diag = Diagnostic::note()
                        .with_message(message)
                        .with_labels(vec![Label::primary(file_id, root_range.0..root_range.1)
                            .with_message("was generated here")]);

                    let writer = StandardStream::stderr(ColorChoice::Auto);
                    let config = codespan_reporting::term::Config::default();
                    term::emit(&mut writer.lock(), &config, &files, &diag)
                        .expect("Failed to write to stderr");
                }

                // about shift/reduce conflict resolving
                for state in parser.states.iter() {
                    let mut reduce_rules = BTreeMap::new();
                    let mut shift_rules = BTreeMap::new();

                    for (shifted_rule_ref, lookaheads) in state.ruleset.rules.iter() {
                        // is end of rule, add to reduce
                        if shifted_rule_ref.shifted
                            == builder.rules[shifted_rule_ref.rule].rule.len()
                        {
                            for token in lookaheads.iter() {
                                reduce_rules.insert(token, shifted_rule_ref.rule);
                            }
                        }

                        // if it is not end, and next token is terminal, add to shift
                        if let Some(rusty_lr_core::Token::Term(token)) = builder.rules
                            [shifted_rule_ref.rule]
                            .rule
                            .get(shifted_rule_ref.shifted)
                        {
                            shift_rules
                                .entry(token)
                                .or_insert_with(BTreeSet::new)
                                .insert(*shifted_rule_ref);
                        }
                    }

                    // check shift/reduce conflict
                    for (term, shift_rules) in shift_rules.into_iter() {
                        if let Some(reduce_rule) = reduce_rules.get(term) {
                            // shift/reduce conflict here
                            // since there were not error reaching here, 'term' must be set reduce_type

                            let mut message = format!(
                        "Shift/Reduce conflict with token {} was resolved:\nReduce rule:\n>>> {}\nShift rules:",
                        term,
                        &builder.rules[*reduce_rule]
                    );
                            for shifted_rule in shift_rules.iter() {
                                let shifted_rule = ShiftedRule {
                                    rule: builder.rules[shifted_rule.rule].clone(),
                                    shifted: shifted_rule.shifted,
                                };
                                message.push_str(format!("\n>>> {}", shifted_rule).as_str());
                            }

                            let (name, rules, rule) = grammar
                                .get_rule_by_id(*reduce_rule)
                                .expect("Rule not found");
                            let mut labels = Vec::new();

                            if !name
                                .to_string()
                                .starts_with(rusty_lr_parser::utils::AUTO_GENERATED_RULE_PREFIX)
                            {
                                let (rule_begin, rule_end) = rules.rule_lines[rule].span_pair();
                                let rule_range =
                                    rule_begin.byte_range().start..rule_end.byte_range().end;
                                labels.push(
                                    Label::primary(file_id, name.span().byte_range()).with_message(
                                        format!("Reduce rule {} was defined here", name),
                                    ),
                                );
                                labels.push(
                                    Label::secondary(file_id, rule_range)
                                        .with_message("in this line".to_string()),
                                );
                            } else {
                                let origin_span = grammar
                                    .generated_root_span
                                    .get(name)
                                    .expect("generated_root_span::rule not found");
                                let origin_range = origin_span.0.byte_range().start
                                    ..origin_span.1.byte_range().end;
                                labels.push(Label::primary(file_id, origin_range).with_message(
                                    format!("Reduce rule {} was generated here", name),
                                ));
                            }

                            for shift_rule in shift_rules.iter() {
                                let (name, rules, rule) = grammar
                                    .get_rule_by_id(shift_rule.rule)
                                    .expect("Rule not found");
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
                                    let rule_range =
                                        first_shift_token_byte..rule_end.byte_range().end;
                                    labels.push(
                                        Label::primary(file_id, name.span().byte_range())
                                            .with_message(format!(
                                                "Shift rule {} was defined here",
                                                name
                                            )),
                                    );
                                    labels.push(
                                        Label::secondary(file_id, rule_range)
                                            .with_message("in this line".to_string()),
                                    );
                                } else {
                                    let origin_span = grammar
                                        .generated_root_span
                                        .get(name)
                                        .expect("generated_root_span::rule not found");
                                    let origin_range = origin_span.0.byte_range().start
                                        ..origin_span.1.byte_range().end;
                                    labels.push(
                                        Label::secondary(file_id, origin_range).with_message(
                                            format!("Shift rule {} was generated here", name),
                                        ),
                                    );
                                }
                            }

                            let reduce_type_origin = grammar
                                .reduce_types_origin
                                .get(term)
                                .expect("reduce_types_origin not found");
                            let range = reduce_type_origin.0.byte_range().start
                                ..reduce_type_origin.1.byte_range().end;
                            let reduce_type = *grammar
                                .reduce_types
                                .get(term)
                                .expect("reduce_types not found");
                            let type_string = match reduce_type {
                                rusty_lr_core::ReduceType::Left => "%left",
                                rusty_lr_core::ReduceType::Right => "%right",
                            };
                            labels.push(Label::primary(file_id, range).with_message(format!(
                                "Reduce type was set as {} here",
                                type_string
                            )));

                            let diag = Diagnostic::note().with_message(message).with_labels(labels);

                            let writer = StandardStream::stderr(ColorChoice::Auto);
                            let config = codespan_reporting::term::Config::default();
                            term::emit(&mut writer.lock(), &config, &files, &diag)
                                .expect("Failed to write to stderr");
                        }
                    }
                }
            }
        }

        Ok(output::Output {
            user_stream: output_stream,
            generated_stream: expanded_stream,
            debug_comments,
        })
    }
}
