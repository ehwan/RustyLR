//! Build script for rusty_lr
//!
//! This crate is private and not intended to be used directly.
//! Please use the [`rusty_lr`](https://crates.io/crates/rusty_lr) crate instead.
//!
//! ```ignore
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

use std::fs::read;
use std::fs::write;

/// Main entry for the build script
pub struct Builder {
    /// input_file to read
    input_file: Option<String>,

    /// when `vebose` is on, print debug information about
    /// any shift/reduce, reduce/reduce conflicts.
    /// This is for GLR parser, to show where the conflicts occured.
    verbose_conflicts: bool,

    /// when `vebose` is on, print debug information about
    /// conflicts resolving process by `%left` or `%right` for any shift/reduce, reduce/reduce conflicts.
    verbose_conflicts_resolving: bool,

    /// print verbose information to stderr
    verbose_on_stderr: bool,
}

impl Builder {
    pub fn new() -> Self {
        Self {
            input_file: None,
            verbose_conflicts: false,
            verbose_conflicts_resolving: false,
            verbose_on_stderr: false,
        }
    }

    /// set input file
    pub fn file(&mut self, filename: &str) -> &mut Self {
        self.input_file = Some(filename.to_string());
        self
    }

    /// turns on all verbose options
    pub fn verbose(&mut self) -> &mut Self {
        self.verbose_conflicts();
        self.verbose_conflicts_resolving();
        self
    }

    /// when `vebose` is on, print debug information about
    /// any shift/reduce, reduce/reduce conflicts.
    pub fn verbose_conflicts(&mut self) -> &mut Self {
        self.verbose_conflicts = true;
        self
    }

    /// when `vebose` is on, print debug information about
    /// conflicts resolving process by `%left` or `%right` for any shift/reduce, reduce/reduce conflicts.
    pub fn verbose_conflicts_resolving(&mut self) -> &mut Self {
        self.verbose_conflicts_resolving = true;
        self
    }

    /// print debug information to stderr.
    pub fn verbose_on_stderr(&mut self) -> &mut Self {
        self.verbose_on_stderr = true;
        self
    }

    fn verbose_stream(&self) -> StandardStream {
        if self.verbose_on_stderr {
            StandardStream::stderr(ColorChoice::Auto)
        } else {
            StandardStream::stdout(ColorChoice::Auto)
        }
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

    /// extend `labels` with messages about the source of the rule
    /// if `ruleid` is auto-generated rule, "{} was generated here" will be added
    /// if `ruleid` is user-written rule, "{} was defined here" will be added
    fn extend_rule_source_label(
        labels: &mut Vec<codespan_reporting::diagnostic::Label<usize>>,
        fileid: usize,
        ruleid: usize,
        grammar: &rusty_lr_parser::grammar::Grammar,
        prefix_str: &str,
        prefix_in_this_line: &str,
    ) {
        let (nonterm, local_rule) = grammar.get_rule_by_id(ruleid).expect("Rule not found");
        if let Some(origin_span) = &nonterm.regex_span {
            let origin_range = origin_span.0.byte_range().start..origin_span.1.byte_range().end;
            labels.push(Label::primary(fileid, origin_range).with_message(format!(
                "{}{} was generated here",
                prefix_str, nonterm.pretty_name,
            )));
        } else {
            let (rule_begin, rule_end) = nonterm.rules[local_rule].span_pair();
            let rule_range = rule_begin.byte_range().start..rule_end.byte_range().end;

            labels.push(
                Label::primary(fileid, nonterm.name.span().byte_range()).with_message(format!(
                    "{}{} was defined here",
                    prefix_str, nonterm.pretty_name
                )),
            );
            labels.push(
                Label::secondary(fileid, rule_range)
                    .with_message(format!("{}in this line", prefix_in_this_line)),
            );
        }
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

        // check conflicts
        let mut dfa_builder = grammar.create_grammar();
        let augmented_rule_id = *grammar
            .nonterminals_index
            .get(&Ident::new(
                rusty_lr_parser::utils::AUGMENTED_NAME,
                Span::call_site(),
            ))
            .unwrap();
        let dfa = if grammar.lalr {
            dfa_builder.build_lalr(augmented_rule_id)
        } else {
            dfa_builder.build(augmented_rule_id)
        };
        let dfa = match dfa {
            Ok(dfa) => dfa,
            Err(_err) => {
                unreachable!("Grammar building failed");
            }
        };

        let mut conflict_diags = Vec::new();
        // to map production rule to its pretty name abbreviation
        let term_mapper = |term_idx: usize| grammar.terminals[term_idx].name.to_string();
        let nonterm_mapper = |nonterm: usize| grammar.nonterminals[nonterm].pretty_name.clone();
        for state in dfa.states.iter() {
            // reduce/reduce conflicts
            for (reduce_rules, lookaheads) in state.conflict_rr() {
                let mut labels = Vec::new();
                for (idx, reduce_rule) in reduce_rules.iter().copied().enumerate() {
                    Self::extend_rule_source_label(
                        &mut labels,
                        file_id,
                        reduce_rule,
                        &grammar,
                        format!("(Rule{}) ", idx + 1).as_str(),
                        "error ",
                    );
                }

                let lookaheads: Vec<_> = lookaheads
                    .iter()
                    .copied()
                    .copied()
                    .map(&term_mapper)
                    .collect();

                let shortest_path: Vec<_> = state
                    .shortest_path
                    .iter()
                    .map(|token| match token {
                        rusty_lr_core::Token::Term(term) => term_mapper(*term),
                        rusty_lr_core::Token::NonTerm(nonterm) => nonterm_mapper(*nonterm),
                    })
                    .collect();

                let mut notes = vec![
                    format!("lookaheads: [{}]", lookaheads.join(", ")),
                    "Possible paths:".to_string(),
                ];
                notes.push(format!(
                    "(Origin): {} •<lookahead>",
                    shortest_path.join(" ")
                ));
                for reduce_rule in reduce_rules.iter().copied() {
                    let rule = &dfa_builder.rules[reduce_rule].0;
                    let mut path = shortest_path.clone();
                    for _ in 0..rule.rule.len() {
                        path.pop();
                    }
                    let nonterm = nonterm_mapper(rule.name);
                    path.push(nonterm);
                    notes.push(format!("(Reduce): {} •<lookahead>", path.join(" "),));
                }

                let diag = Diagnostic::error()
                    .with_message("Reduce/Reduce conflict")
                    .with_labels(labels)
                    .with_notes(notes);
                conflict_diags.push(diag);
            }
            for (&term, reduce_rules, shift_rules) in state.conflict_sr(|idx| &dfa.states[idx]) {
                let mut message = "Shift/Reduce conflict:\nShift rules:".to_string();
                for shifted_rule in shift_rules.iter() {
                    let shifted_rule = ShiftedRule {
                        rule: dfa_builder.rules[shifted_rule.rule]
                            .0
                            .clone()
                            .map(&term_mapper, &nonterm_mapper),
                        shifted: shifted_rule.shifted,
                    };
                    message.push_str(format!("\n\t>>> {}", shifted_rule).as_str());
                }

                let mut labels = Vec::new();
                for (idx, reduce_rule) in reduce_rules.into_iter().copied().enumerate() {
                    let prefix = if reduce_rules.len() == 1 {
                        "(Reduce) ".to_string()
                    } else {
                        format!("(Reduce{}) ", idx + 1)
                    };
                    Self::extend_rule_source_label(
                        &mut labels,
                        file_id,
                        reduce_rule,
                        &grammar,
                        &prefix,
                        &prefix,
                    );
                }

                for (idx, shiftid) in shift_rules.iter().enumerate() {
                    let prefix = if reduce_rules.len() == 1 {
                        "(Shift) ".to_string()
                    } else {
                        format!("(Shift{}) ", idx + 1)
                    };

                    Self::extend_rule_source_label(
                        &mut labels,
                        file_id,
                        shiftid.rule,
                        &grammar,
                        &prefix,
                        &prefix,
                    );
                }
                let term = grammar.terminals[term].name.to_string();

                let shortest_path: Vec<_> = state
                    .shortest_path
                    .iter()
                    .map(|token| match token {
                        rusty_lr_core::Token::Term(term) => {
                            format!("{} ", grammar.terminals[*term].name)
                        }
                        rusty_lr_core::Token::NonTerm(nonterm) => {
                            format!("{} ", grammar.nonterminals[*nonterm].pretty_name)
                        }
                    })
                    .collect();
                let shift_note = format!("(Shift) : {} •{}", shortest_path.join(" "), term);

                let mut notes = vec![
                    format!("Trying to feed terminal: {}", term),
                    format!("Try to rearrange the rules or resolve conflict by set reduce type"),
                    format!(">>> %left {}", term),
                    format!(">>> %right {}", term),
                    "Possible paths:".to_string(),
                    shift_note,
                ];
                for &reduce_rule in reduce_rules.iter() {
                    let rule = &dfa_builder.rules[reduce_rule].0;
                    let mut path = shortest_path.clone();
                    for _ in 0..rule.rule.len() {
                        path.pop();
                    }
                    let nonterm = nonterm_mapper(rule.name);
                    notes.push(format!(
                        "(Reduce): {} {} •{}",
                        path.join(" "),
                        nonterm,
                        term
                    ));
                }

                let diag = Diagnostic::error()
                    .with_message(message)
                    .with_labels(labels)
                    .with_notes(notes);
                conflict_diags.push(diag);
            }
        }

        if !grammar.glr {
            let has_diags = !conflict_diags.is_empty();
            for diag in conflict_diags.into_iter() {
                let writer = StandardStream::stderr(ColorChoice::Auto);
                let config = codespan_reporting::term::Config::default();
                term::emit(&mut writer.lock(), &config, &files, &diag)
                    .expect("Failed to write to stderr");
            }
            if has_diags {
                return Err("Grammar building failed".to_string());
            }
        }
        // print note about reduce/reduce conflict and shift/reduce conflict not resolved
        else if self.verbose_conflicts {
            for diag in conflict_diags.into_iter() {
                let diag = Diagnostic::note()
                    .with_message(diag.message)
                    .with_labels(diag.labels)
                    .with_notes(diag.notes);
                let writer = self.verbose_stream();
                let config = codespan_reporting::term::Config::default();
                term::emit(&mut writer.lock(), &config, &files, &diag)
                    .expect("Failed to write to stderr");
            }
        }

        // print note about shift/reduce conflict resolved with `%left` or `%right`
        if self.verbose_conflicts_resolving {
            let mut diags = Vec::new();
            for state in dfa.states.iter() {
                for (&term, (reduce_type, reduce_rules, shift_rules)) in state.sr_resolved.iter() {
                    let mut message = "Shift/Reduce conflict:\nShift rules:".to_string();
                    for shifted_rule in shift_rules.iter() {
                        let shifted_rule = ShiftedRule {
                            rule: dfa_builder.rules[shifted_rule.rule]
                                .0
                                .clone()
                                .map(&term_mapper, &nonterm_mapper),
                            shifted: shifted_rule.shifted,
                        };
                        message.push_str(format!("\n\t>>> {}", shifted_rule).as_str());
                    }

                    let mut labels = Vec::new();
                    for (idx, reduce_rule) in reduce_rules.into_iter().copied().enumerate() {
                        let prefix = if reduce_rules.len() == 1 {
                            "(Reduce) ".to_string()
                        } else {
                            format!("(Reduce{}) ", idx + 1)
                        };
                        Self::extend_rule_source_label(
                            &mut labels,
                            file_id,
                            reduce_rule,
                            &grammar,
                            &prefix,
                            &prefix,
                        );
                    }

                    for (idx, shiftid) in shift_rules.iter().enumerate() {
                        let prefix = if reduce_rules.len() == 1 {
                            "(Shift) ".to_string()
                        } else {
                            format!("(Shift{}) ", idx + 1)
                        };

                        Self::extend_rule_source_label(
                            &mut labels,
                            file_id,
                            shiftid.rule,
                            &grammar,
                            &prefix,
                            &prefix,
                        );
                    }
                    let term_info = &grammar.terminals[term];
                    if let Some(reduce_type_origin) = &term_info.reduce_type {
                        let reduce_type = reduce_type_origin.reduce_type;
                        let type_string = match reduce_type {
                            rusty_lr_core::ReduceType::Left => "%left",
                            rusty_lr_core::ReduceType::Right => "%right",
                        };
                        for (first, last) in reduce_type_origin.sources.iter() {
                            let range = first.byte_range().start..last.byte_range().end;
                            labels.push(Label::primary(file_id, range).with_message(format!(
                                "Reduce type was set as {} here",
                                type_string
                            )));
                        }
                    }

                    let term = grammar.terminals[term].name.to_string();
                    let mut notes = vec![format!("Trying to feed terminal: {}", term)];
                    match *reduce_type {
                        rusty_lr_core::ReduceType::Left => {
                            notes.push("Shift rule will be ignored".to_string());
                        }
                        rusty_lr_core::ReduceType::Right => {
                            notes.push("Reduce rule will be ignored".to_string());
                        }
                    }
                    let diag = Diagnostic::note()
                        .with_message(message)
                        .with_labels(labels)
                        .with_notes(notes);
                    diags.push(diag);
                }
            }

            for diag in diags.into_iter() {
                let writer = self.verbose_stream();
                let config = codespan_reporting::term::Config::default();
                term::emit(&mut writer.lock(), &config, &files, &diag)
                    .expect("Failed to write to stderr");
            }
        }

        // expand macro
        let expanded_stream = match grammar.emit_compiletime() {
            Ok(expanded_stream) => expanded_stream,
            Err(e) => {
                let diag = match e.as_ref() {
                    EmitError::RuleTypeDefinedButActionNotDefined {
                        name,
                        rule_local_id,
                    } => {
                        // `name` must not be generated rule,
                        // since it is programmically generated, it must have a proper reduce action

                        let rule_id = *grammar.nonterminals_index.get(name).unwrap();
                        let rule_line = &grammar.nonterminals[rule_id].rules[*rule_local_id];
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
        let rules_comments = dfa_builder
            .rules
            .iter()
            .map(|(rule, _)| rule.clone().map(&term_mapper, &nonterm_mapper).to_string())
            .collect::<Vec<_>>()
            .join("\n");
        let debug_comments = format!("{:=^80}\n{rules_comments}\n", "Grammar");

        Ok(output::Output {
            user_stream: output_stream,
            generated_stream: expanded_stream,
            debug_comments,
        })
    }
}

impl Default for Builder {
    fn default() -> Self {
        Self::new()
    }
}
