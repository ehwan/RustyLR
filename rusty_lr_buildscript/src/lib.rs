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

use proc_macro2::TokenStream;

use quote::quote;
use rusty_lr_parser::error::ArgError;
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

    /// print debug information about terminal class optimization
    verbose_optimization: bool,

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
            verbose_optimization: false,
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
        self.verbose_optimization();
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

    pub fn verbose_optimization(&mut self) -> &mut Self {
        self.verbose_optimization = true;
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
            Err(msg) => {
                panic!("{}", msg)
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
        let mut grammar = match rusty_lr_parser::grammar::Grammar::from_grammar_args(grammar_args) {
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
                        let old_range = old.0.byte_range();
                        let old_string = match old.1 {
                            rusty_lr_core::ReduceType::Left => "%left",
                            rusty_lr_core::ReduceType::Right => "%right",
                        };
                        let new_range = new.0.byte_range();
                        let new_string = match new.1 {
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
                    ParseError::UnsupportedLiteralType(literal) => {
                        let range = literal.span().byte_range();

                        Diagnostic::error()
                            .with_message("Unsupported literal type")
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message("This literal type is not supported")])
                            .with_notes(vec![
                                "If %tokentype is `char`, only `char` or `&str` are supported"
                                    .to_string(),
                                "If %tokentype is `u8`, only `u8` or `&[u8]` are supported"
                                    .to_string(),
                            ])
                    }
                    ParseError::InvalidLiteralRange(first, last) => {
                        let first_range = first.span().byte_range();
                        let last_range = last.span().byte_range();
                        let range = first_range.start..last_range.end;

                        Diagnostic::error()
                            .with_message("Invalid literal range")
                            .with_labels(vec![
                                Label::primary(file_id, range).with_message("Invalid range here"),
                            ])
                            .with_notes(vec![
                                "First terminal symbol has to be less than or equal to the last terminal symbol".to_string()
                            ])
                    }
                    ParseError::TokenInLiteralMode(span) => {
                        let range = span.byte_range();
                        Diagnostic::error()
                            .with_message("%token with %tokentype `char` or `u8` is not supported")
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message("use the literal value directly")])
                    }
                    ParseError::MultiplePrecedenceOrderDefinition { cur, old } => {
                        Diagnostic::error()
                            .with_message("Multiple operator precedence defined")
                            .with_labels(vec![
                                Label::primary(file_id, cur.span().byte_range())
                                    .with_message("defined here"),
                                Label::secondary(file_id, old.byte_range())
                                    .with_message("first defined here"),
                            ])
                            .with_notes(vec!["%prec name must be unique".to_string()])
                    }
                    ParseError::RuleTypeDefinedButActionNotDefined { name, span } => {
                        // `name` must not be generated rule,
                        // since it is programmically generated, it must have a proper reduce action
                        let span = span.0.byte_range().start..span.1.byte_range().end;
                        Diagnostic::error()
                            .with_message("Reduce action not defined")
                            .with_labels(vec![
                                Label::secondary(file_id, name.span().byte_range())
                                    .with_message("This rule has a type definition"),
                                Label::primary(file_id, span)
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

        // diagnostics for optimization
        if grammar.optimize {
            use rusty_lr_parser::grammar::OptimizeRemove;
            let optimized = grammar.optimize(10);

            if self.verbose_optimization {
                // terminals merged into terminal class
                let mut class_message = Vec::new();
                for (class_idx, class_def) in grammar.terminal_classes.iter().enumerate() {
                    if class_def.terminals.len() == 1 {
                        continue;
                    }
                    let msg = format!(
                        "TerminalClass{}: {}",
                        class_def.multiterm_counter,
                        grammar.class_pretty_name_list(class_idx, 10)
                    );
                    class_message.push(msg);
                }
                if !class_message.is_empty() {
                    let diag = Diagnostic::note()
                        .with_message("These terminals are merged into terminal class".to_string())
                        .with_notes(class_message);

                    let writer = StandardStream::stdout(ColorChoice::Auto);
                    let config = codespan_reporting::term::Config::default();
                    term::emit(&mut writer.lock(), &config, &files, &diag)
                        .expect("Failed to write to stdout");
                }

                for o in optimized.removed {
                    match o {
                        OptimizeRemove::TerminalClassRuleMerge(rule) => {
                            let message = "Production Rule deleted";
                            let (b, e) = rule.span_pair();
                            let range = b.byte_range().start..e.byte_range().end;
                            let mut labels = Vec::new();
                            labels
                                .push(Label::primary(file_id, range).with_message("defined here"));
                            let mut notes = Vec::new();
                            notes.push("Will be merged into rule using terminal class".to_string());
                            let diag = Diagnostic::note()
                                .with_message(message)
                                .with_labels(labels)
                                .with_notes(notes);

                            let writer = self.verbose_stream();
                            let config = codespan_reporting::term::Config::default();
                            term::emit(&mut writer.lock(), &config, &files, &diag)
                                .expect("Failed to write to verbose stream");
                        }
                        OptimizeRemove::SingleNonTerminalRule(rule, nonterm_span) => {
                            let message = "NonTerminal deleted";
                            let mut labels = Vec::new();
                            let mut notes = Vec::new();
                            notes.push(
                                "This non-terminal will be replaced by terminal class".to_string(),
                            );

                            labels.push(
                                Label::primary(file_id, nonterm_span.byte_range())
                                    .with_message("non-terminal defined here"),
                            );

                            let (b, e) = rule.span_pair();
                            let rule_range = b.byte_range().start..e.byte_range().end;
                            labels.push(
                                Label::secondary(file_id, rule_range)
                                    .with_message("this rule only has one terminal class"),
                            );
                            let diag = Diagnostic::note()
                                .with_message(message)
                                .with_labels(labels)
                                .with_notes(notes);

                            let writer = self.verbose_stream();
                            let config = codespan_reporting::term::Config::default();
                            term::emit(&mut writer.lock(), &config, &files, &diag)
                                .expect("Failed to write to verbose stream");
                        }
                    }
                }

                // if other terminals were not used, print warning about removing them
                let other_terminal_class =
                    &grammar.terminal_classes[grammar.other_terminal_class_id];
                if !optimized.other_used && other_terminal_class.terminals.len() > 1 {
                    let class_name =
                        grammar.class_pretty_name_abbr(grammar.other_terminal_class_id);
                    let terms = grammar.class_pretty_name_list(grammar.other_terminal_class_id, 10);
                    let mut notes = Vec::new();
                    notes.push(format!("{class_name}: {terms}"));

                    let diag = Diagnostic::warning()
                        .with_message(
                            "These terminals are not used in the grammar, consider removing them",
                        )
                        .with_notes(notes);

                    let writer = self.verbose_stream();
                    let config = codespan_reporting::term::Config::default();
                    term::emit(&mut writer.lock(), &config, &files, &diag)
                        .expect("Failed to write to verbose stream");
                }
            }
        }

        grammar.builder = grammar.create_builder();
        grammar.build_grammar_without_resolve();

        let mut conflict_diags = Vec::new();
        let mut conflict_diags_resolved = Vec::new();

        let class_mapper = |class| grammar.class_pretty_name_list(class, 5);

        // calculate conflicts
        {
            use std::collections::BTreeSet;
            let mut shift_reduce_conflict_set = BTreeSet::new();

            for state in &grammar.states {
                use rusty_lr_core::builder::Operator;

                let mut both_in_reduce_shift = Vec::new();
                for term in state.reduce_map.keys().copied() {
                    if state.shift_goto_map_term.contains_key(&term) {
                        both_in_reduce_shift.push(term);
                    }
                }

                for shift_term in both_in_reduce_shift.into_iter() {
                    let reduce_rules = state.reduce_map.get(&shift_term).unwrap();

                    if !shift_reduce_conflict_set.insert((reduce_rules.clone(), shift_term)) {
                        continue;
                    }

                    let mut labels = Vec::new();
                    let Some(&(shift_prec_span, shift_level)) =
                        grammar.precedences.get(&Operator::Term(shift_term))
                    else {
                        continue;
                    };

                    let message = format!(
                        "Conflict resolved with terminal: {}",
                        class_mapper(shift_term)
                    );

                    let mut remove_shift = true;
                    let mut remove_reduces = Vec::new();
                    // if all operators in reduce rules have greater precedence than shift_term,
                    // remove shift action
                    //
                    // if all operators in reduce rules have less or equal precedence than shift_term,
                    // remove reduce rules which has less precedence than shift_term
                    //
                    // otherwise, do nothing
                    for &reduce_rule in reduce_rules {
                        use rusty_lr_core::Token;
                        use std::cmp::Ordering;

                        let (ruleinfo, localid) = grammar.get_rule_by_id(reduce_rule).unwrap();
                        let (reduce_op, op_origin) =
                            if let Some((reduce_op, prec_def)) = &ruleinfo.rules[localid].prec {
                                (*reduce_op, prec_def.byte_range())
                            } else {
                                let mut ret = None;
                                for token in ruleinfo.rules[localid].tokens.iter().rev() {
                                    if let Token::Term(term) = token.token {
                                        let s0 = token.begin_span.byte_range().start;
                                        let s1 = token.end_span.byte_range().end;
                                        ret = Some((Operator::Term(term), s0..s1));
                                        break;
                                    }
                                }
                                if let Some(ret) = ret {
                                    ret
                                } else {
                                    // no operator found
                                    Self::extend_rule_source_label(
                                        &mut labels,
                                        file_id,
                                        reduce_rule,
                                        &grammar,
                                        "(Reduce) ",
                                        "(Reduce) ",
                                    );
                                    remove_shift = false;
                                    continue;
                                }
                            };

                        let Some(&(precedence_defined_origin, reduce_level)) =
                            grammar.precedences.get(&reduce_op)
                        else {
                            // precedence not defined
                            Self::extend_rule_source_label(
                                &mut labels,
                                file_id,
                                reduce_rule,
                                &grammar,
                                "(Reduce) ",
                                "(Reduce) ",
                            );
                            labels.push(
                                Label::secondary(file_id, op_origin)
                                    .with_message(format!("(Reduce) operator for reduce rule")),
                            );
                            remove_shift = false;
                            continue;
                        };
                        match reduce_level.cmp(&shift_level) {
                            Ordering::Less => {
                                remove_shift = false;
                                remove_reduces.push((
                                    reduce_rule,
                                    precedence_defined_origin,
                                    reduce_level,
                                    op_origin,
                                    None,
                                ));
                                // reduce < shift => remove reduce rule
                            }
                            Ordering::Greater => {
                                // reduce > shift => remove shift
                                Self::extend_rule_source_label(
                                    &mut labels,
                                    file_id,
                                    reduce_rule,
                                    &grammar,
                                    "(Reduce) ",
                                    "(Reduce) ",
                                );
                                labels
                                    .push(Label::secondary(file_id, op_origin).with_message(
                                        format!("(Reduce) operator for reduce rule"),
                                    ));
                                labels.push(
                                    Label::secondary(
                                        file_id,
                                        precedence_defined_origin.byte_range(),
                                    )
                                    .with_message(format!(
                                        "(Reduce) Precedence was set as {reduce_level} here"
                                    )),
                                );
                            }
                            Ordering::Equal => {
                                let reduce_info = match reduce_op {
                                    Operator::Term(term) => {
                                        let term_info = &grammar.terminals[term];
                                        term_info.reduce_type.as_ref()
                                    }
                                    Operator::Prec(prec) => {
                                        let prec_info = &grammar.prec_defeinitions[prec];
                                        prec_info.reduce_type.as_ref()
                                    }
                                };
                                if let Some(reduce_info) = reduce_info {
                                    match reduce_info.reduce_type {
                                        rusty_lr_core::ReduceType::Left => {
                                            // reduce == shift => remove shift

                                            Self::extend_rule_source_label(
                                                &mut labels,
                                                file_id,
                                                reduce_rule,
                                                &grammar,
                                                "(Reduce) ",
                                                "(Reduce) ",
                                            );
                                            labels.push(
                                                Label::secondary(file_id, op_origin).with_message(
                                                    format!("(Reduce) operator for reduce rule"),
                                                ),
                                            );
                                            labels.push(
                                                Label::secondary(
                                                    file_id,
                                                    precedence_defined_origin.byte_range(),
                                                )
                                                .with_message(format!(
                                        "(Reduce) Precedence was set as {reduce_level} here"
                                    )),
                                            );

                                            labels.push(
                                                Label::secondary(
                                                    file_id,
                                                    reduce_info.source.byte_range(),
                                                )
                                                .with_message(format!(
                                                    "(Reduce) Reduce type was set as %left here"
                                                )),
                                            );
                                        }
                                        rusty_lr_core::ReduceType::Right => {
                                            // reduce == shift => remove reduce
                                            remove_shift = false;
                                            remove_reduces.push((
                                                reduce_rule,
                                                precedence_defined_origin,
                                                reduce_level,
                                                op_origin,
                                                Some((reduce_info.reduce_type, reduce_info.source)),
                                            ));
                                        }
                                    }
                                } else {
                                    remove_shift = false;
                                }
                            }
                        }
                    }

                    if remove_shift {
                        labels.push(
                            Label::primary(file_id, shift_prec_span.byte_range()).with_message(
                                format!(
                                    "[Removed] (Shift) precedence was set as {shift_level} here"
                                ),
                            ),
                        );

                        // remove shift
                        let next_state = *state.shift_goto_map_term.get(&shift_term).unwrap();
                        for shift_rule in grammar.states[next_state].unshifted_ruleset() {
                            Self::extend_rule_source_label(
                                &mut labels,
                                file_id,
                                shift_rule.rule,
                                &grammar,
                                "[Removed] (Shift) ",
                                "[Removed] (Shift) ",
                            );
                        }
                    } else {
                        labels.push(
                            Label::primary(file_id, shift_prec_span.byte_range()).with_message(
                                format!("(Shift) precedence was set as {shift_level} here"),
                            ),
                        );

                        // remove shift
                        let next_state = *state.shift_goto_map_term.get(&shift_term).unwrap();
                        for shift_rule in grammar.states[next_state].unshifted_ruleset() {
                            Self::extend_rule_source_label(
                                &mut labels,
                                file_id,
                                shift_rule.rule,
                                &grammar,
                                "(Shift) ",
                                "(Shift) ",
                            );
                        }
                    }

                    for (
                        reduce_rule,
                        precedence_defined_op_origin,
                        reduce_level,
                        op_origin,
                        reduce_type,
                    ) in remove_reduces
                    {
                        // remove reduce rule
                        Self::extend_rule_source_label(
                            &mut labels,
                            file_id,
                            reduce_rule,
                            &grammar,
                            "[Removed] (Reduce) ",
                            "[Removed] (Reduce) ",
                        );

                        labels.push(
                            Label::secondary(file_id, op_origin).with_message(format!(
                                "[Removed] (Reduce) operator for reduce rule"
                            )),
                        );
                        labels.push(
                            Label::secondary(file_id, precedence_defined_op_origin.byte_range())
                                .with_message(format!(
                                    "[Removed] (Reduce) Precedence was set as {reduce_level} here"
                                )),
                        );

                        if let Some((reduce_type, span)) = reduce_type {
                            let reduce_type_str = match reduce_type {
                                rusty_lr_core::ReduceType::Left => "%left",
                                rusty_lr_core::ReduceType::Right => "%right",
                            };
                            labels.push(Label::secondary(file_id, span.byte_range()).with_message(
                                format!(
                                "[Removed] (Reduce) Reduce type was set as {reduce_type_str} here"
                            ),
                            ));
                        }
                    }

                    let notes = Vec::new();
                    conflict_diags_resolved.push(
                        Diagnostic::note()
                            .with_message(message)
                            .with_labels(labels)
                            .with_notes(notes),
                    );
                }
            }
        }
        grammar.resolve_precedence();

        let nonterm_mapper = |nonterm| grammar.nonterm_pretty_name(nonterm);
        let class_mapper = |class| grammar.class_pretty_name_list(class, 5);

        {
            use std::collections::BTreeSet;
            let mut shift_reduce_conflict_set = BTreeSet::new();
            let mut reduce_reduce_conflict_set = BTreeSet::new();

            for state in &grammar.states {
                for (&reduce_term, reduce_rules) in state.reduce_map.iter() {
                    if let Some(&next_state) = state.shift_goto_map_term.get(&reduce_term) {
                        let shifted_rules: Vec<_> =
                            grammar.states[next_state].unshifted_ruleset().collect();

                        if !shift_reduce_conflict_set.insert((
                            shifted_rules.clone(),
                            reduce_rules.clone(),
                            reduce_term,
                        )) {
                            continue;
                        }

                        let message = format!(
                            "Conflict detected with terminal: {}",
                            class_mapper(reduce_term)
                        );
                        let mut labels = Vec::new();
                        let notes = Vec::new();

                        for shift_rule in shifted_rules {
                            Self::extend_rule_source_label(
                                &mut labels,
                                file_id,
                                shift_rule.rule,
                                &grammar,
                                "(Shift) ",
                                "(Shift) ",
                            );
                        }
                        for &reduce_rule in reduce_rules {
                            Self::extend_rule_source_label(
                                &mut labels,
                                file_id,
                                reduce_rule,
                                &grammar,
                                "(Reduce) ",
                                "(Reduce) ",
                            );
                        }

                        conflict_diags.push(
                            Diagnostic::error()
                                .with_message(message)
                                .with_labels(labels)
                                .with_notes(notes),
                        );
                    } else if reduce_rules.len() > 1 {
                        if !reduce_reduce_conflict_set.insert((reduce_rules.clone(), reduce_term)) {
                            continue;
                        }

                        let message = format!(
                            "Conflict detected with terminal: {}",
                            class_mapper(reduce_term)
                        );
                        let mut labels = Vec::new();
                        let notes = Vec::new();

                        for &reduce_rule in reduce_rules {
                            Self::extend_rule_source_label(
                                &mut labels,
                                file_id,
                                reduce_rule,
                                &grammar,
                                "(Reduce) ",
                                "(Reduce) ",
                            );
                        }

                        conflict_diags.push(
                            Diagnostic::error()
                                .with_message(message)
                                .with_labels(labels)
                                .with_notes(notes),
                        );
                    }
                }
            }
        }

        // print note about shift/reduce conflict resolved with `%left` or `%right`
        if self.verbose_conflicts_resolving {
            for diag in conflict_diags_resolved.into_iter() {
                let writer = self.verbose_stream();
                let config = codespan_reporting::term::Config::default();
                term::emit(&mut writer.lock(), &config, &files, &diag)
                    .expect("Failed to write to verbose stream");
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

        // expand macro
        let expanded_stream = grammar.emit_compiletime();

        // this comments will be printed to the output file
        // build again here whether it was built before
        // since many informations are removed in the rusty_lr_parser output
        let rules_comments = grammar
            .builder
            .rules
            .iter()
            .map(|rule| {
                rule.rule
                    .clone()
                    .map(&class_mapper, &nonterm_mapper)
                    .to_string()
            })
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
