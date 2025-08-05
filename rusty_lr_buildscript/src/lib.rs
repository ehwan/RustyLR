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
use rusty_lr_core::TerminalSymbol;
use rusty_lr_parser::error::ArgError;
use rusty_lr_parser::error::ParseArgError;
use rusty_lr_parser::error::ParseError;

use std::fs::read;
use std::fs::write;

/// reexport
pub use rusty_lr_parser::target_rusty_lr_version;

/// Main entry for the build script
pub struct Builder {
    /// input_file to read
    input_file: Option<String>,

    /// Print note information about any shift/reduce, reduce/reduce conflicts.
    /// If the target is deterministic parser, conflict will be treated as an error,
    /// so this option will be ignored.
    /// This option is only for non-deterministic GLR parser.
    note_conflicts: bool,

    /// Print debug information about conflicts resolving process by any `%left`, `%right`, or `%precedence` directive.
    note_conflicts_resolving: bool,

    /// Print debug information about optimization process.
    note_optimization: bool,

    /// Print every `note_*` information to stderr.
    note_on_stderr: bool,

    /// Print backtrace of production rules when conflicts occurred. ruleset could be messed up
    note_backtrace: bool,

    /// if true, an executable called this function
    pub is_executable: bool,

    /// if Some, override the settings with these values
    glr: Option<bool>,
    runtime: Option<bool>,
    dense: Option<bool>,
}

impl Builder {
    pub fn new() -> Self {
        Self {
            input_file: None,
            note_conflicts: true,
            note_conflicts_resolving: true,
            note_on_stderr: false,
            note_optimization: true,
            note_backtrace: true,
            is_executable: false,

            glr: None,
            runtime: None,
            dense: None,
        }
    }

    /// override the settings
    pub fn glr(&mut self, glr: bool) -> &mut Self {
        self.glr = Some(glr);
        self
    }
    /// override the settings
    pub fn runtime(&mut self, runtime: bool) -> &mut Self {
        self.runtime = Some(runtime);
        self
    }
    /// override the settings
    pub fn dense(&mut self, dense: bool) -> &mut Self {
        self.dense = Some(dense);
        self
    }

    /// set input file
    pub fn file(&mut self, filename: &str) -> &mut Self {
        self.input_file = Some(filename.to_string());
        self
    }

    /// Print note information about any shift/reduce, reduce/reduce conflicts.
    /// If the target is deterministic parser, conflict will be treated as an error,
    /// so this option will be ignored.
    /// This option is only for non-deterministic GLR parser.
    pub fn note_conflicts(&mut self, val: bool) -> &mut Self {
        self.note_conflicts = val;
        self
    }

    /// Print debug information about conflicts resolving process by any `%left`, `%right`, or `%precedence` directive.
    pub fn note_conflicts_resolving(&mut self, val: bool) -> &mut Self {
        self.note_conflicts_resolving = val;
        self
    }

    /// Print debug information about optimization process.
    pub fn note_optimization(&mut self, val: bool) -> &mut Self {
        self.note_optimization = val;
        self
    }

    /// Print every `note_*` information to stderr.
    pub fn note_on_stderr(&mut self, val: bool) -> &mut Self {
        self.note_on_stderr = val;
        self
    }

    /// Print backtrace of production rules when conflicts occurred. ruleset could be messed up
    pub fn note_backtrace(&mut self, val: bool) -> &mut Self {
        self.note_backtrace = val;
        self
    }

    fn note_stream(&self) -> StandardStream {
        if self.note_on_stderr {
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
        message_secondary: &str,
    ) {
        let (nonterm, local_rule) = grammar.get_rule_by_id(ruleid).expect("Rule not found");
        if let Some(origin_span) = nonterm.origin_span() {
            let origin_range = origin_span.0.byte_range().start..origin_span.1.byte_range().end;
            let message = format!("{}{} was generated here", prefix_str, nonterm.pretty_name,);
            let mut duplicated_primary = false;
            for label in labels.iter() {
                if label.range == origin_range && label.message == message {
                    duplicated_primary = true;
                    break;
                }
            }
            if !duplicated_primary {
                labels.push(Label::primary(fileid, origin_range).with_message(message));
            }
        } else {
            let (rule_begin, rule_end) = nonterm.rules[local_rule].span_pair();
            let rule_range = rule_begin.byte_range().start..rule_end.byte_range().end;
            let origin_range = nonterm.name.span().byte_range();

            let primary_message = format!("{}{} was defined here", prefix_str, nonterm.pretty_name);
            let mut duplicated_primary = false;
            let mut duplicated_secondary = false;
            for label in labels.iter() {
                if label.range == origin_range && label.message == primary_message {
                    duplicated_primary = true;
                    break;
                }
            }
            for label in labels.iter() {
                if label.range == rule_range && label.message == message_secondary {
                    duplicated_secondary = true;
                    break;
                }
            }
            if !duplicated_primary {
                labels.push(Label::primary(fileid, origin_range).with_message(primary_message));
            }
            if !duplicated_secondary {
                labels.push(
                    Label::secondary(fileid, rule_range)
                        .with_message(message_secondary.to_string()),
                );
            }
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

        let mut grammar_args = match rusty_lr_parser::grammar::Grammar::parse_args(macro_stream) {
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

        for error in &grammar_args.error_recovered {
            let range = if let Some((first, last)) = error.span.pair {
                let first_range = first.byte_range();
                let last_range = last.byte_range();
                first_range.start..last_range.end
            } else {
                0..1 // default range if span is not defined
            };
            let diag = Diagnostic::error()
                .with_message("Syntax error in grammar")
                .with_labels(vec![
                    Label::primary(file_id, range).with_message(error.message.clone())
                ])
                .with_notes(vec![format!("refer to: {}", error.link)]);
            let writer = StandardStream::stderr(ColorChoice::Auto);
            let config = codespan_reporting::term::Config::default();
            term::emit(&mut writer.lock(), &config, &files, &diag)
                .expect("Failed to write to stderr");
        }

        if !grammar_args.error_recovered.is_empty() {
            return Err("Syntax error in grammar".to_string());
        }

        match rusty_lr_parser::grammar::Grammar::arg_check_error(&mut grammar_args) {
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

                    ArgError::MultiplePrecDefinition(span) => Diagnostic::error()
                        .with_message("multiple %prec definition")
                        .with_labels(vec![Label::primary(file_id, span.byte_range())
                            .with_message("This %prec is defined here")])
                        .with_notes(vec!["%prec must be unique".to_string()]),

                    ArgError::MultipleDPrecDefinition(span) => Diagnostic::error()
                        .with_message("multiple %dprec definition")
                        .with_labels(vec![Label::primary(file_id, span.byte_range())
                            .with_message("This %dprec is defined here")])
                        .with_notes(vec!["%dprec must be unique".to_string()]),
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

        if let Some(glr) = self.glr {
            grammar_args.glr = glr;
        }
        if let Some(runtime) = self.runtime {
            grammar_args.compiled = !runtime;
        }
        if let Some(dense) = self.dense {
            grammar_args.dense = dense;
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
                            rusty_lr_core::builder::ReduceType::Left => "%left",
                            rusty_lr_core::builder::ReduceType::Right => "%right",
                        };
                        let new_range = new.0.byte_range();
                        let new_string = match new.1 {
                            rusty_lr_core::builder::ReduceType::Left => "%left",
                            rusty_lr_core::builder::ReduceType::Right => "%right",
                        };

                        Diagnostic::error()
                            .with_message(format!("Multiple reduce definition: {}", terminal))
                            .with_labels(vec![
                                Label::primary(file_id, old_range)
                                    .with_message(format!("was set as {} here", old_string)),
                                Label::primary(file_id, new_range)
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

                    ParseError::ReservedName(ident) => {
                        let range = ident.span().byte_range();

                        Diagnostic::error()
                            .with_message(format!("'{ident}' is reserved name"))
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message("This name is reserved")])
                    }
                    ParseError::UnsupportedLiteralType(literal) => {
                        let range = literal.into_iter().next().unwrap().span().byte_range();

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
                    ParseError::PrecedenceNotDefined(ident) => {
                        let range = ident.span().byte_range();
                        Diagnostic::error()
                            .with_message("Precedence is not defined for this token")
                            .with_labels(vec![
                                Label::primary(file_id, range).with_message("token used here")
                            ])
                            .with_notes(vec![
                                "use %left, %right, or %precedence to define precedence"
                                    .to_string(),
                                "refer to https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#operator-precedence".to_string()
                            ])
                    }
                    ParseError::NonTerminalPrecedenceNotDefined(span, _) => {
                        let range = span.byte_range();
                        Diagnostic::error()
                            .with_message("Precedence is not defined for this non-terminal")
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message(format!("non-terminal used here"))])
                            .with_notes(vec![
                                "Every production rule of this non-terminal must have a precedence defined"
                                    .to_string(),
                                "use %left, %right, or %precedence to define precedence"
                                    .to_string(),
                            ])
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

                    ParseError::OnlyTerminalSet(span_begin, span_end) => {
                        let range = span_begin.byte_range().start..span_end.byte_range().end;
                        Diagnostic::error()
                            .with_message("Only terminal or terminal set is allowed")
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message("This pattern is not terminal")])
                            .with_notes(vec!["".to_string()])
                    }
                    ParseError::NonTerminalNotDefined(ident) => {
                        let range = ident.span().byte_range();
                        Diagnostic::error()
                            .with_message("Non-terminal not defined")
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message("This non-terminal is not defined")])
                    }
                    ParseError::OnlyUsizeLiteral(span) => {
                        let range = span.byte_range();
                        Diagnostic::error()
                            .with_message("Only usize literal is allowed for %dprec")
                            .with_labels(vec![Label::primary(file_id, range)])
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

            if self.note_optimization {
                // terminals merged into terminal class
                let mut class_message = Vec::new();
                for (class_idx, class_def) in grammar.terminal_classes.iter().enumerate() {
                    let len: usize = class_def
                        .terminals
                        .iter()
                        .map(|term| grammar.terminals[*term].name.count())
                        .sum();
                    if len == 1 {
                        continue;
                    }
                    let msg = format!(
                        "TerminalClass{}: {}",
                        class_def.multiterm_counter,
                        grammar.class_pretty_name_list(TerminalSymbol::Term(class_idx), 10)
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
                            let labels =
                                vec![Label::primary(file_id, range).with_message("defined here")];
                            let notes =
                                vec!["Will be merged into rule using terminal class".to_string()];
                            let diag = Diagnostic::note()
                                .with_message(message)
                                .with_labels(labels)
                                .with_notes(notes);

                            let writer = self.note_stream();
                            let config = codespan_reporting::term::Config::default();
                            term::emit(&mut writer.lock(), &config, &files, &diag)
                                .expect("Failed to write to verbose stream");
                        }
                        OptimizeRemove::SingleNonTerminalRule(rule, nonterm_span) => {
                            let message = "NonTerminal deleted";
                            let mut labels = Vec::new();
                            let notes = vec![
                                "This non-terminal will be replaced by it's unique child rule"
                                    .to_string(),
                            ];

                            labels.push(
                                Label::primary(file_id, nonterm_span.byte_range())
                                    .with_message("non-terminal defined here"),
                            );

                            let (b, e) = rule.span_pair();
                            let rule_range = b.byte_range().start..e.byte_range().end;
                            labels.push(
                                Label::secondary(file_id, rule_range)
                                    .with_message("this rule has only one child rule"),
                            );
                            let diag = Diagnostic::note()
                                .with_message(message)
                                .with_labels(labels)
                                .with_notes(notes);

                            let writer = self.note_stream();
                            let config = codespan_reporting::term::Config::default();
                            term::emit(&mut writer.lock(), &config, &files, &diag)
                                .expect("Failed to write to verbose stream");
                        }
                        OptimizeRemove::NonTermNotUsed(span) => {
                            let message = "NonTerminal deleted";
                            let mut labels = Vec::new();
                            let notes =
                                vec!["This non-terminal cannot be reached from initial state"
                                    .to_string()];

                            labels.push(
                                Label::primary(file_id, span.byte_range())
                                    .with_message("non-terminal defined here"),
                            );

                            let diag = Diagnostic::note()
                                .with_message(message)
                                .with_labels(labels)
                                .with_notes(notes);

                            let writer = self.note_stream();
                            let config = codespan_reporting::term::Config::default();
                            term::emit(&mut writer.lock(), &config, &files, &diag)
                                .expect("Failed to write to verbose stream");
                        }
                        OptimizeRemove::Cycle(span) => {
                            let message = "Cycle detected";
                            let mut labels = Vec::new();
                            let notes =
                                vec!["This non-terminal is involved in bad cycle".to_string()];

                            labels.push(
                                Label::primary(file_id, span.byte_range())
                                    .with_message("non-terminal defined here"),
                            );

                            let diag = Diagnostic::warning()
                                .with_message(message)
                                .with_labels(labels)
                                .with_notes(notes);

                            let writer = self.note_stream();
                            let config = codespan_reporting::term::Config::default();
                            term::emit(&mut writer.lock(), &config, &files, &diag)
                                .expect("Failed to write to verbose stream");
                        }
                    }
                }

                // if other terminals were not used, print warning about removing them
                let other_terminal_class =
                    &grammar.terminal_classes[grammar.other_terminal_class_id];
                if !grammar.other_used && other_terminal_class.terminals.len() > 1 {
                    let class_name =
                        grammar.class_pretty_name_abbr(grammar.other_terminal_class_id);
                    let terms = grammar.class_pretty_name_list(
                        TerminalSymbol::Term(grammar.other_terminal_class_id),
                        10,
                    );
                    let mut notes = Vec::new();
                    notes.push(format!("{class_name}: {terms}"));

                    let diag = Diagnostic::warning()
                        .with_message(
                            "These terminals are not used in the grammar, consider removing them",
                        )
                        .with_notes(notes);

                    let writer = self.note_stream();
                    let config = codespan_reporting::term::Config::default();
                    term::emit(&mut writer.lock(), &config, &files, &diag)
                        .expect("Failed to write to verbose stream");
                }
            }
        }

        grammar.builder = grammar.create_builder();
        let diags_collector = grammar.build_grammar();

        let mut conflict_diags = Vec::new();
        let mut conflict_diags_resolved = Vec::new();
        let nonterm_mapper = |nonterm| grammar.nonterm_pretty_name(nonterm);
        let class_mapper = |class| grammar.class_pretty_name_list(class, 5);

        // calculate conflicts
        for (max_priority, reduce_rules, deleted_rules) in diags_collector.reduce_reduce_resolved {
            let mut labels = Vec::new();
            for rule in reduce_rules {
                let priority = grammar.builder.rules[rule].priority;
                Self::extend_rule_source_label(
                    &mut labels,
                    file_id,
                    rule,
                    &grammar,
                    "(Reduce) ",
                    format!("(Reduce) rule with the highest priority: {priority}").as_str(),
                );
            }
            for del in deleted_rules {
                let priority = grammar.builder.rules[del].priority;
                Self::extend_rule_source_label(
                    &mut labels,
                    file_id,
                    del,
                    &grammar,
                    "[Removed] (Reduce) ",
                    format!("[Removed] (Reduce) rule with lower priority: {priority}").as_str(),
                );
            }

            let message = "Reduce/Reduce conflict resolved";
            let notes = vec![
                format!("Max priority: {max_priority}"),
                "Set priority for the rule with %dprec".to_string(),
            ];
            conflict_diags_resolved.push(
                Diagnostic::note()
                    .with_message(message)
                    .with_labels(labels)
                    .with_notes(notes),
            );
        }
        for ((term, shift_rules), (shift_prec, reduce_rules)) in
            diags_collector.shift_reduce_resolved_shift
        {
            let mut labels = Vec::new();
            // shift_prec >= reduce_prec

            for (reduce_rule, reduce_prec) in reduce_rules {
                let (nonterm_info, local_id) = grammar.get_rule_by_id(reduce_rule).unwrap();
                let rule_info = &nonterm_info.rules[local_id];
                if shift_prec > reduce_prec {
                    Self::extend_rule_source_label(
                        &mut labels,
                        file_id,
                        reduce_rule,
                        &grammar,
                        "[Removed] (Reduce) ",
                        format!("[Removed] (Reduce) lower precedence than shift: {reduce_prec}")
                            .as_str(),
                    );
                } else {
                    let reduce_type = "%right";
                    Self::extend_rule_source_label(
                        &mut labels,
                        file_id,
                        reduce_rule,
                        &grammar,
                        "[Removed] (Reduce) ",
                        format!("[Removed] (Reduce) has {reduce_type} associativity").as_str(),
                    );
                }
                if !nonterm_info.is_auto_generated() {
                    let op_range = rule_info.prec.unwrap().1.byte_range();
                    labels.push(
                        Label::secondary(file_id, op_range)
                            .with_message("[Removed] (Reduce) operator for reduce rule"),
                    );
                }
            }
            for shift_rule in shift_rules {
                Self::extend_rule_source_label(
                    &mut labels,
                    file_id,
                    shift_rule.rule,
                    &grammar,
                    "(Shift) ",
                    format!("(Shift) precedence: {shift_prec}").as_str(),
                );
            }

            let message = format!(
                "Shift/Reduce conflict resolved with terminal(class): {}",
                grammar.class_pretty_name_list(term, 5)
            );
            let notes = vec![
                        "Operator of production rule is the rightmost terminal symbol with precedence defined".to_string(),
                        "Set operator for rule explicitly with %prec".to_string(),
                        "Set precedence for operator with %left, %right, or %precedence"
                            .to_string(),
                    ];
            conflict_diags_resolved.push(
                Diagnostic::note()
                    .with_message(message)
                    .with_labels(labels)
                    .with_notes(notes),
            );
        }
        for ((term, shift_rules), (shift_prec, reduce_rules)) in
            diags_collector.shift_reduce_resolved_reduce
        {
            let mut labels = Vec::new();

            for (reduce_rule, reduce_prec) in reduce_rules {
                let (nonterm_info, local_id) = grammar.get_rule_by_id(reduce_rule).unwrap();
                let rule_info = &nonterm_info.rules[local_id];
                if reduce_prec > shift_prec {
                    Self::extend_rule_source_label(
                        &mut labels,
                        file_id,
                        reduce_rule,
                        &grammar,
                        "(Reduce) ",
                        format!("(Reduce) higher precedence than shift: {reduce_prec}").as_str(),
                    );
                } else {
                    let reduce_type = "%left";
                    Self::extend_rule_source_label(
                        &mut labels,
                        file_id,
                        reduce_rule,
                        &grammar,
                        "(Reduce) ",
                        format!("(Reduce) has {reduce_type} associativity").as_str(),
                    );
                }

                if !nonterm_info.is_auto_generated() {
                    let op_range = rule_info.prec.unwrap().1.byte_range();
                    labels.push(
                        Label::secondary(file_id, op_range)
                            .with_message("(Reduce) operator for reduce rule"),
                    );
                }
            }

            for shift_rule in shift_rules {
                Self::extend_rule_source_label(
                    &mut labels,
                    file_id,
                    shift_rule.rule,
                    &grammar,
                    "[Removed] (Shift) ",
                    format!("[Removed] (Shift) lower precedence than reduce: {shift_prec}")
                        .as_str(),
                );
            }

            let message = format!(
                "Shift/Reduce conflict resolved with terminal(class): {}",
                grammar.class_pretty_name_list(term, 5)
            );
            let notes = vec![
                        "Operator of production rule is the rightmost terminal symbol with precedence defined".to_string(),
                        "Set operator for rule explicitly with %prec".to_string(),
                        "Set precedence for operator with %left, %right, or %precedence"
                            .to_string(),
                    ];
            conflict_diags_resolved.push(
                Diagnostic::note()
                    .with_message(message)
                    .with_labels(labels)
                    .with_notes(notes),
            );
        }

        for ((term, shift_rules, shift_rules_backtrace), reduce_rules) in
            diags_collector.shift_reduce_conflicts
        {
            let mut labels = Vec::new();
            let mut notes = vec![
                        "Operator of production rule is the rightmost terminal symbol with precedence defined".to_string(),
                        "Set operator for rule explicitly with %prec".to_string(),
                        "Set precedence for operator with %left, %right, or %precedence"
                            .to_string(),
                    ];

            if self.note_backtrace {
                if self.is_executable {
                    notes.push("--no-backtrace to disable backtracing".to_string());
                }
                notes.push("Backtrace for the shift rule:".to_string());
                for shift_rule in shift_rules_backtrace {
                    let rule_str = grammar.builder.rules[shift_rule.rule]
                        .rule
                        .clone()
                        .map(class_mapper, nonterm_mapper)
                        .into_shifted(shift_rule.shifted);
                    notes.push(format!("\t>>> {rule_str}"));
                }
            }
            for shift_rule in shift_rules {
                Self::extend_rule_source_label(
                    &mut labels,
                    file_id,
                    shift_rule.rule,
                    &grammar,
                    "(Shift) ",
                    "(Shift) ",
                );
            }
            for (reduce_rule, reduce_rule_backtrace) in reduce_rules {
                Self::extend_rule_source_label(
                    &mut labels,
                    file_id,
                    reduce_rule,
                    &grammar,
                    "(Reduce) ",
                    "(Reduce) ",
                );

                if self.note_backtrace {
                    let name = nonterm_mapper(grammar.builder.rules[reduce_rule].rule.name);

                    notes.push(format!("Backtrace for the reduce rule ({name}):"));
                    notes.extend(reduce_rule_backtrace.into_iter().map(|shifted_rule| {
                        let rule_str = grammar.builder.rules[shifted_rule.rule]
                            .rule
                            .clone()
                            .map(class_mapper, nonterm_mapper)
                            .into_shifted(shifted_rule.shifted);

                        format!("\t>>> {rule_str}")
                    }));
                }
            }

            let message = format!(
                "Shift/Reduce conflict detected with terminal(class): {}",
                grammar.class_pretty_name_list(term, 5)
            );

            conflict_diags.push(
                Diagnostic::error()
                    .with_message(message)
                    .with_labels(labels)
                    .with_notes(notes),
            );
        }
        for (reduce_rules, reduce_terms) in diags_collector.reduce_reduce_conflicts {
            let mut labels = Vec::new();

            let mut notes = vec!["Set priority for the rule with %dprec".to_string()];
            for (reduce_rule, reduce_rule_from) in reduce_rules {
                Self::extend_rule_source_label(
                    &mut labels,
                    file_id,
                    reduce_rule,
                    &grammar,
                    "(Reduce) ",
                    "(Reduce) ",
                );

                if self.note_backtrace {
                    if self.is_executable {
                        notes.push("--no-backtrace to disable backtracing".to_string());
                    }
                    let name = nonterm_mapper(grammar.builder.rules[reduce_rule].rule.name);

                    notes.push(format!("Backtrace for the reduce rule ({name}):"));
                    notes.extend(reduce_rule_from.into_iter().map(|shifted_rule| {
                        let rule_str = grammar.builder.rules[shifted_rule.rule]
                            .rule
                            .clone()
                            .map(class_mapper, nonterm_mapper)
                            .into_shifted(shifted_rule.shifted);

                        format!("\t>>> {rule_str}")
                    }));
                }
            }

            let message = format!(
                "Reduce/Reduce conflict detected with terminals: {}",
                reduce_terms
                    .into_iter()
                    .map(class_mapper)
                    .collect::<Vec<_>>()
                    .join(", ")
            );

            conflict_diags.push(
                Diagnostic::error()
                    .with_message(message)
                    .with_labels(labels)
                    .with_notes(notes),
            );
        }

        // print note about shift/reduce conflict resolved with `%left` or `%right`
        if self.note_conflicts_resolving {
            for diag in conflict_diags_resolved.into_iter() {
                let writer = self.note_stream();
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
        else if self.note_conflicts {
            for diag in conflict_diags.into_iter() {
                let diag = Diagnostic::note()
                    .with_message(diag.message)
                    .with_labels(diag.labels)
                    .with_notes(diag.notes);
                let writer = self.note_stream();
                let config = codespan_reporting::term::Config::default();
                term::emit(&mut writer.lock(), &config, &files, &diag)
                    .expect("Failed to write to stderr");
            }
        }

        // expand macro
        let expanded_stream = grammar.emit_compiletime();

        let num_classes = grammar.terminal_classes.len();
        let num_states = grammar.states.len();

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
                    .map(class_mapper, nonterm_mapper)
                    .to_string()
            })
            .collect::<Vec<_>>()
            .join("\n");
        let debug_comments = format!(
            "{:=^80}\n
# of terminal classes: {num_classes}\n# of states: {num_states}\n
{rules_comments}\n",
            "Grammar"
        );

        println!("# of terminal classes: {num_classes}");
        println!("# of states: {num_states}");

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
