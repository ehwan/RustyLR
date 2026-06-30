//! Build script for rusty_lr
//!
//! This crate is private and not intended to be used directly.
//! Please use the [`rusty_lr`](https://crates.io/crates/rusty_lr) crate instead.
//!
//! ```ignore
//! fn main() {
//!     println!("cargo::rerun-if-changed=src/parser/parser.rustylr");
//!
//!     let output_dir = std::env::var("OUT_DIR").unwrap();
//!     let output = format!("{}/parser.rs", output_dir);
//!     Builder::new()
//!        .file("src/parser/parser.rustylr")
//!        .build(&output);
//! }
//!

pub mod output;
pub use rusty_lr_parser::TableLayout;
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

    /// Print to stderr.
    stderr: bool,

    /// Print backtrace of production rules when conflicts occurred. ruleset could be messed up
    note_backtrace: bool,

    /// if true, an executable called this function
    pub is_executable: bool,

    /// if Some, override the settings with these values
    glr: Option<bool>,
    layout: Option<rusty_lr_parser::TableLayout>,
    dense_limit: Option<usize>,
}

impl Builder {
    pub fn new() -> Self {
        Self {
            input_file: None,
            note_conflicts: true,
            note_conflicts_resolving: true,
            stderr: false,
            note_optimization: true,
            note_backtrace: true,
            is_executable: false,

            glr: None,
            layout: None,
            dense_limit: None,
        }
    }

    /// override the settings
    pub fn glr(&mut self, glr: bool) -> &mut Self {
        self.glr = Some(glr);
        self
    }
    /// override the settings
    /// Set layout strategy (dense, sparse, auto)
    pub fn layout(&mut self, layout: rusty_lr_parser::TableLayout) -> &mut Self {
        self.layout = Some(layout);
        self
    }
    /// Set dense limit in bytes for auto-layout detection
    pub fn dense_limit(&mut self, limit: usize) -> &mut Self {
        self.dense_limit = Some(limit);
        self
    }
    /// Deprecated: use layout(TableLayout::Dense) instead
    #[deprecated(since = "0.66.0", note = "Use `layout(TableLayout::Dense)` instead")]
    pub fn dense(&mut self, dense: bool) -> &mut Self {
        self.layout = Some(if dense {
            rusty_lr_parser::TableLayout::Dense
        } else {
            rusty_lr_parser::TableLayout::Sparse
        });
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

    /// Print to stderr.
    pub fn stderr(&mut self, val: bool) -> &mut Self {
        self.stderr = val;
        self
    }

    /// Print backtrace of production rules when conflicts occurred. ruleset could be messed up
    pub fn note_backtrace(&mut self, val: bool) -> &mut Self {
        self.note_backtrace = val;
        self
    }

    fn stream(&self) -> StandardStream {
        if self.stderr {
            StandardStream::stderr(ColorChoice::Auto)
        } else {
            StandardStream::stdout(ColorChoice::Auto)
        }
    }

    fn with_extra_note(mut diag: Diagnostic<usize>, note: String) -> Diagnostic<usize> {
        diag.notes.push(note);
        diag
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
        if let Some(root_location) = nonterm.root_location() {
            let origin_range = grammar
                .span_manager
                .get_byterange(&root_location)
                .unwrap_or(0..0);
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
            let rule_location = nonterm.rules[local_rule].location();
            let rule_range = grammar
                .span_manager
                .get_byterange(&rule_location)
                .unwrap_or(0..0);
            let origin_range = grammar
                .span_manager
                .get_byterange(&nonterm.name.location())
                .unwrap_or(0..0);

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
                        Label::primary(file_id, range).with_message(e.to_string()),
                    ]);
                let writer = self.stream();
                let config = codespan_reporting::term::Config::default();
                term::emit_to_write_style(&mut writer.lock(), &config, &files, &diag)
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
                let writer = self.stream();
                let config = codespan_reporting::term::Config::default();
                term::emit_to_write_style(&mut writer.lock(), &config, &files, &diag)
                    .expect("Failed to write to stderr");
                return Err(diag.message);
            }
        };

        let mut grammar_args = match rusty_lr_parser::grammar::Grammar::parse_args(macro_stream) {
            Ok(grammar_args) => grammar_args,
            Err((e, sm)) => {
                let diag = match e {
                    ParseArgError::MacroLineParse { location, message } => Diagnostic::error()
                        .with_message("Parse Failed")
                        .with_labels(vec![
                            Label::primary(file_id, sm.get_byterange(&location).unwrap_or(0..0))
                                .with_message("Error here"),
                        ])
                        .with_notes(vec![message]),
                };

                let writer = self.stream();
                let config = codespan_reporting::term::Config::default();
                term::emit_to_write_style(&mut writer.lock(), &config, &files, &diag)
                    .expect("Failed to write to stderr");
                return Err(diag.message);
            }
        };

        for error in &grammar_args.error_recovered {
            let range = grammar_args
                .span_manager
                .get_byterange(&error.location)
                .unwrap_or(0..0);
            let diag = Diagnostic::error()
                .with_message("Syntax error in grammar")
                .with_labels(vec![
                    Label::primary(file_id, range).with_message(error.message.clone()),
                ])
                .with_notes(vec![format!("refer to: {}", error.link)]);
            let writer = self.stream();
            let config = codespan_reporting::term::Config::default();
            term::emit_to_write_style(&mut writer.lock(), &config, &files, &diag)
                .expect("Failed to write to stderr");
        }

        if !grammar_args.error_recovered.is_empty() {
            return Err("Syntax error in grammar".to_string());
        }

        match rusty_lr_parser::grammar::Grammar::arg_check_error(&grammar_args) {
            Ok(_) => {}
            Err(e) => {
                let diag = match e {
                    ArgError::MultipleModulePrefixDefinition(locs) => {
                        let mut labels = Vec::new();
                        for (i, loc) in locs.iter().enumerate() {
                            let message = if i == 0 {
                                "First definition"
                            } else {
                                "Other definition"
                            };
                            let range = grammar_args
                                .span_manager
                                .get_byterange(&loc)
                                .unwrap_or(0..0);
                            labels.push(Label::primary(file_id, range).with_message(message));
                        }

                        Diagnostic::error()
                            .with_message("Multiple %moduleprefix definition")
                            .with_labels(labels)
                            .with_notes(vec![
                                "Only one %moduleprefix definition is allowed".to_string(),
                            ])
                    }
                    ArgError::MultipleUserDataDefinition(locs) => {
                        let mut labels = Vec::new();
                        for (i, loc) in locs.iter().enumerate() {
                            let range = grammar_args
                                .span_manager
                                .get_byterange(&loc)
                                .unwrap_or(0..0);
                            let message = if i == 0 {
                                "First definition"
                            } else {
                                "Other definition"
                            };
                            labels.push(Label::primary(file_id, range).with_message(message));
                        }
                        Diagnostic::error()
                            .with_message("Multiple %userdata definition")
                            .with_labels(labels)
                            .with_notes(vec![
                                "Only one %userdata definition is allowed".to_string(),
                            ])
                    }
                    ArgError::MultipleErrorDefinition(locs) => {
                        let mut labels = Vec::new();
                        for (i, loc) in locs.iter().enumerate() {
                            let range = grammar_args
                                .span_manager
                                .get_byterange(&loc)
                                .unwrap_or(0..0);
                            let message = if i == 0 {
                                "First definition"
                            } else {
                                "Other definition"
                            };
                            labels.push(Label::primary(file_id, range).with_message(message));
                        }

                        Diagnostic::error()
                            .with_message("Multiple %error definition")
                            .with_labels(labels)
                            .with_notes(vec!["Only one %error definition is allowed".to_string()])
                    }
                    ArgError::MultipleTokenTypeDefinition(locs) => {
                        let mut labels = Vec::new();
                        for (i, loc) in locs.iter().enumerate() {
                            let range = grammar_args
                                .span_manager
                                .get_byterange(&loc)
                                .unwrap_or(0..0);
                            let message = if i == 0 {
                                "First definition"
                            } else {
                                "Other definition"
                            };
                            labels.push(Label::primary(file_id, range).with_message(message));
                        }

                        Diagnostic::error()
                            .with_message("Multiple %tokentype definition")
                            .with_labels(labels)
                            .with_notes(vec![
                                "Only one %tokentype definition is allowed".to_string(),
                            ])
                    }
                    ArgError::MultipleLocationDefinition(locs) => {
                        let mut labels = Vec::new();
                        for (i, loc) in locs.iter().enumerate() {
                            let range = grammar_args
                                .span_manager
                                .get_byterange(&loc)
                                .unwrap_or(0..0);
                            let message = if i == 0 {
                                "First definition"
                            } else {
                                "Other definition"
                            };
                            labels.push(Label::primary(file_id, range).with_message(message));
                        }

                        Diagnostic::error()
                            .with_message("Multiple %location definition")
                            .with_labels(labels)
                            .with_notes(vec![
                                "Only one %location definition is allowed".to_string(),
                            ])
                    }

                    ArgError::MultipleStartDefinition(locs) => {
                        let mut labels = Vec::new();
                        for (i, loc) in locs.iter().enumerate() {
                            let range = grammar_args
                                .span_manager
                                .get_byterange(&loc)
                                .unwrap_or(0..0);
                            let message = if i == 0 {
                                "First definition"
                            } else {
                                "Other definition"
                            };
                            labels.push(Label::primary(file_id, range).with_message(message));
                        }

                        Diagnostic::error()
                            .with_message("Multiple %start definition")
                            .with_labels(labels)
                            .with_notes(vec!["Only one %start definition is allowed".to_string()])
                    }
                    ArgError::MultiplePrecDefinition(locs) => {
                        let mut labels = Vec::new();
                        for (i, loc) in locs.iter().enumerate() {
                            let range = grammar_args
                                .span_manager
                                .get_byterange(&loc)
                                .unwrap_or(0..0);
                            let message = if i == 0 {
                                "First definition"
                            } else {
                                "Other definition"
                            };
                            labels.push(Label::primary(file_id, range).with_message(message));
                        }

                        Diagnostic::error()
                            .with_message("Multiple %prec definition")
                            .with_labels(labels)
                            .with_notes(vec!["%prec must be unique".to_string()])
                    }
                    ArgError::MultipleDPrecDefinition(locs) => {
                        let mut labels = Vec::new();
                        for (i, loc) in locs.iter().enumerate() {
                            let range = grammar_args
                                .span_manager
                                .get_byterange(&loc)
                                .unwrap_or(0..0);
                            let message = if i == 0 {
                                "First definition"
                            } else {
                                "Other definition"
                            };
                            labels.push(Label::primary(file_id, range).with_message(message));
                        }

                        Diagnostic::error()
                            .with_message("Multiple %dprec definition")
                            .with_labels(labels)
                            .with_notes(vec!["%dprec must be unique".to_string()])
                    }

                    ArgError::StartNotDefined => Diagnostic::error()
                        .with_message("%start not defined")
                        .with_labels(vec![])
                        .with_notes(vec![
                            "%start must be defined".to_string(),
                            ">>> %start <non-terminal>".to_string(),
                        ]),
                    ArgError::TokenTypeNotDefined => Diagnostic::error()
                        .with_message("%tokentype not defined")
                        .with_labels(vec![])
                        .with_notes(vec![
                            "%tokentype must be defined".to_string(),
                            ">>> %tokentype <TokenType>".to_string(),
                        ]),

                    ArgError::MultipleNameDefinition(name, locs) => {
                        let mut labels = Vec::new();
                        for (i, loc) in locs.iter().enumerate() {
                            let range = grammar_args
                                .span_manager
                                .get_byterange(&loc)
                                .unwrap_or(0..0);
                            let message = if i == 0 {
                                "First definition"
                            } else {
                                "Other definition"
                            };
                            labels.push(Label::primary(file_id, range).with_message(message));
                        }

                        Diagnostic::error()
                            .with_message(format!("Multiple name definition: {}", name))
                            .with_labels(labels)
                            .with_notes(vec!["Name must be unique".to_string()])
                    }
                    ArgError::ReservedName(names) => {
                        let mut labels = Vec::new();
                        for name in names.iter() {
                            let range = grammar_args
                                .span_manager
                                .get_byterange(&name.location())
                                .unwrap_or(0..0);
                            let message = "used here";
                            labels.push(Label::primary(file_id, range).with_message(message));
                        }

                        Diagnostic::error()
                            .with_message("Reserved name used")
                            .with_labels(labels)
                            .with_notes(vec!["Name is reserved and cannot be used".to_string()])
                    }

                    ArgError::DuplicateStartSymbol { location, name } => {
                        let range = grammar_args
                            .span_manager
                            .get_byterange(&location)
                            .unwrap_or(0..0);
                        Diagnostic::error()
                            .with_message(format!("Duplicate start symbol definition: `{}`", name))
                            .with_labels(vec![
                                Label::primary(file_id, range)
                                    .with_message("duplicate start symbol defined here"),
                            ])
                    }
                };

                let writer = self.stream();
                let config = codespan_reporting::term::Config::default();
                term::emit_to_write_style(&mut writer.lock(), &config, &files, &diag)
                    .expect("Failed to write to stderr");
                return Err(diag.message);
            }
        };

        if let Some(glr) = self.glr {
            grammar_args.glr = glr;
        }
        if let Some(layout) = self.layout {
            grammar_args.layout = layout;
        }
        if let Some(limit) = self.dense_limit {
            grammar_args.dense_limit = limit;
        }

        let span_manager = grammar_args.span_manager.clone();

        // parse lines
        let mut grammar = match rusty_lr_parser::grammar::Grammar::from_grammar_args(grammar_args) {
            Ok(grammar) => grammar,
            Err(e) => {
                let diag = match e {
                    ParseError::MultipleReduceDefinition(locs) => {
                        let mut labels = Vec::new();
                        for loc in locs {
                            let range = span_manager.get_byterange(&loc.location()).unwrap_or(0..0);
                            let reduce_type = match loc.value() {
                                rusty_lr_core::production::Associativity::Left => "%left",
                                rusty_lr_core::production::Associativity::Right => "%right",
                            };
                            labels.push(
                                Label::primary(file_id, range)
                                    .with_message(format!("was set as {} here", reduce_type)),
                            );
                        }

                        Diagnostic::error()
                            .with_message(format!("Multiple reduce definition"))
                            .with_labels(labels)
                            .with_notes(vec![
                                "Reduce type must be unique, either %left or %right".to_string(),
                            ])
                    }

                    ParseError::InvalidTerminalRange {
                        location,
                        start,
                        end,
                    } => {
                        let range1 = span_manager
                            .get_byterange(&start.0.location())
                            .unwrap_or(0..0);
                        let range2 = span_manager
                            .get_byterange(&end.0.location())
                            .unwrap_or(0..0);

                        Diagnostic::error()
                        .with_message("Invalid terminal range")
                        .with_labels(vec![
                            Label::primary(file_id, span_manager.get_byterange(&location).unwrap_or(0..0)).with_message("Invalid range here"),
                            Label::secondary(file_id, range1).with_message(format!("First terminal symbol (index {})", start.1)),
                            Label::secondary(file_id, range2).with_message(format!("Last terminal symbol (index {})", end.1)),
                        ]).with_notes(vec![
                            "First terminal symbol has to be less than or equal to the last terminal symbol".to_string()
                        ])
                    }

                    ParseError::StartNonTerminalNotDefined(loc) => {
                        let range = span_manager.get_byterange(&loc).unwrap_or(0..0);

                        Diagnostic::error()
                            .with_message("Start non-terminal not defined")
                            .with_labels(vec![
                                Label::primary(file_id, range)
                                    .with_message("This name is given to %start"),
                            ])
                            .with_notes(vec!["Non-terminal name must be defined".to_string()])
                    }

                    ParseError::TerminalNotDefined(loc) => {
                        let range = span_manager.get_byterange(&loc).unwrap_or(0..0);

                        Diagnostic::error()
                            .with_message("Terminal symbol not defined")
                            .with_labels(vec![
                                Label::primary(file_id, range)
                                    .with_message("This terminal symbol is not defined"),
                            ])
                            .with_notes(vec!["Terminal symbol must be defined".to_string()])
                    }

                    ParseError::UnsupportedLiteralType(loc) => {
                        let range = span_manager.get_byterange(&loc).unwrap_or(0..0);

                        Diagnostic::error()
                            .with_message("Unsupported literal type")
                            .with_labels(vec![
                                Label::primary(file_id, range)
                                    .with_message("This literal type is not supported"),
                            ])
                            .with_notes(vec![
                                "If %tokentype is `char`, only `char` or `&str` are supported"
                                    .to_string(),
                                "If %tokentype is `u8`, only `u8` or `&[u8]` are supported"
                                    .to_string(),
                            ])
                    }
                    ParseError::InvalidLiteralRange(loc) => {
                        let range = span_manager.get_byterange(&loc).unwrap_or(0..0);

                        Diagnostic::error()
                            .with_message("Invalid literal range")
                            .with_labels(vec![
                                Label::primary(file_id, range).with_message("Invalid range here"),
                            ])
                            .with_notes(vec![
                                "First terminal symbol has to be less than or equal to the last terminal symbol".to_string()
                            ])
                    }
                    ParseError::TokenInLiteralMode(locs) => {
                        let mut labels = Vec::new();
                        for loc in locs.iter() {
                            let range = span_manager.get_byterange(&loc).unwrap_or(0..0);
                            let message = "%token cannot be used in literal mode";
                            labels.push(Label::primary(file_id, range).with_message(message));
                        }
                        Diagnostic::error()
                            .with_message("%token with %tokentype `char` or `u8` is not supported")
                            .with_labels(labels)
                    }
                    ParseError::MultiplePrecedenceOrderDefinition(locs) => {
                        let mut labels = Vec::new();
                        for (i, loc) in locs.iter().enumerate() {
                            let range = span_manager.get_byterange(&loc).unwrap_or(0..0);
                            let message = if i == 0 {
                                "First definition"
                            } else {
                                "Other definition"
                            };
                            labels.push(Label::primary(file_id, range).with_message(message));
                        }

                        Diagnostic::error()
                            .with_message("Multiple operator precedence defined")
                            .with_labels(labels)
                            .with_notes(vec!["%prec name must be unique".to_string()])
                    }
                    ParseError::PrecedenceNotDefined(ident) => {
                        let range = span_manager
                            .get_byterange(&ident.location())
                            .unwrap_or(0..0);
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

                    ParseError::RuleTypeDefinedButActionNotDefined { nonterm, rule } => {
                        // `name` must not be generated rule,
                        // since it is programmically generated, it must have a proper reduce action
                        Diagnostic::error()
                            .with_message("Reduce action not defined")
                            .with_labels(vec![
                                Label::secondary(
                                    file_id,
                                    span_manager.get_byterange(&nonterm).unwrap_or(0..0),
                                )
                                .with_message("This non-terminal has a type definition"),
                                Label::primary(
                                    file_id,
                                    span_manager.get_byterange(&rule).unwrap_or(0..0),
                                )
                                .with_message("This rule line has no reduce action"),
                            ])
                            .with_notes(vec!["".to_string()])
                    }

                    ParseError::OnlyTerminalSet(loc) => {
                        let range = span_manager.get_byterange(&loc).unwrap_or(0..0);
                        Diagnostic::error()
                            .with_message("Only terminal or terminal set is allowed")
                            .with_labels(vec![
                                Label::primary(file_id, range)
                                    .with_message("This pattern is not terminal"),
                            ])
                            .with_notes(vec!["".to_string()])
                    }
                    ParseError::NonTerminalNotDefined(ident) => {
                        let range = span_manager.get_byterange(&ident).unwrap_or(0..0);
                        Diagnostic::error()
                            .with_message("Non-terminal not defined")
                            .with_labels(vec![
                                Label::primary(file_id, range)
                                    .with_message("This non-terminal is not defined"),
                            ])
                    }
                    ParseError::OnlyUsizeLiteral(loc) => {
                        let range = span_manager.get_byterange(&loc).unwrap_or(0..0);
                        Diagnostic::error()
                            .with_message("Only usize literal is allowed for %dprec")
                            .with_labels(vec![Label::primary(file_id, range)])
                    }

                    ParseError::BisonVariableZero(loc) => {
                        let range = span_manager.get_byterange(&loc).unwrap_or(0..0);
                        Diagnostic::error()
                            .with_message("Bison variable $0 is not supported")
                            .with_labels(vec![
                                Label::primary(file_id, range)
                                    .with_message("This variable reference is invalid"),
                            ])
                            .with_notes(vec!["Bison variable $0 is not supported".to_string()])
                    }

                    ParseError::BisonVariableOutOfRange {
                        location,
                        name,
                        max,
                    } => {
                        let range = span_manager.get_byterange(&location).unwrap_or(0..0);
                        Diagnostic::error()
                            .with_message("Bison variable is out of range")
                            .with_labels(vec![Label::primary(file_id, range).with_message(
                                format!("references index out of range (max index: {})", max),
                            )])
                            .with_notes(vec![format!(
                                "Bison variable {} is out of range (max index: {})",
                                name, max
                            )])
                    }

                    ParseError::TypeInferenceFailed(location) => {
                        let range = span_manager.get_byterange(&location).unwrap_or(0..0);
                        Diagnostic::error()
                            .with_message("type inference failed: circular dependency or no identity action found")
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message("failed to infer type for this placeholder")])
                    }

                    ParseError::CircularDependency { location, path } => {
                        let range = span_manager.get_byterange(&location).unwrap_or(0..0);
                        Diagnostic::error()
                            .with_message(format!(
                                "circular dependency detected: {}",
                                path.join(" -> ")
                            ))
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message("circular reference starts here")])
                            .with_notes(vec![
                                "refer to https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#substitution-errors".to_string(),
                            ])
                    }

                    ParseError::InvalidAllowDiagnostic { location, name } => {
                        let range = span_manager.get_byterange(&location).unwrap_or(0..0);
                        Diagnostic::error()
                            .with_message(format!("unknown diagnostic name: `{}`", name))
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message("unknown diagnostic name")])
                            .with_notes(vec![
                                "valid diagnostic names: nonterm_unreachable, unused_nonterm_data, nonterm_unproductive, unused_terminals, terminals_merged, redundant_rule_removed, unit_production_eliminated, glr_optional_expanded, reduce_reduce_conflict_resolved, shift_reduce_conflict_resolved, shift_reduce_conflict_glr, reduce_reduce_conflict_glr".to_string(),
                            ])
                    }

                    ParseError::MaxSubstitutionDepthExceeded {
                        location,
                        max_depth,
                    } => {
                        let range = span_manager.get_byterange(&location).unwrap_or(0..0);
                        Diagnostic::error()
                            .with_message(format!(
                                "maximum variable substitution depth ({}) exceeded",
                                max_depth
                            ))
                            .with_labels(vec![Label::primary(file_id, range)
                                .with_message("recursion depth limit reached here")])
                            .with_notes(vec![
                                "refer to https://github.com/ehwan/RustyLR/blob/main/SYNTAX.md#substitution-errors".to_string(),
                            ])
                    }
                    ParseError::GlrNullableReduceCycle {
                        location,
                        nullable_rule,
                        state,
                        reason,
                        help,
                    } => {
                        let range = span_manager.get_byterange(&location).unwrap_or(0..0);
                        Diagnostic::error()
                            .with_message("GLR nullable reduce cycle cannot be expanded safely")
                            .with_labels(vec![Label::primary(file_id, range).with_message(
                                "this nullable production participates in a zero-consuming GLR cycle",
                            )])
                            .with_notes(vec![
                                format!(
                                    "reducing `{nullable_rule}` in state {state} returns to the same state without consuming input"
                                ),
                                format!("why RustyLR cannot rewrite it automatically: {reason}"),
                                format!("how to fix it: {help}"),
                            ])
                    }
                };

                let writer = self.stream();
                let config = codespan_reporting::term::Config::default();
                term::emit_to_write_style(&mut writer.lock(), &config, &files, &diag)
                    .expect("Failed to write to stderr");

                return Err(diag.message);
            }
        };

        // diagnostics for optimization
        if grammar.optimize {
            grammar.optimize(25);
        }

        grammar.builder = match grammar.create_builder() {
            Ok(builder) => builder,
            Err(err) => {
                let diag = match err {
                    ParseError::GlrNullableReduceCycle {
                        location,
                        nullable_rule,
                        state,
                        reason,
                        help,
                    } => {
                        let range = span_manager.get_byterange(&location).unwrap_or(0..0);
                        Diagnostic::error()
                            .with_message("GLR nullable reduce cycle cannot be expanded safely")
                            .with_labels(vec![Label::primary(file_id, range).with_message(
                                "this nullable production participates in a zero-consuming GLR cycle",
                            )])
                            .with_notes(vec![
                                format!(
                                    "reducing `{nullable_rule}` in state {state} returns to the same state without consuming input"
                                ),
                                format!("why RustyLR cannot rewrite it automatically: {reason}"),
                                format!("how to fix it: {help}"),
                            ])
                    }
                    err => {
                        let range = err
                            .locations()
                            .first()
                            .and_then(|loc| span_manager.get_byterange(loc))
                            .unwrap_or(0..0);
                        Diagnostic::error()
                            .with_message(err.short_message())
                            .with_labels(vec![Label::primary(file_id, range)])
                    }
                };
                let writer = self.stream();
                let config = codespan_reporting::term::Config::default();
                term::emit_to_write_style(&mut writer.lock(), &config, &files, &diag)
                    .expect("Failed to write to stderr");
                return Err(diag.message);
            }
        };
        let diags_collector = grammar.build_grammar();

        let mut conflict_errors = Vec::new();
        let nonterm_mapper = |nonterm| grammar.nonterm_pretty_name(nonterm);
        let class_mapper = |class| grammar.class_pretty_name_list(class, 5);

        // construct conflict errors for non-GLR mode
        if !grammar.glr {
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
                        let rule_str = grammar.builder.rules[shift_rule.production_idx]
                            .rule
                            .clone()
                            .map(class_mapper, nonterm_mapper)
                            .into_shifted(shift_rule.dot);
                        notes.push(format!("\t>>> {rule_str}"));
                    }
                }
                for shift_rule in shift_rules {
                    Self::extend_rule_source_label(
                        &mut labels,
                        file_id,
                        shift_rule.production_idx,
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
                        let name = nonterm_mapper(grammar.builder.rules[reduce_rule].rule.lhs);

                        notes.push(format!("Backtrace for the reduce rule ({name}):"));
                        notes.extend(reduce_rule_backtrace.into_iter().map(|shifted_rule| {
                            let rule_str = grammar.builder.rules[shifted_rule.production_idx]
                                .rule
                                .clone()
                                .map(class_mapper, nonterm_mapper)
                                .into_shifted(shifted_rule.dot);

                            format!("\t>>> {rule_str}")
                        }));
                    }
                }

                let message = format!(
                    "Shift/Reduce conflict detected with terminal(class): {}",
                    grammar.class_pretty_name_list(term, 5)
                );

                conflict_errors.push(
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
                        let name = nonterm_mapper(grammar.builder.rules[reduce_rule].rule.lhs);

                        notes.push(format!("Backtrace for the reduce rule ({name}):"));
                        notes.extend(reduce_rule_from.into_iter().map(|shifted_rule| {
                            let rule_str = grammar.builder.rules[shifted_rule.production_idx]
                                .rule
                                .clone()
                                .map(class_mapper, nonterm_mapper)
                                .into_shifted(shifted_rule.dot);

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

                conflict_errors.push(
                    Diagnostic::error()
                        .with_message(message)
                        .with_labels(labels)
                        .with_notes(notes),
                );
            }
        }

        // Convert warning and info diagnostics from the grammar level
        let mut warnings = Vec::new();
        for warning in &grammar.warnings {
            let diag = match warning {
                rusty_lr_parser::error::Warning::NonTermUnreachable { nonterm_name } => {
                    let range = span_manager
                        .get_byterange(&nonterm_name.location())
                        .unwrap_or(0..0);
                    Diagnostic::warning()
                        .with_message("NonTerminal deleted")
                        .with_labels(vec![
                            Label::primary(file_id, range)
                                .with_message("non-terminal defined here"),
                        ])
                        .with_notes(vec![
                            "This non-terminal cannot be reached from initial state".to_string(),
                        ])
                }
                rusty_lr_parser::error::Warning::NonTermUnproductive { nonterm_name } => {
                    let range = span_manager
                        .get_byterange(&nonterm_name.location())
                        .unwrap_or(0..0);
                    Diagnostic::warning()
                        .with_message("NonTerminal deleted")
                        .with_labels(vec![Label::primary(file_id, range)
                            .with_message("non-terminal defined here")])
                        .with_notes(vec![
                            "This non-terminal is unproductive (cannot derive any terminal strings)".to_string(),
                        ])
                }
                // Cycle warning removed
                rusty_lr_parser::error::Warning::UnusedNonTermData { nonterm_name } => {
                    let range = span_manager
                        .get_byterange(&nonterm_name.location())
                        .unwrap_or(0..0);
                    Diagnostic::warning()
                        .with_message("NonTerminal data type not used")
                        .with_labels(vec![
                            Label::primary(file_id, range)
                                .with_message("non-terminal defined here"),
                        ])
                        .with_notes(vec![
                            "This non-terminal's data type is not used in any reduce action"
                                .to_string(),
                            "Consider removing data type to optimize memory usage".to_string(),
                        ])
                }
                rusty_lr_parser::error::Warning::UnusedTerminals { class_idx } => {
                    let class_name = grammar.class_pretty_name_abbr(*class_idx);
                    let terminals = grammar.terminal_classes[*class_idx]
                        .terminals
                        .iter()
                        .copied()
                        .filter(|&term| term != grammar.other_terminal_index)
                        .map(|term| grammar.term_pretty_name(term))
                        .collect::<Vec<_>>();
                    let notes = vec![format!("{}: {}", class_name, terminals.join(", "))];
                    Diagnostic::warning()
                        .with_message("These terminals are not used in the grammar")
                        .with_notes(notes)
                }
            };
            warnings.push((diag, warning));
        }

        let mut infos = Vec::new();
        for info in &grammar.infos {
            let diag = match info {
                rusty_lr_parser::error::Info::TerminalsMerged { class_idx } => {
                    let class_name = format!(
                        "TerminalClass{}",
                        grammar.terminal_classes[*class_idx].multiterm_counter
                    );
                    let terminals = grammar.terminal_classes[*class_idx]
                        .terminals
                        .iter()
                        .map(|&term| grammar.term_pretty_name(term))
                        .collect::<Vec<_>>();
                    let notes = vec![format!("{}: {}", class_name, terminals.join(", "))];
                    Diagnostic::note()
                        .with_message("These terminals are merged into terminal class")
                        .with_notes(notes)
                }
                rusty_lr_parser::error::Info::RedundantRuleRemoved { rule_location } => {
                    let range = span_manager.get_byterange(rule_location).unwrap_or(0..0);
                    Diagnostic::note()
                        .with_message("Production Rule deleted")
                        .with_labels(vec![
                            Label::primary(file_id, range).with_message("defined here"),
                        ])
                        .with_notes(vec![
                            "Will be merged into rule using terminal class".to_string(),
                        ])
                }
                rusty_lr_parser::error::Info::UnitProductionEliminated {
                    nonterm_name,
                    rule_location,
                } => {
                    let nonterm_range = span_manager
                        .get_byterange(&nonterm_name.location())
                        .unwrap_or(0..0);
                    let rule_range = span_manager.get_byterange(rule_location).unwrap_or(0..0);
                    Diagnostic::note()
                        .with_message("NonTerminal deleted")
                        .with_labels(vec![
                            Label::primary(file_id, nonterm_range)
                                .with_message("non-terminal defined here"),
                            Label::secondary(file_id, rule_range)
                                .with_message("this rule has only one child rule"),
                        ])
                        .with_notes(vec![
                            "This non-terminal will be replaced by it's unique child rule"
                                .to_string(),
                        ])
                }
                rusty_lr_parser::error::Info::GlrOptionalExpanded {
                    nonterm_name,
                    rule_location,
                    before,
                    after,
                } => {
                    let nonterm_range = span_manager
                        .get_byterange(&nonterm_name.location())
                        .unwrap_or(0..0);
                    let rule_range = span_manager.get_byterange(rule_location).unwrap_or(0..0);
                    let mut notes = vec![
                        "This GLR production was expanded because an auto-generated optional empty branch formed a zero-consuming reduce cycle.".to_string(),
                        format!("before: {before}"),
                    ];
                    notes.extend(after.iter().map(|rule| format!("after:  {rule}")));
                    Diagnostic::note()
                        .with_message("Production rule expanded")
                        .with_labels(vec![
                            Label::primary(file_id, rule_range)
                                .with_message("production expanded here"),
                            Label::secondary(file_id, nonterm_range)
                                .with_message("left-hand side defined here"),
                        ])
                        .with_notes(notes)
                }
                rusty_lr_parser::error::Info::ReduceReduceConflictResolved {
                    max_priority,
                    reduce_rules,
                    deleted_rules,
                } => {
                    let mut labels = Vec::new();
                    for &rule in reduce_rules {
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
                    for &del in deleted_rules {
                        let priority = grammar.builder.rules[del].priority;
                        Self::extend_rule_source_label(
                            &mut labels,
                            file_id,
                            del,
                            &grammar,
                            "[Removed] (Reduce) ",
                            format!("[Removed] (Reduce) rule with lower priority: {priority}")
                                .as_str(),
                        );
                    }
                    Diagnostic::note()
                        .with_message("Reduce/Reduce conflict resolved")
                        .with_labels(labels)
                        .with_notes(vec![
                            format!("Max priority: {max_priority}"),
                            "Set priority for the rule with %dprec".to_string(),
                        ])
                }
                rusty_lr_parser::error::Info::ShiftReduceConflictResolvedShift {
                    term,
                    shift_prec,
                    shift_rules,
                    reduce_rules,
                } => {
                    let term_str = grammar.class_pretty_name_list(*term, 5);
                    let mut labels = Vec::new();
                    for &reduce_rule_pair in reduce_rules {
                        let (reduce_rule, reduce_prec) = reduce_rule_pair;
                        let (nonterm_info, local_id) = grammar.get_rule_by_id(reduce_rule).unwrap();
                        let rule_info = &nonterm_info.rules[local_id];
                        if shift_prec > &reduce_prec {
                            Self::extend_rule_source_label(
                                &mut labels,
                                file_id,
                                reduce_rule,
                                &grammar,
                                "[Removed] (Reduce) ",
                                format!(
                                    "[Removed] (Reduce) lower precedence than shift: {reduce_prec}"
                                )
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
                                format!("[Removed] (Reduce) has {reduce_type} associativity")
                                    .as_str(),
                            );
                        }
                        if !nonterm_info.is_auto_generated() {
                            let op_range = span_manager
                                .get_byterange(&rule_info.prec.unwrap().location())
                                .unwrap_or(0..0);
                            labels.push(
                                Label::secondary(file_id, op_range)
                                    .with_message("[Removed] (Reduce) operator for reduce rule"),
                            );
                        }
                    }
                    for &shift_rule in shift_rules {
                        Self::extend_rule_source_label(
                            &mut labels,
                            file_id,
                            shift_rule,
                            &grammar,
                            "(Shift) ",
                            format!("(Shift) precedence: {shift_prec}").as_str(),
                        );
                    }
                    Diagnostic::note()
                        .with_message(format!("Shift/Reduce conflict resolved with terminal(class): {term_str}"))
                        .with_labels(labels)
                        .with_notes(vec![
                            "Operator of production rule is the rightmost terminal symbol with precedence defined".to_string(),
                            "Set operator for rule explicitly with %prec".to_string(),
                            "Set precedence for operator with %left, %right, or %precedence".to_string(),
                        ])
                }
                rusty_lr_parser::error::Info::ShiftReduceConflictResolvedReduce {
                    term,
                    shift_prec,
                    shift_rules,
                    reduce_rules,
                } => {
                    let term_str = grammar.class_pretty_name_list(*term, 5);
                    let mut labels = Vec::new();
                    for &reduce_rule_pair in reduce_rules {
                        let (reduce_rule, reduce_prec) = reduce_rule_pair;
                        let (nonterm_info, local_id) = grammar.get_rule_by_id(reduce_rule).unwrap();
                        let rule_info = &nonterm_info.rules[local_id];
                        if &reduce_prec > shift_prec {
                            Self::extend_rule_source_label(
                                &mut labels,
                                file_id,
                                reduce_rule,
                                &grammar,
                                "(Reduce) ",
                                format!("(Reduce) higher precedence than shift: {reduce_prec}")
                                    .as_str(),
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
                            let op_range = span_manager
                                .get_byterange(&rule_info.prec.unwrap().location())
                                .unwrap_or(0..0);
                            labels.push(
                                Label::secondary(file_id, op_range)
                                    .with_message("(Reduce) operator for reduce rule"),
                            );
                        }
                    }
                    for &shift_rule in shift_rules {
                        Self::extend_rule_source_label(
                            &mut labels,
                            file_id,
                            shift_rule,
                            &grammar,
                            "[Removed] (Shift) ",
                            format!("[Removed] (Shift) lower precedence than reduce: {shift_prec}")
                                .as_str(),
                        );
                    }
                    Diagnostic::note()
                        .with_message(format!("Shift/Reduce conflict resolved with terminal(class): {term_str}"))
                        .with_labels(labels)
                        .with_notes(vec![
                            "Operator of production rule is the rightmost terminal symbol with precedence defined".to_string(),
                            "Set operator for rule explicitly with %prec".to_string(),
                            "Set precedence for operator with %left, %right, or %precedence".to_string(),
                        ])
                }
                rusty_lr_parser::error::Info::ShiftReduceConflictGLR {
                    term,
                    shift_rules,
                    shift_rules_backtrace,
                    reduce_rules,
                } => {
                    let term_str = grammar.class_pretty_name_list(*term, 5);
                    let mut labels = Vec::new();
                    let mut notes = vec![
                        "Operator of production rule is the rightmost terminal symbol with precedence defined".to_string(),
                        "Set operator for rule explicitly with %prec".to_string(),
                        "Set precedence for operator with %left, %right, or %precedence".to_string(),
                    ];

                    if self.note_backtrace {
                        if self.is_executable {
                            notes.push("--no-backtrace to disable backtracing".to_string());
                        }
                        notes.push("Backtrace for the shift rule:".to_string());
                        for s_bt in shift_rules_backtrace {
                            notes.push(s_bt.clone());
                        }
                    }
                    for &shift_rule in shift_rules {
                        Self::extend_rule_source_label(
                            &mut labels,
                            file_id,
                            shift_rule,
                            &grammar,
                            "(Shift) ",
                            "(Shift) ",
                        );
                    }
                    for &(reduce_rule, ref r_bt) in reduce_rules {
                        Self::extend_rule_source_label(
                            &mut labels,
                            file_id,
                            reduce_rule,
                            &grammar,
                            "(Reduce) ",
                            "(Reduce) ",
                        );
                        if self.note_backtrace {
                            for line in r_bt {
                                notes.push(line.clone());
                            }
                        }
                    }
                    Diagnostic::help()
                        .with_message(format!(
                            "Shift/Reduce conflict detected with terminal(class): {term_str}"
                        ))
                        .with_labels(labels)
                        .with_notes(notes)
                }
                rusty_lr_parser::error::Info::ReduceReduceConflictGLR {
                    terms,
                    reduce_rules,
                } => {
                    let mut labels = Vec::new();
                    let mut notes = vec!["Set priority for the rule with %dprec".to_string()];

                    for &(reduce_rule, ref r_bt) in reduce_rules {
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
                            for line in r_bt {
                                notes.push(line.clone());
                            }
                        }
                    }
                    let term_strings = terms
                        .iter()
                        .map(|&t| grammar.class_pretty_name_list(t, 5))
                        .collect::<Vec<_>>();
                    Diagnostic::help()
                        .with_message(format!(
                            "Reduce/Reduce conflict detected with terminals: {}",
                            term_strings.join(", ")
                        ))
                        .with_labels(labels)
                        .with_notes(notes)
                }
            };
            infos.push((diag, info));
        }

        // Print resolved conflict notes and unresolved GLR conflict help notes based on user flags.
        // We match on the new generalized `Info` enum variants to decide whether to print them.
        if self.note_conflicts_resolving || self.note_conflicts {
            for (diag, info_variant) in &infos {
                if grammar.is_info_allowed(info_variant) {
                    continue;
                }
                let should_print = match info_variant {
                    rusty_lr_parser::error::Info::ShiftReduceConflictGLR { .. }
                    | rusty_lr_parser::error::Info::ReduceReduceConflictGLR { .. } => {
                        self.note_conflicts
                    }

                    rusty_lr_parser::error::Info::ReduceReduceConflictResolved { .. }
                    | rusty_lr_parser::error::Info::ShiftReduceConflictResolvedShift { .. }
                    | rusty_lr_parser::error::Info::ShiftReduceConflictResolvedReduce { .. } => {
                        self.note_conflicts_resolving
                    }

                    _ => true, // Optimization notes are printed unconditionally
                };
                if should_print {
                    let diag = Self::with_extra_note(
                        diag.clone(),
                        format!(
                            "to ignore this info, add `{}` to the grammar",
                            info_variant.suggestion(&grammar)
                        ),
                    );
                    let writer = self.stream();
                    let config = codespan_reporting::term::Config::default();
                    term::emit_to_write_style(&mut writer.lock(), &config, &files, &diag)
                        .expect("Failed to write verbose/verbose stream");
                }
            }
        } else {
            // Print only optimization notes unconditionally if conflict flags are disabled.
            for (diag, info_variant) in &infos {
                if grammar.is_info_allowed(info_variant) {
                    continue;
                }
                let should_print = match info_variant {
                    rusty_lr_parser::error::Info::TerminalsMerged { .. }
                    | rusty_lr_parser::error::Info::RedundantRuleRemoved { .. }
                    | rusty_lr_parser::error::Info::UnitProductionEliminated { .. }
                    | rusty_lr_parser::error::Info::GlrOptionalExpanded { .. } => true,
                    _ => false,
                };
                if should_print {
                    let diag = Self::with_extra_note(
                        diag.clone(),
                        format!(
                            "to ignore this info, add `{}` to the grammar",
                            info_variant.suggestion(&grammar)
                        ),
                    );
                    let writer = self.stream();
                    let config = codespan_reporting::term::Config::default();
                    term::emit_to_write_style(&mut writer.lock(), &config, &files, &diag)
                        .expect("Failed to write verbose/verbose stream");
                }
            }
        }

        // print warnings
        for (diag, warning) in &warnings {
            if grammar.is_warning_allowed(warning) {
                continue;
            }
            let diag = Self::with_extra_note(
                diag.clone(),
                format!(
                    "to ignore this warning, add `{}` to the grammar",
                    warning.suggestion(&grammar)
                ),
            );
            let writer = self.stream();
            let config = codespan_reporting::term::Config::default();
            term::emit_to_write_style(&mut writer.lock(), &config, &files, &diag)
                .expect("Failed to write to verbose stream");
        }

        // print conflict errors if not GLR
        if !grammar.glr {
            let has_errors = !conflict_errors.is_empty();
            for diag in conflict_errors {
                let writer = self.stream();
                let config = codespan_reporting::term::Config::default();
                term::emit_to_write_style(&mut writer.lock(), &config, &files, &diag)
                    .expect("Failed to write to stderr");
            }
            if has_errors {
                return Err("Grammar building failed".to_string());
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
            .enumerate()
            .map(|(rule_id, rule)| {
                format!(
                    "{rule_id}: {}",
                    rule.rule.clone().map(class_mapper, nonterm_mapper)
                )
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
            grammar,
        })
    }
}

impl Default for Builder {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn adding_suppression_note_preserves_existing_notes_once() {
        let diag = Diagnostic::note()
            .with_message("example")
            .with_notes(vec!["existing note".to_string()]);

        let diag = Builder::with_extra_note(diag, "suppression note".to_string());

        assert_eq!(
            diag.notes,
            vec!["existing note".to_string(), "suppression note".to_string()]
        );
    }
}
