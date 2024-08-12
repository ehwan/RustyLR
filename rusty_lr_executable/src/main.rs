use clap::Parser;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::ColorChoice;
use codespan_reporting::term::termcolor::StandardStream;
use proc_macro2::Span;
use proc_macro2::TokenStream;

use std::collections::BTreeSet;
use std::fs::read;
use std::fs::write;
use std::process::Command;

use codespan_reporting::diagnostic::Diagnostic;
use codespan_reporting::diagnostic::Label;
use codespan_reporting::files::SimpleFiles;

mod arg;
mod split;

struct Error {
    files: SimpleFiles<String, String>,
    fileid: usize,
    writer: StandardStream,
}
impl Error {
    pub fn new(name: String, source: String) -> Self {
        let mut files = SimpleFiles::new();
        let id = files.add(name, source);
        Self {
            files,
            fileid: id,
            writer: StandardStream::stderr(ColorChoice::Auto),
        }
    }

    pub fn error(&self, span: Span, message: String) {
        let range = span.byte_range();
        let diagnostic = Diagnostic::error()
            .with_message(message.clone())
            .with_labels(vec![Label::primary(self.fileid, range)]);
        let config = term::Config::default();

        match term::emit(&mut self.writer.lock(), &config, &self.files, &diagnostic) {
            Ok(_) => {}
            Err(_e) => eprintln!("{}", message),
        }
    }
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
    let error = Error::new(args.input_file.clone(), str.clone());

    // lex with proc-macro2
    let token_stream: TokenStream = match str.parse() {
        Ok(token_stream) => token_stream,
        Err(e) => {
            error.error(e.span(), e.to_string());
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
            error.error(e.span(), format!("{}", e));
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
            error.error(e.span(), format!("{}", e));
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
                error.error(e.span(), format!("{}", e));
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
