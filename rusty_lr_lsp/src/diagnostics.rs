use lsp_types::{Diagnostic, DiagnosticSeverity, Range};
use proc_macro2::{Spacing, TokenStream, TokenTree};
use rusty_lr_parser::grammar::Grammar;
use std::str::FromStr;

use crate::position::range_to_lsp_range;

/// Splits a TokenStream by the `%%` separator.
pub fn split_stream(token_stream: TokenStream) -> Result<(TokenStream, TokenStream), ()> {
    let mut token_stream = token_stream.into_iter().peekable();
    let mut output_stream = TokenStream::new();

    while let Some(token) = token_stream.next() {
        if let TokenTree::Punct(token) = &token {
            if token.as_char() == '%' && token.spacing() == Spacing::Joint {
                if let Some(TokenTree::Punct(next)) = token_stream.peek() {
                    if next.as_char() == '%' && next.spacing() == Spacing::Alone {
                        token_stream.next();
                        let macro_stream: TokenStream = token_stream.collect();
                        return Ok((output_stream, macro_stream));
                    }
                }
            }
        }
        output_stream.extend(std::iter::once(token));
    }
    Err(())
}

/// Runs the compiler's parser/builder pipeline on the given file content and gathers all diagnostics.
pub fn compile_and_get_diagnostics(content: &str) -> Vec<Diagnostic> {
    // 1. Parse TokenStream from content
    let token_stream = match TokenStream::from_str(content) {
        Ok(ts) => ts,
        Err(e) => {
            let range = e.span().byte_range();
            return vec![Diagnostic {
                range: range_to_lsp_range(content, range),
                severity: Some(DiagnosticSeverity::ERROR),
                code: None,
                code_description: None,
                source: Some("rusty_lr".to_string()),
                message: format!("Lexing/parsing error: {}", e),
                related_information: None,
                tags: None,
                data: None,
            }];
        }
    };

    // 2. Split into Rust code and grammar sections
    let (_, macro_stream) = match split_stream(token_stream) {
        Ok(res) => res,
        Err(_) => {
            return vec![Diagnostic {
                range: Range::default(),
                severity: Some(DiagnosticSeverity::WARNING),
                code: None,
                code_description: None,
                source: Some("rusty_lr".to_string()),
                message: "Cannot find `%%` to separate the Rust code and the grammar parts"
                    .to_string(),
                related_information: None,
                tags: None,
                data: None,
            }];
        }
    };

    // 3. Parse grammar arguments
    let grammar_args = match Grammar::parse_args(macro_stream) {
        Ok(args) => args,
        Err((e, sm)) => {
            let location = e.location();
            let range = sm.get_byterange(&location).unwrap_or(0..0);
            return vec![Diagnostic {
                range: range_to_lsp_range(content, range),
                severity: Some(DiagnosticSeverity::ERROR),
                code: None,
                code_description: None,
                source: Some("rusty_lr".to_string()),
                message: e.short_message(),
                related_information: None,
                tags: None,
                data: None,
            }];
        }
    };

    // 4. Collect recovered parser errors
    let mut diagnostics = Vec::new();
    for error in &grammar_args.error_recovered {
        let range = grammar_args
            .span_manager
            .get_byterange(&error.location)
            .unwrap_or(0..0);
        diagnostics.push(Diagnostic {
            range: range_to_lsp_range(content, range),
            severity: Some(DiagnosticSeverity::ERROR),
            code: None,
            code_description: None,
            source: Some("rusty_lr".to_string()),
            message: format!("{} (refer to: {})", error.message, error.link),
            related_information: None,
            tags: None,
            data: None,
        });
    }

    if !grammar_args.error_recovered.is_empty() {
        return diagnostics;
    }

    let span_manager = grammar_args.span_manager.clone();

    // 5. Run arg validation
    if let Err(e) = Grammar::arg_check_error(&grammar_args) {
        let msg = e.short_message();
        for loc in e.locations() {
            let range = span_manager.get_byterange(&loc).unwrap_or(0..0);
            diagnostics.push(Diagnostic {
                range: range_to_lsp_range(content, range),
                severity: Some(DiagnosticSeverity::ERROR),
                code: None,
                code_description: None,
                source: Some("rusty_lr".to_string()),
                message: msg.clone(),
                related_information: None,
                tags: None,
                data: None,
            });
        }
        return diagnostics;
    }

    // 6. Build the Grammar structure
    let mut grammar = match Grammar::from_grammar_args(grammar_args) {
        Ok(g) => g,
        Err(e) => {
            let msg = e.short_message();
            for loc in e.locations() {
                let range = span_manager.get_byterange(&loc).unwrap_or(0..0);
                diagnostics.push(Diagnostic {
                    range: range_to_lsp_range(content, range),
                    severity: Some(DiagnosticSeverity::ERROR),
                    code: None,
                    code_description: None,
                    source: Some("rusty_lr".to_string()),
                    message: msg.clone(),
                    related_information: None,
                    tags: None,
                    data: None,
                });
            }
            return diagnostics;
        }
    };

    if grammar.optimize {
        grammar.optimize(25);
    }
    grammar.builder = grammar.create_builder();

    // 7. Verify Shift/Reduce and Reduce/Reduce conflicts in non-GLR mode
    let diags_collector = grammar.build_grammar();
    if !grammar.glr {
        // Shift/Reduce conflicts
        for ((term, shift_rules, _), reduce_rules) in diags_collector.shift_reduce_conflicts {
            let term_str = grammar.class_pretty_name_list(term, 5);
            let message = format!(
                "Shift/Reduce conflict detected with terminal(class): {}",
                term_str
            );

            for shift_rule in shift_rules {
                if let Some((nonterm, local_rule)) =
                    grammar.get_rule_by_id(shift_rule.production_idx)
                {
                    let loc = nonterm.rules[local_rule].location();
                    let range = span_manager.get_byterange(&loc).unwrap_or(0..0);
                    diagnostics.push(Diagnostic {
                        range: range_to_lsp_range(content, range),
                        severity: Some(DiagnosticSeverity::ERROR),
                        code: None,
                        code_description: None,
                        source: Some("rusty_lr".to_string()),
                        message: format!("(Shift) {}", message),
                        related_information: None,
                        tags: None,
                        data: None,
                    });
                }
            }
            for (reduce_rule, _) in reduce_rules {
                if let Some((nonterm, local_rule)) = grammar.get_rule_by_id(reduce_rule) {
                    let loc = nonterm.rules[local_rule].location();
                    let range = span_manager.get_byterange(&loc).unwrap_or(0..0);
                    diagnostics.push(Diagnostic {
                        range: range_to_lsp_range(content, range),
                        severity: Some(DiagnosticSeverity::ERROR),
                        code: None,
                        code_description: None,
                        source: Some("rusty_lr".to_string()),
                        message: format!("(Reduce) {}", message),
                        related_information: None,
                        tags: None,
                        data: None,
                    });
                }
            }
        }

        // Reduce/Reduce conflicts
        for (reduce_rules, reduce_terms) in diags_collector.reduce_reduce_conflicts {
            let mut terms = Vec::new();
            for term in reduce_terms {
                terms.push(grammar.class_pretty_name_list(term, 5));
            }
            let message = format!(
                "Reduce/Reduce conflict detected with terminals: {}",
                terms.join(", ")
            );

            for (reduce_rule, _) in reduce_rules {
                if let Some((nonterm, local_rule)) = grammar.get_rule_by_id(reduce_rule) {
                    let loc = nonterm.rules[local_rule].location();
                    let range = span_manager.get_byterange(&loc).unwrap_or(0..0);
                    diagnostics.push(Diagnostic {
                        range: range_to_lsp_range(content, range),
                        severity: Some(DiagnosticSeverity::ERROR),
                        code: None,
                        code_description: None,
                        source: Some("rusty_lr".to_string()),
                        message: message.clone(),
                        related_information: None,
                        tags: None,
                        data: None,
                    });
                }
            }
        }
    }

    // 8. Collect Warnings
    for warning in &grammar.warnings {
        if grammar.is_warning_allowed(warning) {
            continue;
        }
        let msg = warning.short_message(&grammar);
        let locs = warning.locations();
        if locs.is_empty() {
            let sep_idx = content.find("%%").unwrap_or(0);
            let range = sep_idx..(sep_idx + 2);
            diagnostics.push(Diagnostic {
                range: range_to_lsp_range(content, range),
                severity: Some(DiagnosticSeverity::WARNING),
                code: None,
                code_description: None,
                source: Some("rusty_lr".to_string()),
                message: msg,
                related_information: None,
                tags: None,
                data: None,
            });
        } else {
            for loc in locs {
                let range = span_manager.get_byterange(&loc).unwrap_or(0..0);
                diagnostics.push(Diagnostic {
                    range: range_to_lsp_range(content, range),
                    severity: Some(DiagnosticSeverity::WARNING),
                    code: None,
                    code_description: None,
                    source: Some("rusty_lr".to_string()),
                    message: msg.clone(),
                    related_information: None,
                    tags: None,
                    data: None,
                });
            }
        }
    }

    diagnostics
}
