use lsp_types::{Diagnostic, DiagnosticSeverity, Range};
use proc_macro2::{Spacing, TokenStream, TokenTree};
use rusty_lr_core::{TerminalSymbol, production::LR0ItemRef};
use rusty_lr_parser::error::ParseError;
use rusty_lr_parser::grammar::Grammar;
use serde_json::json;
use std::collections::{BTreeMap, BTreeSet};
use std::str::FromStr;

use crate::lsp::position::range_to_lsp_range;

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

fn format_lr0_item(grammar: &Grammar, item: LR0ItemRef) -> String {
    grammar.builder.rules[item.production_idx]
        .rule
        .clone()
        .map(
            |class| grammar.class_pretty_name_list(class, 5),
            |nonterm| grammar.nonterm_pretty_name(nonterm),
        )
        .into_shifted(item.dot)
        .to_string()
}

fn format_production(grammar: &Grammar, rule: usize) -> String {
    grammar.builder.rules[rule]
        .rule
        .clone()
        .map(
            |class| grammar.class_pretty_name_list(class, 5),
            |nonterm| grammar.nonterm_pretty_name(nonterm),
        )
        .to_string()
}

fn format_shift_reduce_conflict_message(
    grammar: &Grammar,
    term: TerminalSymbol<usize>,
    shift_rules: &[LR0ItemRef],
    reduce_rules: &BTreeMap<usize, Vec<LR0ItemRef>>,
) -> String {
    let term_str = grammar.class_pretty_name_list(term, 5);
    let mut message = format!("Shift/Reduce conflict detected with terminal(class): {term_str}");

    message.push_str("\n\nConflicting rules:");
    message.push_str("\n- Shift:");
    for &shift_rule in shift_rules {
        message.push_str(&format!("\n  >>> {}", format_lr0_item(grammar, shift_rule)));
    }
    message.push_str("\n- Reduce:");
    for &reduce_rule in reduce_rules.keys() {
        message.push_str(&format!(
            "\n  >>> {}",
            format_production(grammar, reduce_rule)
        ));
    }

    message
}

fn format_reduce_reduce_conflict_message(
    grammar: &Grammar,
    reduce_rules: &[(usize, Vec<LR0ItemRef>)],
    reduce_terms: &BTreeSet<TerminalSymbol<usize>>,
) -> String {
    let terms = reduce_terms
        .iter()
        .map(|&term| grammar.class_pretty_name_list(term, 5))
        .collect::<Vec<_>>()
        .join(", ");
    let mut message = format!("Reduce/Reduce conflict detected with terminals: {terms}");

    message.push_str("\n\nConflicting rules:");
    for &(reduce_rule, _) in reduce_rules {
        message.push_str(&format!(
            "\n  >>> {}",
            format_production(grammar, reduce_rule)
        ));
    }

    message
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

    grammar.optimize(25);
    grammar.builder = match grammar.create_builder() {
        Ok(builder) => builder,
        Err(err) => {
            match err {
                ParseError::GlrNullableReduceCycle {
                    location,
                    nullable_rule,
                    state,
                    reason,
                    help,
                } => {
                    let range = span_manager.get_byterange(&location).unwrap_or(0..0);
                    diagnostics.push(Diagnostic {
                        range: range_to_lsp_range(content, range),
                        severity: Some(DiagnosticSeverity::ERROR),
                        code: None,
                        code_description: None,
                        source: Some("rusty_lr".to_string()),
                        message: format!(
                            "GLR nullable reduce cycle cannot be expanded safely: reducing `{nullable_rule}` in state {state} returns to the same state without consuming input. Reason: {reason}. Fix: {help}"
                        ),
                        related_information: None,
                        tags: None,
                        data: None,
                    });
                }
                err => {
                    let msg = err.short_message();
                    for loc in err.locations() {
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
                }
            }
            return diagnostics;
        }
    };

    // 7. Verify Shift/Reduce and Reduce/Reduce conflicts in non-GLR mode
    let diags_collector = grammar.build_grammar();
    if !grammar.glr {
        // Shift/Reduce conflicts
        for ((term, shift_rules, _), reduce_rules) in diags_collector.shift_reduce_conflicts {
            let message =
                format_shift_reduce_conflict_message(&grammar, term, &shift_rules, &reduce_rules);

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
                        message: format!("(Shift) {message}"),
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
                        message: format!("(Reduce) {message}"),
                        related_information: None,
                        tags: None,
                        data: None,
                    });
                }
            }
        }

        // Reduce/Reduce conflicts
        for (reduce_rules, reduce_terms) in diags_collector.reduce_reduce_conflicts {
            let message =
                format_reduce_reduce_conflict_message(&grammar, &reduce_rules, &reduce_terms);

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
                data: Some(json!({ "rustylr_allow": warning.suggestion(&grammar) })),
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
                    data: Some(json!({ "rustylr_allow": warning.suggestion(&grammar) })),
                });
            }
        }
    }

    diagnostics
}

#[cfg(test)]
mod tests {
    use super::*;

    const SHIFT_REDUCE_CONFLICT_GRAMMAR: &str = r#"
#[derive(Debug, Clone)]
pub enum Token {
    Num(i32),
    Plus,
}

%%

%tokentype Token;
%start E;

%token num Token::Num(_);
%token plus Token::Plus;

E(i32) : E plus E { 0 }
  | num { 0 }
  ;
"#;

    const REDUCE_REDUCE_CONFLICT_GRAMMAR: &str = r#"
#[derive(Debug, Clone)]
pub enum Token {
    Num(i32),
}

%%

%tokentype Token;
%start S;

%token num Token::Num(_);

S(i32) : A { A }
  | B { B }
  ;
A(i32) : num { 0 };
B(i32) : num { 0 };
"#;

    #[test]
    fn conflict_diagnostics_include_conflicting_rules() {
        let diagnostics = compile_and_get_diagnostics(SHIFT_REDUCE_CONFLICT_GRAMMAR);
        let conflict = diagnostics
            .iter()
            .find(|diagnostic| {
                diagnostic
                    .message
                    .contains("Shift/Reduce conflict detected")
            })
            .unwrap_or_else(|| {
                panic!(
                    "expected a shift/reduce conflict diagnostic, got: {:?}",
                    diagnostics
                        .iter()
                        .map(|diagnostic| diagnostic.message.as_str())
                        .collect::<Vec<_>>()
                )
            });

        assert!(conflict.message.contains("Conflicting rules:"));
        assert!(conflict.message.contains("- Shift:"));
        assert!(conflict.message.contains("- Reduce:"));
        assert!(conflict.message.contains(">>> E ->"));
        assert!(conflict.message.contains("•"));
        assert!(!conflict.message.contains("backtrace"));
        assert!(!conflict.message.contains("item trace"));
    }

    #[test]
    fn reduce_reduce_conflict_diagnostics_include_conflicting_rules() {
        let diagnostics = compile_and_get_diagnostics(REDUCE_REDUCE_CONFLICT_GRAMMAR);
        let conflict = diagnostics
            .iter()
            .find(|diagnostic| {
                diagnostic
                    .message
                    .contains("Reduce/Reduce conflict detected")
            })
            .unwrap_or_else(|| {
                panic!(
                    "expected a reduce/reduce conflict diagnostic, got: {:?}",
                    diagnostics
                        .iter()
                        .map(|diagnostic| diagnostic.message.as_str())
                        .collect::<Vec<_>>()
                )
            });

        assert!(conflict.message.contains("Conflicting rules:"));
        assert!(conflict.message.contains(">>> S -> num"));
        assert!(!conflict.message.contains("backtrace"));
        assert!(!conflict.message.contains("item trace"));
    }
}
