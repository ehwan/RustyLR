use lsp_types::{Position, Range};
use proc_macro2::TokenStream;
use rusty_lr_parser::grammar::Grammar;
use rusty_lr_parser::{
    GrammarArgs, IdentOrLiteral, Located, PatternArgs, PrecDPrecArgs, TerminalSetItem,
};
use std::str::FromStr;

use crate::lsp::diagnostics::split_stream;
use crate::lsp::position::{position_to_offset, range_to_lsp_range};

/// Traverses the AST of GrammarArgs to collect all Located<String> instances.
fn collect_located(args: &GrammarArgs) -> Vec<Located<String>> {
    let mut collected = Vec::new();

    // 1. %start names
    for start_name in &args.start_rule_name {
        collected.push(start_name.clone());
    }

    // 2. %token definitions
    for (t_name, _) in &args.terminals {
        collected.push(t_name.clone());
    }

    // 3. Precedence definitions
    for (_, _, items) in &args.precedences {
        for item in items {
            if let IdentOrLiteral::Ident(ident) = item {
                collected.push(ident.clone());
            }
        }
    }

    // 4. %allow diagnostics names
    for (allow_name, _) in &args.allowed_diagnostics {
        collected.push(allow_name.clone());
    }

    // 5. Rule definitions
    for rule in &args.rules {
        collected.push(rule.name.clone());
        for line in &rule.rule_lines {
            for (opt_loc, pattern) in &line.tokens {
                if let Some(loc) = opt_loc {
                    collected.push(loc.clone());
                }
                collect_pattern_located(pattern, &mut collected);
            }
            // %prec identifiers
            for prec in &line.precs {
                if let PrecDPrecArgs::Prec(IdentOrLiteral::Ident(ident)) = prec {
                    collected.push(ident.clone());
                }
            }
        }
    }

    collected
}

/// Recursively traverses a PatternArgs structure to collect Located<String> instances.
fn collect_pattern_located(pattern: &PatternArgs, collected: &mut Vec<Located<String>>) {
    match pattern {
        PatternArgs::Ident(ident) => {
            collected.push(ident.clone());
        }
        PatternArgs::Plus { base, .. }
        | PatternArgs::Star { base, .. }
        | PatternArgs::Question { base, .. }
        | PatternArgs::Exclamation { base, .. } => {
            collect_pattern_located(base, collected);
        }
        PatternArgs::TerminalSet(ts) => {
            for item in &ts.items {
                match item {
                    TerminalSetItem::Terminal(ident) => {
                        collected.push(ident.clone());
                    }
                    TerminalSetItem::Range(first, last) => {
                        collected.push(first.clone());
                        collected.push(last.clone());
                    }
                    _ => {}
                }
            }
        }
        PatternArgs::Group { alternatives, .. } => {
            for alt in alternatives {
                for pat in alt {
                    collect_pattern_located(pat, collected);
                }
            }
        }
        PatternArgs::Minus { base, exclude } => {
            collect_pattern_located(base, collected);
            collect_pattern_located(exclude, collected);
        }
        PatternArgs::Sep {
            base, delimiter, ..
        } => {
            collect_pattern_located(base, collected);
            collect_pattern_located(delimiter, collected);
        }
        _ => {}
    }
}

/// Locates the definition of the symbol under the cursor.
pub fn find_definition(content: &str, target_pos: Position) -> Option<Range> {
    let offset = position_to_offset(content, target_pos);

    // Parse the entire document into TokenStream
    let token_stream = TokenStream::from_str(content).ok()?;
    let (_, macro_stream) = split_stream(token_stream).ok()?;
    let grammar_args = Grammar::parse_args(macro_stream).ok()?;
    let span_manager = grammar_args.span_manager.clone();

    // Collect all located identifier strings in the AST
    let all_located = collect_located(&grammar_args);

    // Find the one that contains the click offset
    let clicked = all_located.iter().find(|loc| {
        if let Some(range) = span_manager.get_byterange(&loc.location()) {
            range.contains(&offset)
        } else {
            false
        }
    })?;

    // Look up the definition by name
    let name = clicked.value();

    // 1. Check rule definitions
    if let Some(rule) = grammar_args.rules.iter().find(|r| r.name.value == *name) {
        let def_range = span_manager.get_byterange(&rule.name.location())?;
        return Some(range_to_lsp_range(content, def_range));
    }

    // 2. Check token definitions
    if let Some((t_name, _)) = grammar_args
        .terminals
        .iter()
        .find(|(t, _)| t.value == *name)
    {
        let def_range = span_manager.get_byterange(&t_name.location())?;
        return Some(range_to_lsp_range(content, def_range));
    }

    // 3. Check precedence definitions
    for (_, _, items) in &grammar_args.precedences {
        for item in items {
            if let IdentOrLiteral::Ident(ident) = item {
                if ident.value() == name {
                    let def_range = span_manager.get_byterange(&ident.location())?;
                    return Some(range_to_lsp_range(content, def_range));
                }
            }
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;

    const MOCK_GRAMMAR: &str = r#"
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

E(_) : E plus num { 0 }
  | num { 0 }
  ;
"#;

    const MOCK_GRAMMAR_WITH_ERROR: &str = r#"
%%
%start E;
E : num plus error ;
"#;

    #[test]
    fn test_split_stream() {
        let ts = TokenStream::from_str(MOCK_GRAMMAR).unwrap();
        let (output, macro_stream) = split_stream(ts).unwrap();

        let output_str = output.to_string();
        let macro_str = macro_stream.to_string();

        assert!(output_str.contains("enum Token"));
        assert!(macro_str.contains("tokentype"));
        assert!(macro_str.contains("start E"));
    }

    #[test]
    fn test_diagnostics() {
        // Test valid grammar diagnostics (should be empty or only warnings about unused tokens/etc if any)
        let diags = crate::lsp::diagnostics::compile_and_get_diagnostics(MOCK_GRAMMAR);
        // Under normal circumstances, MOCK_GRAMMAR is valid
        for diag in &diags {
            eprintln!("Diag: {:?}", diag.message);
        }

        // Test invalid grammar diagnostics
        let diags_err =
            crate::lsp::diagnostics::compile_and_get_diagnostics(MOCK_GRAMMAR_WITH_ERROR);
        assert!(!diags_err.is_empty());
        assert!(
            diags_err
                .iter()
                .any(|d| d.message.contains("not defined") || d.message.contains("error"))
        );
    }

    #[test]
    fn test_goto_definition() {
        // Find position of the 'plus' reference in rule "E : E plus num"
        // Let's search for "plus num" inside the string
        let index = MOCK_GRAMMAR.find("plus num").unwrap();
        let pos = crate::lsp::position::offset_to_position(MOCK_GRAMMAR, index);

        let def_range = find_definition(MOCK_GRAMMAR, pos).unwrap();

        // The definition should point to "%token plus Token::Plus;"
        let def_offset = crate::lsp::position::position_to_offset(MOCK_GRAMMAR, def_range.start);
        let def_substring = &MOCK_GRAMMAR[def_offset..];
        assert!(def_substring.starts_with("plus"));

        // It should be on the line "%token plus Token::Plus;"
        let token_def_index = MOCK_GRAMMAR.find("%token plus").unwrap();
        let expected_start_pos =
            crate::lsp::position::offset_to_position(MOCK_GRAMMAR, token_def_index + 7); // start of 'plus'
        assert_eq!(def_range.start, expected_start_pos);
    }

    #[test]
    fn test_goto_definition_prec() {
        let grammar = r#"
#[derive(Debug, Clone)]
pub enum Token {
    Num(i32),
}

%%

%tokentype Token;
%start E;

%precedence empty_action;
%token num Token::Num(_);

E(_) : num
     | %prec empty_action { 0 }
     ;
"#;

        // Click on 'empty_action' after '%prec'
        let index = grammar.find("%prec empty_action").unwrap() + 6; // start of 'empty_action'
        let pos = crate::lsp::position::offset_to_position(grammar, index);

        let def_range = find_definition(grammar, pos).unwrap();

        // The definition should point to '%precedence empty_action;'
        let def_offset = crate::lsp::position::position_to_offset(grammar, def_range.start);
        let def_substring = &grammar[def_offset..];
        assert!(def_substring.starts_with("empty_action"));

        let prec_def_index = grammar.find("%precedence empty_action").unwrap();
        let expected_start_pos =
            crate::lsp::position::offset_to_position(grammar, prec_def_index + 12); // start of 'empty_action'
        assert_eq!(def_range.start, expected_start_pos);
    }
}
