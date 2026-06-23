use lsp_types::{Position, Range};
use proc_macro2::TokenStream;
use rusty_lr_parser::grammar::Grammar;
use rusty_lr_parser::{
    GrammarArgs, IdentOrLiteral, Located, PatternArgs, PrecDPrecArgs, TerminalSetItem,
};
use std::str::FromStr;

use crate::lsp::diagnostics::split_stream;
use crate::lsp::position::{position_to_offset, range_to_lsp_range};

/// Traverses the AST of GrammarArgs to collect only terminal, non-terminal, prec, and error references.
fn collect_references(args: &GrammarArgs) -> Vec<Located<String>> {
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

    // 4. Rule definitions, pattern idents, and %prec
    for rule in &args.rules {
        collected.push(rule.name.clone());
        for line in &rule.rule_lines {
            // Pattern idents
            for (_, pattern) in &line.tokens {
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

/// Finds all references of the terminal or non-terminal symbol under the cursor.
pub fn find_references(content: &str, target_pos: Position) -> Option<Vec<Range>> {
    let offset = position_to_offset(content, target_pos);

    // Parse the entire document into TokenStream
    let token_stream = TokenStream::from_str(content).ok()?;
    let (_, macro_stream) = split_stream(token_stream).ok()?;
    let grammar_args = Grammar::parse_args(macro_stream).ok()?;
    let span_manager = grammar_args.span_manager.clone();

    // Collect all referenceable locations
    let all_references = collect_references(&grammar_args);

    // Find the one that contains the click offset
    let clicked = all_references.iter().find(|loc| {
        if let Some(range) = span_manager.get_byterange(&loc.location()) {
            range.contains(&offset)
        } else {
            false
        }
    })?;

    let name = clicked.value();

    // Ensure the symbol is indeed a valid terminal, non-terminal, precedence symbol, or 'error'
    let is_terminal = grammar_args.terminals.iter().any(|(t, _)| t.value == *name);
    let is_nonterminal = grammar_args.rules.iter().any(|r| r.name.value == *name);
    let is_prec_symbol = grammar_args.precedences.iter().any(|(_, _, items)| {
        items.iter().any(|item| match item {
            IdentOrLiteral::Ident(ident) => ident.value() == name,
            _ => false,
        })
    });
    let is_error = name == "error";

    if !is_terminal && !is_nonterminal && !is_prec_symbol && !is_error {
        return None;
    }

    // Filter and map all matches of the clicked name to LSP Range
    let mut result = Vec::new();
    for loc in &all_references {
        if loc.value() == name {
            if let Some(range) = span_manager.get_byterange(&loc.location()) {
                result.push(range_to_lsp_range(content, range));
            }
        }
    }

    Some(result)
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

    #[test]
    fn test_find_references_terminal() {
        // Find position of the 'plus' in rule "E plus num"
        let index = MOCK_GRAMMAR.find("plus num").unwrap();
        let pos = crate::lsp::position::offset_to_position(MOCK_GRAMMAR, index);

        let refs = find_references(MOCK_GRAMMAR, pos).unwrap();

        // There should be 2 references:
        // 1. "%token plus Token::Plus;" (definition)
        // 2. "E plus num" (usage)
        assert_eq!(refs.len(), 2);

        // Verify the content at each range
        for range in refs {
            let start = crate::lsp::position::position_to_offset(MOCK_GRAMMAR, range.start);
            let end = crate::lsp::position::position_to_offset(MOCK_GRAMMAR, range.end);
            assert_eq!(&MOCK_GRAMMAR[start..end], "plus");
        }
    }

    #[test]
    fn test_find_references_nonterminal() {
        // Find position of '%start E'
        let index = MOCK_GRAMMAR.find("start E").unwrap() + 6; // start of 'E'
        let pos = crate::lsp::position::offset_to_position(MOCK_GRAMMAR, index);

        let refs = find_references(MOCK_GRAMMAR, pos).unwrap();

        // References to E:
        // 1. "%start E;"
        // 2. "E(_)" (definition)
        // 3. "E plus num" (usage)
        assert_eq!(refs.len(), 3);

        for range in refs {
            let start = crate::lsp::position::position_to_offset(MOCK_GRAMMAR, range.start);
            let end = crate::lsp::position::position_to_offset(MOCK_GRAMMAR, range.end);
            assert_eq!(&MOCK_GRAMMAR[start..end], "E");
        }
    }

    #[test]
    fn test_find_references_prec_and_error() {
        let grammar = r#"
#[derive(Debug, Clone)]
pub enum Token {
    Num(i32),
    Plus,
    Minus,
}

%%

%tokentype Token;
%start E;

%left plus;
%left minus;
%token num Token::Num(_);
%token plus Token::Plus;
%token minus Token::Minus;

E(_) : E plus E
     | minus E %prec minus
     | error
     ;
"#;

        // 1. Find references to precedence/terminal 'minus'
        // Click on '%prec minus'
        let index = grammar.find("%prec minus").unwrap() + 6; // start of 'minus'
        let pos = crate::lsp::position::offset_to_position(grammar, index);
        let refs = find_references(grammar, pos).unwrap();

        // References to 'minus':
        // - "%left minus;"
        // - "%token minus Token::Minus;"
        // - "minus E" (rule pattern)
        // - "%prec minus" (precedence override)
        assert_eq!(refs.len(), 4);
        for range in refs {
            let start = crate::lsp::position::position_to_offset(grammar, range.start);
            let end = crate::lsp::position::position_to_offset(grammar, range.end);
            assert_eq!(&grammar[start..end], "minus");
        }

        // 2. Find references to 'error'
        let index = grammar.find("error").unwrap();
        let pos = crate::lsp::position::offset_to_position(grammar, index);
        let refs = find_references(grammar, pos).unwrap();

        assert_eq!(refs.len(), 1);
        let range = refs[0];
        let start = crate::lsp::position::position_to_offset(grammar, range.start);
        let end = crate::lsp::position::position_to_offset(grammar, range.end);
        assert_eq!(&grammar[start..end], "error");
    }

    #[test]
    fn test_find_references_no_action_leak() {
        // The mock grammar has `{ 0 }` inside the reduce action.
        // If we search inside the reduce action, it shouldn't match anything.
        // We verify that clicking inside `{ 0 }` returns None.
        let index = MOCK_GRAMMAR.find("{ 0 }").unwrap() + 2; // points to '0'
        let pos = crate::lsp::position::offset_to_position(MOCK_GRAMMAR, index);

        let refs = find_references(MOCK_GRAMMAR, pos);
        assert!(refs.is_none());
    }
}
