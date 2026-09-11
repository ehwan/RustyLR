use lsp_types::{Position, Range};

use super::grammar::{
    IdentifierScope, collect_identifiers, identifier_at_offset, parse_args, symbol_definition,
};
use super::position::{position_to_offset, range_to_lsp_range};

/// Locates the definition of the symbol under the cursor.
pub fn find_definition(content: &str, target_pos: Position) -> Option<Range> {
    let args = parse_args(content).ok()?;
    let identifiers = collect_identifiers(&args, IdentifierScope::DefinitionLookup);
    let clicked =
        identifier_at_offset(&args, &identifiers, position_to_offset(content, target_pos))?;
    let definition = symbol_definition(&args, clicked.value())?;
    let range = args.span_manager.get_byterange(&definition.location())?;
    Some(range_to_lsp_range(content, range))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lsp::diagnostics::split_stream;
    use proc_macro2::TokenStream;
    use std::str::FromStr;

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
