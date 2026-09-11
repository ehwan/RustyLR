use lsp_types::{Position, Range};

use super::grammar::{
    IdentifierScope, collect_identifiers, identifier_at_offset, parse_args, symbol_definition,
};
use super::position::{position_to_offset, range_to_lsp_range};

/// Finds all references of the symbol under the cursor, including declarations.
pub fn find_references(content: &str, target_pos: Position) -> Option<Vec<Range>> {
    let args = parse_args(content).ok()?;
    let identifiers = collect_identifiers(&args, IdentifierScope::SymbolReferences);
    let clicked =
        identifier_at_offset(&args, &identifiers, position_to_offset(content, target_pos))?;
    let name = clicked.value();

    // The recovery symbol is implicit and has no declaration in the grammar.
    if name != "error" && symbol_definition(&args, name).is_none() {
        return None;
    }

    Some(
        identifiers
            .iter()
            .filter(|ident| ident.value() == name)
            .filter_map(|ident| args.span_manager.get_byterange(&ident.location()))
            .map(|range| range_to_lsp_range(content, range))
            .collect(),
    )
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
