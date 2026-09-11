use proc_macro2::TokenStream;
use rusty_lr_parser::grammar::Grammar;
use rusty_lr_parser::{
    GrammarArgs, IdentOrLiteral, Located, PatternArgs, PrecDPrecArgs, TerminalSetItem,
};
use std::str::FromStr;

use super::diagnostics::split_stream;

pub(super) fn parse_args(content: &str) -> Result<GrammarArgs, ()> {
    let token_stream = TokenStream::from_str(content).map_err(|_| ())?;
    let (_, macro_stream) = split_stream(token_stream)?;
    Grammar::parse_args(macro_stream).map_err(|_| ())
}

#[derive(Clone, Copy)]
pub(super) enum IdentifierScope {
    /// Definition lookup also accepts local bindings and diagnostic names.
    DefinitionLookup,
    /// References exclude local bindings and diagnostic names, even when names coincide.
    SymbolReferences,
}

/// Collects identifiers in AST traversal order without copying their names or locations.
pub(super) fn collect_identifiers(
    args: &GrammarArgs,
    scope: IdentifierScope,
) -> Vec<&Located<String>> {
    let mut collected = Vec::new();

    for start_name in &args.start_rule_name {
        collected.push(start_name);
    }

    for (terminal, _) in &args.terminals {
        collected.push(terminal);
    }

    for (_, _, items) in &args.precedences {
        for item in items {
            if let IdentOrLiteral::Ident(ident) = item {
                collected.push(ident);
            }
        }
    }

    if matches!(scope, IdentifierScope::DefinitionLookup) {
        for (allow_name, _) in &args.allowed_diagnostics {
            collected.push(allow_name);
        }
    }

    for nonterminal in &args.rules {
        collected.push(&nonterminal.name);
        for production in &nonterminal.rule_lines {
            for (binding, pattern) in &production.tokens {
                if matches!(scope, IdentifierScope::DefinitionLookup) {
                    if let Some(identifier) = binding {
                        collected.push(identifier);
                    }
                }
                collect_pattern_identifiers(pattern, &mut collected);
            }
            for prec in &production.precs {
                if let PrecDPrecArgs::Prec(IdentOrLiteral::Ident(ident)) = prec {
                    collected.push(ident);
                }
            }
        }
    }

    collected
}

fn collect_pattern_identifiers<'a>(
    pattern: &'a PatternArgs,
    collected: &mut Vec<&'a Located<String>>,
) {
    match pattern {
        PatternArgs::Ident(ident) => {
            collected.push(ident);
        }
        PatternArgs::Plus { base, .. }
        | PatternArgs::Star { base, .. }
        | PatternArgs::Question { base, .. }
        | PatternArgs::Exclamation { base, .. } => {
            collect_pattern_identifiers(base, collected);
        }
        PatternArgs::TerminalSet(ts) => {
            for item in &ts.items {
                match item {
                    TerminalSetItem::Terminal(ident) => {
                        collected.push(ident);
                    }
                    TerminalSetItem::Range(first, last) => {
                        collected.push(first);
                        collected.push(last);
                    }
                    _ => {}
                }
            }
        }
        PatternArgs::Group { alternatives, .. } => {
            for alt in alternatives {
                for pat in alt {
                    collect_pattern_identifiers(pat, collected);
                }
            }
        }
        PatternArgs::Minus { base, exclude } => {
            collect_pattern_identifiers(base, collected);
            collect_pattern_identifiers(exclude, collected);
        }
        PatternArgs::Sep {
            base, delimiter, ..
        } => {
            collect_pattern_identifiers(base, collected);
            collect_pattern_identifiers(delimiter, collected);
        }
        PatternArgs::Byte(_)
        | PatternArgs::ByteString(_)
        | PatternArgs::Char(_)
        | PatternArgs::String(_) => {}
    }
}

pub(super) fn identifier_at_offset<'a>(
    args: &GrammarArgs,
    identifiers: &[&'a Located<String>],
    offset: usize,
) -> Option<&'a Located<String>> {
    identifiers.iter().copied().find(|ident| {
        args.span_manager
            .get_byterange(&ident.location())
            .is_some_and(|range| range.contains(&offset))
    })
}

/// Nonterminal and terminal declarations take priority over precedence declarations.
pub(super) fn symbol_definition<'a>(
    args: &'a GrammarArgs,
    name: &str,
) -> Option<&'a Located<String>> {
    let nonterminals = args.rules.iter().map(|nonterminal| &nonterminal.name);
    let terminals = args.terminals.iter().map(|(name, _)| name);
    let precedence_symbols = args
        .precedences
        .iter()
        .flat_map(|(_, _, items)| items)
        .filter_map(|item| match item {
            IdentOrLiteral::Ident(ident) => Some(ident),
            IdentOrLiteral::Byte(_) | IdentOrLiteral::Char(_) => None,
        });

    nonterminals
        .chain(terminals)
        .chain(precedence_symbols)
        .find(|ident| ident.value() == name)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lsp::goto_definition::find_definition;
    use crate::lsp::position::{offset_to_position, range_to_lsp_range};
    use crate::lsp::references::find_references;

    #[test]
    fn navigation_covers_nested_patterns() {
        let content = r#"
%%
%tokentype char;
%start E;
%token a 'a';
%token z 'z';
%left a;
E : (a+ | a* | a? | a!) $sep(a, z, +) [a-z] [a z]-z %prec a;
"#;
        // Each occurrence must navigate to the terminal declaration, including
        // precedence declarations and both endpoints of a terminal range.
        for name in ["a", "z"] {
            let declaration_offset = content.find(&format!("%token {name}")).unwrap() + 7;
            let expected =
                range_to_lsp_range(content, declaration_offset..declaration_offset + name.len());
            let offsets: Vec<_> = content
                .match_indices(name)
                .filter(|(offset, _)| {
                    let previous = content[..*offset].chars().next_back();
                    let next = content[*offset + name.len()..].chars().next();
                    !previous.is_some_and(|c| c.is_alphanumeric() || c == '\'')
                        && !next.is_some_and(|c| c.is_alphanumeric() || c == '\'')
                })
                .map(|(offset, _)| offset)
                .collect();
            let expected_references: Vec<_> = offsets
                .iter()
                .map(|&offset| range_to_lsp_range(content, offset..offset + name.len()))
                .collect();
            for offset in offsets {
                let position = offset_to_position(content, offset);
                assert_eq!(find_definition(content, position), Some(expected));
                let mut actual = find_references(content, position).unwrap();
                actual.sort_by_key(|range| (range.start.line, range.start.character));
                assert_eq!(actual, expected_references);
            }
        }
    }

    #[test]
    fn reference_scope_excludes_bindings_diagnostics_and_actions() {
        let content = r#"
%%
%tokentype char;
%start E;
%token a 'a';
%allow a;
E : a=a { a };
"#;
        let declaration_offset = content.find("%token a").unwrap() + 7;
        let expected = range_to_lsp_range(content, declaration_offset..declaration_offset + 1);
        for offset in [
            content.find("%allow a").unwrap() + 7,
            content.find("a=a").unwrap(),
        ] {
            let position = offset_to_position(content, offset);
            assert_eq!(find_definition(content, position), Some(expected));
            assert_eq!(find_references(content, position), None);
        }
        let usage_offset = content.find("a=a").unwrap() + 2;
        assert_eq!(
            find_references(content, offset_to_position(content, usage_offset)),
            Some(vec![
                expected,
                range_to_lsp_range(content, usage_offset..usage_offset + 1)
            ])
        );
        for offset in [usage_offset + 1, content.find("{ a }").unwrap() + 2] {
            let position = offset_to_position(content, offset);
            assert_eq!(find_definition(content, position), None);
            assert_eq!(find_references(content, position), None);
        }
    }

    #[test]
    fn navigation_rejects_invalid_input_and_undeclared_symbols() {
        for content in ["", "%% E : (", "%% %start E; E : missing;"] {
            let offset = content.find("missing").unwrap_or(0);
            let position = offset_to_position(content, offset);
            assert_eq!(find_definition(content, position), None);
            assert_eq!(find_references(content, position), None);
        }
    }
}
