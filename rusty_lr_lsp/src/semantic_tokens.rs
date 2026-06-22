use crate::position::offset_to_position;
use lsp_types::{SemanticToken, SemanticTokens};
use proc_macro2::{TokenStream, TokenTree};
use std::collections::HashSet;
use std::str::FromStr;

#[derive(Debug, Clone, PartialEq, Eq)]
struct RawSemanticToken {
    line: u32,
    start: u32,
    length: u32,
    token_type: u32,
}

/// Main entry point for semantic tokens: takes file content and returns encoded SemanticTokens.
pub fn semantic_tokens(content: &str) -> Option<SemanticTokens> {
    let grammar_start = find_grammar_start_offset(content).unwrap_or(0);
    let grammar_section = &content[grammar_start..];

    let token_stream = TokenStream::from_str(grammar_section).ok()?;
    let tokens: Vec<TokenTree> = token_stream.into_iter().collect();

    // 1. Collect terminal and non-terminal names
    let (mut terminals, mut non_terminals) = collect_names(&tokens);

    // Also attempt to get names from completion module's parsed GrammarArgs if possible
    if let Ok(args) = crate::completion::parse_args(content) {
        for (term, _) in args.terminals {
            terminals.insert(term.value().clone());
        }
        for rule in args.rules {
            non_terminals.insert(rule.name.value().clone());
        }
    }

    terminals.insert("error".to_string());

    // 2. Traverse tokens to build RawSemanticToken list
    let mut raw_tokens = Vec::new();
    traverse_tokens(
        tokens,
        content,
        grammar_start,
        false,
        &terminals,
        &non_terminals,
        &mut raw_tokens,
    );

    // 3. Sort tokens: by line, then by start character
    raw_tokens.sort_by(|a, b| {
        if a.line != b.line {
            a.line.cmp(&b.line)
        } else {
            a.start.cmp(&b.start)
        }
    });

    // 4. Delta-encode the sorted tokens
    let mut data = Vec::new();
    let mut last_line = 0;
    let mut last_start = 0;

    for token in raw_tokens {
        let delta_line = token.line - last_line;
        let delta_start = if delta_line == 0 {
            token.start - last_start
        } else {
            token.start
        };

        data.push(SemanticToken {
            delta_line,
            delta_start,
            length: token.length,
            token_type: token.token_type,
            token_modifiers_bitset: 0,
        });

        last_line = token.line;
        last_start = token.start;
    }

    Some(SemanticTokens {
        result_id: None,
        data,
    })
}

/// Find the end of the `%%` separator, which marks the start of the grammar section.
fn find_grammar_start_offset(content: &str) -> Option<usize> {
    let token_stream = TokenStream::from_str(content).ok()?;
    let mut iter = token_stream.into_iter().peekable();
    while let Some(token) = iter.next() {
        if let TokenTree::Punct(punct) = &token {
            if punct.as_char() == '%' && punct.spacing() == proc_macro2::Spacing::Joint {
                if let Some(TokenTree::Punct(next)) = iter.peek() {
                    if next.as_char() == '%' && next.spacing() == proc_macro2::Spacing::Alone {
                        return Some(next.span().byte_range().end);
                    }
                }
            }
        }
    }
    None
}

/// Helper to scan top-level tokens in the grammar section to extract terminal/non-terminal declarations.
fn collect_names(tokens: &[TokenTree]) -> (HashSet<String>, HashSet<String>) {
    let mut terminals = HashSet::new();
    let mut non_terminals = HashSet::new();
    let mut iter = tokens.iter().peekable();

    while let Some(token) = iter.next() {
        match token {
            TokenTree::Punct(punct) if punct.as_char() == '%' => {
                if let Some(TokenTree::Ident(ident)) = iter.peek() {
                    if ident.to_string() == "token" {
                        iter.next(); // consume "token"
                        if let Some(TokenTree::Ident(term_name)) = iter.peek() {
                            terminals.insert(term_name.to_string());
                        }
                    }
                }
            }
            TokenTree::Ident(ident) => {
                // Rule definition: Ident [type] : ...
                let mut temp_iter = iter.clone();
                let mut is_rule = false;
                if let Some(next) = temp_iter.peek() {
                    if let TokenTree::Group(group) = next {
                        if group.delimiter() == proc_macro2::Delimiter::Parenthesis {
                            temp_iter.next();
                        }
                    }
                }
                if let Some(TokenTree::Punct(punct)) = temp_iter.peek() {
                    if punct.as_char() == ':' {
                        is_rule = true;
                    }
                }
                if is_rule {
                    non_terminals.insert(ident.to_string());
                }
            }
            _ => {}
        }
    }

    (terminals, non_terminals)
}

/// Recursive traversal of the token stream.
fn traverse_tokens(
    tokens: Vec<TokenTree>,
    full_content: &str,
    section_offset: usize,
    in_action: bool,
    terminals: &HashSet<String>,
    non_terminals: &HashSet<String>,
    raw_tokens: &mut Vec<RawSemanticToken>,
) {
    let mut iter = tokens.into_iter().peekable();

    while let Some(token) = iter.next() {
        match token {
            TokenTree::Group(group) => {
                let delimiter = group.delimiter();
                let sub_in_action = in_action || delimiter == proc_macro2::Delimiter::Brace;
                let sub_tokens: Vec<TokenTree> = group.stream().into_iter().collect();
                traverse_tokens(
                    sub_tokens,
                    full_content,
                    section_offset,
                    sub_in_action,
                    terminals,
                    non_terminals,
                    raw_tokens,
                );
            }
            TokenTree::Punct(punct) => {
                let ch = punct.as_char();
                let span = punct.span();
                let range = span.byte_range();
                let absolute_start = section_offset + range.start;

                if ch == '%' {
                    if !in_action {
                        if let Some(TokenTree::Ident(next_ident)) = iter.peek() {
                            let next_range = next_ident.span().byte_range();
                            let absolute_end = section_offset + next_range.end;
                            let start_pos = offset_to_position(full_content, absolute_start);
                            let end_pos = offset_to_position(full_content, absolute_end);
                            raw_tokens.push(RawSemanticToken {
                                line: start_pos.line,
                                start: start_pos.character,
                                length: end_pos.character - start_pos.character,
                                token_type: 2, // directive
                            });
                            iter.next();
                            continue;
                        }
                    }
                } else if ch == '$' {
                    let mut handled = false;
                    if let Some(next) = iter.peek() {
                        match next {
                            TokenTree::Ident(next_ident) => {
                                let next_range = next_ident.span().byte_range();
                                let absolute_end = section_offset + next_range.end;
                                let start_pos = offset_to_position(full_content, absolute_start);
                                let end_pos = offset_to_position(full_content, absolute_end);
                                raw_tokens.push(RawSemanticToken {
                                    line: start_pos.line,
                                    start: start_pos.character,
                                    length: end_pos.character - start_pos.character,
                                    token_type: 4, // $var
                                });
                                iter.next();
                                handled = true;
                            }
                            TokenTree::Literal(next_lit) => {
                                let next_str = next_lit.to_string();
                                if next_str.chars().all(|c| c.is_ascii_digit()) {
                                    let next_range = next_lit.span().byte_range();
                                    let absolute_end = section_offset + next_range.end;
                                    let start_pos =
                                        offset_to_position(full_content, absolute_start);
                                    let end_pos = offset_to_position(full_content, absolute_end);
                                    raw_tokens.push(RawSemanticToken {
                                        line: start_pos.line,
                                        start: start_pos.character,
                                        length: end_pos.character - start_pos.character,
                                        token_type: 4, // $var
                                    });
                                    iter.next();
                                    handled = true;
                                }
                            }
                            TokenTree::Punct(next_punct) if next_punct.as_char() == '$' => {
                                let next_range = next_punct.span().byte_range();
                                let absolute_end = section_offset + next_range.end;
                                let start_pos = offset_to_position(full_content, absolute_start);
                                let end_pos = offset_to_position(full_content, absolute_end);
                                raw_tokens.push(RawSemanticToken {
                                    line: start_pos.line,
                                    start: start_pos.character,
                                    length: end_pos.character - start_pos.character,
                                    token_type: 4, // $var
                                });
                                iter.next();
                                handled = true;
                            }
                            _ => {}
                        }
                    }
                    if !handled {
                        let start_pos = offset_to_position(full_content, absolute_start);
                        let end_pos = offset_to_position(full_content, section_offset + range.end);
                        raw_tokens.push(RawSemanticToken {
                            line: start_pos.line,
                            start: start_pos.character,
                            length: end_pos.character - start_pos.character,
                            token_type: 4, // $var
                        });
                    }
                } else if ch == '@' {
                    let mut handled = false;
                    if let Some(next) = iter.peek() {
                        match next {
                            TokenTree::Ident(next_ident) => {
                                let next_range = next_ident.span().byte_range();
                                let absolute_end = section_offset + next_range.end;
                                let start_pos = offset_to_position(full_content, absolute_start);
                                let end_pos = offset_to_position(full_content, absolute_end);
                                raw_tokens.push(RawSemanticToken {
                                    line: start_pos.line,
                                    start: start_pos.character,
                                    length: end_pos.character - start_pos.character,
                                    token_type: 5, // @loc
                                });
                                iter.next();
                                handled = true;
                            }
                            TokenTree::Literal(next_lit) => {
                                let next_str = next_lit.to_string();
                                if next_str.chars().all(|c| c.is_ascii_digit()) {
                                    let next_range = next_lit.span().byte_range();
                                    let absolute_end = section_offset + next_range.end;
                                    let start_pos =
                                        offset_to_position(full_content, absolute_start);
                                    let end_pos = offset_to_position(full_content, absolute_end);
                                    raw_tokens.push(RawSemanticToken {
                                        line: start_pos.line,
                                        start: start_pos.character,
                                        length: end_pos.character - start_pos.character,
                                        token_type: 5, // @loc
                                    });
                                    iter.next();
                                    handled = true;
                                }
                            }
                            TokenTree::Punct(next_punct) if next_punct.as_char() == '$' => {
                                let next_range = next_punct.span().byte_range();
                                let absolute_end = section_offset + next_range.end;
                                let start_pos = offset_to_position(full_content, absolute_start);
                                let end_pos = offset_to_position(full_content, absolute_end);
                                raw_tokens.push(RawSemanticToken {
                                    line: start_pos.line,
                                    start: start_pos.character,
                                    length: end_pos.character - start_pos.character,
                                    token_type: 5, // @loc
                                });
                                iter.next();
                                handled = true;
                            }
                            _ => {}
                        }
                    }
                    if !handled {
                        let start_pos = offset_to_position(full_content, absolute_start);
                        let end_pos = offset_to_position(full_content, section_offset + range.end);
                        raw_tokens.push(RawSemanticToken {
                            line: start_pos.line,
                            start: start_pos.character,
                            length: end_pos.character - start_pos.character,
                            token_type: 5, // @loc
                        });
                    }
                }
            }
            TokenTree::Ident(ident) => {
                let name = ident.to_string();
                if in_action {
                    if name == "data" || name == "lookahead" || name == "shift" {
                        let span = ident.span();
                        let range = span.byte_range();
                        let absolute_start = section_offset + range.start;
                        let absolute_end = section_offset + range.end;
                        let start_pos = offset_to_position(full_content, absolute_start);
                        let end_pos = offset_to_position(full_content, absolute_end);
                        raw_tokens.push(RawSemanticToken {
                            line: start_pos.line,
                            start: start_pos.character,
                            length: end_pos.character - start_pos.character,
                            token_type: 3, // binding / parameter
                        });
                    }
                    continue;
                }

                let span = ident.span();
                let range = span.byte_range();
                let absolute_start = section_offset + range.start;
                let absolute_end = section_offset + range.end;
                let name = ident.to_string();

                let mut is_binding = false;
                if let Some(TokenTree::Punct(next_punct)) = iter.peek() {
                    if next_punct.as_char() == '=' {
                        is_binding = true;
                    }
                }

                let token_type = if is_binding {
                    Some(3) // binding
                } else if terminals.contains(&name) {
                    Some(0) // terminal
                } else if non_terminals.contains(&name) {
                    Some(1) // non-terminal
                } else {
                    None
                };

                if let Some(tt) = token_type {
                    let start_pos = offset_to_position(full_content, absolute_start);
                    let end_pos = offset_to_position(full_content, absolute_end);
                    raw_tokens.push(RawSemanticToken {
                        line: start_pos.line,
                        start: start_pos.character,
                        length: end_pos.character - start_pos.character,
                        token_type: tt,
                    });
                }
            }
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const MOCK_GRAMMAR: &str = r#"
#[derive(Debug, Clone)]
pub enum Token {
    Num(i32),
    Plus,
    Comma,
}

%%

%tokentype Token;
%start List;

%token num Token::Num(_);
%token plus Token::Plus;
%token comma Token::Comma;

E(i32) : left=E plus num { $1 + $3 }
       | error { *data += 1; 0 }
       ;
List(Vec<i32>) : $sep(E, comma, +) { E };
"#;

    #[test]
    fn test_semantic_tokens() {
        let tokens_res = semantic_tokens(MOCK_GRAMMAR).expect("Failed to parse semantic tokens");
        let data_res = tokens_res.data;
        assert!(!data_res.is_empty());

        // Decode delta-encoded tokens and map back to substrings
        let mut decoded = Vec::new();
        let mut current_line = 0;
        let mut current_char = 0;
        for token in &data_res {
            current_line += token.delta_line;
            if token.delta_line == 0 {
                current_char += token.delta_start;
            } else {
                current_char = token.delta_start;
            }

            // Find substring in MOCK_GRAMMAR
            let pos = lsp_types::Position::new(current_line, current_char);
            let start_offset = crate::position::position_to_offset(MOCK_GRAMMAR, pos);
            let end_offset = start_offset + token.length as usize;
            let text = &MOCK_GRAMMAR[start_offset..end_offset];
            decoded.push((text.to_string(), token.token_type));
        }

        // Directives (type 2)
        assert!(decoded.contains(&("%tokentype".to_string(), 2)));
        assert!(decoded.contains(&("%start".to_string(), 2)));
        assert!(decoded.contains(&("%token".to_string(), 2)));

        // Terminals (type 0)
        assert!(decoded.contains(&("num".to_string(), 0)));
        assert!(decoded.contains(&("plus".to_string(), 0)));
        assert!(decoded.contains(&("comma".to_string(), 0)));
        assert!(decoded.contains(&("error".to_string(), 0))); // reserved terminal

        // Non-terminals (type 1)
        assert!(decoded.contains(&("E".to_string(), 1)));
        assert!(decoded.contains(&("List".to_string(), 1)));

        // Bindings / parameters (type 3)
        assert!(decoded.contains(&("left".to_string(), 3)));
        assert!(decoded.contains(&("data".to_string(), 3))); // reserved reduce parameter

        // $vars (type 4)
        assert!(decoded.contains(&("$1".to_string(), 4)));
        assert!(decoded.contains(&("$3".to_string(), 4)));
        assert!(decoded.contains(&("$sep".to_string(), 4)));
    }
}
