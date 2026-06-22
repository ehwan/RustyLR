use lsp_types::TextEdit;
use proc_macro2::{TokenStream, TokenTree};
use rusty_lr_parser::{GrammarArgs, PatternArgs};

use crate::completion;
use crate::position::range_to_lsp_range;

const RULE_INDENT: &str = "    ";
const ACTION_INNER_INDENT: &str = "        ";

pub fn formatting(content: &str) -> Vec<TextEdit> {
    let Ok(args) = completion::parse_args(content) else {
        return Vec::new();
    };

    let mut edits = Vec::new();
    edits.extend(token_edits(&args, content));
    edits.extend(rule_edits(&args, content));
    edits
}

fn token_edits(args: &GrammarArgs, content: &str) -> Vec<TextEdit> {
    args.terminals
        .iter()
        .filter_map(|(name, body)| {
            let name_range = args.span_manager.get_byterange(&name.location())?;
            let line_start = line_start(content, name_range.start);
            let line_end = line_end(content, name_range.end);
            let semicolon = content[name_range.end.min(content.len())..line_end]
                .find(';')
                .map(|idx| name_range.end + idx)?;

            let body_text = token_stream_text(content, body).unwrap_or_default();
            let trailing = content[semicolon + 1..line_end].trim_end();
            let formatted = if body_text.is_empty() {
                format!("%token {}", name.value())
            } else {
                format!("%token {} {}", name.value(), body_text)
            };
            let mut new_text = format!("{formatted};");
            if !trailing.is_empty() {
                new_text.push_str(trailing);
            }

            Some(TextEdit {
                range: range_to_lsp_range(content, line_start..line_end),
                new_text,
            })
        })
        .collect()
}

fn rule_edits(args: &GrammarArgs, content: &str) -> Vec<TextEdit> {
    args.rules
        .iter()
        .filter_map(|rule| {
            let rule_start = args
                .span_manager
                .get_byterange(&rule.name.location())?
                .start;
            let start = line_start(content, rule_start);
            let first_separator = rule.rule_lines.first().and_then(|line| {
                args.span_manager
                    .get_byterange(&line.separator_location)
                    .map(|range| range.start)
            })?;
            let header = content[start..first_separator.min(content.len())].trim();

            let mut formatted = String::new();
            formatted.push_str(header);
            for (line_idx, line) in rule.rule_lines.iter().enumerate() {
                let tokens = line_tokens_text(args, content, line);
                let modifiers = line_modifiers_text(args, content, rule, line_idx);
                let action = line
                    .reduce_action
                    .as_ref()
                    .and_then(|action| token_stream_text(content, action));

                formatted.push('\n');
                formatted.push_str(RULE_INDENT);
                formatted.push(if line_idx == 0 { ':' } else { '|' });
                if !tokens.is_empty() {
                    formatted.push(' ');
                    formatted.push_str(&tokens);
                }
                if !modifiers.is_empty() {
                    formatted.push(' ');
                    formatted.push_str(&modifiers);
                }
                if let Some(action) = action {
                    formatted.push(' ');
                    formatted.push_str(&format_reduce_action(&action));
                }
            }
            formatted.push('\n');
            formatted.push_str(RULE_INDENT);
            formatted.push(';');

            let end = rule_block_end(args, content, rule)?;
            Some(TextEdit {
                range: range_to_lsp_range(content, start..end),
                new_text: formatted,
            })
        })
        .collect()
}

fn format_reduce_action(action: &str) -> String {
    let trimmed = action.trim();
    if !trimmed.contains('\n') {
        return trimmed.to_string();
    }

    let lines = trimmed.lines().collect::<Vec<_>>();
    let last_non_empty = lines.iter().rposition(|line| !line.trim().is_empty());
    let body_indent = lines
        .iter()
        .enumerate()
        .filter(|(idx, line)| *idx != 0 && Some(*idx) != last_non_empty && !line.trim().is_empty())
        .map(|(_, line)| leading_indent_len(line))
        .min()
        .unwrap_or(0);

    lines
        .iter()
        .enumerate()
        .map(|(idx, line)| {
            if idx == 0 {
                line.trim_end().to_string()
            } else if line.trim().is_empty() {
                String::new()
            } else if Some(idx) == last_non_empty && line.trim_start().starts_with('}') {
                format!("{RULE_INDENT}{}", line.trim_start())
            } else {
                format!("{ACTION_INNER_INDENT}{}", strip_indent(line, body_indent))
            }
        })
        .collect::<Vec<_>>()
        .join("\n")
}

fn leading_indent_len(line: &str) -> usize {
    line.char_indices()
        .find_map(|(idx, ch)| (!matches!(ch, ' ' | '\t')).then_some(idx))
        .unwrap_or(line.len())
}

fn strip_indent(line: &str, indent: usize) -> &str {
    if leading_indent_len(line) >= indent {
        &line[indent..]
    } else {
        line.trim_start()
    }
}

fn line_tokens_text(
    args: &GrammarArgs,
    content: &str,
    line: &rusty_lr_parser::RuleLineArgs,
) -> String {
    line.tokens
        .iter()
        .map(|(mapped_name, pattern)| {
            let start = mapped_name
                .as_ref()
                .and_then(|name| {
                    args.span_manager
                        .get_byterange(&name.location())
                        .map(|range| range.start)
                })
                .unwrap_or_else(|| pattern_start(args, pattern));
            let end = pattern_end(args, pattern);
            content[start.min(content.len())..end.min(content.len())]
                .trim()
                .to_string()
        })
        .collect::<Vec<_>>()
        .join(" ")
}

fn line_modifiers_text(
    args: &GrammarArgs,
    content: &str,
    rule: &rusty_lr_parser::RuleDefArgs,
    line_idx: usize,
) -> String {
    let line = &rule.rule_lines[line_idx];
    let start = line
        .tokens
        .iter()
        .map(|(_, pattern)| pattern_end(args, pattern))
        .max()
        .unwrap_or_else(|| {
            args.span_manager
                .get_byterange(&line.separator_location)
                .map_or(0, |range| range.end)
        });
    let end = line
        .reduce_action
        .as_ref()
        .and_then(|action| token_stream_range(action).map(|range| range.start))
        .unwrap_or_else(|| rule_line_end(args, content, rule, line_idx));

    content[start.min(content.len())..end.min(content.len())]
        .trim()
        .to_string()
}

fn rule_block_end(
    args: &GrammarArgs,
    content: &str,
    rule: &rusty_lr_parser::RuleDefArgs,
) -> Option<usize> {
    let last_line_idx = rule.rule_lines.len().checked_sub(1)?;
    let end_hint = rule_line_end(args, content, rule, last_line_idx);
    let semicolon = content[end_hint.min(content.len())..].find(';')?;
    Some(line_end(content, end_hint + semicolon + 1))
}

fn rule_line_end(
    args: &GrammarArgs,
    content: &str,
    rule: &rusty_lr_parser::RuleDefArgs,
    line_idx: usize,
) -> usize {
    if let Some(next_line) = rule.rule_lines.get(line_idx + 1) {
        return args
            .span_manager
            .get_byterange(&next_line.separator_location)
            .map_or(content.len(), |range| range.start);
    }

    let mut end = args
        .span_manager
        .get_byterange(&rule.name.location())
        .map_or(0, |range| range.end);
    for (_, pattern) in &rule.rule_lines[line_idx].tokens {
        end = end.max(pattern_end(args, pattern));
    }
    if let Some(action) = &rule.rule_lines[line_idx].reduce_action {
        if let Some(range) = token_stream_range(action) {
            end = end.max(range.end);
        }
    }

    content[end.min(content.len())..]
        .find(';')
        .map_or(content.len(), |semi| end + semi)
}

fn pattern_end(args: &GrammarArgs, pattern: &PatternArgs) -> usize {
    match pattern {
        PatternArgs::Ident(ident) => args
            .span_manager
            .get_byterange(&ident.location())
            .map_or(0, |range| range.end),
        PatternArgs::Plus { base, op_location }
        | PatternArgs::Star { base, op_location }
        | PatternArgs::Question { base, op_location }
        | PatternArgs::Exclamation { base, op_location } => pattern_end(args, base).max(
            args.span_manager
                .get_byterange(op_location)
                .map_or(0, |range| range.end),
        ),
        PatternArgs::TerminalSet(set) => args
            .span_manager
            .get_byterange(&set.location())
            .map_or(0, |range| range.end),
        PatternArgs::Group {
            alternatives,
            close_location,
            ..
        } => alternatives
            .iter()
            .flatten()
            .map(|pattern| pattern_end(args, pattern))
            .max()
            .unwrap_or(0)
            .max(
                args.span_manager
                    .get_byterange(close_location)
                    .map_or(0, |range| range.end),
            ),
        PatternArgs::Byte(lit) => args
            .span_manager
            .get_byterange(&lit.location())
            .map_or(0, |range| range.end),
        PatternArgs::ByteString(lit) => args
            .span_manager
            .get_byterange(&lit.location())
            .map_or(0, |range| range.end),
        PatternArgs::Char(lit) => args
            .span_manager
            .get_byterange(&lit.location())
            .map_or(0, |range| range.end),
        PatternArgs::String(lit) => args
            .span_manager
            .get_byterange(&lit.location())
            .map_or(0, |range| range.end),
        PatternArgs::Minus { base, exclude } => {
            pattern_end(args, base).max(pattern_end(args, exclude))
        }
        PatternArgs::Sep {
            base,
            delimiter,
            location,
            ..
        } => pattern_end(args, base)
            .max(pattern_end(args, delimiter))
            .max(
                args.span_manager
                    .get_byterange(location)
                    .map_or(0, |range| range.end),
            ),
    }
}

fn pattern_start(args: &GrammarArgs, pattern: &PatternArgs) -> usize {
    match pattern {
        PatternArgs::Ident(ident) => args
            .span_manager
            .get_byterange(&ident.location())
            .map_or(0, |range| range.start),
        PatternArgs::Plus { base, .. }
        | PatternArgs::Star { base, .. }
        | PatternArgs::Question { base, .. }
        | PatternArgs::Exclamation { base, .. } => pattern_start(args, base),
        PatternArgs::TerminalSet(set) => args
            .span_manager
            .get_byterange(&set.location())
            .map_or(0, |range| range.start),
        PatternArgs::Group { open_location, .. } => args
            .span_manager
            .get_byterange(open_location)
            .map_or(0, |range| range.start),
        PatternArgs::Byte(lit) => args
            .span_manager
            .get_byterange(&lit.location())
            .map_or(0, |range| range.start),
        PatternArgs::ByteString(lit) => args
            .span_manager
            .get_byterange(&lit.location())
            .map_or(0, |range| range.start),
        PatternArgs::Char(lit) => args
            .span_manager
            .get_byterange(&lit.location())
            .map_or(0, |range| range.start),
        PatternArgs::String(lit) => args
            .span_manager
            .get_byterange(&lit.location())
            .map_or(0, |range| range.start),
        PatternArgs::Minus { base, .. } => pattern_start(args, base),
        PatternArgs::Sep { location, .. } => args
            .span_manager
            .get_byterange(location)
            .map_or(0, |range| range.start),
    }
}

fn token_stream_text(content: &str, stream: &TokenStream) -> Option<String> {
    let range = token_stream_range(stream)?;
    content
        .get(range.start.min(content.len())..range.end.min(content.len()))
        .map(str::trim)
        .filter(|text| !text.is_empty())
        .map(str::to_string)
}

fn token_stream_range(stream: &TokenStream) -> Option<std::ops::Range<usize>> {
    let mut start = usize::MAX;
    let mut end = 0;
    for token in stream.clone() {
        let range = token_tree_range(token);
        start = start.min(range.start);
        end = end.max(range.end);
    }
    if start == usize::MAX {
        None
    } else {
        Some(start..end)
    }
}

fn token_tree_range(token: TokenTree) -> std::ops::Range<usize> {
    match token {
        TokenTree::Group(group) => {
            let open = group.span_open().byte_range();
            let close = group.span_close().byte_range();
            let inner = token_stream_range(&group.stream());
            let start = inner
                .as_ref()
                .map_or(open.start, |range| range.start)
                .min(open.start);
            let end = inner
                .as_ref()
                .map_or(close.end, |range| range.end)
                .max(close.end);
            start..end
        }
        TokenTree::Ident(ident) => ident.span().byte_range(),
        TokenTree::Punct(punct) => punct.span().byte_range(),
        TokenTree::Literal(lit) => lit.span().byte_range(),
    }
}

fn line_start(content: &str, offset: usize) -> usize {
    content[..offset.min(content.len())]
        .rfind('\n')
        .map_or(0, |idx| idx + 1)
}

fn line_end(content: &str, offset: usize) -> usize {
    content[offset.min(content.len())..]
        .find('\n')
        .map_or(content.len(), |idx| offset + idx)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::position::position_to_offset;

    const MOCK_GRAMMAR: &str = r#"
#[derive(Debug, Clone)]
pub enum Token {
    Num(i32),
    Plus,
}

%%

%tokentype Token;
%start E;
%token n Token::Num(_);
%token plus   Token::Plus;

E(i32):left=E plus n { left }
| n {
    n
}
;
"#;

    #[test]
    fn formats_tokens_and_productions() {
        let edits = formatting(MOCK_GRAMMAR);
        let formatted = apply_edits(MOCK_GRAMMAR, edits);

        assert!(formatted.contains("%token n Token::Num(_);"));
        assert!(formatted.contains("%token plus Token::Plus;"));
        assert!(formatted
            .contains("E(i32)\n    : left=E plus n { left }\n    | n {\n        n\n    }\n    ;"));
    }

    #[test]
    fn indents_reduce_action_body_one_level_deeper() {
        let edits = formatting(MOCK_GRAMMAR);
        let formatted = apply_edits(MOCK_GRAMMAR, edits);
        assert!(formatted.contains("{\n        n\n    }"));
    }

    #[test]
    fn reindents_reduce_action_as_a_whole_block() {
        let action = r#"{
                if n > 0 {
                    n
                } else {
                    0
                }
            }"#;

        assert_eq!(
            format_reduce_action(action),
            "{\n        if n > 0 {\n            n\n        } else {\n            0\n        }\n    }"
        );
    }

    fn apply_edits(content: &str, edits: Vec<TextEdit>) -> String {
        let mut edits = edits
            .into_iter()
            .map(|edit| {
                let start = position_to_offset(content, edit.range.start);
                let end = position_to_offset(content, edit.range.end);
                (start, end, edit.new_text)
            })
            .collect::<Vec<_>>();
        edits.sort_by_key(|(start, _, _)| *start);

        let mut result = String::new();
        let mut cursor = 0;
        for (start, end, new_text) in edits {
            result.push_str(&content[cursor..start]);
            result.push_str(&new_text);
            cursor = end;
        }
        result.push_str(&content[cursor..]);
        result
    }
}
