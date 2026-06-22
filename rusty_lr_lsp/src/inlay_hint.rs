use lsp_types::{InlayHint, InlayHintKind, InlayHintLabel, Range};
use rusty_lr_parser::grammar::Grammar;
use rusty_lr_parser::{GrammarArgs, PatternArgs};

use crate::completion;
use crate::hover;
use crate::position::{offset_to_position, position_to_offset};

pub fn inlay_hints(content: &str, range: Range) -> Vec<InlayHint> {
    let Ok(args) = completion::parse_args(content) else {
        return Vec::new();
    };
    let Ok(grammar) = Grammar::from_grammar_args(args.clone()) else {
        return Vec::new();
    };

    let range_start = position_to_offset(content, range.start);
    let range_end = position_to_offset(content, range.end);
    let mut hints = Vec::new();

    for rule in &args.rules {
        for line in &rule.rule_lines {
            for (_, pattern) in &line.tokens {
                let Some(pattern_range) = args.span_manager.get_byterange(&pattern.location())
                else {
                    continue;
                };
                if !ranges_overlap(
                    pattern_range.start,
                    pattern_range.end,
                    range_start,
                    range_end,
                ) {
                    continue;
                }

                hints.push(pattern_inlay_hint(&args, &grammar, content, pattern));
            }
        }
    }

    hints
}

fn pattern_inlay_hint(
    args: &GrammarArgs,
    grammar: &Grammar,
    content: &str,
    pattern: &PatternArgs,
) -> InlayHint {
    let end = args
        .span_manager
        .get_byterange(&pattern.location())
        .map_or(0, |range| range.end);
    let final_type = hover::pattern_final_type(args, grammar, pattern);
    InlayHint {
        position: offset_to_position(content, end),
        label: InlayHintLabel::String(format!(": {final_type}")),
        kind: Some(InlayHintKind::TYPE),
        text_edits: None,
        tooltip: None,
        padding_left: Some(true),
        padding_right: None,
        data: None,
    }
}

fn ranges_overlap(start_a: usize, end_a: usize, start_b: usize, end_b: usize) -> bool {
    start_a <= end_b && start_b <= end_a
}

#[cfg(test)]
mod tests {
    use super::*;
    use lsp_types::Position;

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

E(i32) : left=E plus num { 0 }
       | num { 0 }
       ;
List(Vec<i32>) : $sep(E, comma, +) { E };
"#;

    #[test]
    fn hints_top_level_patterns_in_rule_definitions() {
        let hints = inlay_hints(
            MOCK_GRAMMAR,
            Range::new(Position::new(0, 0), Position::new(100, 0)),
        );
        let labels = hints
            .iter()
            .map(|hint| match &hint.label {
                InlayHintLabel::String(label) => label.as_str(),
                InlayHintLabel::LabelParts(_) => "",
            })
            .collect::<Vec<_>>();

        assert!(labels.contains(&": i32"));
        assert!(labels.contains(&": Token"));
        assert!(labels.contains(&": Vec<i32>"));
    }

    #[test]
    fn does_not_hint_nested_sep_children_separately() {
        let sep_line_start = MOCK_GRAMMAR.find("List(Vec<i32>)").unwrap();
        let sep_line_end = MOCK_GRAMMAR[sep_line_start..].find(';').unwrap() + sep_line_start;
        let hints = inlay_hints(
            MOCK_GRAMMAR,
            Range::new(
                offset_to_position(MOCK_GRAMMAR, sep_line_start),
                offset_to_position(MOCK_GRAMMAR, sep_line_end),
            ),
        );
        let labels = hints
            .iter()
            .map(|hint| match &hint.label {
                InlayHintLabel::String(label) => label.as_str(),
                InlayHintLabel::LabelParts(_) => "",
            })
            .collect::<Vec<_>>();

        assert_eq!(labels, vec![": Vec<i32>"]);
    }
}
