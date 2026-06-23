use lsp_types::{
    CodeAction, CodeActionKind, CodeActionOrCommand, Diagnostic, Position, Range, TextEdit, Uri,
    WorkspaceEdit,
};
use std::collections::{HashMap, HashSet};

pub fn code_actions(
    content: &str,
    uri: Uri,
    diagnostics: Vec<Diagnostic>,
) -> Vec<CodeActionOrCommand> {
    let insert_position = allow_insert_position(content);
    let mut seen = HashSet::new();
    let mut actions = Vec::new();

    for diagnostic in diagnostics {
        let Some(allow) = allow_suggestion(&diagnostic) else {
            continue;
        };
        if !seen.insert(allow.clone()) {
            continue;
        }

        let mut changes = HashMap::new();
        changes.insert(
            uri.clone(),
            vec![TextEdit {
                range: Range::new(insert_position, insert_position),
                new_text: format!("{allow}\n"),
            }],
        );

        actions.push(CodeActionOrCommand::CodeAction(CodeAction {
            title: format!("Insert `{allow}`"),
            kind: Some(CodeActionKind::QUICKFIX),
            diagnostics: Some(vec![diagnostic]),
            edit: Some(WorkspaceEdit {
                changes: Some(changes),
                document_changes: None,
                change_annotations: None,
            }),
            command: None,
            is_preferred: Some(true),
            disabled: None,
            data: None,
        }));
    }

    actions
}

fn allow_suggestion(diagnostic: &Diagnostic) -> Option<String> {
    diagnostic
        .data
        .as_ref()?
        .get("rustylr_allow")?
        .as_str()
        .filter(|suggestion| suggestion.starts_with("%allow "))
        .map(str::to_string)
}

fn allow_insert_position(content: &str) -> Position {
    let line = content
        .lines()
        .position(|line| line.trim() == "%%")
        .map_or(0, |line| line + 1);
    Position::new(line as u32, 0)
}

#[cfg(test)]
mod tests {
    use super::*;
    use lsp_types::DiagnosticSeverity;
    use serde_json::json;

    #[test]
    fn creates_allow_quick_fix_from_diagnostic_data() {
        let uri = "file:///test.rustylr".parse().unwrap();
        let diagnostic = Diagnostic {
            range: Range::default(),
            severity: Some(DiagnosticSeverity::WARNING),
            code: None,
            code_description: None,
            source: Some("rusty_lr".to_string()),
            message: "unused".to_string(),
            related_information: None,
            tags: None,
            data: Some(json!({ "rustylr_allow": "%allow unused_terminals(num);" })),
        };

        let actions = code_actions("mod x {}\n%%\n%start E;\n", uri, vec![diagnostic]);
        assert_eq!(actions.len(), 1);

        let CodeActionOrCommand::CodeAction(action) = &actions[0] else {
            panic!("expected code action");
        };
        assert_eq!(action.title, "Insert `%allow unused_terminals(num);`");
        let edit = action.edit.as_ref().unwrap();
        let changes = edit.changes.as_ref().unwrap();
        let text_edit = changes.values().next().unwrap().first().unwrap();
        assert_eq!(text_edit.range.start, Position::new(2, 0));
        assert_eq!(text_edit.new_text, "%allow unused_terminals(num);\n");
    }

    #[test]
    fn deduplicates_same_allow_suggestion() {
        let uri = "file:///test.rustylr".parse().unwrap();
        let diagnostic = Diagnostic {
            range: Range::default(),
            severity: Some(DiagnosticSeverity::WARNING),
            code: None,
            code_description: None,
            source: Some("rusty_lr".to_string()),
            message: "unused".to_string(),
            related_information: None,
            tags: None,
            data: Some(json!({ "rustylr_allow": "%allow nonterm_unreachable(E);" })),
        };

        let actions = code_actions("%%\n%start E;\n", uri, vec![diagnostic.clone(), diagnostic]);
        assert_eq!(actions.len(), 1);
    }
}
