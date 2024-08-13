use super::token::TokenMapped;
use proc_macro2::Span;
use proc_macro2::TokenStream;

#[derive(Debug)]
pub struct RuleLine {
    pub tokens: Vec<TokenMapped>,
    pub reduce_action: Option<TokenStream>,
    pub separator_span: Span,
}

impl RuleLine {
    pub fn span_pair(&self) -> (Span, Span) {
        let begin = self.separator_span;
        let end = if let Some(token) = self.tokens.last() {
            token.end_span
        } else {
            begin
        };
        (begin, end)
    }
}

#[derive(Debug)]
pub struct RuleLines {
    pub rule_lines: Vec<RuleLine>,
}
