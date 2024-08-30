use std::collections::BTreeSet;

use super::token::TokenMapped;
use proc_macro2::Ident;
use proc_macro2::Span;
use proc_macro2::TokenStream;

#[derive(Debug)]
pub struct RuleLine {
    pub tokens: Vec<TokenMapped>,
    pub reduce_action: Option<TokenStream>,
    pub separator_span: Span,
    /// force lookahead tokens for this pattern.
    pub lookaheads: Option<BTreeSet<Ident>>,
    /// user assigned id for this rule line, currently not in use
    pub id: usize,
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

    /// Name of auto generated rule are in the format of `__AutoRule ...`
    /// So we need other abbreviation for auto generated rules.
    pub pretty_name: String,
}
