use std::collections::BTreeSet;

use crate::parser::args::IdentOrLiteral;

use super::token::TokenMapped;
use proc_macro2::Ident;
use proc_macro2::Span;
use proc_macro2::TokenStream;

pub struct Rule {
    pub tokens: Vec<TokenMapped>,
    pub reduce_action: Option<TokenStream>,
    pub reduce_action_generated: bool,
    /// span of '|' or ':' before this production rule
    pub separator_span: Span,
    /// force lookahead tokens for this pattern.
    pub lookaheads: Option<BTreeSet<usize>>,
    /// %prec definition
    pub prec: Option<(rusty_lr_core::builder::Operator<usize>, IdentOrLiteral)>,
}

impl Rule {
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

pub struct NonTerminalInfo {
    pub name: Ident,

    /// Name of auto generated rule are in the format of `__AutoRule ...`
    /// So we need other abbreviation for auto generated rules.
    pub pretty_name: String,

    /// The rule type of this non-terminal
    pub ruletype: Option<TokenStream>,

    /// Every set of production rules
    pub rules: Vec<Rule>,

    /// If this non-terminal is auto-generated from regex pattern,
    /// the (begin, end) span-pair of the regex pattern.
    pub regex_span: Option<(Span, Span)>,
}
