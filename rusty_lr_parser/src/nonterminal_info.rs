use std::collections::BTreeSet;

use super::token::TokenMapped;
use proc_macro2::Ident;
use proc_macro2::Span;
use proc_macro2::TokenStream;

pub struct ReduceAction {
    /// The {BraceGroup} of the action
    pub stream: TokenStream,
    /// if this action is auto-generated by this parser
    pub generated: bool,

    /// if this action is identity action |x| x (from auto-generated rules)
    pub identity: bool,
}

pub struct Rule {
    pub tokens: Vec<TokenMapped>,
    /// reduce action called when this rule is reduced
    pub reduce_action: Option<ReduceAction>,
    /// span of '|' or ':' before this production rule
    pub separator_span: Span,
    /// force lookahead tokens for this pattern.
    pub lookaheads: Option<BTreeSet<usize>>,
    /// %prec definition
    pub prec: Option<(rusty_lr_core::builder::Operator<usize>, Span)>,
    /// %dprec definition
    pub dprec: Option<(usize, Span)>,
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
    pub(crate) regex_span: Option<(Span, Span)>,

    pub(crate) trace: bool,
    /// protected from optimization removal; trace rules are always protected
    pub(crate) protected: bool,
}

impl NonTerminalInfo {
    pub fn is_auto_generated(&self) -> bool {
        self.regex_span.is_some()
    }
    /// only for auto-generated rules
    /// returns the span of the regex pattern that generated this rule
    pub fn origin_span(&self) -> Option<(Span, Span)> {
        self.regex_span
    }

    /// if this non-terminal is protected from optimization; will not be automatically deleted
    pub(crate) fn is_protected(&self) -> bool {
        self.protected
    }
}
