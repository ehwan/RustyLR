use std::collections::BTreeSet;

use super::token::TokenMapped;
use proc_macro2::Ident;
use proc_macro2::Span;
use proc_macro2::TokenStream;

pub enum ReduceAction {
    /// reduce action that is function-like TokenStream
    Custom(TokenStream),
    /// reduce action that is auto-generated, and simply returns the i'th token itself
    Identity(usize), // index of the token in the rule
}

impl ReduceAction {
    pub fn is_identity(&self) -> bool {
        matches!(self, ReduceAction::Identity(_))
    }
    pub fn is_custom(&self) -> bool {
        matches!(self, ReduceAction::Custom(_))
    }
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
    pub prec: Option<(rusty_lr_core::rule::Precedence, Span)>,
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

    /// if this non-terminal is auto-generated, the pattern that generated this rule.
    /// This field is used in rusty_lr_core/tree.rs to unwrap left/right recursion parsing tree into flat array.
    pub(crate) nonterm_type: Option<rusty_lr_core::nonterminal::NonTerminalType>,
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
