use super::token::TokenMapped;
use proc_macro2::TokenStream;

#[derive(Debug)]
pub struct RuleLine {
    pub tokens: Vec<TokenMapped>,
    pub reduce_action: Option<TokenStream>,
}

#[derive(Debug)]
pub struct RuleLines {
    pub rule_lines: Vec<RuleLine>,
}
