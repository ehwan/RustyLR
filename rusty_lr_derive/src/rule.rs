use super::token::Token;
use proc_macro2::TokenStream;

#[derive(Debug)]
pub struct RuleLine {
    pub tokens: Vec<Token>,
    pub reduce_action: Option<TokenStream>,
}

#[derive(Debug)]
pub struct RuleLines {
    pub rule_lines: Vec<RuleLine>,
}
