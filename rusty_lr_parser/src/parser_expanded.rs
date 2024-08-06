use crate::error::ParseError;
use crate::grammar::Grammar;
use crate::rule::RuleLine;
use crate::rule::RuleLines;
use crate::term::TermType;
use crate::token::Token;
use crate::token::TokenMapped;
use proc_macro2::Group;
use proc_macro2::Ident;
use proc_macro2::Span;
use proc_macro2::TokenStream;
use quote::ToTokens;
use rusty_lr_core::ReduceType;
#[allow(
    unused_braces,
    unused_parens,
    unused_variables,
    non_snake_case,
    unused_mut
)]
pub struct GrammarContext {
    pub rustylr_macro_generated_rl_terms_stack: Vec<TermType>,
    pub rustylr_macro_generated_rl_end_stack: Vec<usize>,
    pub state_stack: Vec<usize>,
    pub rustylr_macro_generated_TokensOne_stack: Vec<(Vec<TokenMapped>)>,
    pub rustylr_macro_generated_StartDef_stack: Vec<(Ident)>,
    pub rustylr_macro_generated_UserDataDef_stack: Vec<((Span, TokenStream))>,
    pub rustylr_macro_generated_ErrorDef_stack: Vec<((Span, TokenStream))>,
    pub rustylr_macro_generated_ReduceDef_stack: Vec<((Ident, ReduceType))>,
    pub rustylr_macro_generated_RuleDef_stack: Vec<(Vec<TokenMapped>)>,
    pub rustylr_macro_generated_RuleType_stack: Vec<(Option<Group>)>,
    pub rustylr_macro_generated_EofDef_stack: Vec<((Span, TokenStream))>,
    pub rustylr_macro_generated_RuleLine_stack: Vec<(RuleLine)>,
    pub rustylr_macro_generated_Rule_stack: Vec<((Ident, Option<TokenStream>, RuleLines))>,
    pub rustylr_macro_generated_Action_stack: Vec<(Option<Group>)>,
    pub rustylr_macro_generated_RustCode_stack: Vec<(TokenStream)>,
    pub rustylr_macro_generated_TokenTypeDef_stack: Vec<((Span, TokenStream))>,
    pub rustylr_macro_generated_Tokens_stack: Vec<(Vec<TokenMapped>)>,
    pub rustylr_macro_generated_RuleLines_stack: Vec<(Vec<RuleLine>)>,
    pub rustylr_macro_generated_Token_stack: Vec<(TokenMapped)>,
    pub rustylr_macro_generated_TokenDef_stack: Vec<((Ident, TokenStream))>,
    pub rustylr_macro_generated_ModulePrefixDef_stack: Vec<((Span, TokenStream))>,
    pub rustylr_macro_generated_Grammar_stack: Vec<(Grammar)>,
}
#[allow(
    unused_braces,
    unused_parens,
    unused_variables,
    non_snake_case,
    unused_mut
)]
impl GrammarContext {
    pub fn new() -> Self {
        Self {
            rustylr_macro_generated_rl_terms_stack: Vec::new(),
            rustylr_macro_generated_rl_end_stack: vec![0],
            state_stack: vec![0],
            rustylr_macro_generated_TokensOne_stack: Vec::new(),
            rustylr_macro_generated_StartDef_stack: Vec::new(),
            rustylr_macro_generated_UserDataDef_stack: Vec::new(),
            rustylr_macro_generated_ErrorDef_stack: Vec::new(),
            rustylr_macro_generated_ReduceDef_stack: Vec::new(),
            rustylr_macro_generated_RuleDef_stack: Vec::new(),
            rustylr_macro_generated_RuleType_stack: Vec::new(),
            rustylr_macro_generated_EofDef_stack: Vec::new(),
            rustylr_macro_generated_RuleLine_stack: Vec::new(),
            rustylr_macro_generated_Rule_stack: Vec::new(),
            rustylr_macro_generated_Action_stack: Vec::new(),
            rustylr_macro_generated_RustCode_stack: Vec::new(),
            rustylr_macro_generated_TokenTypeDef_stack: Vec::new(),
            rustylr_macro_generated_Tokens_stack: Vec::new(),
            rustylr_macro_generated_RuleLines_stack: Vec::new(),
            rustylr_macro_generated_Token_stack: Vec::new(),
            rustylr_macro_generated_TokenDef_stack: Vec::new(),
            rustylr_macro_generated_ModulePrefixDef_stack: Vec::new(),
            rustylr_macro_generated_Grammar_stack: Vec::new(),
        }
    }
    pub fn reduce(
        &mut self,
        rulelen: usize,
        rustylr_macro_generated_ruleid__: usize,
    ) -> Result<(), ParseError> {
        let rusty_lr_macro_generated_new_begin = *self
            .rustylr_macro_generated_rl_end_stack
            .get(self.rustylr_macro_generated_rl_end_stack.len() - rulelen - 1)
            .unwrap();
        let rusty_lr_macro_generated_new_end =
            *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
        let s = &self.rustylr_macro_generated_rl_terms_stack
            [rusty_lr_macro_generated_new_begin..rusty_lr_macro_generated_new_end];
        match rustylr_macro_generated_ruleid__ {
            0usize => {
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut Token = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    self.rustylr_macro_generated_Token_stack.pop().unwrap(),
                    begin..end,
                );
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut TokensOne = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    self.rustylr_macro_generated_TokensOne_stack.pop().unwrap(),
                    begin..end,
                );
                self.rustylr_macro_generated_TokensOne_stack.push({
                    let mut v = TokensOne.value;
                    v.push(Token.value);
                    v
                });
            }
            1usize => {
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut Token = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    self.rustylr_macro_generated_Token_stack.pop().unwrap(),
                    begin..end,
                );
                self.rustylr_macro_generated_TokensOne_stack
                    .push({ vec![Token.value] });
            }
            2usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut semicolon = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut ident = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut start = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
                self.rustylr_macro_generated_StartDef_stack.push({
                    if let TermType::Ident(ident) = *ident {
                        ident.as_ref().unwrap().clone()
                    } else {
                        unreachable!("StartDef-Ident");
                    }
                });
            }
            3usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut semicolon = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut RustCode = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    self.rustylr_macro_generated_RustCode_stack.pop().unwrap(),
                    begin..end,
                );
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut userdata = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
                self.rustylr_macro_generated_UserDataDef_stack
                    .push({ (userdata.value.span().unwrap(), RustCode.value) });
            }
            4usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut semicolon = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut RustCode = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    self.rustylr_macro_generated_RustCode_stack.pop().unwrap(),
                    begin..end,
                );
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut errortype = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
                self.rustylr_macro_generated_ErrorDef_stack
                    .push({ (errortype.value.span().unwrap(), RustCode.value) });
            }
            5usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut semicolon = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut ident = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut left = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
                self.rustylr_macro_generated_ReduceDef_stack.push({
                    if let TermType::Ident(ident) = *ident {
                        (ident.as_ref().unwrap().clone(), ReduceType::Left)
                    } else {
                        unreachable!("ReduceDef-Ident (Left)");
                    }
                });
            }
            6usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut semicolon = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut ident = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut right = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
                self.rustylr_macro_generated_ReduceDef_stack.push({
                    if let TermType::Ident(ident) = *ident {
                        (ident.as_ref().unwrap().clone(), ReduceType::Right)
                    } else {
                        unreachable!("ReduceDef-Ident (Right)");
                    }
                });
            }
            7usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut ident = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
            }
            8usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut colon = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
            }
            9usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut pipe = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
            }
            10usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut percent = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
            }
            11usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut left = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
            }
            12usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut right = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
            }
            13usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut token = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
            }
            14usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut start = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
            }
            15usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut eofdef = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
            }
            16usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut tokentype = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
            }
            17usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut userdata = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
            }
            18usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut errortype = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
            }
            19usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut group = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
            }
            20usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut literal = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
            }
            21usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut equal = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
            }
            22usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut otherpunct = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
            }
            23usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut moduleprefix = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
            }
            24usize => {
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut Tokens = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    self.rustylr_macro_generated_Tokens_stack.pop().unwrap(),
                    begin..end,
                );
                self.rustylr_macro_generated_RuleDef_stack
                    .push({ Tokens.value });
            }
            25usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut group = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
                self.rustylr_macro_generated_RuleType_stack.push({
                    if let TermType::Group(group) = *group {
                        if let Some(group) = group {
                            if group.delimiter() != proc_macro2::Delimiter::Parenthesis {
                                return Err(ParseError::InvalidRuletypeDelimiter(group.span()));
                            }
                            Some(group.clone())
                        } else {
                            unreachable!("RuleType - Some");
                        }
                    } else {
                        unreachable!("RuleType - Group");
                    }
                });
            }
            26usize => {
                self.rustylr_macro_generated_RuleType_stack.push({ None });
            }
            27usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut semicolon = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut RustCode = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    self.rustylr_macro_generated_RustCode_stack.pop().unwrap(),
                    begin..end,
                );
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut eofdef = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
                self.rustylr_macro_generated_EofDef_stack
                    .push({ (eofdef.value.span().unwrap(), RustCode.value) });
            }
            28usize => {
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut Action = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    self.rustylr_macro_generated_Action_stack.pop().unwrap(),
                    begin..end,
                );
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut RuleDef = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    self.rustylr_macro_generated_RuleDef_stack.pop().unwrap(),
                    begin..end,
                );
                self.rustylr_macro_generated_RuleLine_stack.push({
                    RuleLine {
                        tokens: RuleDef.value,
                        reduce_action: Action.value.map(|action| action.to_token_stream()),
                    }
                });
            }
            29usize => {
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut AnyTokens = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    (),
                    begin..end,
                );
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut AnyTokenNoSemi = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    (),
                    begin..end,
                );
            }
            30usize => {
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut AnyTokenNoSemi = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    (),
                    begin..end,
                );
            }
            31usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut semicolon = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut RuleLines = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    self.rustylr_macro_generated_RuleLines_stack.pop().unwrap(),
                    begin..end,
                );
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut colon = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut RuleType = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    self.rustylr_macro_generated_RuleType_stack.pop().unwrap(),
                    begin..end,
                );
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut ident = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
                self.rustylr_macro_generated_Rule_stack.push({
                    let ident = if let TermType::Ident(ident) = ident.value {
                        ident.as_ref().unwrap().clone()
                    } else {
                        unreachable!("Rule-Ident");
                    };
                    (
                        ident,
                        RuleType.value.map(|t| t.to_token_stream()),
                        RuleLines {
                            rule_lines: RuleLines.value,
                        },
                    )
                });
            }
            32usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut group = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
                self.rustylr_macro_generated_Action_stack.push({
                    if let TermType::Group(group) = *group {
                        if let Some(action) = group {
                            if action.delimiter() != proc_macro2::Delimiter::Brace {
                                return Err(ParseError::InvalidReduceActionDelimiter(
                                    action.span(),
                                ));
                            }
                            Some(action.clone())
                        } else {
                            unreachable!("Action1");
                        }
                    } else {
                        unreachable!("Action0");
                    }
                });
            }
            33usize => {
                self.rustylr_macro_generated_Action_stack.push({ None });
            }
            34usize => {
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut AnyTokens = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    (),
                    begin..end,
                );
                self.rustylr_macro_generated_RustCode_stack.push({
                    let mut tokens = TokenStream::new();
                    for token in AnyTokens.slice.iter() {
                        tokens.extend(token.clone().stream());
                    }
                    tokens
                });
            }
            35usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut semicolon = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut RustCode = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    self.rustylr_macro_generated_RustCode_stack.pop().unwrap(),
                    begin..end,
                );
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut tokentype = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
                self.rustylr_macro_generated_TokenTypeDef_stack
                    .push({ (tokentype.value.span().unwrap(), RustCode.value) });
            }
            36usize => {
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut TokensOne = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    self.rustylr_macro_generated_TokensOne_stack.pop().unwrap(),
                    begin..end,
                );
                self.rustylr_macro_generated_Tokens_stack
                    .push({ TokensOne.value });
            }
            37usize => {
                self.rustylr_macro_generated_Tokens_stack.push({ vec![] });
            }
            38usize => {
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut RuleLine = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    self.rustylr_macro_generated_RuleLine_stack.pop().unwrap(),
                    begin..end,
                );
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut pipe = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut RuleLines = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    self.rustylr_macro_generated_RuleLines_stack.pop().unwrap(),
                    begin..end,
                );
                self.rustylr_macro_generated_RuleLines_stack.push({
                    let mut v = RuleLines.value;
                    v.push(RuleLine.value);
                    v
                });
            }
            39usize => {
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut RuleLine = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    self.rustylr_macro_generated_RuleLine_stack.pop().unwrap(),
                    begin..end,
                );
                self.rustylr_macro_generated_RuleLines_stack
                    .push({ vec![RuleLine.value] });
            }
            40usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut ident = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
                self.rustylr_macro_generated_Token_stack.push({
                    if let TermType::Ident(ident) = *ident {
                        TokenMapped {
                            token: Token::NonTerm(ident.as_ref().unwrap().clone()),
                            mapped: None,
                        }
                    } else {
                        unreachable!("Token-Ident");
                    }
                });
            }
            41usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut ident = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut equal = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut mapto = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
                self.rustylr_macro_generated_Token_stack.push({
                    let ident = if let TermType::Ident(ident) = *ident {
                        ident.as_ref().unwrap().clone()
                    } else {
                        unreachable!("Token-Ident2");
                    };
                    let mapto = if let TermType::Ident(mapto) = *mapto {
                        mapto.as_ref().unwrap().clone()
                    } else {
                        unreachable!("Token-Ident3");
                    };
                    TokenMapped {
                        token: Token::NonTerm(ident),
                        mapped: Some(mapto),
                    }
                });
            }
            42usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut semicolon = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut RustCode = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    self.rustylr_macro_generated_RustCode_stack.pop().unwrap(),
                    begin..end,
                );
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut ident = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut token = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
                self.rustylr_macro_generated_TokenDef_stack.push({
                    if let TermType::Ident(ident) = *ident {
                        (ident.as_ref().unwrap().clone(), RustCode.value)
                    } else {
                        unreachable!("TokenDef-Ident");
                    }
                });
            }
            43usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut semicolon = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut RustCode = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    self.rustylr_macro_generated_RustCode_stack.pop().unwrap(),
                    begin..end,
                );
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut moduleprefix = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
                self.rustylr_macro_generated_ModulePrefixDef_stack
                    .push({ (moduleprefix.value.span().unwrap(), RustCode.value) });
            }
            44usize => {
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut Grammar = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    self.rustylr_macro_generated_Grammar_stack.pop().unwrap(),
                    begin..end,
                );
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut Rule = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    self.rustylr_macro_generated_Rule_stack.pop().unwrap(),
                    begin..end,
                );
                self.rustylr_macro_generated_Grammar_stack.push({
                    let mut g = Grammar.value;
                    let r = Rule.value;
                    let name = r.0.to_string();
                    let span = r.0.span();
                    if let Some(old) = g.rules.insert(name.clone(), r) {
                        return Err(ParseError::MultipleRuleDefinition(span, name));
                    }
                    g
                });
            }
            45usize => {
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut Rule = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    self.rustylr_macro_generated_Rule_stack.pop().unwrap(),
                    begin..end,
                );
                self.rustylr_macro_generated_Grammar_stack.push({
                    let mut g = Grammar::new();
                    let r = Rule.value;
                    g.rules.insert(r.0.to_string(), r);
                    g
                });
            }
            46usize => {
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut Grammar = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    self.rustylr_macro_generated_Grammar_stack.pop().unwrap(),
                    begin..end,
                );
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut TokenDef = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    self.rustylr_macro_generated_TokenDef_stack.pop().unwrap(),
                    begin..end,
                );
                self.rustylr_macro_generated_Grammar_stack.push({
                    let mut g = Grammar.value;
                    let t = TokenDef.value;
                    let ident = t.0.clone();
                    let stream = t.1.clone();
                    if let Some(old) = g.terminals.insert(t.0.to_string(), t) {
                        return Err(ParseError::MultipleTokenDefinition(
                            ident.span(),
                            ident,
                            old.1,
                            stream,
                        ));
                    }
                    g
                });
            }
            47usize => {
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut TokenDef = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    self.rustylr_macro_generated_TokenDef_stack.pop().unwrap(),
                    begin..end,
                );
                self.rustylr_macro_generated_Grammar_stack.push({
                    let mut g = Grammar::new();
                    let t = TokenDef.value;
                    g.terminals.insert(t.0.to_string(), t);
                    g
                });
            }
            48usize => {
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut Grammar = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    self.rustylr_macro_generated_Grammar_stack.pop().unwrap(),
                    begin..end,
                );
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut StartDef = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    self.rustylr_macro_generated_StartDef_stack.pop().unwrap(),
                    begin..end,
                );
                self.rustylr_macro_generated_Grammar_stack.push({
                    let mut g = Grammar.value;
                    let start = StartDef.value;
                    let span = start.span();
                    if let Some(old) = g.start_rule_name {
                        return Err(ParseError::MultipleStartDefinition(span, old, start));
                    }
                    g.start_rule_name = Some(start);
                    g
                });
            }
            49usize => {
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut StartDef = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    self.rustylr_macro_generated_StartDef_stack.pop().unwrap(),
                    begin..end,
                );
                self.rustylr_macro_generated_Grammar_stack.push({
                    let mut g = Grammar::new();
                    let start = StartDef.value;
                    g.start_rule_name = Some(start);
                    g
                });
            }
            50usize => {
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut Grammar = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    self.rustylr_macro_generated_Grammar_stack.pop().unwrap(),
                    begin..end,
                );
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut EofDef = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    self.rustylr_macro_generated_EofDef_stack.pop().unwrap(),
                    begin..end,
                );
                self.rustylr_macro_generated_Grammar_stack.push({
                    let mut g = Grammar.value;
                    let (span, eof) = EofDef.value;
                    if let Some(old) = g.eof {
                        return Err(ParseError::MultipleEofDefinition(span, old, eof));
                    }
                    g.eof = Some(eof);
                    g
                });
            }
            51usize => {
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut EofDef = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    self.rustylr_macro_generated_EofDef_stack.pop().unwrap(),
                    begin..end,
                );
                self.rustylr_macro_generated_Grammar_stack.push({
                    let mut g = Grammar::new();
                    let (span, eof) = EofDef.value;
                    g.eof = Some(eof);
                    g
                });
            }
            52usize => {
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut Grammar = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    self.rustylr_macro_generated_Grammar_stack.pop().unwrap(),
                    begin..end,
                );
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut TokenTypeDef = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    self.rustylr_macro_generated_TokenTypeDef_stack
                        .pop()
                        .unwrap(),
                    begin..end,
                );
                self.rustylr_macro_generated_Grammar_stack.push({
                    let mut g = Grammar.value;
                    let (span, token_type) = TokenTypeDef.value;
                    if let Some(old) = g.token_typename {
                        return Err(ParseError::MultipleTokenTypeDefinition(
                            span, old, token_type,
                        ));
                    }
                    g.token_typename = Some(token_type);
                    g
                });
            }
            53usize => {
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut TokenTypeDef = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    self.rustylr_macro_generated_TokenTypeDef_stack
                        .pop()
                        .unwrap(),
                    begin..end,
                );
                self.rustylr_macro_generated_Grammar_stack.push({
                    let mut g = Grammar::new();
                    let (span, token_type) = TokenTypeDef.value;
                    g.token_typename = Some(token_type);
                    g
                });
            }
            54usize => {
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut Grammar = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    self.rustylr_macro_generated_Grammar_stack.pop().unwrap(),
                    begin..end,
                );
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut UserDataDef = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    self.rustylr_macro_generated_UserDataDef_stack
                        .pop()
                        .unwrap(),
                    begin..end,
                );
                self.rustylr_macro_generated_Grammar_stack.push({
                    let mut g = Grammar.value;
                    let (span, user_data) = UserDataDef.value;
                    if let Some(old) = g.userdata_typename {
                        return Err(ParseError::MultipleUserDataDefinition(span, old, user_data));
                    }
                    g.userdata_typename = Some(user_data);
                    g
                });
            }
            55usize => {
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut UserDataDef = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    self.rustylr_macro_generated_UserDataDef_stack
                        .pop()
                        .unwrap(),
                    begin..end,
                );
                self.rustylr_macro_generated_Grammar_stack.push({
                    let mut g = Grammar::new();
                    let (span, user_data) = UserDataDef.value;
                    g.userdata_typename = Some(user_data);
                    g
                });
            }
            56usize => {
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut Grammar = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    self.rustylr_macro_generated_Grammar_stack.pop().unwrap(),
                    begin..end,
                );
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut ReduceDef = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    self.rustylr_macro_generated_ReduceDef_stack.pop().unwrap(),
                    begin..end,
                );
                self.rustylr_macro_generated_Grammar_stack.push({
                    let mut g = Grammar.value;
                    let reduce = ReduceDef.value;
                    let span = reduce.0.span();
                    let name = reduce.0.to_string();
                    if let Some((ident, ReduceType)) = g.reduce_types.insert(name.clone(), reduce) {
                        return Err(ParseError::MultipleReduceDefinition(span, name));
                    }
                    g
                });
            }
            57usize => {
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut ReduceDef = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    self.rustylr_macro_generated_ReduceDef_stack.pop().unwrap(),
                    begin..end,
                );
                self.rustylr_macro_generated_Grammar_stack.push({
                    let mut g = Grammar::new();
                    let reduce = ReduceDef.value;
                    g.reduce_types.insert(reduce.0.to_string(), reduce);
                    g
                });
            }
            58usize => {
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut Grammar = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    self.rustylr_macro_generated_Grammar_stack.pop().unwrap(),
                    begin..end,
                );
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut ErrorDef = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    self.rustylr_macro_generated_ErrorDef_stack.pop().unwrap(),
                    begin..end,
                );
                self.rustylr_macro_generated_Grammar_stack.push({
                    let mut g = Grammar.value;
                    let (span, error) = ErrorDef.value;
                    if let Some(old) = g.error_typename {
                        return Err(ParseError::MultipleErrorDefinition(span, old, error));
                    }
                    g.error_typename = Some(error);
                    g
                });
            }
            59usize => {
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut ErrorDef = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    self.rustylr_macro_generated_ErrorDef_stack.pop().unwrap(),
                    begin..end,
                );
                self.rustylr_macro_generated_Grammar_stack.push({
                    let mut g = Grammar::new();
                    let (span, error) = ErrorDef.value;
                    g.error_typename = Some(error);
                    g
                });
            }
            60usize => {
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut Grammar = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    self.rustylr_macro_generated_Grammar_stack.pop().unwrap(),
                    begin..end,
                );
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut ModulePrefixDef = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    self.rustylr_macro_generated_ModulePrefixDef_stack
                        .pop()
                        .unwrap(),
                    begin..end,
                );
                self.rustylr_macro_generated_Grammar_stack.push({
                    let mut g = Grammar.value;
                    let (span, module_prefix) = ModulePrefixDef.value;
                    g.module_prefix = Some(module_prefix);
                    g
                });
            }
            61usize => {
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut ModulePrefixDef = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    self.rustylr_macro_generated_ModulePrefixDef_stack
                        .pop()
                        .unwrap(),
                    begin..end,
                );
                self.rustylr_macro_generated_Grammar_stack.push({
                    let mut g = Grammar::new();
                    let (span, module_prefix) = ModulePrefixDef.value;
                    g.module_prefix = Some(module_prefix);
                    g
                });
            }
            _ => {
                unreachable!("Invalid Rule: {}", rustylr_macro_generated_ruleid__);
            }
        }
        self.rustylr_macro_generated_rl_end_stack
            .push(rusty_lr_macro_generated_new_end);
        Ok(())
    }
    pub fn accept(&mut self) -> (Grammar) {
        self.rustylr_macro_generated_Grammar_stack.pop().unwrap()
    }
    pub fn push(&mut self, term: TermType) {
        self.rustylr_macro_generated_rl_terms_stack.push(term);
        self.rustylr_macro_generated_rl_end_stack
            .push(self.rustylr_macro_generated_rl_terms_stack.len());
    }
}
#[allow(
    unused_braces,
    unused_parens,
    unused_variables,
    non_snake_case,
    unused_mut
)]
pub struct GrammarParser {
    pub rules: Vec<::rusty_lr_core::ProductionRule<TermType, &'static str>>,
    pub states: Vec<::rusty_lr_core::State<TermType, &'static str>>,
}
#[allow(
    unused_braces,
    unused_parens,
    unused_variables,
    non_snake_case,
    unused_mut
)]
impl GrammarParser {
    pub fn new() -> Self {
        let mut rules = Vec::new();
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "TokensOne",
                rule: vec![
                    ::rusty_lr_core::Token::NonTerm("TokensOne"),
                    ::rusty_lr_core::Token::NonTerm("Token"),
                ],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "TokensOne",
                rule: vec![::rusty_lr_core::Token::NonTerm("Token")],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "StartDef",
                rule: vec![
                    ::rusty_lr_core::Token::Term(TermType::Start(None)),
                    ::rusty_lr_core::Token::Term(TermType::Ident(None)),
                    ::rusty_lr_core::Token::Term(TermType::Semicolon(None)),
                ],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "UserDataDef",
                rule: vec![
                    ::rusty_lr_core::Token::Term(TermType::UserData(None)),
                    ::rusty_lr_core::Token::NonTerm("RustCode"),
                    ::rusty_lr_core::Token::Term(TermType::Semicolon(None)),
                ],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "ErrorDef",
                rule: vec![
                    ::rusty_lr_core::Token::Term(TermType::ErrorType(None)),
                    ::rusty_lr_core::Token::NonTerm("RustCode"),
                    ::rusty_lr_core::Token::Term(TermType::Semicolon(None)),
                ],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "ReduceDef",
                rule: vec![
                    ::rusty_lr_core::Token::Term(TermType::Left(None)),
                    ::rusty_lr_core::Token::Term(TermType::Ident(None)),
                    ::rusty_lr_core::Token::Term(TermType::Semicolon(None)),
                ],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "ReduceDef",
                rule: vec![
                    ::rusty_lr_core::Token::Term(TermType::Right(None)),
                    ::rusty_lr_core::Token::Term(TermType::Ident(None)),
                    ::rusty_lr_core::Token::Term(TermType::Semicolon(None)),
                ],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "AnyTokenNoSemi",
                rule: vec![::rusty_lr_core::Token::Term(TermType::Ident(None))],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "AnyTokenNoSemi",
                rule: vec![::rusty_lr_core::Token::Term(TermType::Colon(None))],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "AnyTokenNoSemi",
                rule: vec![::rusty_lr_core::Token::Term(TermType::Pipe(None))],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "AnyTokenNoSemi",
                rule: vec![::rusty_lr_core::Token::Term(TermType::Percent(None))],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "AnyTokenNoSemi",
                rule: vec![::rusty_lr_core::Token::Term(TermType::Left(None))],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "AnyTokenNoSemi",
                rule: vec![::rusty_lr_core::Token::Term(TermType::Right(None))],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "AnyTokenNoSemi",
                rule: vec![::rusty_lr_core::Token::Term(TermType::Token(None))],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "AnyTokenNoSemi",
                rule: vec![::rusty_lr_core::Token::Term(TermType::Start(None))],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "AnyTokenNoSemi",
                rule: vec![::rusty_lr_core::Token::Term(TermType::EofDef(None))],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "AnyTokenNoSemi",
                rule: vec![::rusty_lr_core::Token::Term(TermType::TokenType(None))],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "AnyTokenNoSemi",
                rule: vec![::rusty_lr_core::Token::Term(TermType::UserData(None))],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "AnyTokenNoSemi",
                rule: vec![::rusty_lr_core::Token::Term(TermType::ErrorType(None))],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "AnyTokenNoSemi",
                rule: vec![::rusty_lr_core::Token::Term(TermType::Group(None))],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "AnyTokenNoSemi",
                rule: vec![::rusty_lr_core::Token::Term(TermType::Literal(None))],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "AnyTokenNoSemi",
                rule: vec![::rusty_lr_core::Token::Term(TermType::Equal(None))],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "AnyTokenNoSemi",
                rule: vec![::rusty_lr_core::Token::Term(TermType::OtherPunct(None))],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "AnyTokenNoSemi",
                rule: vec![::rusty_lr_core::Token::Term(TermType::ModulePrefix(None))],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "RuleDef",
                rule: vec![::rusty_lr_core::Token::NonTerm("Tokens")],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "RuleType",
                rule: vec![::rusty_lr_core::Token::Term(TermType::Group(None))],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "RuleType",
                rule: vec![],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "EofDef",
                rule: vec![
                    ::rusty_lr_core::Token::Term(TermType::EofDef(None)),
                    ::rusty_lr_core::Token::NonTerm("RustCode"),
                    ::rusty_lr_core::Token::Term(TermType::Semicolon(None)),
                ],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "RuleLine",
                rule: vec![
                    ::rusty_lr_core::Token::NonTerm("RuleDef"),
                    ::rusty_lr_core::Token::NonTerm("Action"),
                ],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "AnyTokens",
                rule: vec![
                    ::rusty_lr_core::Token::NonTerm("AnyTokenNoSemi"),
                    ::rusty_lr_core::Token::NonTerm("AnyTokens"),
                ],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "AnyTokens",
                rule: vec![::rusty_lr_core::Token::NonTerm("AnyTokenNoSemi")],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "Rule",
                rule: vec![
                    ::rusty_lr_core::Token::Term(TermType::Ident(None)),
                    ::rusty_lr_core::Token::NonTerm("RuleType"),
                    ::rusty_lr_core::Token::Term(TermType::Colon(None)),
                    ::rusty_lr_core::Token::NonTerm("RuleLines"),
                    ::rusty_lr_core::Token::Term(TermType::Semicolon(None)),
                ],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "Action",
                rule: vec![::rusty_lr_core::Token::Term(TermType::Group(None))],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "Action",
                rule: vec![],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "RustCode",
                rule: vec![::rusty_lr_core::Token::NonTerm("AnyTokens")],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "TokenTypeDef",
                rule: vec![
                    ::rusty_lr_core::Token::Term(TermType::TokenType(None)),
                    ::rusty_lr_core::Token::NonTerm("RustCode"),
                    ::rusty_lr_core::Token::Term(TermType::Semicolon(None)),
                ],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "Tokens",
                rule: vec![::rusty_lr_core::Token::NonTerm("TokensOne")],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "Tokens",
                rule: vec![],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "RuleLines",
                rule: vec![
                    ::rusty_lr_core::Token::NonTerm("RuleLines"),
                    ::rusty_lr_core::Token::Term(TermType::Pipe(None)),
                    ::rusty_lr_core::Token::NonTerm("RuleLine"),
                ],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "RuleLines",
                rule: vec![::rusty_lr_core::Token::NonTerm("RuleLine")],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "Token",
                rule: vec![::rusty_lr_core::Token::Term(TermType::Ident(None))],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "Token",
                rule: vec![
                    ::rusty_lr_core::Token::Term(TermType::Ident(None)),
                    ::rusty_lr_core::Token::Term(TermType::Equal(None)),
                    ::rusty_lr_core::Token::Term(TermType::Ident(None)),
                ],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "TokenDef",
                rule: vec![
                    ::rusty_lr_core::Token::Term(TermType::Token(None)),
                    ::rusty_lr_core::Token::Term(TermType::Ident(None)),
                    ::rusty_lr_core::Token::NonTerm("RustCode"),
                    ::rusty_lr_core::Token::Term(TermType::Semicolon(None)),
                ],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "ModulePrefixDef",
                rule: vec![
                    ::rusty_lr_core::Token::Term(TermType::ModulePrefix(None)),
                    ::rusty_lr_core::Token::NonTerm("RustCode"),
                    ::rusty_lr_core::Token::Term(TermType::Semicolon(None)),
                ],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "Grammar",
                rule: vec![
                    ::rusty_lr_core::Token::NonTerm("Rule"),
                    ::rusty_lr_core::Token::NonTerm("Grammar"),
                ],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "Grammar",
                rule: vec![::rusty_lr_core::Token::NonTerm("Rule")],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "Grammar",
                rule: vec![
                    ::rusty_lr_core::Token::NonTerm("TokenDef"),
                    ::rusty_lr_core::Token::NonTerm("Grammar"),
                ],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "Grammar",
                rule: vec![::rusty_lr_core::Token::NonTerm("TokenDef")],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "Grammar",
                rule: vec![
                    ::rusty_lr_core::Token::NonTerm("StartDef"),
                    ::rusty_lr_core::Token::NonTerm("Grammar"),
                ],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "Grammar",
                rule: vec![::rusty_lr_core::Token::NonTerm("StartDef")],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "Grammar",
                rule: vec![
                    ::rusty_lr_core::Token::NonTerm("EofDef"),
                    ::rusty_lr_core::Token::NonTerm("Grammar"),
                ],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "Grammar",
                rule: vec![::rusty_lr_core::Token::NonTerm("EofDef")],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "Grammar",
                rule: vec![
                    ::rusty_lr_core::Token::NonTerm("TokenTypeDef"),
                    ::rusty_lr_core::Token::NonTerm("Grammar"),
                ],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "Grammar",
                rule: vec![::rusty_lr_core::Token::NonTerm("TokenTypeDef")],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "Grammar",
                rule: vec![
                    ::rusty_lr_core::Token::NonTerm("UserDataDef"),
                    ::rusty_lr_core::Token::NonTerm("Grammar"),
                ],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "Grammar",
                rule: vec![::rusty_lr_core::Token::NonTerm("UserDataDef")],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "Grammar",
                rule: vec![
                    ::rusty_lr_core::Token::NonTerm("ReduceDef"),
                    ::rusty_lr_core::Token::NonTerm("Grammar"),
                ],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "Grammar",
                rule: vec![::rusty_lr_core::Token::NonTerm("ReduceDef")],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "Grammar",
                rule: vec![
                    ::rusty_lr_core::Token::NonTerm("ErrorDef"),
                    ::rusty_lr_core::Token::NonTerm("Grammar"),
                ],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "Grammar",
                rule: vec![::rusty_lr_core::Token::NonTerm("ErrorDef")],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "Grammar",
                rule: vec![
                    ::rusty_lr_core::Token::NonTerm("ModulePrefixDef"),
                    ::rusty_lr_core::Token::NonTerm("Grammar"),
                ],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "Grammar",
                rule: vec![::rusty_lr_core::Token::NonTerm("ModulePrefixDef")],
            };
            rules.push(production_rule);
        }
        {
            let production_rule = ::rusty_lr_core::ProductionRule {
                name: "<Augmented>",
                rule: vec![
                    ::rusty_lr_core::Token::NonTerm("Grammar"),
                    ::rusty_lr_core::Token::Term(TermType::Eof),
                ],
            };
            rules.push(production_rule);
        }
        let mut states = Vec::new();
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            shift_goto_map_term.insert(TermType::ModulePrefix(None), 59usize);
            shift_goto_map_term.insert(TermType::UserData(None), 1usize);
            shift_goto_map_term.insert(TermType::Left(None), 31usize);
            shift_goto_map_term.insert(TermType::EofDef(None), 65usize);
            shift_goto_map_term.insert(TermType::Start(None), 62usize);
            shift_goto_map_term.insert(TermType::Ident(None), 37usize);
            shift_goto_map_term.insert(TermType::Token(None), 24usize);
            shift_goto_map_term.insert(TermType::Right(None), 28usize);
            shift_goto_map_term.insert(TermType::ErrorType(None), 56usize);
            shift_goto_map_term.insert(TermType::TokenType(None), 34usize);
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            shift_goto_map_nonterm.insert("Grammar", 86usize);
            shift_goto_map_nonterm.insert("ModulePrefixDef", 77usize);
            shift_goto_map_nonterm.insert("ErrorDef", 73usize);
            shift_goto_map_nonterm.insert("TokenDef", 72usize);
            shift_goto_map_nonterm.insert("UserDataDef", 75usize);
            shift_goto_map_nonterm.insert("TokenTypeDef", 74usize);
            shift_goto_map_nonterm.insert("Rule", 69usize);
            shift_goto_map_nonterm.insert("EofDef", 68usize);
            shift_goto_map_nonterm.insert("ReduceDef", 71usize);
            shift_goto_map_nonterm.insert("StartDef", 70usize);
            let mut reduce_map = std::collections::HashMap::new();
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 2usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 3usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 4usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 5usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 6usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 27usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 31usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 35usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 42usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 43usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 44usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 45usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 46usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 47usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 48usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 49usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 50usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 51usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 52usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 53usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 54usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 55usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 56usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 57usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 58usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 59usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 60usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 61usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 62usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            shift_goto_map_term.insert(TermType::ErrorType(None), 5usize);
            shift_goto_map_term.insert(TermType::Ident(None), 6usize);
            shift_goto_map_term.insert(TermType::Start(None), 2usize);
            shift_goto_map_term.insert(TermType::OtherPunct(None), 10usize);
            shift_goto_map_term.insert(TermType::Group(None), 12usize);
            shift_goto_map_term.insert(TermType::Pipe(None), 13usize);
            shift_goto_map_term.insert(TermType::Colon(None), 18usize);
            shift_goto_map_term.insert(TermType::Left(None), 17usize);
            shift_goto_map_term.insert(TermType::TokenType(None), 8usize);
            shift_goto_map_term.insert(TermType::EofDef(None), 3usize);
            shift_goto_map_term.insert(TermType::Right(None), 11usize);
            shift_goto_map_term.insert(TermType::Token(None), 15usize);
            shift_goto_map_term.insert(TermType::Literal(None), 16usize);
            shift_goto_map_term.insert(TermType::ModulePrefix(None), 7usize);
            shift_goto_map_term.insert(TermType::UserData(None), 4usize);
            shift_goto_map_term.insert(TermType::Percent(None), 14usize);
            shift_goto_map_term.insert(TermType::Equal(None), 9usize);
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            shift_goto_map_nonterm.insert("AnyTokenNoSemi", 19usize);
            shift_goto_map_nonterm.insert("RustCode", 22usize);
            shift_goto_map_nonterm.insert("AnyTokens", 21usize);
            let mut reduce_map = std::collections::HashMap::new();
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 3usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 7usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 8usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 9usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 10usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 11usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 12usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 13usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 14usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 15usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 16usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 17usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 18usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 19usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 20usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 21usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 22usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 23usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 29usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 30usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 34usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::ErrorType(None), 14usize);
            reduce_map.insert(TermType::Left(None), 14usize);
            reduce_map.insert(TermType::ModulePrefix(None), 14usize);
            reduce_map.insert(TermType::Literal(None), 14usize);
            reduce_map.insert(TermType::Token(None), 14usize);
            reduce_map.insert(TermType::Ident(None), 14usize);
            reduce_map.insert(TermType::Right(None), 14usize);
            reduce_map.insert(TermType::TokenType(None), 14usize);
            reduce_map.insert(TermType::OtherPunct(None), 14usize);
            reduce_map.insert(TermType::EofDef(None), 14usize);
            reduce_map.insert(TermType::Colon(None), 14usize);
            reduce_map.insert(TermType::Equal(None), 14usize);
            reduce_map.insert(TermType::Percent(None), 14usize);
            reduce_map.insert(TermType::UserData(None), 14usize);
            reduce_map.insert(TermType::Start(None), 14usize);
            reduce_map.insert(TermType::Semicolon(None), 14usize);
            reduce_map.insert(TermType::Pipe(None), 14usize);
            reduce_map.insert(TermType::Group(None), 14usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 14usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::ModulePrefix(None), 15usize);
            reduce_map.insert(TermType::ErrorType(None), 15usize);
            reduce_map.insert(TermType::UserData(None), 15usize);
            reduce_map.insert(TermType::EofDef(None), 15usize);
            reduce_map.insert(TermType::Right(None), 15usize);
            reduce_map.insert(TermType::Colon(None), 15usize);
            reduce_map.insert(TermType::OtherPunct(None), 15usize);
            reduce_map.insert(TermType::Token(None), 15usize);
            reduce_map.insert(TermType::Literal(None), 15usize);
            reduce_map.insert(TermType::Start(None), 15usize);
            reduce_map.insert(TermType::Group(None), 15usize);
            reduce_map.insert(TermType::Percent(None), 15usize);
            reduce_map.insert(TermType::TokenType(None), 15usize);
            reduce_map.insert(TermType::Semicolon(None), 15usize);
            reduce_map.insert(TermType::Left(None), 15usize);
            reduce_map.insert(TermType::Pipe(None), 15usize);
            reduce_map.insert(TermType::Equal(None), 15usize);
            reduce_map.insert(TermType::Ident(None), 15usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 15usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::Ident(None), 17usize);
            reduce_map.insert(TermType::Start(None), 17usize);
            reduce_map.insert(TermType::Token(None), 17usize);
            reduce_map.insert(TermType::ErrorType(None), 17usize);
            reduce_map.insert(TermType::TokenType(None), 17usize);
            reduce_map.insert(TermType::Colon(None), 17usize);
            reduce_map.insert(TermType::Semicolon(None), 17usize);
            reduce_map.insert(TermType::EofDef(None), 17usize);
            reduce_map.insert(TermType::Group(None), 17usize);
            reduce_map.insert(TermType::UserData(None), 17usize);
            reduce_map.insert(TermType::Literal(None), 17usize);
            reduce_map.insert(TermType::ModulePrefix(None), 17usize);
            reduce_map.insert(TermType::Equal(None), 17usize);
            reduce_map.insert(TermType::OtherPunct(None), 17usize);
            reduce_map.insert(TermType::Pipe(None), 17usize);
            reduce_map.insert(TermType::Right(None), 17usize);
            reduce_map.insert(TermType::Percent(None), 17usize);
            reduce_map.insert(TermType::Left(None), 17usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 17usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::Start(None), 18usize);
            reduce_map.insert(TermType::ErrorType(None), 18usize);
            reduce_map.insert(TermType::UserData(None), 18usize);
            reduce_map.insert(TermType::Equal(None), 18usize);
            reduce_map.insert(TermType::Group(None), 18usize);
            reduce_map.insert(TermType::OtherPunct(None), 18usize);
            reduce_map.insert(TermType::TokenType(None), 18usize);
            reduce_map.insert(TermType::Ident(None), 18usize);
            reduce_map.insert(TermType::Left(None), 18usize);
            reduce_map.insert(TermType::Percent(None), 18usize);
            reduce_map.insert(TermType::Token(None), 18usize);
            reduce_map.insert(TermType::Semicolon(None), 18usize);
            reduce_map.insert(TermType::Literal(None), 18usize);
            reduce_map.insert(TermType::EofDef(None), 18usize);
            reduce_map.insert(TermType::Colon(None), 18usize);
            reduce_map.insert(TermType::ModulePrefix(None), 18usize);
            reduce_map.insert(TermType::Pipe(None), 18usize);
            reduce_map.insert(TermType::Right(None), 18usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 18usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::Literal(None), 7usize);
            reduce_map.insert(TermType::Percent(None), 7usize);
            reduce_map.insert(TermType::Colon(None), 7usize);
            reduce_map.insert(TermType::EofDef(None), 7usize);
            reduce_map.insert(TermType::Pipe(None), 7usize);
            reduce_map.insert(TermType::Left(None), 7usize);
            reduce_map.insert(TermType::Token(None), 7usize);
            reduce_map.insert(TermType::ModulePrefix(None), 7usize);
            reduce_map.insert(TermType::ErrorType(None), 7usize);
            reduce_map.insert(TermType::OtherPunct(None), 7usize);
            reduce_map.insert(TermType::TokenType(None), 7usize);
            reduce_map.insert(TermType::Equal(None), 7usize);
            reduce_map.insert(TermType::Right(None), 7usize);
            reduce_map.insert(TermType::Group(None), 7usize);
            reduce_map.insert(TermType::Ident(None), 7usize);
            reduce_map.insert(TermType::Start(None), 7usize);
            reduce_map.insert(TermType::UserData(None), 7usize);
            reduce_map.insert(TermType::Semicolon(None), 7usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 7usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::Token(None), 23usize);
            reduce_map.insert(TermType::OtherPunct(None), 23usize);
            reduce_map.insert(TermType::Equal(None), 23usize);
            reduce_map.insert(TermType::Group(None), 23usize);
            reduce_map.insert(TermType::Literal(None), 23usize);
            reduce_map.insert(TermType::Right(None), 23usize);
            reduce_map.insert(TermType::Ident(None), 23usize);
            reduce_map.insert(TermType::Percent(None), 23usize);
            reduce_map.insert(TermType::Pipe(None), 23usize);
            reduce_map.insert(TermType::ErrorType(None), 23usize);
            reduce_map.insert(TermType::Start(None), 23usize);
            reduce_map.insert(TermType::EofDef(None), 23usize);
            reduce_map.insert(TermType::Colon(None), 23usize);
            reduce_map.insert(TermType::Left(None), 23usize);
            reduce_map.insert(TermType::Semicolon(None), 23usize);
            reduce_map.insert(TermType::ModulePrefix(None), 23usize);
            reduce_map.insert(TermType::TokenType(None), 23usize);
            reduce_map.insert(TermType::UserData(None), 23usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 23usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::ErrorType(None), 16usize);
            reduce_map.insert(TermType::Literal(None), 16usize);
            reduce_map.insert(TermType::ModulePrefix(None), 16usize);
            reduce_map.insert(TermType::Group(None), 16usize);
            reduce_map.insert(TermType::Equal(None), 16usize);
            reduce_map.insert(TermType::Token(None), 16usize);
            reduce_map.insert(TermType::Colon(None), 16usize);
            reduce_map.insert(TermType::TokenType(None), 16usize);
            reduce_map.insert(TermType::EofDef(None), 16usize);
            reduce_map.insert(TermType::OtherPunct(None), 16usize);
            reduce_map.insert(TermType::UserData(None), 16usize);
            reduce_map.insert(TermType::Right(None), 16usize);
            reduce_map.insert(TermType::Ident(None), 16usize);
            reduce_map.insert(TermType::Start(None), 16usize);
            reduce_map.insert(TermType::Semicolon(None), 16usize);
            reduce_map.insert(TermType::Left(None), 16usize);
            reduce_map.insert(TermType::Percent(None), 16usize);
            reduce_map.insert(TermType::Pipe(None), 16usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 16usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::ModulePrefix(None), 21usize);
            reduce_map.insert(TermType::Left(None), 21usize);
            reduce_map.insert(TermType::ErrorType(None), 21usize);
            reduce_map.insert(TermType::Pipe(None), 21usize);
            reduce_map.insert(TermType::Right(None), 21usize);
            reduce_map.insert(TermType::Start(None), 21usize);
            reduce_map.insert(TermType::UserData(None), 21usize);
            reduce_map.insert(TermType::Token(None), 21usize);
            reduce_map.insert(TermType::Semicolon(None), 21usize);
            reduce_map.insert(TermType::TokenType(None), 21usize);
            reduce_map.insert(TermType::OtherPunct(None), 21usize);
            reduce_map.insert(TermType::Colon(None), 21usize);
            reduce_map.insert(TermType::Percent(None), 21usize);
            reduce_map.insert(TermType::Equal(None), 21usize);
            reduce_map.insert(TermType::Group(None), 21usize);
            reduce_map.insert(TermType::EofDef(None), 21usize);
            reduce_map.insert(TermType::Ident(None), 21usize);
            reduce_map.insert(TermType::Literal(None), 21usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 21usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::Equal(None), 22usize);
            reduce_map.insert(TermType::Ident(None), 22usize);
            reduce_map.insert(TermType::EofDef(None), 22usize);
            reduce_map.insert(TermType::ModulePrefix(None), 22usize);
            reduce_map.insert(TermType::TokenType(None), 22usize);
            reduce_map.insert(TermType::UserData(None), 22usize);
            reduce_map.insert(TermType::Literal(None), 22usize);
            reduce_map.insert(TermType::Start(None), 22usize);
            reduce_map.insert(TermType::Left(None), 22usize);
            reduce_map.insert(TermType::Pipe(None), 22usize);
            reduce_map.insert(TermType::Token(None), 22usize);
            reduce_map.insert(TermType::Semicolon(None), 22usize);
            reduce_map.insert(TermType::ErrorType(None), 22usize);
            reduce_map.insert(TermType::Group(None), 22usize);
            reduce_map.insert(TermType::Percent(None), 22usize);
            reduce_map.insert(TermType::Right(None), 22usize);
            reduce_map.insert(TermType::Colon(None), 22usize);
            reduce_map.insert(TermType::OtherPunct(None), 22usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 22usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::Semicolon(None), 12usize);
            reduce_map.insert(TermType::Percent(None), 12usize);
            reduce_map.insert(TermType::Token(None), 12usize);
            reduce_map.insert(TermType::OtherPunct(None), 12usize);
            reduce_map.insert(TermType::TokenType(None), 12usize);
            reduce_map.insert(TermType::UserData(None), 12usize);
            reduce_map.insert(TermType::Start(None), 12usize);
            reduce_map.insert(TermType::Equal(None), 12usize);
            reduce_map.insert(TermType::Ident(None), 12usize);
            reduce_map.insert(TermType::Colon(None), 12usize);
            reduce_map.insert(TermType::EofDef(None), 12usize);
            reduce_map.insert(TermType::Left(None), 12usize);
            reduce_map.insert(TermType::Pipe(None), 12usize);
            reduce_map.insert(TermType::ErrorType(None), 12usize);
            reduce_map.insert(TermType::Group(None), 12usize);
            reduce_map.insert(TermType::ModulePrefix(None), 12usize);
            reduce_map.insert(TermType::Literal(None), 12usize);
            reduce_map.insert(TermType::Right(None), 12usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 12usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::Pipe(None), 19usize);
            reduce_map.insert(TermType::Group(None), 19usize);
            reduce_map.insert(TermType::UserData(None), 19usize);
            reduce_map.insert(TermType::Literal(None), 19usize);
            reduce_map.insert(TermType::TokenType(None), 19usize);
            reduce_map.insert(TermType::Left(None), 19usize);
            reduce_map.insert(TermType::EofDef(None), 19usize);
            reduce_map.insert(TermType::ErrorType(None), 19usize);
            reduce_map.insert(TermType::Colon(None), 19usize);
            reduce_map.insert(TermType::Start(None), 19usize);
            reduce_map.insert(TermType::Token(None), 19usize);
            reduce_map.insert(TermType::Right(None), 19usize);
            reduce_map.insert(TermType::Semicolon(None), 19usize);
            reduce_map.insert(TermType::ModulePrefix(None), 19usize);
            reduce_map.insert(TermType::OtherPunct(None), 19usize);
            reduce_map.insert(TermType::Equal(None), 19usize);
            reduce_map.insert(TermType::Percent(None), 19usize);
            reduce_map.insert(TermType::Ident(None), 19usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 19usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::Semicolon(None), 9usize);
            reduce_map.insert(TermType::UserData(None), 9usize);
            reduce_map.insert(TermType::Percent(None), 9usize);
            reduce_map.insert(TermType::Literal(None), 9usize);
            reduce_map.insert(TermType::ErrorType(None), 9usize);
            reduce_map.insert(TermType::Pipe(None), 9usize);
            reduce_map.insert(TermType::Start(None), 9usize);
            reduce_map.insert(TermType::Left(None), 9usize);
            reduce_map.insert(TermType::Colon(None), 9usize);
            reduce_map.insert(TermType::EofDef(None), 9usize);
            reduce_map.insert(TermType::ModulePrefix(None), 9usize);
            reduce_map.insert(TermType::OtherPunct(None), 9usize);
            reduce_map.insert(TermType::Ident(None), 9usize);
            reduce_map.insert(TermType::Right(None), 9usize);
            reduce_map.insert(TermType::Token(None), 9usize);
            reduce_map.insert(TermType::Equal(None), 9usize);
            reduce_map.insert(TermType::TokenType(None), 9usize);
            reduce_map.insert(TermType::Group(None), 9usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 9usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::ErrorType(None), 10usize);
            reduce_map.insert(TermType::Ident(None), 10usize);
            reduce_map.insert(TermType::EofDef(None), 10usize);
            reduce_map.insert(TermType::Left(None), 10usize);
            reduce_map.insert(TermType::OtherPunct(None), 10usize);
            reduce_map.insert(TermType::Percent(None), 10usize);
            reduce_map.insert(TermType::Group(None), 10usize);
            reduce_map.insert(TermType::Pipe(None), 10usize);
            reduce_map.insert(TermType::Start(None), 10usize);
            reduce_map.insert(TermType::TokenType(None), 10usize);
            reduce_map.insert(TermType::ModulePrefix(None), 10usize);
            reduce_map.insert(TermType::Equal(None), 10usize);
            reduce_map.insert(TermType::UserData(None), 10usize);
            reduce_map.insert(TermType::Semicolon(None), 10usize);
            reduce_map.insert(TermType::Literal(None), 10usize);
            reduce_map.insert(TermType::Right(None), 10usize);
            reduce_map.insert(TermType::Colon(None), 10usize);
            reduce_map.insert(TermType::Token(None), 10usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 10usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::Semicolon(None), 13usize);
            reduce_map.insert(TermType::Ident(None), 13usize);
            reduce_map.insert(TermType::Colon(None), 13usize);
            reduce_map.insert(TermType::UserData(None), 13usize);
            reduce_map.insert(TermType::Pipe(None), 13usize);
            reduce_map.insert(TermType::Group(None), 13usize);
            reduce_map.insert(TermType::OtherPunct(None), 13usize);
            reduce_map.insert(TermType::Start(None), 13usize);
            reduce_map.insert(TermType::EofDef(None), 13usize);
            reduce_map.insert(TermType::Percent(None), 13usize);
            reduce_map.insert(TermType::Left(None), 13usize);
            reduce_map.insert(TermType::Equal(None), 13usize);
            reduce_map.insert(TermType::Token(None), 13usize);
            reduce_map.insert(TermType::Right(None), 13usize);
            reduce_map.insert(TermType::Literal(None), 13usize);
            reduce_map.insert(TermType::ModulePrefix(None), 13usize);
            reduce_map.insert(TermType::TokenType(None), 13usize);
            reduce_map.insert(TermType::ErrorType(None), 13usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 13usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::Start(None), 20usize);
            reduce_map.insert(TermType::Equal(None), 20usize);
            reduce_map.insert(TermType::Percent(None), 20usize);
            reduce_map.insert(TermType::Colon(None), 20usize);
            reduce_map.insert(TermType::OtherPunct(None), 20usize);
            reduce_map.insert(TermType::Ident(None), 20usize);
            reduce_map.insert(TermType::Semicolon(None), 20usize);
            reduce_map.insert(TermType::ErrorType(None), 20usize);
            reduce_map.insert(TermType::Token(None), 20usize);
            reduce_map.insert(TermType::TokenType(None), 20usize);
            reduce_map.insert(TermType::Literal(None), 20usize);
            reduce_map.insert(TermType::Pipe(None), 20usize);
            reduce_map.insert(TermType::EofDef(None), 20usize);
            reduce_map.insert(TermType::Left(None), 20usize);
            reduce_map.insert(TermType::UserData(None), 20usize);
            reduce_map.insert(TermType::Right(None), 20usize);
            reduce_map.insert(TermType::Group(None), 20usize);
            reduce_map.insert(TermType::ModulePrefix(None), 20usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 20usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::Left(None), 11usize);
            reduce_map.insert(TermType::TokenType(None), 11usize);
            reduce_map.insert(TermType::Group(None), 11usize);
            reduce_map.insert(TermType::Token(None), 11usize);
            reduce_map.insert(TermType::Start(None), 11usize);
            reduce_map.insert(TermType::UserData(None), 11usize);
            reduce_map.insert(TermType::Colon(None), 11usize);
            reduce_map.insert(TermType::Pipe(None), 11usize);
            reduce_map.insert(TermType::Ident(None), 11usize);
            reduce_map.insert(TermType::OtherPunct(None), 11usize);
            reduce_map.insert(TermType::Right(None), 11usize);
            reduce_map.insert(TermType::EofDef(None), 11usize);
            reduce_map.insert(TermType::Equal(None), 11usize);
            reduce_map.insert(TermType::Literal(None), 11usize);
            reduce_map.insert(TermType::ModulePrefix(None), 11usize);
            reduce_map.insert(TermType::Percent(None), 11usize);
            reduce_map.insert(TermType::Semicolon(None), 11usize);
            reduce_map.insert(TermType::ErrorType(None), 11usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 11usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::Token(None), 8usize);
            reduce_map.insert(TermType::Right(None), 8usize);
            reduce_map.insert(TermType::Pipe(None), 8usize);
            reduce_map.insert(TermType::Semicolon(None), 8usize);
            reduce_map.insert(TermType::Colon(None), 8usize);
            reduce_map.insert(TermType::EofDef(None), 8usize);
            reduce_map.insert(TermType::UserData(None), 8usize);
            reduce_map.insert(TermType::Ident(None), 8usize);
            reduce_map.insert(TermType::Percent(None), 8usize);
            reduce_map.insert(TermType::Start(None), 8usize);
            reduce_map.insert(TermType::ErrorType(None), 8usize);
            reduce_map.insert(TermType::ModulePrefix(None), 8usize);
            reduce_map.insert(TermType::Left(None), 8usize);
            reduce_map.insert(TermType::Equal(None), 8usize);
            reduce_map.insert(TermType::OtherPunct(None), 8usize);
            reduce_map.insert(TermType::Literal(None), 8usize);
            reduce_map.insert(TermType::TokenType(None), 8usize);
            reduce_map.insert(TermType::Group(None), 8usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 8usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            shift_goto_map_term.insert(TermType::Percent(None), 14usize);
            shift_goto_map_term.insert(TermType::Start(None), 2usize);
            shift_goto_map_term.insert(TermType::Right(None), 11usize);
            shift_goto_map_term.insert(TermType::Token(None), 15usize);
            shift_goto_map_term.insert(TermType::Ident(None), 6usize);
            shift_goto_map_term.insert(TermType::UserData(None), 4usize);
            shift_goto_map_term.insert(TermType::TokenType(None), 8usize);
            shift_goto_map_term.insert(TermType::OtherPunct(None), 10usize);
            shift_goto_map_term.insert(TermType::Colon(None), 18usize);
            shift_goto_map_term.insert(TermType::EofDef(None), 3usize);
            shift_goto_map_term.insert(TermType::Pipe(None), 13usize);
            shift_goto_map_term.insert(TermType::Literal(None), 16usize);
            shift_goto_map_term.insert(TermType::Group(None), 12usize);
            shift_goto_map_term.insert(TermType::ModulePrefix(None), 7usize);
            shift_goto_map_term.insert(TermType::Equal(None), 9usize);
            shift_goto_map_term.insert(TermType::Left(None), 17usize);
            shift_goto_map_term.insert(TermType::ErrorType(None), 5usize);
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            shift_goto_map_nonterm.insert("AnyTokenNoSemi", 19usize);
            shift_goto_map_nonterm.insert("AnyTokens", 20usize);
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::Semicolon(None), 30usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 7usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 8usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 9usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 10usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 11usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 12usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 13usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 14usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 15usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 16usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 17usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 18usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 19usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 20usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 21usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 22usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 23usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 29usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 29usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 30usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 30usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::Semicolon(None), 29usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 29usize,
                    shifted: 2usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::Semicolon(None), 34usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 34usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            shift_goto_map_term.insert(TermType::Semicolon(None), 23usize);
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 3usize,
                    shifted: 2usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::Token(None), 3usize);
            reduce_map.insert(TermType::EofDef(None), 3usize);
            reduce_map.insert(TermType::ErrorType(None), 3usize);
            reduce_map.insert(TermType::Ident(None), 3usize);
            reduce_map.insert(TermType::TokenType(None), 3usize);
            reduce_map.insert(TermType::UserData(None), 3usize);
            reduce_map.insert(TermType::Eof, 3usize);
            reduce_map.insert(TermType::ModulePrefix(None), 3usize);
            reduce_map.insert(TermType::Start(None), 3usize);
            reduce_map.insert(TermType::Left(None), 3usize);
            reduce_map.insert(TermType::Right(None), 3usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 3usize,
                    shifted: 3usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            shift_goto_map_term.insert(TermType::Ident(None), 25usize);
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 42usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            shift_goto_map_term.insert(TermType::OtherPunct(None), 10usize);
            shift_goto_map_term.insert(TermType::Group(None), 12usize);
            shift_goto_map_term.insert(TermType::Ident(None), 6usize);
            shift_goto_map_term.insert(TermType::EofDef(None), 3usize);
            shift_goto_map_term.insert(TermType::Colon(None), 18usize);
            shift_goto_map_term.insert(TermType::TokenType(None), 8usize);
            shift_goto_map_term.insert(TermType::Pipe(None), 13usize);
            shift_goto_map_term.insert(TermType::Percent(None), 14usize);
            shift_goto_map_term.insert(TermType::Start(None), 2usize);
            shift_goto_map_term.insert(TermType::UserData(None), 4usize);
            shift_goto_map_term.insert(TermType::ModulePrefix(None), 7usize);
            shift_goto_map_term.insert(TermType::ErrorType(None), 5usize);
            shift_goto_map_term.insert(TermType::Equal(None), 9usize);
            shift_goto_map_term.insert(TermType::Right(None), 11usize);
            shift_goto_map_term.insert(TermType::Token(None), 15usize);
            shift_goto_map_term.insert(TermType::Left(None), 17usize);
            shift_goto_map_term.insert(TermType::Literal(None), 16usize);
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            shift_goto_map_nonterm.insert("RustCode", 26usize);
            shift_goto_map_nonterm.insert("AnyTokenNoSemi", 19usize);
            shift_goto_map_nonterm.insert("AnyTokens", 21usize);
            let mut reduce_map = std::collections::HashMap::new();
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 7usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 8usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 9usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 10usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 11usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 12usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 13usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 14usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 15usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 16usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 17usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 18usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 19usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 20usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 21usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 22usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 23usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 29usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 30usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 34usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 42usize,
                    shifted: 2usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            shift_goto_map_term.insert(TermType::Semicolon(None), 27usize);
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 42usize,
                    shifted: 3usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::ModulePrefix(None), 42usize);
            reduce_map.insert(TermType::Eof, 42usize);
            reduce_map.insert(TermType::Right(None), 42usize);
            reduce_map.insert(TermType::Ident(None), 42usize);
            reduce_map.insert(TermType::Start(None), 42usize);
            reduce_map.insert(TermType::EofDef(None), 42usize);
            reduce_map.insert(TermType::UserData(None), 42usize);
            reduce_map.insert(TermType::TokenType(None), 42usize);
            reduce_map.insert(TermType::ErrorType(None), 42usize);
            reduce_map.insert(TermType::Left(None), 42usize);
            reduce_map.insert(TermType::Token(None), 42usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 42usize,
                    shifted: 4usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            shift_goto_map_term.insert(TermType::Ident(None), 29usize);
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 6usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            shift_goto_map_term.insert(TermType::Semicolon(None), 30usize);
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 6usize,
                    shifted: 2usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::UserData(None), 6usize);
            reduce_map.insert(TermType::ModulePrefix(None), 6usize);
            reduce_map.insert(TermType::Start(None), 6usize);
            reduce_map.insert(TermType::Right(None), 6usize);
            reduce_map.insert(TermType::ErrorType(None), 6usize);
            reduce_map.insert(TermType::EofDef(None), 6usize);
            reduce_map.insert(TermType::Ident(None), 6usize);
            reduce_map.insert(TermType::TokenType(None), 6usize);
            reduce_map.insert(TermType::Left(None), 6usize);
            reduce_map.insert(TermType::Eof, 6usize);
            reduce_map.insert(TermType::Token(None), 6usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 6usize,
                    shifted: 3usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            shift_goto_map_term.insert(TermType::Ident(None), 32usize);
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 5usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            shift_goto_map_term.insert(TermType::Semicolon(None), 33usize);
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 5usize,
                    shifted: 2usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::Left(None), 5usize);
            reduce_map.insert(TermType::Start(None), 5usize);
            reduce_map.insert(TermType::EofDef(None), 5usize);
            reduce_map.insert(TermType::Ident(None), 5usize);
            reduce_map.insert(TermType::Right(None), 5usize);
            reduce_map.insert(TermType::Token(None), 5usize);
            reduce_map.insert(TermType::Eof, 5usize);
            reduce_map.insert(TermType::ErrorType(None), 5usize);
            reduce_map.insert(TermType::ModulePrefix(None), 5usize);
            reduce_map.insert(TermType::TokenType(None), 5usize);
            reduce_map.insert(TermType::UserData(None), 5usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 5usize,
                    shifted: 3usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            shift_goto_map_term.insert(TermType::ModulePrefix(None), 7usize);
            shift_goto_map_term.insert(TermType::EofDef(None), 3usize);
            shift_goto_map_term.insert(TermType::Ident(None), 6usize);
            shift_goto_map_term.insert(TermType::Token(None), 15usize);
            shift_goto_map_term.insert(TermType::Equal(None), 9usize);
            shift_goto_map_term.insert(TermType::Right(None), 11usize);
            shift_goto_map_term.insert(TermType::OtherPunct(None), 10usize);
            shift_goto_map_term.insert(TermType::ErrorType(None), 5usize);
            shift_goto_map_term.insert(TermType::Percent(None), 14usize);
            shift_goto_map_term.insert(TermType::TokenType(None), 8usize);
            shift_goto_map_term.insert(TermType::Literal(None), 16usize);
            shift_goto_map_term.insert(TermType::UserData(None), 4usize);
            shift_goto_map_term.insert(TermType::Start(None), 2usize);
            shift_goto_map_term.insert(TermType::Left(None), 17usize);
            shift_goto_map_term.insert(TermType::Group(None), 12usize);
            shift_goto_map_term.insert(TermType::Colon(None), 18usize);
            shift_goto_map_term.insert(TermType::Pipe(None), 13usize);
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            shift_goto_map_nonterm.insert("AnyTokens", 21usize);
            shift_goto_map_nonterm.insert("RustCode", 35usize);
            shift_goto_map_nonterm.insert("AnyTokenNoSemi", 19usize);
            let mut reduce_map = std::collections::HashMap::new();
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 7usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 8usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 9usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 10usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 11usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 12usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 13usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 14usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 15usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 16usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 17usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 18usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 19usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 20usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 21usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 22usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 23usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 29usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 30usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 34usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 35usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            shift_goto_map_term.insert(TermType::Semicolon(None), 36usize);
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 35usize,
                    shifted: 2usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::Left(None), 35usize);
            reduce_map.insert(TermType::Ident(None), 35usize);
            reduce_map.insert(TermType::ErrorType(None), 35usize);
            reduce_map.insert(TermType::Eof, 35usize);
            reduce_map.insert(TermType::Token(None), 35usize);
            reduce_map.insert(TermType::Start(None), 35usize);
            reduce_map.insert(TermType::TokenType(None), 35usize);
            reduce_map.insert(TermType::Right(None), 35usize);
            reduce_map.insert(TermType::EofDef(None), 35usize);
            reduce_map.insert(TermType::ModulePrefix(None), 35usize);
            reduce_map.insert(TermType::UserData(None), 35usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 35usize,
                    shifted: 3usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            shift_goto_map_term.insert(TermType::Group(None), 38usize);
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            shift_goto_map_nonterm.insert("RuleType", 39usize);
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::Colon(None), 26usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 25usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 26usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 31usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::Colon(None), 25usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 25usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            shift_goto_map_term.insert(TermType::Colon(None), 40usize);
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 31usize,
                    shifted: 2usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            shift_goto_map_term.insert(TermType::Ident(None), 41usize);
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            shift_goto_map_nonterm.insert("RuleLine", 46usize);
            shift_goto_map_nonterm.insert("RuleLines", 48usize);
            shift_goto_map_nonterm.insert("RuleDef", 51usize);
            shift_goto_map_nonterm.insert("TokensOne", 44usize);
            shift_goto_map_nonterm.insert("Token", 47usize);
            shift_goto_map_nonterm.insert("Tokens", 50usize);
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::Group(None), 37usize);
            reduce_map.insert(TermType::Pipe(None), 37usize);
            reduce_map.insert(TermType::Semicolon(None), 37usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 0usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 1usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 24usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 28usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 31usize,
                    shifted: 3usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 36usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 37usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 38usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 39usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 40usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 41usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            shift_goto_map_term.insert(TermType::Equal(None), 42usize);
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::Group(None), 40usize);
            reduce_map.insert(TermType::Semicolon(None), 40usize);
            reduce_map.insert(TermType::Ident(None), 40usize);
            reduce_map.insert(TermType::Pipe(None), 40usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 40usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 41usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            shift_goto_map_term.insert(TermType::Ident(None), 43usize);
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 41usize,
                    shifted: 2usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::Group(None), 41usize);
            reduce_map.insert(TermType::Semicolon(None), 41usize);
            reduce_map.insert(TermType::Ident(None), 41usize);
            reduce_map.insert(TermType::Pipe(None), 41usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 41usize,
                    shifted: 3usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            shift_goto_map_term.insert(TermType::Ident(None), 41usize);
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            shift_goto_map_nonterm.insert("Token", 45usize);
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::Semicolon(None), 36usize);
            reduce_map.insert(TermType::Pipe(None), 36usize);
            reduce_map.insert(TermType::Group(None), 36usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 0usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 36usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 40usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 41usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::Ident(None), 0usize);
            reduce_map.insert(TermType::Pipe(None), 0usize);
            reduce_map.insert(TermType::Group(None), 0usize);
            reduce_map.insert(TermType::Semicolon(None), 0usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 0usize,
                    shifted: 2usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::Pipe(None), 39usize);
            reduce_map.insert(TermType::Semicolon(None), 39usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 39usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::Group(None), 1usize);
            reduce_map.insert(TermType::Semicolon(None), 1usize);
            reduce_map.insert(TermType::Pipe(None), 1usize);
            reduce_map.insert(TermType::Ident(None), 1usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 1usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            shift_goto_map_term.insert(TermType::Pipe(None), 49usize);
            shift_goto_map_term.insert(TermType::Semicolon(None), 55usize);
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 31usize,
                    shifted: 4usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 38usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            shift_goto_map_term.insert(TermType::Ident(None), 41usize);
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            shift_goto_map_nonterm.insert("RuleLine", 54usize);
            shift_goto_map_nonterm.insert("TokensOne", 44usize);
            shift_goto_map_nonterm.insert("Token", 47usize);
            shift_goto_map_nonterm.insert("Tokens", 50usize);
            shift_goto_map_nonterm.insert("RuleDef", 51usize);
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::Pipe(None), 37usize);
            reduce_map.insert(TermType::Semicolon(None), 37usize);
            reduce_map.insert(TermType::Group(None), 37usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 0usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 1usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 24usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 28usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 36usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 37usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 38usize,
                    shifted: 2usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 40usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 41usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::Group(None), 24usize);
            reduce_map.insert(TermType::Pipe(None), 24usize);
            reduce_map.insert(TermType::Semicolon(None), 24usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 24usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            shift_goto_map_term.insert(TermType::Group(None), 52usize);
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            shift_goto_map_nonterm.insert("Action", 53usize);
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::Pipe(None), 33usize);
            reduce_map.insert(TermType::Semicolon(None), 33usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 28usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 32usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 33usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::Semicolon(None), 32usize);
            reduce_map.insert(TermType::Pipe(None), 32usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 32usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::Semicolon(None), 28usize);
            reduce_map.insert(TermType::Pipe(None), 28usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 28usize,
                    shifted: 2usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::Semicolon(None), 38usize);
            reduce_map.insert(TermType::Pipe(None), 38usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 38usize,
                    shifted: 3usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::Eof, 31usize);
            reduce_map.insert(TermType::Left(None), 31usize);
            reduce_map.insert(TermType::Start(None), 31usize);
            reduce_map.insert(TermType::Ident(None), 31usize);
            reduce_map.insert(TermType::Token(None), 31usize);
            reduce_map.insert(TermType::EofDef(None), 31usize);
            reduce_map.insert(TermType::TokenType(None), 31usize);
            reduce_map.insert(TermType::UserData(None), 31usize);
            reduce_map.insert(TermType::ErrorType(None), 31usize);
            reduce_map.insert(TermType::ModulePrefix(None), 31usize);
            reduce_map.insert(TermType::Right(None), 31usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 31usize,
                    shifted: 5usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            shift_goto_map_term.insert(TermType::UserData(None), 4usize);
            shift_goto_map_term.insert(TermType::Pipe(None), 13usize);
            shift_goto_map_term.insert(TermType::OtherPunct(None), 10usize);
            shift_goto_map_term.insert(TermType::TokenType(None), 8usize);
            shift_goto_map_term.insert(TermType::ErrorType(None), 5usize);
            shift_goto_map_term.insert(TermType::Equal(None), 9usize);
            shift_goto_map_term.insert(TermType::EofDef(None), 3usize);
            shift_goto_map_term.insert(TermType::ModulePrefix(None), 7usize);
            shift_goto_map_term.insert(TermType::Ident(None), 6usize);
            shift_goto_map_term.insert(TermType::Colon(None), 18usize);
            shift_goto_map_term.insert(TermType::Literal(None), 16usize);
            shift_goto_map_term.insert(TermType::Left(None), 17usize);
            shift_goto_map_term.insert(TermType::Group(None), 12usize);
            shift_goto_map_term.insert(TermType::Token(None), 15usize);
            shift_goto_map_term.insert(TermType::Percent(None), 14usize);
            shift_goto_map_term.insert(TermType::Start(None), 2usize);
            shift_goto_map_term.insert(TermType::Right(None), 11usize);
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            shift_goto_map_nonterm.insert("AnyTokenNoSemi", 19usize);
            shift_goto_map_nonterm.insert("AnyTokens", 21usize);
            shift_goto_map_nonterm.insert("RustCode", 57usize);
            let mut reduce_map = std::collections::HashMap::new();
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 4usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 7usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 8usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 9usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 10usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 11usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 12usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 13usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 14usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 15usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 16usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 17usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 18usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 19usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 20usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 21usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 22usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 23usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 29usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 30usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 34usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            shift_goto_map_term.insert(TermType::Semicolon(None), 58usize);
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 4usize,
                    shifted: 2usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::Token(None), 4usize);
            reduce_map.insert(TermType::Ident(None), 4usize);
            reduce_map.insert(TermType::Start(None), 4usize);
            reduce_map.insert(TermType::Eof, 4usize);
            reduce_map.insert(TermType::UserData(None), 4usize);
            reduce_map.insert(TermType::Left(None), 4usize);
            reduce_map.insert(TermType::EofDef(None), 4usize);
            reduce_map.insert(TermType::TokenType(None), 4usize);
            reduce_map.insert(TermType::Right(None), 4usize);
            reduce_map.insert(TermType::ModulePrefix(None), 4usize);
            reduce_map.insert(TermType::ErrorType(None), 4usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 4usize,
                    shifted: 3usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            shift_goto_map_term.insert(TermType::ErrorType(None), 5usize);
            shift_goto_map_term.insert(TermType::Right(None), 11usize);
            shift_goto_map_term.insert(TermType::TokenType(None), 8usize);
            shift_goto_map_term.insert(TermType::Colon(None), 18usize);
            shift_goto_map_term.insert(TermType::Start(None), 2usize);
            shift_goto_map_term.insert(TermType::Equal(None), 9usize);
            shift_goto_map_term.insert(TermType::Left(None), 17usize);
            shift_goto_map_term.insert(TermType::ModulePrefix(None), 7usize);
            shift_goto_map_term.insert(TermType::Percent(None), 14usize);
            shift_goto_map_term.insert(TermType::Ident(None), 6usize);
            shift_goto_map_term.insert(TermType::Token(None), 15usize);
            shift_goto_map_term.insert(TermType::Pipe(None), 13usize);
            shift_goto_map_term.insert(TermType::Group(None), 12usize);
            shift_goto_map_term.insert(TermType::OtherPunct(None), 10usize);
            shift_goto_map_term.insert(TermType::UserData(None), 4usize);
            shift_goto_map_term.insert(TermType::Literal(None), 16usize);
            shift_goto_map_term.insert(TermType::EofDef(None), 3usize);
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            shift_goto_map_nonterm.insert("AnyTokenNoSemi", 19usize);
            shift_goto_map_nonterm.insert("RustCode", 60usize);
            shift_goto_map_nonterm.insert("AnyTokens", 21usize);
            let mut reduce_map = std::collections::HashMap::new();
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 7usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 8usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 9usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 10usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 11usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 12usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 13usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 14usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 15usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 16usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 17usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 18usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 19usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 20usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 21usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 22usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 23usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 29usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 30usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 34usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 43usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            shift_goto_map_term.insert(TermType::Semicolon(None), 61usize);
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 43usize,
                    shifted: 2usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::Ident(None), 43usize);
            reduce_map.insert(TermType::TokenType(None), 43usize);
            reduce_map.insert(TermType::ModulePrefix(None), 43usize);
            reduce_map.insert(TermType::ErrorType(None), 43usize);
            reduce_map.insert(TermType::Token(None), 43usize);
            reduce_map.insert(TermType::EofDef(None), 43usize);
            reduce_map.insert(TermType::Left(None), 43usize);
            reduce_map.insert(TermType::Right(None), 43usize);
            reduce_map.insert(TermType::UserData(None), 43usize);
            reduce_map.insert(TermType::Eof, 43usize);
            reduce_map.insert(TermType::Start(None), 43usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 43usize,
                    shifted: 3usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            shift_goto_map_term.insert(TermType::Ident(None), 63usize);
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 2usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            shift_goto_map_term.insert(TermType::Semicolon(None), 64usize);
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 2usize,
                    shifted: 2usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::EofDef(None), 2usize);
            reduce_map.insert(TermType::UserData(None), 2usize);
            reduce_map.insert(TermType::Token(None), 2usize);
            reduce_map.insert(TermType::Start(None), 2usize);
            reduce_map.insert(TermType::Ident(None), 2usize);
            reduce_map.insert(TermType::Right(None), 2usize);
            reduce_map.insert(TermType::TokenType(None), 2usize);
            reduce_map.insert(TermType::ModulePrefix(None), 2usize);
            reduce_map.insert(TermType::Left(None), 2usize);
            reduce_map.insert(TermType::Eof, 2usize);
            reduce_map.insert(TermType::ErrorType(None), 2usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 2usize,
                    shifted: 3usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            shift_goto_map_term.insert(TermType::Token(None), 15usize);
            shift_goto_map_term.insert(TermType::Colon(None), 18usize);
            shift_goto_map_term.insert(TermType::Equal(None), 9usize);
            shift_goto_map_term.insert(TermType::Right(None), 11usize);
            shift_goto_map_term.insert(TermType::EofDef(None), 3usize);
            shift_goto_map_term.insert(TermType::Group(None), 12usize);
            shift_goto_map_term.insert(TermType::TokenType(None), 8usize);
            shift_goto_map_term.insert(TermType::Pipe(None), 13usize);
            shift_goto_map_term.insert(TermType::ErrorType(None), 5usize);
            shift_goto_map_term.insert(TermType::OtherPunct(None), 10usize);
            shift_goto_map_term.insert(TermType::Literal(None), 16usize);
            shift_goto_map_term.insert(TermType::ModulePrefix(None), 7usize);
            shift_goto_map_term.insert(TermType::UserData(None), 4usize);
            shift_goto_map_term.insert(TermType::Start(None), 2usize);
            shift_goto_map_term.insert(TermType::Left(None), 17usize);
            shift_goto_map_term.insert(TermType::Percent(None), 14usize);
            shift_goto_map_term.insert(TermType::Ident(None), 6usize);
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            shift_goto_map_nonterm.insert("AnyTokenNoSemi", 19usize);
            shift_goto_map_nonterm.insert("RustCode", 66usize);
            shift_goto_map_nonterm.insert("AnyTokens", 21usize);
            let mut reduce_map = std::collections::HashMap::new();
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 7usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 8usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 9usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 10usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 11usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 12usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 13usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 14usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 15usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 16usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 17usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 18usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 19usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 20usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 21usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 22usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 23usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Colon(None));
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::Equal(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Group(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::Literal(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::OtherPunct(None));
                lookaheads.insert(TermType::Percent(None));
                lookaheads.insert(TermType::Pipe(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Semicolon(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 27usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 29usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 30usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 34usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Semicolon(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            shift_goto_map_term.insert(TermType::Semicolon(None), 67usize);
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 27usize,
                    shifted: 2usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::EofDef(None), 27usize);
            reduce_map.insert(TermType::TokenType(None), 27usize);
            reduce_map.insert(TermType::Left(None), 27usize);
            reduce_map.insert(TermType::Start(None), 27usize);
            reduce_map.insert(TermType::Token(None), 27usize);
            reduce_map.insert(TermType::UserData(None), 27usize);
            reduce_map.insert(TermType::ErrorType(None), 27usize);
            reduce_map.insert(TermType::Right(None), 27usize);
            reduce_map.insert(TermType::Eof, 27usize);
            reduce_map.insert(TermType::ModulePrefix(None), 27usize);
            reduce_map.insert(TermType::Ident(None), 27usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 27usize,
                    shifted: 3usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            shift_goto_map_term.insert(TermType::Ident(None), 37usize);
            shift_goto_map_term.insert(TermType::ModulePrefix(None), 59usize);
            shift_goto_map_term.insert(TermType::Start(None), 62usize);
            shift_goto_map_term.insert(TermType::Right(None), 28usize);
            shift_goto_map_term.insert(TermType::ErrorType(None), 56usize);
            shift_goto_map_term.insert(TermType::EofDef(None), 65usize);
            shift_goto_map_term.insert(TermType::UserData(None), 1usize);
            shift_goto_map_term.insert(TermType::Left(None), 31usize);
            shift_goto_map_term.insert(TermType::TokenType(None), 34usize);
            shift_goto_map_term.insert(TermType::Token(None), 24usize);
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            shift_goto_map_nonterm.insert("StartDef", 70usize);
            shift_goto_map_nonterm.insert("TokenDef", 72usize);
            shift_goto_map_nonterm.insert("UserDataDef", 75usize);
            shift_goto_map_nonterm.insert("TokenTypeDef", 74usize);
            shift_goto_map_nonterm.insert("ReduceDef", 71usize);
            shift_goto_map_nonterm.insert("EofDef", 68usize);
            shift_goto_map_nonterm.insert("Rule", 69usize);
            shift_goto_map_nonterm.insert("ErrorDef", 73usize);
            shift_goto_map_nonterm.insert("ModulePrefixDef", 77usize);
            shift_goto_map_nonterm.insert("Grammar", 85usize);
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::Eof, 51usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 2usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 3usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 4usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 5usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 6usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 27usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 31usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 35usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 42usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 43usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 44usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 45usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 46usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 47usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 48usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 49usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 50usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 50usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 51usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 51usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 52usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 53usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 54usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 55usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 56usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 57usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 58usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 59usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 60usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 61usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            shift_goto_map_term.insert(TermType::EofDef(None), 65usize);
            shift_goto_map_term.insert(TermType::Start(None), 62usize);
            shift_goto_map_term.insert(TermType::ModulePrefix(None), 59usize);
            shift_goto_map_term.insert(TermType::ErrorType(None), 56usize);
            shift_goto_map_term.insert(TermType::Left(None), 31usize);
            shift_goto_map_term.insert(TermType::Right(None), 28usize);
            shift_goto_map_term.insert(TermType::Token(None), 24usize);
            shift_goto_map_term.insert(TermType::TokenType(None), 34usize);
            shift_goto_map_term.insert(TermType::UserData(None), 1usize);
            shift_goto_map_term.insert(TermType::Ident(None), 37usize);
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            shift_goto_map_nonterm.insert("StartDef", 70usize);
            shift_goto_map_nonterm.insert("ErrorDef", 73usize);
            shift_goto_map_nonterm.insert("TokenDef", 72usize);
            shift_goto_map_nonterm.insert("UserDataDef", 75usize);
            shift_goto_map_nonterm.insert("EofDef", 68usize);
            shift_goto_map_nonterm.insert("ModulePrefixDef", 77usize);
            shift_goto_map_nonterm.insert("Grammar", 84usize);
            shift_goto_map_nonterm.insert("ReduceDef", 71usize);
            shift_goto_map_nonterm.insert("Rule", 69usize);
            shift_goto_map_nonterm.insert("TokenTypeDef", 74usize);
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::Eof, 45usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 2usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 3usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 4usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 5usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 6usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 27usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 31usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 35usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 42usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 43usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 44usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 44usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 45usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 45usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 46usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 47usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 48usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 49usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 50usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 51usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 52usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 53usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 54usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 55usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 56usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 57usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 58usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 59usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 60usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 61usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            shift_goto_map_term.insert(TermType::Start(None), 62usize);
            shift_goto_map_term.insert(TermType::Right(None), 28usize);
            shift_goto_map_term.insert(TermType::UserData(None), 1usize);
            shift_goto_map_term.insert(TermType::Left(None), 31usize);
            shift_goto_map_term.insert(TermType::ModulePrefix(None), 59usize);
            shift_goto_map_term.insert(TermType::ErrorType(None), 56usize);
            shift_goto_map_term.insert(TermType::EofDef(None), 65usize);
            shift_goto_map_term.insert(TermType::Ident(None), 37usize);
            shift_goto_map_term.insert(TermType::Token(None), 24usize);
            shift_goto_map_term.insert(TermType::TokenType(None), 34usize);
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            shift_goto_map_nonterm.insert("ReduceDef", 71usize);
            shift_goto_map_nonterm.insert("TokenDef", 72usize);
            shift_goto_map_nonterm.insert("Grammar", 83usize);
            shift_goto_map_nonterm.insert("ErrorDef", 73usize);
            shift_goto_map_nonterm.insert("Rule", 69usize);
            shift_goto_map_nonterm.insert("StartDef", 70usize);
            shift_goto_map_nonterm.insert("EofDef", 68usize);
            shift_goto_map_nonterm.insert("UserDataDef", 75usize);
            shift_goto_map_nonterm.insert("ModulePrefixDef", 77usize);
            shift_goto_map_nonterm.insert("TokenTypeDef", 74usize);
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::Eof, 49usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 2usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 3usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 4usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 5usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 6usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 27usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 31usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 35usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 42usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 43usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 44usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 45usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 46usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 47usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 48usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 48usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 49usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 49usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 50usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 51usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 52usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 53usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 54usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 55usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 56usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 57usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 58usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 59usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 60usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 61usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            shift_goto_map_term.insert(TermType::TokenType(None), 34usize);
            shift_goto_map_term.insert(TermType::Right(None), 28usize);
            shift_goto_map_term.insert(TermType::UserData(None), 1usize);
            shift_goto_map_term.insert(TermType::ModulePrefix(None), 59usize);
            shift_goto_map_term.insert(TermType::ErrorType(None), 56usize);
            shift_goto_map_term.insert(TermType::Left(None), 31usize);
            shift_goto_map_term.insert(TermType::Ident(None), 37usize);
            shift_goto_map_term.insert(TermType::Token(None), 24usize);
            shift_goto_map_term.insert(TermType::EofDef(None), 65usize);
            shift_goto_map_term.insert(TermType::Start(None), 62usize);
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            shift_goto_map_nonterm.insert("TokenDef", 72usize);
            shift_goto_map_nonterm.insert("ErrorDef", 73usize);
            shift_goto_map_nonterm.insert("Rule", 69usize);
            shift_goto_map_nonterm.insert("ModulePrefixDef", 77usize);
            shift_goto_map_nonterm.insert("ReduceDef", 71usize);
            shift_goto_map_nonterm.insert("UserDataDef", 75usize);
            shift_goto_map_nonterm.insert("Grammar", 82usize);
            shift_goto_map_nonterm.insert("TokenTypeDef", 74usize);
            shift_goto_map_nonterm.insert("StartDef", 70usize);
            shift_goto_map_nonterm.insert("EofDef", 68usize);
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::Eof, 57usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 2usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 3usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 4usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 5usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 6usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 27usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 31usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 35usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 42usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 43usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 44usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 45usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 46usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 47usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 48usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 49usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 50usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 51usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 52usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 53usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 54usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 55usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 56usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 56usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 57usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 57usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 58usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 59usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 60usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 61usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            shift_goto_map_term.insert(TermType::ModulePrefix(None), 59usize);
            shift_goto_map_term.insert(TermType::Ident(None), 37usize);
            shift_goto_map_term.insert(TermType::ErrorType(None), 56usize);
            shift_goto_map_term.insert(TermType::Start(None), 62usize);
            shift_goto_map_term.insert(TermType::Left(None), 31usize);
            shift_goto_map_term.insert(TermType::Token(None), 24usize);
            shift_goto_map_term.insert(TermType::Right(None), 28usize);
            shift_goto_map_term.insert(TermType::EofDef(None), 65usize);
            shift_goto_map_term.insert(TermType::TokenType(None), 34usize);
            shift_goto_map_term.insert(TermType::UserData(None), 1usize);
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            shift_goto_map_nonterm.insert("ErrorDef", 73usize);
            shift_goto_map_nonterm.insert("TokenTypeDef", 74usize);
            shift_goto_map_nonterm.insert("ModulePrefixDef", 77usize);
            shift_goto_map_nonterm.insert("ReduceDef", 71usize);
            shift_goto_map_nonterm.insert("Rule", 69usize);
            shift_goto_map_nonterm.insert("Grammar", 81usize);
            shift_goto_map_nonterm.insert("StartDef", 70usize);
            shift_goto_map_nonterm.insert("TokenDef", 72usize);
            shift_goto_map_nonterm.insert("UserDataDef", 75usize);
            shift_goto_map_nonterm.insert("EofDef", 68usize);
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::Eof, 47usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 2usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 3usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 4usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 5usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 6usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 27usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 31usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 35usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 42usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 43usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 44usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 45usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 46usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 46usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 47usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 47usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 48usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 49usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 50usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 51usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 52usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 53usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 54usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 55usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 56usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 57usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 58usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 59usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 60usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 61usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            shift_goto_map_term.insert(TermType::EofDef(None), 65usize);
            shift_goto_map_term.insert(TermType::UserData(None), 1usize);
            shift_goto_map_term.insert(TermType::Start(None), 62usize);
            shift_goto_map_term.insert(TermType::Token(None), 24usize);
            shift_goto_map_term.insert(TermType::ModulePrefix(None), 59usize);
            shift_goto_map_term.insert(TermType::TokenType(None), 34usize);
            shift_goto_map_term.insert(TermType::ErrorType(None), 56usize);
            shift_goto_map_term.insert(TermType::Right(None), 28usize);
            shift_goto_map_term.insert(TermType::Ident(None), 37usize);
            shift_goto_map_term.insert(TermType::Left(None), 31usize);
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            shift_goto_map_nonterm.insert("StartDef", 70usize);
            shift_goto_map_nonterm.insert("Grammar", 80usize);
            shift_goto_map_nonterm.insert("ModulePrefixDef", 77usize);
            shift_goto_map_nonterm.insert("EofDef", 68usize);
            shift_goto_map_nonterm.insert("TokenDef", 72usize);
            shift_goto_map_nonterm.insert("Rule", 69usize);
            shift_goto_map_nonterm.insert("TokenTypeDef", 74usize);
            shift_goto_map_nonterm.insert("ReduceDef", 71usize);
            shift_goto_map_nonterm.insert("UserDataDef", 75usize);
            shift_goto_map_nonterm.insert("ErrorDef", 73usize);
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::Eof, 59usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 2usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 3usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 4usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 5usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 6usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 27usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 31usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 35usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 42usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 43usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 44usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 45usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 46usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 47usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 48usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 49usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 50usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 51usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 52usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 53usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 54usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 55usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 56usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 57usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 58usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 58usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 59usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 59usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 60usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 61usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            shift_goto_map_term.insert(TermType::Right(None), 28usize);
            shift_goto_map_term.insert(TermType::ErrorType(None), 56usize);
            shift_goto_map_term.insert(TermType::Left(None), 31usize);
            shift_goto_map_term.insert(TermType::Start(None), 62usize);
            shift_goto_map_term.insert(TermType::EofDef(None), 65usize);
            shift_goto_map_term.insert(TermType::ModulePrefix(None), 59usize);
            shift_goto_map_term.insert(TermType::Ident(None), 37usize);
            shift_goto_map_term.insert(TermType::UserData(None), 1usize);
            shift_goto_map_term.insert(TermType::TokenType(None), 34usize);
            shift_goto_map_term.insert(TermType::Token(None), 24usize);
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            shift_goto_map_nonterm.insert("StartDef", 70usize);
            shift_goto_map_nonterm.insert("Grammar", 79usize);
            shift_goto_map_nonterm.insert("TokenTypeDef", 74usize);
            shift_goto_map_nonterm.insert("ErrorDef", 73usize);
            shift_goto_map_nonterm.insert("TokenDef", 72usize);
            shift_goto_map_nonterm.insert("Rule", 69usize);
            shift_goto_map_nonterm.insert("ModulePrefixDef", 77usize);
            shift_goto_map_nonterm.insert("EofDef", 68usize);
            shift_goto_map_nonterm.insert("UserDataDef", 75usize);
            shift_goto_map_nonterm.insert("ReduceDef", 71usize);
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::Eof, 53usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 2usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 3usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 4usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 5usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 6usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 27usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 31usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 35usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 42usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 43usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 44usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 45usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 46usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 47usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 48usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 49usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 50usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 51usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 52usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 52usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 53usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 53usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 54usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 55usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 56usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 57usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 58usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 59usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 60usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 61usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            shift_goto_map_term.insert(TermType::ErrorType(None), 56usize);
            shift_goto_map_term.insert(TermType::TokenType(None), 34usize);
            shift_goto_map_term.insert(TermType::Token(None), 24usize);
            shift_goto_map_term.insert(TermType::ModulePrefix(None), 59usize);
            shift_goto_map_term.insert(TermType::UserData(None), 1usize);
            shift_goto_map_term.insert(TermType::Ident(None), 37usize);
            shift_goto_map_term.insert(TermType::Right(None), 28usize);
            shift_goto_map_term.insert(TermType::Start(None), 62usize);
            shift_goto_map_term.insert(TermType::EofDef(None), 65usize);
            shift_goto_map_term.insert(TermType::Left(None), 31usize);
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            shift_goto_map_nonterm.insert("ErrorDef", 73usize);
            shift_goto_map_nonterm.insert("ModulePrefixDef", 77usize);
            shift_goto_map_nonterm.insert("ReduceDef", 71usize);
            shift_goto_map_nonterm.insert("UserDataDef", 75usize);
            shift_goto_map_nonterm.insert("TokenDef", 72usize);
            shift_goto_map_nonterm.insert("Rule", 69usize);
            shift_goto_map_nonterm.insert("EofDef", 68usize);
            shift_goto_map_nonterm.insert("TokenTypeDef", 74usize);
            shift_goto_map_nonterm.insert("Grammar", 76usize);
            shift_goto_map_nonterm.insert("StartDef", 70usize);
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::Eof, 55usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 2usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 3usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 4usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 5usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 6usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 27usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 31usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 35usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 42usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 43usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 44usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 45usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 46usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 47usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 48usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 49usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 50usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 51usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 52usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 53usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 54usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 54usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 55usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 55usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 56usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 57usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 58usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 59usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 60usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 61usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::Eof, 54usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 54usize,
                    shifted: 2usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            shift_goto_map_term.insert(TermType::EofDef(None), 65usize);
            shift_goto_map_term.insert(TermType::Start(None), 62usize);
            shift_goto_map_term.insert(TermType::TokenType(None), 34usize);
            shift_goto_map_term.insert(TermType::Right(None), 28usize);
            shift_goto_map_term.insert(TermType::ErrorType(None), 56usize);
            shift_goto_map_term.insert(TermType::Token(None), 24usize);
            shift_goto_map_term.insert(TermType::ModulePrefix(None), 59usize);
            shift_goto_map_term.insert(TermType::Left(None), 31usize);
            shift_goto_map_term.insert(TermType::UserData(None), 1usize);
            shift_goto_map_term.insert(TermType::Ident(None), 37usize);
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            shift_goto_map_nonterm.insert("TokenDef", 72usize);
            shift_goto_map_nonterm.insert("TokenTypeDef", 74usize);
            shift_goto_map_nonterm.insert("Rule", 69usize);
            shift_goto_map_nonterm.insert("Grammar", 78usize);
            shift_goto_map_nonterm.insert("EofDef", 68usize);
            shift_goto_map_nonterm.insert("ModulePrefixDef", 77usize);
            shift_goto_map_nonterm.insert("StartDef", 70usize);
            shift_goto_map_nonterm.insert("ErrorDef", 73usize);
            shift_goto_map_nonterm.insert("ReduceDef", 71usize);
            shift_goto_map_nonterm.insert("UserDataDef", 75usize);
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::Eof, 61usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 2usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 3usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 4usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 5usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 6usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 27usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 31usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 35usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 42usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 43usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                lookaheads.insert(TermType::EofDef(None));
                lookaheads.insert(TermType::ErrorType(None));
                lookaheads.insert(TermType::Ident(None));
                lookaheads.insert(TermType::Left(None));
                lookaheads.insert(TermType::ModulePrefix(None));
                lookaheads.insert(TermType::Right(None));
                lookaheads.insert(TermType::Start(None));
                lookaheads.insert(TermType::Token(None));
                lookaheads.insert(TermType::TokenType(None));
                lookaheads.insert(TermType::UserData(None));
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 44usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 45usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 46usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 47usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 48usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 49usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 50usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 51usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 52usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 53usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 54usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 55usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 56usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 57usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 58usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 59usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 60usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 60usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 61usize,
                    shifted: 0usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 61usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::Eof, 60usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 60usize,
                    shifted: 2usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::Eof, 52usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 52usize,
                    shifted: 2usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::Eof, 58usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 58usize,
                    shifted: 2usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::Eof, 46usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 46usize,
                    shifted: 2usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::Eof, 56usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 56usize,
                    shifted: 2usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::Eof, 48usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 48usize,
                    shifted: 2usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::Eof, 44usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 44usize,
                    shifted: 2usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            reduce_map.insert(TermType::Eof, 50usize);
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 50usize,
                    shifted: 2usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                lookaheads.insert(TermType::Eof);
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            shift_goto_map_term.insert(TermType::Eof, 87usize);
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 62usize,
                    shifted: 1usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let mut shift_goto_map_term = std::collections::HashMap::new();
            let mut shift_goto_map_nonterm = std::collections::HashMap::new();
            let mut reduce_map = std::collections::HashMap::new();
            let mut ruleset = ::rusty_lr_core::LookaheadRuleRefSet::new();
            {
                let shifted_rule = ::rusty_lr_core::ShiftedRuleRef {
                    rule: 62usize,
                    shifted: 2usize,
                };
                let mut lookaheads = std::collections::BTreeSet::new();
                ruleset.add(shifted_rule, lookaheads);
            }
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        Self { rules, states }
    }
    #[doc = r" give lookahead token to parser, and check if there is any reduce action"]
    fn lookahead<'a, C: ::rusty_lr_core::Callback<TermType, &'static str>>(
        &'a self,
        context: &mut GrammarContext,
        callback: &mut C,
        term: &TermType,
    ) -> Result<(), ::rusty_lr_core::ParseError<'a, TermType, &'static str, C::Error, ParseError>>
    {
        let state = &self.states[*context.state_stack.last().unwrap()];
        if let Some(reduce_rule) = state.reduce(term) {
            let rule = &self.rules[reduce_rule];
            if context.state_stack.len() < rule.rule.len() {
                panic!(
                    "State stack not enough for reduce: {:?}",
                    context.state_stack
                );
            }
            context
                .state_stack
                .truncate(context.state_stack.len() - rule.rule.len());
            context
                .reduce(self.rules[reduce_rule].rule.len(), reduce_rule)
                .map_err(|e| ::rusty_lr_core::ParseError::ReduceAction(e))?;
            callback
                .reduce(&self.rules, &self.states, &context.state_stack, reduce_rule)
                .map_err(|e| ::rusty_lr_core::ParseError::Callback(e))?;
            self.feed_nonterm(context, callback, &rule.name)?;
            self.lookahead(context, callback, term)?;
        }
        Ok(())
    }
    #[doc = r" feed one terminal to parser, and update state stack"]
    pub fn feed<'a>(
        &'a self,
        context: &mut GrammarContext,
        term: TermType,
    ) -> Result<(), ::rusty_lr_core::ParseError<'a, TermType, &'static str, u8, ParseError>> {
        self.feed_callback(context, &mut ::rusty_lr_core::DefaultCallback {}, term)
    }
    #[doc = r" feed one terminal to parser, and update state stack"]
    pub fn feed_callback<'a, C: ::rusty_lr_core::Callback<TermType, &'static str>>(
        &'a self,
        context: &mut GrammarContext,
        callback: &mut C,
        term: TermType,
    ) -> Result<(), ::rusty_lr_core::ParseError<'a, TermType, &'static str, C::Error, ParseError>>
    {
        self.lookahead(context, callback, &term)?;
        let state = &self.states[*context.state_stack.last().unwrap()];
        if let Some(next_state_id) = state.shift_goto_term(&term) {
            context.state_stack.push(next_state_id);
            callback
                .shift_and_goto(&self.rules, &self.states, &context.state_stack, &term)
                .map_err(|e| ::rusty_lr_core::ParseError::Callback(e))?;
            context.push(term);
            Ok(())
        } else {
            Err(::rusty_lr_core::ParseError::InvalidTerminal(
                term,
                &self.rules,
                &self.states,
                context.state_stack.clone(),
            ))
        }
    }
    #[doc = r" feed one non-terminal to parser, and update state stack"]
    fn feed_nonterm<'a, C: ::rusty_lr_core::Callback<TermType, &'static str>>(
        &'a self,
        context: &mut GrammarContext,
        callback: &mut C,
        nonterm: &'a &'static str,
    ) -> Result<(), ::rusty_lr_core::ParseError<'a, TermType, &'static str, C::Error, ParseError>>
    {
        let state = &self.states[*context.state_stack.last().unwrap()];
        if let Some(next_state_id) = state.shift_goto_nonterm(nonterm) {
            context.state_stack.push(next_state_id);
            callback
                .shift_and_goto_nonterm(&self.rules, &self.states, &context.state_stack, nonterm)
                .map_err(|e| ::rusty_lr_core::ParseError::Callback(e))?;
            Ok(())
        } else {
            Err(::rusty_lr_core::ParseError::InvalidNonTerminal(
                &nonterm,
                &self.rules,
                &self.states,
                context.state_stack.clone(),
            ))
        }
    }
    pub fn begin(&self) -> GrammarContext {
        GrammarContext::new()
    }
}
