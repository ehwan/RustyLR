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
    pub rustylr_macro_generated_ErrorDef_stack: Vec<((Span, TokenStream))>,
    pub rustylr_macro_generated_EofDef_stack: Vec<((Span, TokenStream))>,
    pub rustylr_macro_generated_TokensOne_stack: Vec<(Vec<TokenMapped>)>,
    pub rustylr_macro_generated_RuleType_stack: Vec<(Option<Group>)>,
    pub rustylr_macro_generated_ModulePrefixDef_stack: Vec<((Span, TokenStream))>,
    pub rustylr_macro_generated_Tokens_stack: Vec<(Vec<TokenMapped>)>,
    pub rustylr_macro_generated_Rule_stack: Vec<((Ident, Option<TokenStream>, RuleLines))>,
    pub rustylr_macro_generated_RustCode_stack: Vec<(TokenStream)>,
    pub rustylr_macro_generated_TokenDef_stack: Vec<((Ident, TokenStream))>,
    pub rustylr_macro_generated_Action_stack: Vec<(Option<Group>)>,
    pub rustylr_macro_generated_UserDataDef_stack: Vec<((Span, TokenStream))>,
    pub rustylr_macro_generated_StartDef_stack: Vec<(Ident)>,
    pub rustylr_macro_generated_RuleLines_stack: Vec<(Vec<RuleLine>)>,
    pub rustylr_macro_generated_RuleLine_stack: Vec<(RuleLine)>,
    pub rustylr_macro_generated_RuleDef_stack: Vec<(Vec<TokenMapped>)>,
    pub rustylr_macro_generated_Grammar_stack: Vec<(Grammar)>,
    pub rustylr_macro_generated_ReduceDef_stack: Vec<((Ident, ReduceType))>,
    pub rustylr_macro_generated_Token_stack: Vec<(TokenMapped)>,
    pub rustylr_macro_generated_TokenTypeDef_stack: Vec<((Span, TokenStream))>,
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
            rustylr_macro_generated_ErrorDef_stack: Vec::new(),
            rustylr_macro_generated_EofDef_stack: Vec::new(),
            rustylr_macro_generated_TokensOne_stack: Vec::new(),
            rustylr_macro_generated_RuleType_stack: Vec::new(),
            rustylr_macro_generated_ModulePrefixDef_stack: Vec::new(),
            rustylr_macro_generated_Tokens_stack: Vec::new(),
            rustylr_macro_generated_Rule_stack: Vec::new(),
            rustylr_macro_generated_RustCode_stack: Vec::new(),
            rustylr_macro_generated_TokenDef_stack: Vec::new(),
            rustylr_macro_generated_Action_stack: Vec::new(),
            rustylr_macro_generated_UserDataDef_stack: Vec::new(),
            rustylr_macro_generated_StartDef_stack: Vec::new(),
            rustylr_macro_generated_RuleLines_stack: Vec::new(),
            rustylr_macro_generated_RuleLine_stack: Vec::new(),
            rustylr_macro_generated_RuleDef_stack: Vec::new(),
            rustylr_macro_generated_Grammar_stack: Vec::new(),
            rustylr_macro_generated_ReduceDef_stack: Vec::new(),
            rustylr_macro_generated_Token_stack: Vec::new(),
            rustylr_macro_generated_TokenTypeDef_stack: Vec::new(),
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
            1usize => {
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
            2usize => {
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
            3usize => {
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
            4usize => {
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
            5usize => {
                self.rustylr_macro_generated_RuleType_stack.push({ None });
            }
            6usize => {
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
            7usize => {
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
            8usize => {
                self.rustylr_macro_generated_Tokens_stack.push({ vec![] });
            }
            9usize => {
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
            10usize => {
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
            11usize => {
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
            12usize => {
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
            13usize => {
                self.rustylr_macro_generated_Action_stack.push({ None });
            }
            14usize => {
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
            15usize => {
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
            16usize => {
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
            17usize => {
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
            18usize => {
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
            19usize => {
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
            20usize => {
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
            21usize => {
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
            22usize => {
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
            23usize => {
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
            24usize => {
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
            25usize => {
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
            26usize => {
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
            27usize => {
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
            28usize => {
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
            29usize => {
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
            30usize => {
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
            31usize => {
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
            32usize => {
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
            33usize => {
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
            34usize => {
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
            35usize => {
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
            36usize => {
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
            37usize => {
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
            38usize => {
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
            39usize => {
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
            40usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut ident = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
            }
            41usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut colon = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
            }
            42usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut pipe = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
            }
            43usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut percent = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
            }
            44usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut left = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
            }
            45usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut right = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
            }
            46usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut token = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
            }
            47usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut start = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
            }
            48usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut eofdef = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
            }
            49usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut tokentype = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
            }
            50usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut userdata = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
            }
            51usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut errortype = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
            }
            52usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut group = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
            }
            53usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut literal = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
            }
            54usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut equal = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
            }
            55usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut otherpunct = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
            }
            56usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut moduleprefix = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
            }
            57usize => {
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
            58usize => {
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
            59usize => {
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
            60usize => {
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut AnyTokenNoSemi = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    (),
                    begin..end,
                );
            }
            61usize => {
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
        let mut grammar = ::rusty_lr_core::Grammar::new();
        grammar.add_rule(
            "ErrorDef",
            vec![
                ::rusty_lr_core::Token::Term(TermType::ErrorType(None)),
                ::rusty_lr_core::Token::NonTerm("RustCode"),
                ::rusty_lr_core::Token::Term(TermType::Semicolon(None)),
            ],
        );
        grammar.add_rule(
            "EofDef",
            vec![
                ::rusty_lr_core::Token::Term(TermType::EofDef(None)),
                ::rusty_lr_core::Token::NonTerm("RustCode"),
                ::rusty_lr_core::Token::Term(TermType::Semicolon(None)),
            ],
        );
        grammar.add_rule(
            "TokensOne",
            vec![
                ::rusty_lr_core::Token::NonTerm("TokensOne"),
                ::rusty_lr_core::Token::NonTerm("Token"),
            ],
        );
        grammar.add_rule("TokensOne", vec![::rusty_lr_core::Token::NonTerm("Token")]);
        grammar.add_rule(
            "RuleType",
            vec![::rusty_lr_core::Token::Term(TermType::Group(None))],
        );
        grammar.add_rule("RuleType", vec![]);
        grammar.add_rule(
            "ModulePrefixDef",
            vec![
                ::rusty_lr_core::Token::Term(TermType::ModulePrefix(None)),
                ::rusty_lr_core::Token::NonTerm("RustCode"),
                ::rusty_lr_core::Token::Term(TermType::Semicolon(None)),
            ],
        );
        grammar.add_rule("Tokens", vec![::rusty_lr_core::Token::NonTerm("TokensOne")]);
        grammar.add_rule("Tokens", vec![]);
        grammar.add_rule(
            "Rule",
            vec![
                ::rusty_lr_core::Token::Term(TermType::Ident(None)),
                ::rusty_lr_core::Token::NonTerm("RuleType"),
                ::rusty_lr_core::Token::Term(TermType::Colon(None)),
                ::rusty_lr_core::Token::NonTerm("RuleLines"),
                ::rusty_lr_core::Token::Term(TermType::Semicolon(None)),
            ],
        );
        grammar.add_rule(
            "RustCode",
            vec![::rusty_lr_core::Token::NonTerm("AnyTokens")],
        );
        grammar.add_rule(
            "TokenDef",
            vec![
                ::rusty_lr_core::Token::Term(TermType::Token(None)),
                ::rusty_lr_core::Token::Term(TermType::Ident(None)),
                ::rusty_lr_core::Token::NonTerm("RustCode"),
                ::rusty_lr_core::Token::Term(TermType::Semicolon(None)),
            ],
        );
        grammar.add_rule(
            "Action",
            vec![::rusty_lr_core::Token::Term(TermType::Group(None))],
        );
        grammar.add_rule("Action", vec![]);
        grammar.add_rule(
            "UserDataDef",
            vec![
                ::rusty_lr_core::Token::Term(TermType::UserData(None)),
                ::rusty_lr_core::Token::NonTerm("RustCode"),
                ::rusty_lr_core::Token::Term(TermType::Semicolon(None)),
            ],
        );
        grammar.add_rule(
            "StartDef",
            vec![
                ::rusty_lr_core::Token::Term(TermType::Start(None)),
                ::rusty_lr_core::Token::Term(TermType::Ident(None)),
                ::rusty_lr_core::Token::Term(TermType::Semicolon(None)),
            ],
        );
        grammar.add_rule(
            "RuleLines",
            vec![
                ::rusty_lr_core::Token::NonTerm("RuleLines"),
                ::rusty_lr_core::Token::Term(TermType::Pipe(None)),
                ::rusty_lr_core::Token::NonTerm("RuleLine"),
            ],
        );
        grammar.add_rule(
            "RuleLines",
            vec![::rusty_lr_core::Token::NonTerm("RuleLine")],
        );
        grammar.add_rule(
            "RuleLine",
            vec![
                ::rusty_lr_core::Token::NonTerm("RuleDef"),
                ::rusty_lr_core::Token::NonTerm("Action"),
            ],
        );
        grammar.add_rule("RuleDef", vec![::rusty_lr_core::Token::NonTerm("Tokens")]);
        grammar.add_rule(
            "Grammar",
            vec![
                ::rusty_lr_core::Token::NonTerm("Rule"),
                ::rusty_lr_core::Token::NonTerm("Grammar"),
            ],
        );
        grammar.add_rule("Grammar", vec![::rusty_lr_core::Token::NonTerm("Rule")]);
        grammar.add_rule(
            "Grammar",
            vec![
                ::rusty_lr_core::Token::NonTerm("TokenDef"),
                ::rusty_lr_core::Token::NonTerm("Grammar"),
            ],
        );
        grammar.add_rule("Grammar", vec![::rusty_lr_core::Token::NonTerm("TokenDef")]);
        grammar.add_rule(
            "Grammar",
            vec![
                ::rusty_lr_core::Token::NonTerm("StartDef"),
                ::rusty_lr_core::Token::NonTerm("Grammar"),
            ],
        );
        grammar.add_rule("Grammar", vec![::rusty_lr_core::Token::NonTerm("StartDef")]);
        grammar.add_rule(
            "Grammar",
            vec![
                ::rusty_lr_core::Token::NonTerm("EofDef"),
                ::rusty_lr_core::Token::NonTerm("Grammar"),
            ],
        );
        grammar.add_rule("Grammar", vec![::rusty_lr_core::Token::NonTerm("EofDef")]);
        grammar.add_rule(
            "Grammar",
            vec![
                ::rusty_lr_core::Token::NonTerm("TokenTypeDef"),
                ::rusty_lr_core::Token::NonTerm("Grammar"),
            ],
        );
        grammar.add_rule(
            "Grammar",
            vec![::rusty_lr_core::Token::NonTerm("TokenTypeDef")],
        );
        grammar.add_rule(
            "Grammar",
            vec![
                ::rusty_lr_core::Token::NonTerm("UserDataDef"),
                ::rusty_lr_core::Token::NonTerm("Grammar"),
            ],
        );
        grammar.add_rule(
            "Grammar",
            vec![::rusty_lr_core::Token::NonTerm("UserDataDef")],
        );
        grammar.add_rule(
            "Grammar",
            vec![
                ::rusty_lr_core::Token::NonTerm("ReduceDef"),
                ::rusty_lr_core::Token::NonTerm("Grammar"),
            ],
        );
        grammar.add_rule(
            "Grammar",
            vec![::rusty_lr_core::Token::NonTerm("ReduceDef")],
        );
        grammar.add_rule(
            "Grammar",
            vec![
                ::rusty_lr_core::Token::NonTerm("ErrorDef"),
                ::rusty_lr_core::Token::NonTerm("Grammar"),
            ],
        );
        grammar.add_rule("Grammar", vec![::rusty_lr_core::Token::NonTerm("ErrorDef")]);
        grammar.add_rule(
            "Grammar",
            vec![
                ::rusty_lr_core::Token::NonTerm("ModulePrefixDef"),
                ::rusty_lr_core::Token::NonTerm("Grammar"),
            ],
        );
        grammar.add_rule(
            "Grammar",
            vec![::rusty_lr_core::Token::NonTerm("ModulePrefixDef")],
        );
        grammar.add_rule(
            "ReduceDef",
            vec![
                ::rusty_lr_core::Token::Term(TermType::Left(None)),
                ::rusty_lr_core::Token::Term(TermType::Ident(None)),
                ::rusty_lr_core::Token::Term(TermType::Semicolon(None)),
            ],
        );
        grammar.add_rule(
            "ReduceDef",
            vec![
                ::rusty_lr_core::Token::Term(TermType::Right(None)),
                ::rusty_lr_core::Token::Term(TermType::Ident(None)),
                ::rusty_lr_core::Token::Term(TermType::Semicolon(None)),
            ],
        );
        grammar.add_rule(
            "AnyTokenNoSemi",
            vec![::rusty_lr_core::Token::Term(TermType::Ident(None))],
        );
        grammar.add_rule(
            "AnyTokenNoSemi",
            vec![::rusty_lr_core::Token::Term(TermType::Colon(None))],
        );
        grammar.add_rule(
            "AnyTokenNoSemi",
            vec![::rusty_lr_core::Token::Term(TermType::Pipe(None))],
        );
        grammar.add_rule(
            "AnyTokenNoSemi",
            vec![::rusty_lr_core::Token::Term(TermType::Percent(None))],
        );
        grammar.add_rule(
            "AnyTokenNoSemi",
            vec![::rusty_lr_core::Token::Term(TermType::Left(None))],
        );
        grammar.add_rule(
            "AnyTokenNoSemi",
            vec![::rusty_lr_core::Token::Term(TermType::Right(None))],
        );
        grammar.add_rule(
            "AnyTokenNoSemi",
            vec![::rusty_lr_core::Token::Term(TermType::Token(None))],
        );
        grammar.add_rule(
            "AnyTokenNoSemi",
            vec![::rusty_lr_core::Token::Term(TermType::Start(None))],
        );
        grammar.add_rule(
            "AnyTokenNoSemi",
            vec![::rusty_lr_core::Token::Term(TermType::EofDef(None))],
        );
        grammar.add_rule(
            "AnyTokenNoSemi",
            vec![::rusty_lr_core::Token::Term(TermType::TokenType(None))],
        );
        grammar.add_rule(
            "AnyTokenNoSemi",
            vec![::rusty_lr_core::Token::Term(TermType::UserData(None))],
        );
        grammar.add_rule(
            "AnyTokenNoSemi",
            vec![::rusty_lr_core::Token::Term(TermType::ErrorType(None))],
        );
        grammar.add_rule(
            "AnyTokenNoSemi",
            vec![::rusty_lr_core::Token::Term(TermType::Group(None))],
        );
        grammar.add_rule(
            "AnyTokenNoSemi",
            vec![::rusty_lr_core::Token::Term(TermType::Literal(None))],
        );
        grammar.add_rule(
            "AnyTokenNoSemi",
            vec![::rusty_lr_core::Token::Term(TermType::Equal(None))],
        );
        grammar.add_rule(
            "AnyTokenNoSemi",
            vec![::rusty_lr_core::Token::Term(TermType::OtherPunct(None))],
        );
        grammar.add_rule(
            "AnyTokenNoSemi",
            vec![::rusty_lr_core::Token::Term(TermType::ModulePrefix(None))],
        );
        grammar.add_rule(
            "Token",
            vec![::rusty_lr_core::Token::Term(TermType::Ident(None))],
        );
        grammar.add_rule(
            "Token",
            vec![
                ::rusty_lr_core::Token::Term(TermType::Ident(None)),
                ::rusty_lr_core::Token::Term(TermType::Equal(None)),
                ::rusty_lr_core::Token::Term(TermType::Ident(None)),
            ],
        );
        grammar.add_rule(
            "AnyTokens",
            vec![
                ::rusty_lr_core::Token::NonTerm("AnyTokenNoSemi"),
                ::rusty_lr_core::Token::NonTerm("AnyTokens"),
            ],
        );
        grammar.add_rule(
            "AnyTokens",
            vec![::rusty_lr_core::Token::NonTerm("AnyTokenNoSemi")],
        );
        grammar.add_rule(
            "TokenTypeDef",
            vec![
                ::rusty_lr_core::Token::Term(TermType::TokenType(None)),
                ::rusty_lr_core::Token::NonTerm("RustCode"),
                ::rusty_lr_core::Token::Term(TermType::Semicolon(None)),
            ],
        );
        grammar.add_rule(
            "<Augmented>",
            vec![
                ::rusty_lr_core::Token::NonTerm("Grammar"),
                ::rusty_lr_core::Token::Term(TermType::Eof),
            ],
        );
        let parser = match grammar.build_lalr("<Augmented>") {
            Ok(parser) => parser,
            Err(err) => {
                panic!("Error building LR parser:\n{:?}", err);
            }
        };
        let rules = parser.rules;
        let states = parser.states;
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
