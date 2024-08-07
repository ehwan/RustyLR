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
    rustylr_macro_generated_rl_terms_stack: Vec<TermType>,
    rustylr_macro_generated_rl_end_stack: Vec<usize>,
    pub state_stack: Vec<usize>,
    rustylr_macro_generated_Action_stack: Vec<(Option<Group>)>,
    rustylr_macro_generated_EofDef_stack: Vec<((Span, TokenStream))>,
    rustylr_macro_generated_ErrorDef_stack: Vec<((Span, TokenStream))>,
    rustylr_macro_generated_Grammar_stack: Vec<(Grammar)>,
    rustylr_macro_generated_ModulePrefixDef_stack: Vec<((Span, TokenStream))>,
    rustylr_macro_generated_ReduceDef_stack: Vec<((Ident, ReduceType))>,
    rustylr_macro_generated_Rule_stack: Vec<((Ident, Option<TokenStream>, RuleLines))>,
    rustylr_macro_generated_RuleDef_stack: Vec<(Vec<TokenMapped>)>,
    rustylr_macro_generated_RuleLine_stack: Vec<(RuleLine)>,
    rustylr_macro_generated_RuleLines_stack: Vec<(Vec<RuleLine>)>,
    rustylr_macro_generated_RuleType_stack: Vec<(Option<Group>)>,
    rustylr_macro_generated_RustCode_stack: Vec<(TokenStream)>,
    rustylr_macro_generated_StartDef_stack: Vec<(Ident)>,
    rustylr_macro_generated_SymbolPattern_stack: Vec<(Token)>,
    rustylr_macro_generated_TokenDef_stack: Vec<((Ident, TokenStream))>,
    rustylr_macro_generated_TokenMapped_stack: Vec<(TokenMapped)>,
    rustylr_macro_generated_TokenTypeDef_stack: Vec<((Span, TokenStream))>,
    rustylr_macro_generated_UserDataDef_stack: Vec<((Span, TokenStream))>,
    rustylr_macro_generated___TokenMapped__plus__stack: Vec<(Vec<(TokenMapped)>)>,
    rustylr_macro_generated___TokenMapped__star__stack: Vec<(Vec<(TokenMapped)>)>,
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
            rustylr_macro_generated_Action_stack: Vec::new(),
            rustylr_macro_generated_EofDef_stack: Vec::new(),
            rustylr_macro_generated_ErrorDef_stack: Vec::new(),
            rustylr_macro_generated_Grammar_stack: Vec::new(),
            rustylr_macro_generated_ModulePrefixDef_stack: Vec::new(),
            rustylr_macro_generated_ReduceDef_stack: Vec::new(),
            rustylr_macro_generated_Rule_stack: Vec::new(),
            rustylr_macro_generated_RuleDef_stack: Vec::new(),
            rustylr_macro_generated_RuleLine_stack: Vec::new(),
            rustylr_macro_generated_RuleLines_stack: Vec::new(),
            rustylr_macro_generated_RuleType_stack: Vec::new(),
            rustylr_macro_generated_RustCode_stack: Vec::new(),
            rustylr_macro_generated_StartDef_stack: Vec::new(),
            rustylr_macro_generated_SymbolPattern_stack: Vec::new(),
            rustylr_macro_generated_TokenDef_stack: Vec::new(),
            rustylr_macro_generated_TokenMapped_stack: Vec::new(),
            rustylr_macro_generated_TokenTypeDef_stack: Vec::new(),
            rustylr_macro_generated_UserDataDef_stack: Vec::new(),
            rustylr_macro_generated___TokenMapped__plus__stack: Vec::new(),
            rustylr_macro_generated___TokenMapped__star__stack: Vec::new(),
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
            1usize => {
                self.rustylr_macro_generated_Action_stack.push({ None });
            }
            2usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut ident = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
            }
            3usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut colon = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
            }
            4usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut pipe = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
            }
            5usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut percent = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
            }
            6usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut left = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
            }
            7usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut right = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
            }
            8usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut token = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
            }
            9usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut start = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
            }
            10usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut eofdef = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
            }
            11usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut tokentype = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
            }
            12usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut userdata = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
            }
            13usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut errortype = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
            }
            14usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut group = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
            }
            15usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut literal = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
            }
            16usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut equal = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
            }
            17usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut plus = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
            }
            18usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut star = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
            }
            19usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut question = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
            }
            20usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut otherpunct = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
            }
            21usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut moduleprefix = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
            }
            22usize => {
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
            23usize => {
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
            25usize => {
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
            27usize => {
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
            29usize => {
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
            31usize => {
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
            33usize => {
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
            35usize => {
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
            37usize => {
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
            38usize => {
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
            39usize => {
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
            40usize => {
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
            41usize => {
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
                let mut moduleprefix = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
                self.rustylr_macro_generated_ModulePrefixDef_stack
                    .push({ (moduleprefix.value.span().unwrap(), RustCode.value) });
            }
            43usize => {
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
            44usize => {
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
            45usize => {
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
            46usize => {
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut TokenMapped = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    self.rustylr_macro_generated___TokenMapped__star__stack
                        .pop()
                        .unwrap(),
                    begin..end,
                );
                self.rustylr_macro_generated_RuleDef_stack
                    .push({ TokenMapped.value });
            }
            47usize => {
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
            48usize => {
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
            49usize => {
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
            50usize => {
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
            51usize => {
                self.rustylr_macro_generated_RuleType_stack.push({ None });
            }
            52usize => {
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut AnyTokenNoSemi = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    (),
                    begin..end,
                );
                self.rustylr_macro_generated_RustCode_stack.push({
                    let mut tokens = TokenStream::new();
                    for token in AnyTokenNoSemi.slice.iter() {
                        tokens.extend(token.clone().stream());
                    }
                    tokens
                });
            }
            53usize => {
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
            54usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut ident = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
                self.rustylr_macro_generated_SymbolPattern_stack.push({
                    if let TermType::Ident(ident) = ident.value {
                        let ident = ident.as_ref().unwrap();
                        Token::NonTerm(ident.clone())
                    } else {
                        unreachable!("SymbolPattern-Ident");
                    }
                });
            }
            55usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut star = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut ident = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
                self.rustylr_macro_generated_SymbolPattern_stack.push({
                    if let TermType::Ident(ident) = ident.value {
                        let ident = ident.as_ref().unwrap();
                        Token::Star(ident.clone())
                    } else {
                        unreachable!("SymbolPattern-Star");
                    }
                });
            }
            56usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut plus = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut ident = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
                self.rustylr_macro_generated_SymbolPattern_stack.push({
                    if let TermType::Ident(ident) = ident.value {
                        let ident = ident.as_ref().unwrap();
                        Token::Plus(ident.clone())
                    } else {
                        unreachable!("SymbolPattern-Plus");
                    }
                });
            }
            57usize => {
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut question = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut ident = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
                self.rustylr_macro_generated_SymbolPattern_stack.push({
                    if let TermType::Ident(ident) = ident.value {
                        let ident = ident.as_ref().unwrap();
                        Token::Question(ident.clone())
                    } else {
                        unreachable!("SymbolPattern-Question");
                    }
                });
            }
            58usize => {
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
            59usize => {
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut SymbolPattern = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    self.rustylr_macro_generated_SymbolPattern_stack
                        .pop()
                        .unwrap(),
                    begin..end,
                );
                self.rustylr_macro_generated_TokenMapped_stack.push({
                    let mapto = SymbolPattern.value.ident();
                    TokenMapped {
                        token: SymbolPattern.value,
                        mapto,
                    }
                });
            }
            60usize => {
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut SymbolPattern = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    self.rustylr_macro_generated_SymbolPattern_stack
                        .pop()
                        .unwrap(),
                    begin..end,
                );
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut equal = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
                let index = self.rustylr_macro_generated_rl_end_stack.pop().unwrap() - 1;
                let mut ident = ::rusty_lr_core::TermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[index],
                    index,
                );
                self.rustylr_macro_generated_TokenMapped_stack.push({
                    if let TermType::Ident(ident) = ident.value {
                        let ident = ident.as_ref().unwrap();
                        TokenMapped {
                            token: SymbolPattern.value,
                            mapto: ident.clone(),
                        }
                    } else {
                        unreachable!("Token-Ident");
                    }
                });
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
            62usize => {
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
            63usize => {
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut A = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    (),
                    begin..end,
                );
            }
            64usize => {
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut Ap = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    (),
                    begin..end,
                );
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut A = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    (),
                    begin..end,
                );
            }
            65usize => {
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut A = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    self.rustylr_macro_generated_TokenMapped_stack
                        .pop()
                        .unwrap(),
                    begin..end,
                );
                self.rustylr_macro_generated___TokenMapped__plus__stack
                    .push({ vec![A.value] });
            }
            66usize => {
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut A = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    self.rustylr_macro_generated_TokenMapped_stack
                        .pop()
                        .unwrap(),
                    begin..end,
                );
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut Ap = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    self.rustylr_macro_generated___TokenMapped__plus__stack
                        .pop()
                        .unwrap(),
                    begin..end,
                );
                self.rustylr_macro_generated___TokenMapped__plus__stack
                    .push({
                        Ap.value.push(A.value);
                        Ap.value
                    });
            }
            67usize => {
                self.rustylr_macro_generated___TokenMapped__star__stack
                    .push({ vec![] });
            }
            68usize => {
                let end = self.rustylr_macro_generated_rl_end_stack.pop().unwrap();
                let begin = *self.rustylr_macro_generated_rl_end_stack.last().unwrap();
                let mut Ap = ::rusty_lr_core::NonTermData::new(
                    &self.rustylr_macro_generated_rl_terms_stack[begin..end],
                    self.rustylr_macro_generated___TokenMapped__plus__stack
                        .pop()
                        .unwrap(),
                    begin..end,
                );
                self.rustylr_macro_generated___TokenMapped__star__stack
                    .push({ Ap.value });
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
        let rules = vec![
            ::rusty_lr_core::ProductionRule {
                name: "Action",
                rule: vec![::rusty_lr_core::Token::Term(TermType::Group(None))],
            },
            ::rusty_lr_core::ProductionRule {
                name: "Action",
                rule: vec![],
            },
            ::rusty_lr_core::ProductionRule {
                name: "AnyTokenNoSemi",
                rule: vec![::rusty_lr_core::Token::Term(TermType::Ident(None))],
            },
            ::rusty_lr_core::ProductionRule {
                name: "AnyTokenNoSemi",
                rule: vec![::rusty_lr_core::Token::Term(TermType::Colon(None))],
            },
            ::rusty_lr_core::ProductionRule {
                name: "AnyTokenNoSemi",
                rule: vec![::rusty_lr_core::Token::Term(TermType::Pipe(None))],
            },
            ::rusty_lr_core::ProductionRule {
                name: "AnyTokenNoSemi",
                rule: vec![::rusty_lr_core::Token::Term(TermType::Percent(None))],
            },
            ::rusty_lr_core::ProductionRule {
                name: "AnyTokenNoSemi",
                rule: vec![::rusty_lr_core::Token::Term(TermType::Left(None))],
            },
            ::rusty_lr_core::ProductionRule {
                name: "AnyTokenNoSemi",
                rule: vec![::rusty_lr_core::Token::Term(TermType::Right(None))],
            },
            ::rusty_lr_core::ProductionRule {
                name: "AnyTokenNoSemi",
                rule: vec![::rusty_lr_core::Token::Term(TermType::Token(None))],
            },
            ::rusty_lr_core::ProductionRule {
                name: "AnyTokenNoSemi",
                rule: vec![::rusty_lr_core::Token::Term(TermType::Start(None))],
            },
            ::rusty_lr_core::ProductionRule {
                name: "AnyTokenNoSemi",
                rule: vec![::rusty_lr_core::Token::Term(TermType::EofDef(None))],
            },
            ::rusty_lr_core::ProductionRule {
                name: "AnyTokenNoSemi",
                rule: vec![::rusty_lr_core::Token::Term(TermType::TokenType(None))],
            },
            ::rusty_lr_core::ProductionRule {
                name: "AnyTokenNoSemi",
                rule: vec![::rusty_lr_core::Token::Term(TermType::UserData(None))],
            },
            ::rusty_lr_core::ProductionRule {
                name: "AnyTokenNoSemi",
                rule: vec![::rusty_lr_core::Token::Term(TermType::ErrorType(None))],
            },
            ::rusty_lr_core::ProductionRule {
                name: "AnyTokenNoSemi",
                rule: vec![::rusty_lr_core::Token::Term(TermType::Group(None))],
            },
            ::rusty_lr_core::ProductionRule {
                name: "AnyTokenNoSemi",
                rule: vec![::rusty_lr_core::Token::Term(TermType::Literal(None))],
            },
            ::rusty_lr_core::ProductionRule {
                name: "AnyTokenNoSemi",
                rule: vec![::rusty_lr_core::Token::Term(TermType::Equal(None))],
            },
            ::rusty_lr_core::ProductionRule {
                name: "AnyTokenNoSemi",
                rule: vec![::rusty_lr_core::Token::Term(TermType::Plus(None))],
            },
            ::rusty_lr_core::ProductionRule {
                name: "AnyTokenNoSemi",
                rule: vec![::rusty_lr_core::Token::Term(TermType::Star(None))],
            },
            ::rusty_lr_core::ProductionRule {
                name: "AnyTokenNoSemi",
                rule: vec![::rusty_lr_core::Token::Term(TermType::Question(None))],
            },
            ::rusty_lr_core::ProductionRule {
                name: "AnyTokenNoSemi",
                rule: vec![::rusty_lr_core::Token::Term(TermType::OtherPunct(None))],
            },
            ::rusty_lr_core::ProductionRule {
                name: "AnyTokenNoSemi",
                rule: vec![::rusty_lr_core::Token::Term(TermType::ModulePrefix(None))],
            },
            ::rusty_lr_core::ProductionRule {
                name: "EofDef",
                rule: vec![
                    ::rusty_lr_core::Token::Term(TermType::EofDef(None)),
                    ::rusty_lr_core::Token::NonTerm("RustCode"),
                    ::rusty_lr_core::Token::Term(TermType::Semicolon(None)),
                ],
            },
            ::rusty_lr_core::ProductionRule {
                name: "ErrorDef",
                rule: vec![
                    ::rusty_lr_core::Token::Term(TermType::ErrorType(None)),
                    ::rusty_lr_core::Token::NonTerm("RustCode"),
                    ::rusty_lr_core::Token::Term(TermType::Semicolon(None)),
                ],
            },
            ::rusty_lr_core::ProductionRule {
                name: "Grammar",
                rule: vec![
                    ::rusty_lr_core::Token::NonTerm("Rule"),
                    ::rusty_lr_core::Token::NonTerm("Grammar"),
                ],
            },
            ::rusty_lr_core::ProductionRule {
                name: "Grammar",
                rule: vec![::rusty_lr_core::Token::NonTerm("Rule")],
            },
            ::rusty_lr_core::ProductionRule {
                name: "Grammar",
                rule: vec![
                    ::rusty_lr_core::Token::NonTerm("TokenDef"),
                    ::rusty_lr_core::Token::NonTerm("Grammar"),
                ],
            },
            ::rusty_lr_core::ProductionRule {
                name: "Grammar",
                rule: vec![::rusty_lr_core::Token::NonTerm("TokenDef")],
            },
            ::rusty_lr_core::ProductionRule {
                name: "Grammar",
                rule: vec![
                    ::rusty_lr_core::Token::NonTerm("StartDef"),
                    ::rusty_lr_core::Token::NonTerm("Grammar"),
                ],
            },
            ::rusty_lr_core::ProductionRule {
                name: "Grammar",
                rule: vec![::rusty_lr_core::Token::NonTerm("StartDef")],
            },
            ::rusty_lr_core::ProductionRule {
                name: "Grammar",
                rule: vec![
                    ::rusty_lr_core::Token::NonTerm("EofDef"),
                    ::rusty_lr_core::Token::NonTerm("Grammar"),
                ],
            },
            ::rusty_lr_core::ProductionRule {
                name: "Grammar",
                rule: vec![::rusty_lr_core::Token::NonTerm("EofDef")],
            },
            ::rusty_lr_core::ProductionRule {
                name: "Grammar",
                rule: vec![
                    ::rusty_lr_core::Token::NonTerm("TokenTypeDef"),
                    ::rusty_lr_core::Token::NonTerm("Grammar"),
                ],
            },
            ::rusty_lr_core::ProductionRule {
                name: "Grammar",
                rule: vec![::rusty_lr_core::Token::NonTerm("TokenTypeDef")],
            },
            ::rusty_lr_core::ProductionRule {
                name: "Grammar",
                rule: vec![
                    ::rusty_lr_core::Token::NonTerm("UserDataDef"),
                    ::rusty_lr_core::Token::NonTerm("Grammar"),
                ],
            },
            ::rusty_lr_core::ProductionRule {
                name: "Grammar",
                rule: vec![::rusty_lr_core::Token::NonTerm("UserDataDef")],
            },
            ::rusty_lr_core::ProductionRule {
                name: "Grammar",
                rule: vec![
                    ::rusty_lr_core::Token::NonTerm("ReduceDef"),
                    ::rusty_lr_core::Token::NonTerm("Grammar"),
                ],
            },
            ::rusty_lr_core::ProductionRule {
                name: "Grammar",
                rule: vec![::rusty_lr_core::Token::NonTerm("ReduceDef")],
            },
            ::rusty_lr_core::ProductionRule {
                name: "Grammar",
                rule: vec![
                    ::rusty_lr_core::Token::NonTerm("ErrorDef"),
                    ::rusty_lr_core::Token::NonTerm("Grammar"),
                ],
            },
            ::rusty_lr_core::ProductionRule {
                name: "Grammar",
                rule: vec![::rusty_lr_core::Token::NonTerm("ErrorDef")],
            },
            ::rusty_lr_core::ProductionRule {
                name: "Grammar",
                rule: vec![
                    ::rusty_lr_core::Token::NonTerm("ModulePrefixDef"),
                    ::rusty_lr_core::Token::NonTerm("Grammar"),
                ],
            },
            ::rusty_lr_core::ProductionRule {
                name: "Grammar",
                rule: vec![::rusty_lr_core::Token::NonTerm("ModulePrefixDef")],
            },
            ::rusty_lr_core::ProductionRule {
                name: "ModulePrefixDef",
                rule: vec![
                    ::rusty_lr_core::Token::Term(TermType::ModulePrefix(None)),
                    ::rusty_lr_core::Token::NonTerm("RustCode"),
                    ::rusty_lr_core::Token::Term(TermType::Semicolon(None)),
                ],
            },
            ::rusty_lr_core::ProductionRule {
                name: "ReduceDef",
                rule: vec![
                    ::rusty_lr_core::Token::Term(TermType::Left(None)),
                    ::rusty_lr_core::Token::Term(TermType::Ident(None)),
                    ::rusty_lr_core::Token::Term(TermType::Semicolon(None)),
                ],
            },
            ::rusty_lr_core::ProductionRule {
                name: "ReduceDef",
                rule: vec![
                    ::rusty_lr_core::Token::Term(TermType::Right(None)),
                    ::rusty_lr_core::Token::Term(TermType::Ident(None)),
                    ::rusty_lr_core::Token::Term(TermType::Semicolon(None)),
                ],
            },
            ::rusty_lr_core::ProductionRule {
                name: "Rule",
                rule: vec![
                    ::rusty_lr_core::Token::Term(TermType::Ident(None)),
                    ::rusty_lr_core::Token::NonTerm("RuleType"),
                    ::rusty_lr_core::Token::Term(TermType::Colon(None)),
                    ::rusty_lr_core::Token::NonTerm("RuleLines"),
                    ::rusty_lr_core::Token::Term(TermType::Semicolon(None)),
                ],
            },
            ::rusty_lr_core::ProductionRule {
                name: "RuleDef",
                rule: vec![::rusty_lr_core::Token::NonTerm("__TokenMapped__star_")],
            },
            ::rusty_lr_core::ProductionRule {
                name: "RuleLine",
                rule: vec![
                    ::rusty_lr_core::Token::NonTerm("RuleDef"),
                    ::rusty_lr_core::Token::NonTerm("Action"),
                ],
            },
            ::rusty_lr_core::ProductionRule {
                name: "RuleLines",
                rule: vec![
                    ::rusty_lr_core::Token::NonTerm("RuleLines"),
                    ::rusty_lr_core::Token::Term(TermType::Pipe(None)),
                    ::rusty_lr_core::Token::NonTerm("RuleLine"),
                ],
            },
            ::rusty_lr_core::ProductionRule {
                name: "RuleLines",
                rule: vec![::rusty_lr_core::Token::NonTerm("RuleLine")],
            },
            ::rusty_lr_core::ProductionRule {
                name: "RuleType",
                rule: vec![::rusty_lr_core::Token::Term(TermType::Group(None))],
            },
            ::rusty_lr_core::ProductionRule {
                name: "RuleType",
                rule: vec![],
            },
            ::rusty_lr_core::ProductionRule {
                name: "RustCode",
                rule: vec![::rusty_lr_core::Token::NonTerm("__AnyTokenNoSemi__plus_")],
            },
            ::rusty_lr_core::ProductionRule {
                name: "StartDef",
                rule: vec![
                    ::rusty_lr_core::Token::Term(TermType::Start(None)),
                    ::rusty_lr_core::Token::Term(TermType::Ident(None)),
                    ::rusty_lr_core::Token::Term(TermType::Semicolon(None)),
                ],
            },
            ::rusty_lr_core::ProductionRule {
                name: "SymbolPattern",
                rule: vec![::rusty_lr_core::Token::Term(TermType::Ident(None))],
            },
            ::rusty_lr_core::ProductionRule {
                name: "SymbolPattern",
                rule: vec![
                    ::rusty_lr_core::Token::Term(TermType::Ident(None)),
                    ::rusty_lr_core::Token::Term(TermType::Star(None)),
                ],
            },
            ::rusty_lr_core::ProductionRule {
                name: "SymbolPattern",
                rule: vec![
                    ::rusty_lr_core::Token::Term(TermType::Ident(None)),
                    ::rusty_lr_core::Token::Term(TermType::Plus(None)),
                ],
            },
            ::rusty_lr_core::ProductionRule {
                name: "SymbolPattern",
                rule: vec![
                    ::rusty_lr_core::Token::Term(TermType::Ident(None)),
                    ::rusty_lr_core::Token::Term(TermType::Question(None)),
                ],
            },
            ::rusty_lr_core::ProductionRule {
                name: "TokenDef",
                rule: vec![
                    ::rusty_lr_core::Token::Term(TermType::Token(None)),
                    ::rusty_lr_core::Token::Term(TermType::Ident(None)),
                    ::rusty_lr_core::Token::NonTerm("RustCode"),
                    ::rusty_lr_core::Token::Term(TermType::Semicolon(None)),
                ],
            },
            ::rusty_lr_core::ProductionRule {
                name: "TokenMapped",
                rule: vec![::rusty_lr_core::Token::NonTerm("SymbolPattern")],
            },
            ::rusty_lr_core::ProductionRule {
                name: "TokenMapped",
                rule: vec![
                    ::rusty_lr_core::Token::Term(TermType::Ident(None)),
                    ::rusty_lr_core::Token::Term(TermType::Equal(None)),
                    ::rusty_lr_core::Token::NonTerm("SymbolPattern"),
                ],
            },
            ::rusty_lr_core::ProductionRule {
                name: "TokenTypeDef",
                rule: vec![
                    ::rusty_lr_core::Token::Term(TermType::TokenType(None)),
                    ::rusty_lr_core::Token::NonTerm("RustCode"),
                    ::rusty_lr_core::Token::Term(TermType::Semicolon(None)),
                ],
            },
            ::rusty_lr_core::ProductionRule {
                name: "UserDataDef",
                rule: vec![
                    ::rusty_lr_core::Token::Term(TermType::UserData(None)),
                    ::rusty_lr_core::Token::NonTerm("RustCode"),
                    ::rusty_lr_core::Token::Term(TermType::Semicolon(None)),
                ],
            },
            ::rusty_lr_core::ProductionRule {
                name: "__AnyTokenNoSemi__plus_",
                rule: vec![::rusty_lr_core::Token::NonTerm("AnyTokenNoSemi")],
            },
            ::rusty_lr_core::ProductionRule {
                name: "__AnyTokenNoSemi__plus_",
                rule: vec![
                    ::rusty_lr_core::Token::NonTerm("AnyTokenNoSemi"),
                    ::rusty_lr_core::Token::NonTerm("__AnyTokenNoSemi__plus_"),
                ],
            },
            ::rusty_lr_core::ProductionRule {
                name: "__TokenMapped__plus_",
                rule: vec![::rusty_lr_core::Token::NonTerm("TokenMapped")],
            },
            ::rusty_lr_core::ProductionRule {
                name: "__TokenMapped__plus_",
                rule: vec![
                    ::rusty_lr_core::Token::NonTerm("__TokenMapped__plus_"),
                    ::rusty_lr_core::Token::NonTerm("TokenMapped"),
                ],
            },
            ::rusty_lr_core::ProductionRule {
                name: "__TokenMapped__star_",
                rule: vec![],
            },
            ::rusty_lr_core::ProductionRule {
                name: "__TokenMapped__star_",
                rule: vec![::rusty_lr_core::Token::NonTerm("__TokenMapped__plus_")],
            },
            ::rusty_lr_core::ProductionRule {
                name: "<Augmented>",
                rule: vec![
                    ::rusty_lr_core::Token::NonTerm("Grammar"),
                    ::rusty_lr_core::Token::Term(TermType::Eof),
                ],
            },
        ];
        let rustylr_macrogenerated_lookaheads_0 = std::collections::BTreeSet::from([
            TermType::Eof,
            TermType::EofDef(None),
            TermType::ErrorType(None),
            TermType::Ident(None),
            TermType::Left(None),
            TermType::ModulePrefix(None),
            TermType::Right(None),
            TermType::Start(None),
            TermType::Token(None),
            TermType::TokenType(None),
            TermType::UserData(None),
        ]);
        let rustylr_macrogenerated_lookaheads_1 = std::collections::BTreeSet::from([TermType::Eof]);
        let rustylr_macrogenerated_lookaheads_2 = std::collections::BTreeSet::from([]);
        let rustylr_macrogenerated_lookaheads_3 = std::collections::BTreeSet::from([
            TermType::Colon(None),
            TermType::EofDef(None),
            TermType::Equal(None),
            TermType::ErrorType(None),
            TermType::Group(None),
            TermType::Ident(None),
            TermType::Left(None),
            TermType::Literal(None),
            TermType::ModulePrefix(None),
            TermType::OtherPunct(None),
            TermType::Percent(None),
            TermType::Pipe(None),
            TermType::Plus(None),
            TermType::Question(None),
            TermType::Right(None),
            TermType::Semicolon(None),
            TermType::Star(None),
            TermType::Start(None),
            TermType::Token(None),
            TermType::TokenType(None),
            TermType::UserData(None),
        ]);
        let rustylr_macrogenerated_lookaheads_4 =
            std::collections::BTreeSet::from([TermType::Semicolon(None)]);
        let rustylr_macrogenerated_lookaheads_5 =
            std::collections::BTreeSet::from([TermType::Colon(None)]);
        let rustylr_macrogenerated_lookaheads_6 = std::collections::BTreeSet::from([
            TermType::Group(None),
            TermType::Pipe(None),
            TermType::Semicolon(None),
        ]);
        let rustylr_macrogenerated_lookaheads_7 =
            std::collections::BTreeSet::from([TermType::Pipe(None), TermType::Semicolon(None)]);
        let rustylr_macrogenerated_lookaheads_8 = std::collections::BTreeSet::from([
            TermType::Group(None),
            TermType::Ident(None),
            TermType::Pipe(None),
            TermType::Semicolon(None),
        ]);
        let mut states = Vec::with_capacity(96usize);
        {
            let shift_goto_map_term = std::collections::HashMap::from([
                (TermType::EofDef(None), 1usize),
                (TermType::ErrorType(None), 27usize),
                (TermType::Ident(None), 30usize),
                (TermType::Left(None), 54usize),
                (TermType::ModulePrefix(None), 57usize),
                (TermType::Right(None), 60usize),
                (TermType::Start(None), 63usize),
                (TermType::Token(None), 66usize),
                (TermType::TokenType(None), 70usize),
                (TermType::UserData(None), 73usize),
            ]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([
                ("EofDef", 76usize),
                ("ErrorDef", 77usize),
                ("Grammar", 94usize),
                ("ModulePrefixDef", 79usize),
                ("ReduceDef", 81usize),
                ("Rule", 83usize),
                ("StartDef", 85usize),
                ("TokenDef", 87usize),
                ("TokenTypeDef", 89usize),
                ("UserDataDef", 91usize),
            ]);
            let reduce_map = std::collections::HashMap::from([]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 22usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 23usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 24usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 25usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 26usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 27usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 28usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 29usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 30usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 31usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 32usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 33usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 34usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 35usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 36usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 37usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 38usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 39usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 40usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 41usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 42usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 43usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 44usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 45usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 53usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 58usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 61usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 62usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 69usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_2.clone(),
                    ),
                ]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([
                (TermType::Colon(None), 2usize),
                (TermType::EofDef(None), 3usize),
                (TermType::Equal(None), 4usize),
                (TermType::ErrorType(None), 5usize),
                (TermType::Group(None), 6usize),
                (TermType::Ident(None), 7usize),
                (TermType::Left(None), 8usize),
                (TermType::Literal(None), 9usize),
                (TermType::ModulePrefix(None), 10usize),
                (TermType::OtherPunct(None), 11usize),
                (TermType::Percent(None), 12usize),
                (TermType::Pipe(None), 13usize),
                (TermType::Plus(None), 14usize),
                (TermType::Question(None), 15usize),
                (TermType::Right(None), 16usize),
                (TermType::Star(None), 17usize),
                (TermType::Start(None), 18usize),
                (TermType::Token(None), 19usize),
                (TermType::TokenType(None), 20usize),
                (TermType::UserData(None), 21usize),
            ]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([
                ("AnyTokenNoSemi", 22usize),
                ("RustCode", 24usize),
                ("__AnyTokenNoSemi__plus_", 26usize),
            ]);
            let reduce_map = std::collections::HashMap::from([]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 2usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 3usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 4usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 5usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 6usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 7usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 8usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 9usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 10usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 11usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 12usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 13usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 14usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 15usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 16usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 17usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 18usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 19usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 20usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 21usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 22usize,
                            shifted: 1usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 52usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_4.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 63usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_4.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 64usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_4.clone(),
                    ),
                ]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([
                (TermType::Colon(None), 3usize),
                (TermType::EofDef(None), 3usize),
                (TermType::Equal(None), 3usize),
                (TermType::ErrorType(None), 3usize),
                (TermType::Group(None), 3usize),
                (TermType::Ident(None), 3usize),
                (TermType::Left(None), 3usize),
                (TermType::Literal(None), 3usize),
                (TermType::ModulePrefix(None), 3usize),
                (TermType::OtherPunct(None), 3usize),
                (TermType::Percent(None), 3usize),
                (TermType::Pipe(None), 3usize),
                (TermType::Plus(None), 3usize),
                (TermType::Question(None), 3usize),
                (TermType::Right(None), 3usize),
                (TermType::Semicolon(None), 3usize),
                (TermType::Star(None), 3usize),
                (TermType::Start(None), 3usize),
                (TermType::Token(None), 3usize),
                (TermType::TokenType(None), 3usize),
                (TermType::UserData(None), 3usize),
            ]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 3usize,
                        shifted: 1usize,
                    },
                    rustylr_macrogenerated_lookaheads_3.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([
                (TermType::Colon(None), 10usize),
                (TermType::EofDef(None), 10usize),
                (TermType::Equal(None), 10usize),
                (TermType::ErrorType(None), 10usize),
                (TermType::Group(None), 10usize),
                (TermType::Ident(None), 10usize),
                (TermType::Left(None), 10usize),
                (TermType::Literal(None), 10usize),
                (TermType::ModulePrefix(None), 10usize),
                (TermType::OtherPunct(None), 10usize),
                (TermType::Percent(None), 10usize),
                (TermType::Pipe(None), 10usize),
                (TermType::Plus(None), 10usize),
                (TermType::Question(None), 10usize),
                (TermType::Right(None), 10usize),
                (TermType::Semicolon(None), 10usize),
                (TermType::Star(None), 10usize),
                (TermType::Start(None), 10usize),
                (TermType::Token(None), 10usize),
                (TermType::TokenType(None), 10usize),
                (TermType::UserData(None), 10usize),
            ]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 10usize,
                        shifted: 1usize,
                    },
                    rustylr_macrogenerated_lookaheads_3.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([
                (TermType::Colon(None), 16usize),
                (TermType::EofDef(None), 16usize),
                (TermType::Equal(None), 16usize),
                (TermType::ErrorType(None), 16usize),
                (TermType::Group(None), 16usize),
                (TermType::Ident(None), 16usize),
                (TermType::Left(None), 16usize),
                (TermType::Literal(None), 16usize),
                (TermType::ModulePrefix(None), 16usize),
                (TermType::OtherPunct(None), 16usize),
                (TermType::Percent(None), 16usize),
                (TermType::Pipe(None), 16usize),
                (TermType::Plus(None), 16usize),
                (TermType::Question(None), 16usize),
                (TermType::Right(None), 16usize),
                (TermType::Semicolon(None), 16usize),
                (TermType::Star(None), 16usize),
                (TermType::Start(None), 16usize),
                (TermType::Token(None), 16usize),
                (TermType::TokenType(None), 16usize),
                (TermType::UserData(None), 16usize),
            ]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 16usize,
                        shifted: 1usize,
                    },
                    rustylr_macrogenerated_lookaheads_3.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([
                (TermType::Colon(None), 13usize),
                (TermType::EofDef(None), 13usize),
                (TermType::Equal(None), 13usize),
                (TermType::ErrorType(None), 13usize),
                (TermType::Group(None), 13usize),
                (TermType::Ident(None), 13usize),
                (TermType::Left(None), 13usize),
                (TermType::Literal(None), 13usize),
                (TermType::ModulePrefix(None), 13usize),
                (TermType::OtherPunct(None), 13usize),
                (TermType::Percent(None), 13usize),
                (TermType::Pipe(None), 13usize),
                (TermType::Plus(None), 13usize),
                (TermType::Question(None), 13usize),
                (TermType::Right(None), 13usize),
                (TermType::Semicolon(None), 13usize),
                (TermType::Star(None), 13usize),
                (TermType::Start(None), 13usize),
                (TermType::Token(None), 13usize),
                (TermType::TokenType(None), 13usize),
                (TermType::UserData(None), 13usize),
            ]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 13usize,
                        shifted: 1usize,
                    },
                    rustylr_macrogenerated_lookaheads_3.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([
                (TermType::Colon(None), 14usize),
                (TermType::EofDef(None), 14usize),
                (TermType::Equal(None), 14usize),
                (TermType::ErrorType(None), 14usize),
                (TermType::Group(None), 14usize),
                (TermType::Ident(None), 14usize),
                (TermType::Left(None), 14usize),
                (TermType::Literal(None), 14usize),
                (TermType::ModulePrefix(None), 14usize),
                (TermType::OtherPunct(None), 14usize),
                (TermType::Percent(None), 14usize),
                (TermType::Pipe(None), 14usize),
                (TermType::Plus(None), 14usize),
                (TermType::Question(None), 14usize),
                (TermType::Right(None), 14usize),
                (TermType::Semicolon(None), 14usize),
                (TermType::Star(None), 14usize),
                (TermType::Start(None), 14usize),
                (TermType::Token(None), 14usize),
                (TermType::TokenType(None), 14usize),
                (TermType::UserData(None), 14usize),
            ]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 14usize,
                        shifted: 1usize,
                    },
                    rustylr_macrogenerated_lookaheads_3.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([
                (TermType::Colon(None), 2usize),
                (TermType::EofDef(None), 2usize),
                (TermType::Equal(None), 2usize),
                (TermType::ErrorType(None), 2usize),
                (TermType::Group(None), 2usize),
                (TermType::Ident(None), 2usize),
                (TermType::Left(None), 2usize),
                (TermType::Literal(None), 2usize),
                (TermType::ModulePrefix(None), 2usize),
                (TermType::OtherPunct(None), 2usize),
                (TermType::Percent(None), 2usize),
                (TermType::Pipe(None), 2usize),
                (TermType::Plus(None), 2usize),
                (TermType::Question(None), 2usize),
                (TermType::Right(None), 2usize),
                (TermType::Semicolon(None), 2usize),
                (TermType::Star(None), 2usize),
                (TermType::Start(None), 2usize),
                (TermType::Token(None), 2usize),
                (TermType::TokenType(None), 2usize),
                (TermType::UserData(None), 2usize),
            ]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 2usize,
                        shifted: 1usize,
                    },
                    rustylr_macrogenerated_lookaheads_3.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([
                (TermType::Colon(None), 6usize),
                (TermType::EofDef(None), 6usize),
                (TermType::Equal(None), 6usize),
                (TermType::ErrorType(None), 6usize),
                (TermType::Group(None), 6usize),
                (TermType::Ident(None), 6usize),
                (TermType::Left(None), 6usize),
                (TermType::Literal(None), 6usize),
                (TermType::ModulePrefix(None), 6usize),
                (TermType::OtherPunct(None), 6usize),
                (TermType::Percent(None), 6usize),
                (TermType::Pipe(None), 6usize),
                (TermType::Plus(None), 6usize),
                (TermType::Question(None), 6usize),
                (TermType::Right(None), 6usize),
                (TermType::Semicolon(None), 6usize),
                (TermType::Star(None), 6usize),
                (TermType::Start(None), 6usize),
                (TermType::Token(None), 6usize),
                (TermType::TokenType(None), 6usize),
                (TermType::UserData(None), 6usize),
            ]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 6usize,
                        shifted: 1usize,
                    },
                    rustylr_macrogenerated_lookaheads_3.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([
                (TermType::Colon(None), 15usize),
                (TermType::EofDef(None), 15usize),
                (TermType::Equal(None), 15usize),
                (TermType::ErrorType(None), 15usize),
                (TermType::Group(None), 15usize),
                (TermType::Ident(None), 15usize),
                (TermType::Left(None), 15usize),
                (TermType::Literal(None), 15usize),
                (TermType::ModulePrefix(None), 15usize),
                (TermType::OtherPunct(None), 15usize),
                (TermType::Percent(None), 15usize),
                (TermType::Pipe(None), 15usize),
                (TermType::Plus(None), 15usize),
                (TermType::Question(None), 15usize),
                (TermType::Right(None), 15usize),
                (TermType::Semicolon(None), 15usize),
                (TermType::Star(None), 15usize),
                (TermType::Start(None), 15usize),
                (TermType::Token(None), 15usize),
                (TermType::TokenType(None), 15usize),
                (TermType::UserData(None), 15usize),
            ]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 15usize,
                        shifted: 1usize,
                    },
                    rustylr_macrogenerated_lookaheads_3.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([
                (TermType::Colon(None), 21usize),
                (TermType::EofDef(None), 21usize),
                (TermType::Equal(None), 21usize),
                (TermType::ErrorType(None), 21usize),
                (TermType::Group(None), 21usize),
                (TermType::Ident(None), 21usize),
                (TermType::Left(None), 21usize),
                (TermType::Literal(None), 21usize),
                (TermType::ModulePrefix(None), 21usize),
                (TermType::OtherPunct(None), 21usize),
                (TermType::Percent(None), 21usize),
                (TermType::Pipe(None), 21usize),
                (TermType::Plus(None), 21usize),
                (TermType::Question(None), 21usize),
                (TermType::Right(None), 21usize),
                (TermType::Semicolon(None), 21usize),
                (TermType::Star(None), 21usize),
                (TermType::Start(None), 21usize),
                (TermType::Token(None), 21usize),
                (TermType::TokenType(None), 21usize),
                (TermType::UserData(None), 21usize),
            ]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 21usize,
                        shifted: 1usize,
                    },
                    rustylr_macrogenerated_lookaheads_3.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([
                (TermType::Colon(None), 20usize),
                (TermType::EofDef(None), 20usize),
                (TermType::Equal(None), 20usize),
                (TermType::ErrorType(None), 20usize),
                (TermType::Group(None), 20usize),
                (TermType::Ident(None), 20usize),
                (TermType::Left(None), 20usize),
                (TermType::Literal(None), 20usize),
                (TermType::ModulePrefix(None), 20usize),
                (TermType::OtherPunct(None), 20usize),
                (TermType::Percent(None), 20usize),
                (TermType::Pipe(None), 20usize),
                (TermType::Plus(None), 20usize),
                (TermType::Question(None), 20usize),
                (TermType::Right(None), 20usize),
                (TermType::Semicolon(None), 20usize),
                (TermType::Star(None), 20usize),
                (TermType::Start(None), 20usize),
                (TermType::Token(None), 20usize),
                (TermType::TokenType(None), 20usize),
                (TermType::UserData(None), 20usize),
            ]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 20usize,
                        shifted: 1usize,
                    },
                    rustylr_macrogenerated_lookaheads_3.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([
                (TermType::Colon(None), 5usize),
                (TermType::EofDef(None), 5usize),
                (TermType::Equal(None), 5usize),
                (TermType::ErrorType(None), 5usize),
                (TermType::Group(None), 5usize),
                (TermType::Ident(None), 5usize),
                (TermType::Left(None), 5usize),
                (TermType::Literal(None), 5usize),
                (TermType::ModulePrefix(None), 5usize),
                (TermType::OtherPunct(None), 5usize),
                (TermType::Percent(None), 5usize),
                (TermType::Pipe(None), 5usize),
                (TermType::Plus(None), 5usize),
                (TermType::Question(None), 5usize),
                (TermType::Right(None), 5usize),
                (TermType::Semicolon(None), 5usize),
                (TermType::Star(None), 5usize),
                (TermType::Start(None), 5usize),
                (TermType::Token(None), 5usize),
                (TermType::TokenType(None), 5usize),
                (TermType::UserData(None), 5usize),
            ]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 5usize,
                        shifted: 1usize,
                    },
                    rustylr_macrogenerated_lookaheads_3.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([
                (TermType::Colon(None), 4usize),
                (TermType::EofDef(None), 4usize),
                (TermType::Equal(None), 4usize),
                (TermType::ErrorType(None), 4usize),
                (TermType::Group(None), 4usize),
                (TermType::Ident(None), 4usize),
                (TermType::Left(None), 4usize),
                (TermType::Literal(None), 4usize),
                (TermType::ModulePrefix(None), 4usize),
                (TermType::OtherPunct(None), 4usize),
                (TermType::Percent(None), 4usize),
                (TermType::Pipe(None), 4usize),
                (TermType::Plus(None), 4usize),
                (TermType::Question(None), 4usize),
                (TermType::Right(None), 4usize),
                (TermType::Semicolon(None), 4usize),
                (TermType::Star(None), 4usize),
                (TermType::Start(None), 4usize),
                (TermType::Token(None), 4usize),
                (TermType::TokenType(None), 4usize),
                (TermType::UserData(None), 4usize),
            ]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 4usize,
                        shifted: 1usize,
                    },
                    rustylr_macrogenerated_lookaheads_3.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([
                (TermType::Colon(None), 17usize),
                (TermType::EofDef(None), 17usize),
                (TermType::Equal(None), 17usize),
                (TermType::ErrorType(None), 17usize),
                (TermType::Group(None), 17usize),
                (TermType::Ident(None), 17usize),
                (TermType::Left(None), 17usize),
                (TermType::Literal(None), 17usize),
                (TermType::ModulePrefix(None), 17usize),
                (TermType::OtherPunct(None), 17usize),
                (TermType::Percent(None), 17usize),
                (TermType::Pipe(None), 17usize),
                (TermType::Plus(None), 17usize),
                (TermType::Question(None), 17usize),
                (TermType::Right(None), 17usize),
                (TermType::Semicolon(None), 17usize),
                (TermType::Star(None), 17usize),
                (TermType::Start(None), 17usize),
                (TermType::Token(None), 17usize),
                (TermType::TokenType(None), 17usize),
                (TermType::UserData(None), 17usize),
            ]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 17usize,
                        shifted: 1usize,
                    },
                    rustylr_macrogenerated_lookaheads_3.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([
                (TermType::Colon(None), 19usize),
                (TermType::EofDef(None), 19usize),
                (TermType::Equal(None), 19usize),
                (TermType::ErrorType(None), 19usize),
                (TermType::Group(None), 19usize),
                (TermType::Ident(None), 19usize),
                (TermType::Left(None), 19usize),
                (TermType::Literal(None), 19usize),
                (TermType::ModulePrefix(None), 19usize),
                (TermType::OtherPunct(None), 19usize),
                (TermType::Percent(None), 19usize),
                (TermType::Pipe(None), 19usize),
                (TermType::Plus(None), 19usize),
                (TermType::Question(None), 19usize),
                (TermType::Right(None), 19usize),
                (TermType::Semicolon(None), 19usize),
                (TermType::Star(None), 19usize),
                (TermType::Start(None), 19usize),
                (TermType::Token(None), 19usize),
                (TermType::TokenType(None), 19usize),
                (TermType::UserData(None), 19usize),
            ]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 19usize,
                        shifted: 1usize,
                    },
                    rustylr_macrogenerated_lookaheads_3.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([
                (TermType::Colon(None), 7usize),
                (TermType::EofDef(None), 7usize),
                (TermType::Equal(None), 7usize),
                (TermType::ErrorType(None), 7usize),
                (TermType::Group(None), 7usize),
                (TermType::Ident(None), 7usize),
                (TermType::Left(None), 7usize),
                (TermType::Literal(None), 7usize),
                (TermType::ModulePrefix(None), 7usize),
                (TermType::OtherPunct(None), 7usize),
                (TermType::Percent(None), 7usize),
                (TermType::Pipe(None), 7usize),
                (TermType::Plus(None), 7usize),
                (TermType::Question(None), 7usize),
                (TermType::Right(None), 7usize),
                (TermType::Semicolon(None), 7usize),
                (TermType::Star(None), 7usize),
                (TermType::Start(None), 7usize),
                (TermType::Token(None), 7usize),
                (TermType::TokenType(None), 7usize),
                (TermType::UserData(None), 7usize),
            ]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 7usize,
                        shifted: 1usize,
                    },
                    rustylr_macrogenerated_lookaheads_3.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([
                (TermType::Colon(None), 18usize),
                (TermType::EofDef(None), 18usize),
                (TermType::Equal(None), 18usize),
                (TermType::ErrorType(None), 18usize),
                (TermType::Group(None), 18usize),
                (TermType::Ident(None), 18usize),
                (TermType::Left(None), 18usize),
                (TermType::Literal(None), 18usize),
                (TermType::ModulePrefix(None), 18usize),
                (TermType::OtherPunct(None), 18usize),
                (TermType::Percent(None), 18usize),
                (TermType::Pipe(None), 18usize),
                (TermType::Plus(None), 18usize),
                (TermType::Question(None), 18usize),
                (TermType::Right(None), 18usize),
                (TermType::Semicolon(None), 18usize),
                (TermType::Star(None), 18usize),
                (TermType::Start(None), 18usize),
                (TermType::Token(None), 18usize),
                (TermType::TokenType(None), 18usize),
                (TermType::UserData(None), 18usize),
            ]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 18usize,
                        shifted: 1usize,
                    },
                    rustylr_macrogenerated_lookaheads_3.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([
                (TermType::Colon(None), 9usize),
                (TermType::EofDef(None), 9usize),
                (TermType::Equal(None), 9usize),
                (TermType::ErrorType(None), 9usize),
                (TermType::Group(None), 9usize),
                (TermType::Ident(None), 9usize),
                (TermType::Left(None), 9usize),
                (TermType::Literal(None), 9usize),
                (TermType::ModulePrefix(None), 9usize),
                (TermType::OtherPunct(None), 9usize),
                (TermType::Percent(None), 9usize),
                (TermType::Pipe(None), 9usize),
                (TermType::Plus(None), 9usize),
                (TermType::Question(None), 9usize),
                (TermType::Right(None), 9usize),
                (TermType::Semicolon(None), 9usize),
                (TermType::Star(None), 9usize),
                (TermType::Start(None), 9usize),
                (TermType::Token(None), 9usize),
                (TermType::TokenType(None), 9usize),
                (TermType::UserData(None), 9usize),
            ]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 9usize,
                        shifted: 1usize,
                    },
                    rustylr_macrogenerated_lookaheads_3.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([
                (TermType::Colon(None), 8usize),
                (TermType::EofDef(None), 8usize),
                (TermType::Equal(None), 8usize),
                (TermType::ErrorType(None), 8usize),
                (TermType::Group(None), 8usize),
                (TermType::Ident(None), 8usize),
                (TermType::Left(None), 8usize),
                (TermType::Literal(None), 8usize),
                (TermType::ModulePrefix(None), 8usize),
                (TermType::OtherPunct(None), 8usize),
                (TermType::Percent(None), 8usize),
                (TermType::Pipe(None), 8usize),
                (TermType::Plus(None), 8usize),
                (TermType::Question(None), 8usize),
                (TermType::Right(None), 8usize),
                (TermType::Semicolon(None), 8usize),
                (TermType::Star(None), 8usize),
                (TermType::Start(None), 8usize),
                (TermType::Token(None), 8usize),
                (TermType::TokenType(None), 8usize),
                (TermType::UserData(None), 8usize),
            ]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 8usize,
                        shifted: 1usize,
                    },
                    rustylr_macrogenerated_lookaheads_3.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([
                (TermType::Colon(None), 11usize),
                (TermType::EofDef(None), 11usize),
                (TermType::Equal(None), 11usize),
                (TermType::ErrorType(None), 11usize),
                (TermType::Group(None), 11usize),
                (TermType::Ident(None), 11usize),
                (TermType::Left(None), 11usize),
                (TermType::Literal(None), 11usize),
                (TermType::ModulePrefix(None), 11usize),
                (TermType::OtherPunct(None), 11usize),
                (TermType::Percent(None), 11usize),
                (TermType::Pipe(None), 11usize),
                (TermType::Plus(None), 11usize),
                (TermType::Question(None), 11usize),
                (TermType::Right(None), 11usize),
                (TermType::Semicolon(None), 11usize),
                (TermType::Star(None), 11usize),
                (TermType::Start(None), 11usize),
                (TermType::Token(None), 11usize),
                (TermType::TokenType(None), 11usize),
                (TermType::UserData(None), 11usize),
            ]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 11usize,
                        shifted: 1usize,
                    },
                    rustylr_macrogenerated_lookaheads_3.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([
                (TermType::Colon(None), 12usize),
                (TermType::EofDef(None), 12usize),
                (TermType::Equal(None), 12usize),
                (TermType::ErrorType(None), 12usize),
                (TermType::Group(None), 12usize),
                (TermType::Ident(None), 12usize),
                (TermType::Left(None), 12usize),
                (TermType::Literal(None), 12usize),
                (TermType::ModulePrefix(None), 12usize),
                (TermType::OtherPunct(None), 12usize),
                (TermType::Percent(None), 12usize),
                (TermType::Pipe(None), 12usize),
                (TermType::Plus(None), 12usize),
                (TermType::Question(None), 12usize),
                (TermType::Right(None), 12usize),
                (TermType::Semicolon(None), 12usize),
                (TermType::Star(None), 12usize),
                (TermType::Start(None), 12usize),
                (TermType::Token(None), 12usize),
                (TermType::TokenType(None), 12usize),
                (TermType::UserData(None), 12usize),
            ]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 12usize,
                        shifted: 1usize,
                    },
                    rustylr_macrogenerated_lookaheads_3.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([
                (TermType::Colon(None), 2usize),
                (TermType::EofDef(None), 3usize),
                (TermType::Equal(None), 4usize),
                (TermType::ErrorType(None), 5usize),
                (TermType::Group(None), 6usize),
                (TermType::Ident(None), 7usize),
                (TermType::Left(None), 8usize),
                (TermType::Literal(None), 9usize),
                (TermType::ModulePrefix(None), 10usize),
                (TermType::OtherPunct(None), 11usize),
                (TermType::Percent(None), 12usize),
                (TermType::Pipe(None), 13usize),
                (TermType::Plus(None), 14usize),
                (TermType::Question(None), 15usize),
                (TermType::Right(None), 16usize),
                (TermType::Star(None), 17usize),
                (TermType::Start(None), 18usize),
                (TermType::Token(None), 19usize),
                (TermType::TokenType(None), 20usize),
                (TermType::UserData(None), 21usize),
            ]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([
                ("AnyTokenNoSemi", 22usize),
                ("__AnyTokenNoSemi__plus_", 23usize),
            ]);
            let reduce_map =
                std::collections::HashMap::from([(TermType::Semicolon(None), 63usize)]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 2usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 3usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 4usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 5usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 6usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 7usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 8usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 9usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 10usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 11usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 12usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 13usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 14usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 15usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 16usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 17usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 18usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 19usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 20usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 21usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 63usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_4.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 63usize,
                            shifted: 1usize,
                        },
                        rustylr_macrogenerated_lookaheads_4.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 64usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_4.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 64usize,
                            shifted: 1usize,
                        },
                        rustylr_macrogenerated_lookaheads_4.clone(),
                    ),
                ]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map =
                std::collections::HashMap::from([(TermType::Semicolon(None), 64usize)]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 64usize,
                        shifted: 2usize,
                    },
                    rustylr_macrogenerated_lookaheads_4.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term =
                std::collections::HashMap::from([(TermType::Semicolon(None), 25usize)]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 22usize,
                        shifted: 2usize,
                    },
                    rustylr_macrogenerated_lookaheads_0.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([
                (TermType::Eof, 22usize),
                (TermType::EofDef(None), 22usize),
                (TermType::ErrorType(None), 22usize),
                (TermType::Ident(None), 22usize),
                (TermType::Left(None), 22usize),
                (TermType::ModulePrefix(None), 22usize),
                (TermType::Right(None), 22usize),
                (TermType::Start(None), 22usize),
                (TermType::Token(None), 22usize),
                (TermType::TokenType(None), 22usize),
                (TermType::UserData(None), 22usize),
            ]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 22usize,
                        shifted: 3usize,
                    },
                    rustylr_macrogenerated_lookaheads_0.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map =
                std::collections::HashMap::from([(TermType::Semicolon(None), 52usize)]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 52usize,
                        shifted: 1usize,
                    },
                    rustylr_macrogenerated_lookaheads_4.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([
                (TermType::Colon(None), 2usize),
                (TermType::EofDef(None), 3usize),
                (TermType::Equal(None), 4usize),
                (TermType::ErrorType(None), 5usize),
                (TermType::Group(None), 6usize),
                (TermType::Ident(None), 7usize),
                (TermType::Left(None), 8usize),
                (TermType::Literal(None), 9usize),
                (TermType::ModulePrefix(None), 10usize),
                (TermType::OtherPunct(None), 11usize),
                (TermType::Percent(None), 12usize),
                (TermType::Pipe(None), 13usize),
                (TermType::Plus(None), 14usize),
                (TermType::Question(None), 15usize),
                (TermType::Right(None), 16usize),
                (TermType::Star(None), 17usize),
                (TermType::Start(None), 18usize),
                (TermType::Token(None), 19usize),
                (TermType::TokenType(None), 20usize),
                (TermType::UserData(None), 21usize),
            ]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([
                ("AnyTokenNoSemi", 22usize),
                ("RustCode", 28usize),
                ("__AnyTokenNoSemi__plus_", 26usize),
            ]);
            let reduce_map = std::collections::HashMap::from([]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 2usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 3usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 4usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 5usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 6usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 7usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 8usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 9usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 10usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 11usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 12usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 13usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 14usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 15usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 16usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 17usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 18usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 19usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 20usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 21usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 23usize,
                            shifted: 1usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 52usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_4.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 63usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_4.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 64usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_4.clone(),
                    ),
                ]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term =
                std::collections::HashMap::from([(TermType::Semicolon(None), 29usize)]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 23usize,
                        shifted: 2usize,
                    },
                    rustylr_macrogenerated_lookaheads_0.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([
                (TermType::Eof, 23usize),
                (TermType::EofDef(None), 23usize),
                (TermType::ErrorType(None), 23usize),
                (TermType::Ident(None), 23usize),
                (TermType::Left(None), 23usize),
                (TermType::ModulePrefix(None), 23usize),
                (TermType::Right(None), 23usize),
                (TermType::Start(None), 23usize),
                (TermType::Token(None), 23usize),
                (TermType::TokenType(None), 23usize),
                (TermType::UserData(None), 23usize),
            ]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 23usize,
                        shifted: 3usize,
                    },
                    rustylr_macrogenerated_lookaheads_0.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term =
                std::collections::HashMap::from([(TermType::Group(None), 31usize)]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([("RuleType", 32usize)]);
            let reduce_map = std::collections::HashMap::from([(TermType::Colon(None), 51usize)]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 45usize,
                            shifted: 1usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 50usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_5.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 51usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_5.clone(),
                    ),
                ]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([(TermType::Colon(None), 50usize)]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 50usize,
                        shifted: 1usize,
                    },
                    rustylr_macrogenerated_lookaheads_5.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term =
                std::collections::HashMap::from([(TermType::Colon(None), 33usize)]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 45usize,
                        shifted: 2usize,
                    },
                    rustylr_macrogenerated_lookaheads_0.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term =
                std::collections::HashMap::from([(TermType::Ident(None), 34usize)]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([
                ("RuleDef", 41usize),
                ("RuleLine", 44usize),
                ("RuleLines", 45usize),
                ("SymbolPattern", 48usize),
                ("TokenMapped", 49usize),
                ("__TokenMapped__plus_", 50usize),
                ("__TokenMapped__star_", 52usize),
            ]);
            let reduce_map = std::collections::HashMap::from([
                (TermType::Group(None), 67usize),
                (TermType::Pipe(None), 67usize),
                (TermType::Semicolon(None), 67usize),
            ]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 45usize,
                            shifted: 3usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 46usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_6.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 47usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_7.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 48usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_7.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 49usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_7.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 54usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_8.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 55usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_8.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 56usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_8.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 57usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_8.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 59usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_8.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 60usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_8.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 65usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_8.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 66usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_8.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 67usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_6.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 68usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_6.clone(),
                    ),
                ]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([
                (TermType::Equal(None), 35usize),
                (TermType::Plus(None), 37usize),
                (TermType::Question(None), 38usize),
                (TermType::Star(None), 39usize),
            ]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([
                (TermType::Group(None), 54usize),
                (TermType::Ident(None), 54usize),
                (TermType::Pipe(None), 54usize),
                (TermType::Semicolon(None), 54usize),
            ]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 54usize,
                            shifted: 1usize,
                        },
                        rustylr_macrogenerated_lookaheads_8.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 55usize,
                            shifted: 1usize,
                        },
                        rustylr_macrogenerated_lookaheads_8.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 56usize,
                            shifted: 1usize,
                        },
                        rustylr_macrogenerated_lookaheads_8.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 57usize,
                            shifted: 1usize,
                        },
                        rustylr_macrogenerated_lookaheads_8.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 60usize,
                            shifted: 1usize,
                        },
                        rustylr_macrogenerated_lookaheads_8.clone(),
                    ),
                ]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term =
                std::collections::HashMap::from([(TermType::Ident(None), 36usize)]);
            let shift_goto_map_nonterm =
                std::collections::HashMap::from([("SymbolPattern", 40usize)]);
            let reduce_map = std::collections::HashMap::from([]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 54usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_8.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 55usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_8.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 56usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_8.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 57usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_8.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 60usize,
                            shifted: 2usize,
                        },
                        rustylr_macrogenerated_lookaheads_8.clone(),
                    ),
                ]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([
                (TermType::Plus(None), 37usize),
                (TermType::Question(None), 38usize),
                (TermType::Star(None), 39usize),
            ]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([
                (TermType::Group(None), 54usize),
                (TermType::Ident(None), 54usize),
                (TermType::Pipe(None), 54usize),
                (TermType::Semicolon(None), 54usize),
            ]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 54usize,
                            shifted: 1usize,
                        },
                        rustylr_macrogenerated_lookaheads_8.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 55usize,
                            shifted: 1usize,
                        },
                        rustylr_macrogenerated_lookaheads_8.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 56usize,
                            shifted: 1usize,
                        },
                        rustylr_macrogenerated_lookaheads_8.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 57usize,
                            shifted: 1usize,
                        },
                        rustylr_macrogenerated_lookaheads_8.clone(),
                    ),
                ]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([
                (TermType::Group(None), 56usize),
                (TermType::Ident(None), 56usize),
                (TermType::Pipe(None), 56usize),
                (TermType::Semicolon(None), 56usize),
            ]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 56usize,
                        shifted: 2usize,
                    },
                    rustylr_macrogenerated_lookaheads_8.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([
                (TermType::Group(None), 57usize),
                (TermType::Ident(None), 57usize),
                (TermType::Pipe(None), 57usize),
                (TermType::Semicolon(None), 57usize),
            ]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 57usize,
                        shifted: 2usize,
                    },
                    rustylr_macrogenerated_lookaheads_8.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([
                (TermType::Group(None), 55usize),
                (TermType::Ident(None), 55usize),
                (TermType::Pipe(None), 55usize),
                (TermType::Semicolon(None), 55usize),
            ]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 55usize,
                        shifted: 2usize,
                    },
                    rustylr_macrogenerated_lookaheads_8.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([
                (TermType::Group(None), 60usize),
                (TermType::Ident(None), 60usize),
                (TermType::Pipe(None), 60usize),
                (TermType::Semicolon(None), 60usize),
            ]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 60usize,
                        shifted: 3usize,
                    },
                    rustylr_macrogenerated_lookaheads_8.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term =
                std::collections::HashMap::from([(TermType::Group(None), 42usize)]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([("Action", 43usize)]);
            let reduce_map = std::collections::HashMap::from([
                (TermType::Pipe(None), 1usize),
                (TermType::Semicolon(None), 1usize),
            ]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 0usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_7.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 1usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_7.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 47usize,
                            shifted: 1usize,
                        },
                        rustylr_macrogenerated_lookaheads_7.clone(),
                    ),
                ]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([
                (TermType::Pipe(None), 0usize),
                (TermType::Semicolon(None), 0usize),
            ]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 0usize,
                        shifted: 1usize,
                    },
                    rustylr_macrogenerated_lookaheads_7.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([
                (TermType::Pipe(None), 47usize),
                (TermType::Semicolon(None), 47usize),
            ]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 47usize,
                        shifted: 2usize,
                    },
                    rustylr_macrogenerated_lookaheads_7.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([
                (TermType::Pipe(None), 49usize),
                (TermType::Semicolon(None), 49usize),
            ]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 49usize,
                        shifted: 1usize,
                    },
                    rustylr_macrogenerated_lookaheads_7.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([
                (TermType::Pipe(None), 46usize),
                (TermType::Semicolon(None), 53usize),
            ]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 45usize,
                            shifted: 4usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 48usize,
                            shifted: 1usize,
                        },
                        rustylr_macrogenerated_lookaheads_7.clone(),
                    ),
                ]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term =
                std::collections::HashMap::from([(TermType::Ident(None), 34usize)]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([
                ("RuleDef", 41usize),
                ("RuleLine", 47usize),
                ("SymbolPattern", 48usize),
                ("TokenMapped", 49usize),
                ("__TokenMapped__plus_", 50usize),
                ("__TokenMapped__star_", 52usize),
            ]);
            let reduce_map = std::collections::HashMap::from([
                (TermType::Group(None), 67usize),
                (TermType::Pipe(None), 67usize),
                (TermType::Semicolon(None), 67usize),
            ]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 46usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_6.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 47usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_7.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 48usize,
                            shifted: 2usize,
                        },
                        rustylr_macrogenerated_lookaheads_7.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 54usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_8.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 55usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_8.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 56usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_8.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 57usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_8.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 59usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_8.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 60usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_8.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 65usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_8.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 66usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_8.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 67usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_6.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 68usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_6.clone(),
                    ),
                ]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([
                (TermType::Pipe(None), 48usize),
                (TermType::Semicolon(None), 48usize),
            ]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 48usize,
                        shifted: 3usize,
                    },
                    rustylr_macrogenerated_lookaheads_7.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([
                (TermType::Group(None), 59usize),
                (TermType::Ident(None), 59usize),
                (TermType::Pipe(None), 59usize),
                (TermType::Semicolon(None), 59usize),
            ]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 59usize,
                        shifted: 1usize,
                    },
                    rustylr_macrogenerated_lookaheads_8.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([
                (TermType::Group(None), 65usize),
                (TermType::Ident(None), 65usize),
                (TermType::Pipe(None), 65usize),
                (TermType::Semicolon(None), 65usize),
            ]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 65usize,
                        shifted: 1usize,
                    },
                    rustylr_macrogenerated_lookaheads_8.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term =
                std::collections::HashMap::from([(TermType::Ident(None), 34usize)]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([
                ("SymbolPattern", 48usize),
                ("TokenMapped", 51usize),
            ]);
            let reduce_map = std::collections::HashMap::from([
                (TermType::Group(None), 68usize),
                (TermType::Pipe(None), 68usize),
                (TermType::Semicolon(None), 68usize),
            ]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 54usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_8.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 55usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_8.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 56usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_8.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 57usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_8.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 59usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_8.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 60usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_8.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 66usize,
                            shifted: 1usize,
                        },
                        rustylr_macrogenerated_lookaheads_8.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 68usize,
                            shifted: 1usize,
                        },
                        rustylr_macrogenerated_lookaheads_6.clone(),
                    ),
                ]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([
                (TermType::Group(None), 66usize),
                (TermType::Ident(None), 66usize),
                (TermType::Pipe(None), 66usize),
                (TermType::Semicolon(None), 66usize),
            ]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 66usize,
                        shifted: 2usize,
                    },
                    rustylr_macrogenerated_lookaheads_8.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([
                (TermType::Group(None), 46usize),
                (TermType::Pipe(None), 46usize),
                (TermType::Semicolon(None), 46usize),
            ]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 46usize,
                        shifted: 1usize,
                    },
                    rustylr_macrogenerated_lookaheads_6.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([
                (TermType::Eof, 45usize),
                (TermType::EofDef(None), 45usize),
                (TermType::ErrorType(None), 45usize),
                (TermType::Ident(None), 45usize),
                (TermType::Left(None), 45usize),
                (TermType::ModulePrefix(None), 45usize),
                (TermType::Right(None), 45usize),
                (TermType::Start(None), 45usize),
                (TermType::Token(None), 45usize),
                (TermType::TokenType(None), 45usize),
                (TermType::UserData(None), 45usize),
            ]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 45usize,
                        shifted: 5usize,
                    },
                    rustylr_macrogenerated_lookaheads_0.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term =
                std::collections::HashMap::from([(TermType::Ident(None), 55usize)]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 43usize,
                        shifted: 1usize,
                    },
                    rustylr_macrogenerated_lookaheads_0.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term =
                std::collections::HashMap::from([(TermType::Semicolon(None), 56usize)]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 43usize,
                        shifted: 2usize,
                    },
                    rustylr_macrogenerated_lookaheads_0.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([
                (TermType::Eof, 43usize),
                (TermType::EofDef(None), 43usize),
                (TermType::ErrorType(None), 43usize),
                (TermType::Ident(None), 43usize),
                (TermType::Left(None), 43usize),
                (TermType::ModulePrefix(None), 43usize),
                (TermType::Right(None), 43usize),
                (TermType::Start(None), 43usize),
                (TermType::Token(None), 43usize),
                (TermType::TokenType(None), 43usize),
                (TermType::UserData(None), 43usize),
            ]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 43usize,
                        shifted: 3usize,
                    },
                    rustylr_macrogenerated_lookaheads_0.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([
                (TermType::Colon(None), 2usize),
                (TermType::EofDef(None), 3usize),
                (TermType::Equal(None), 4usize),
                (TermType::ErrorType(None), 5usize),
                (TermType::Group(None), 6usize),
                (TermType::Ident(None), 7usize),
                (TermType::Left(None), 8usize),
                (TermType::Literal(None), 9usize),
                (TermType::ModulePrefix(None), 10usize),
                (TermType::OtherPunct(None), 11usize),
                (TermType::Percent(None), 12usize),
                (TermType::Pipe(None), 13usize),
                (TermType::Plus(None), 14usize),
                (TermType::Question(None), 15usize),
                (TermType::Right(None), 16usize),
                (TermType::Star(None), 17usize),
                (TermType::Start(None), 18usize),
                (TermType::Token(None), 19usize),
                (TermType::TokenType(None), 20usize),
                (TermType::UserData(None), 21usize),
            ]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([
                ("AnyTokenNoSemi", 22usize),
                ("RustCode", 58usize),
                ("__AnyTokenNoSemi__plus_", 26usize),
            ]);
            let reduce_map = std::collections::HashMap::from([]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 2usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 3usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 4usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 5usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 6usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 7usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 8usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 9usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 10usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 11usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 12usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 13usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 14usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 15usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 16usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 17usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 18usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 19usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 20usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 21usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 42usize,
                            shifted: 1usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 52usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_4.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 63usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_4.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 64usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_4.clone(),
                    ),
                ]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term =
                std::collections::HashMap::from([(TermType::Semicolon(None), 59usize)]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 42usize,
                        shifted: 2usize,
                    },
                    rustylr_macrogenerated_lookaheads_0.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([
                (TermType::Eof, 42usize),
                (TermType::EofDef(None), 42usize),
                (TermType::ErrorType(None), 42usize),
                (TermType::Ident(None), 42usize),
                (TermType::Left(None), 42usize),
                (TermType::ModulePrefix(None), 42usize),
                (TermType::Right(None), 42usize),
                (TermType::Start(None), 42usize),
                (TermType::Token(None), 42usize),
                (TermType::TokenType(None), 42usize),
                (TermType::UserData(None), 42usize),
            ]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 42usize,
                        shifted: 3usize,
                    },
                    rustylr_macrogenerated_lookaheads_0.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term =
                std::collections::HashMap::from([(TermType::Ident(None), 61usize)]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 44usize,
                        shifted: 1usize,
                    },
                    rustylr_macrogenerated_lookaheads_0.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term =
                std::collections::HashMap::from([(TermType::Semicolon(None), 62usize)]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 44usize,
                        shifted: 2usize,
                    },
                    rustylr_macrogenerated_lookaheads_0.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([
                (TermType::Eof, 44usize),
                (TermType::EofDef(None), 44usize),
                (TermType::ErrorType(None), 44usize),
                (TermType::Ident(None), 44usize),
                (TermType::Left(None), 44usize),
                (TermType::ModulePrefix(None), 44usize),
                (TermType::Right(None), 44usize),
                (TermType::Start(None), 44usize),
                (TermType::Token(None), 44usize),
                (TermType::TokenType(None), 44usize),
                (TermType::UserData(None), 44usize),
            ]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 44usize,
                        shifted: 3usize,
                    },
                    rustylr_macrogenerated_lookaheads_0.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term =
                std::collections::HashMap::from([(TermType::Ident(None), 64usize)]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 53usize,
                        shifted: 1usize,
                    },
                    rustylr_macrogenerated_lookaheads_0.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term =
                std::collections::HashMap::from([(TermType::Semicolon(None), 65usize)]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 53usize,
                        shifted: 2usize,
                    },
                    rustylr_macrogenerated_lookaheads_0.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([
                (TermType::Eof, 53usize),
                (TermType::EofDef(None), 53usize),
                (TermType::ErrorType(None), 53usize),
                (TermType::Ident(None), 53usize),
                (TermType::Left(None), 53usize),
                (TermType::ModulePrefix(None), 53usize),
                (TermType::Right(None), 53usize),
                (TermType::Start(None), 53usize),
                (TermType::Token(None), 53usize),
                (TermType::TokenType(None), 53usize),
                (TermType::UserData(None), 53usize),
            ]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 53usize,
                        shifted: 3usize,
                    },
                    rustylr_macrogenerated_lookaheads_0.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term =
                std::collections::HashMap::from([(TermType::Ident(None), 67usize)]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 58usize,
                        shifted: 1usize,
                    },
                    rustylr_macrogenerated_lookaheads_0.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([
                (TermType::Colon(None), 2usize),
                (TermType::EofDef(None), 3usize),
                (TermType::Equal(None), 4usize),
                (TermType::ErrorType(None), 5usize),
                (TermType::Group(None), 6usize),
                (TermType::Ident(None), 7usize),
                (TermType::Left(None), 8usize),
                (TermType::Literal(None), 9usize),
                (TermType::ModulePrefix(None), 10usize),
                (TermType::OtherPunct(None), 11usize),
                (TermType::Percent(None), 12usize),
                (TermType::Pipe(None), 13usize),
                (TermType::Plus(None), 14usize),
                (TermType::Question(None), 15usize),
                (TermType::Right(None), 16usize),
                (TermType::Star(None), 17usize),
                (TermType::Start(None), 18usize),
                (TermType::Token(None), 19usize),
                (TermType::TokenType(None), 20usize),
                (TermType::UserData(None), 21usize),
            ]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([
                ("AnyTokenNoSemi", 22usize),
                ("RustCode", 68usize),
                ("__AnyTokenNoSemi__plus_", 26usize),
            ]);
            let reduce_map = std::collections::HashMap::from([]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 2usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 3usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 4usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 5usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 6usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 7usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 8usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 9usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 10usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 11usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 12usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 13usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 14usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 15usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 16usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 17usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 18usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 19usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 20usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 21usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 52usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_4.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 58usize,
                            shifted: 2usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 63usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_4.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 64usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_4.clone(),
                    ),
                ]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term =
                std::collections::HashMap::from([(TermType::Semicolon(None), 69usize)]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 58usize,
                        shifted: 3usize,
                    },
                    rustylr_macrogenerated_lookaheads_0.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([
                (TermType::Eof, 58usize),
                (TermType::EofDef(None), 58usize),
                (TermType::ErrorType(None), 58usize),
                (TermType::Ident(None), 58usize),
                (TermType::Left(None), 58usize),
                (TermType::ModulePrefix(None), 58usize),
                (TermType::Right(None), 58usize),
                (TermType::Start(None), 58usize),
                (TermType::Token(None), 58usize),
                (TermType::TokenType(None), 58usize),
                (TermType::UserData(None), 58usize),
            ]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 58usize,
                        shifted: 4usize,
                    },
                    rustylr_macrogenerated_lookaheads_0.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([
                (TermType::Colon(None), 2usize),
                (TermType::EofDef(None), 3usize),
                (TermType::Equal(None), 4usize),
                (TermType::ErrorType(None), 5usize),
                (TermType::Group(None), 6usize),
                (TermType::Ident(None), 7usize),
                (TermType::Left(None), 8usize),
                (TermType::Literal(None), 9usize),
                (TermType::ModulePrefix(None), 10usize),
                (TermType::OtherPunct(None), 11usize),
                (TermType::Percent(None), 12usize),
                (TermType::Pipe(None), 13usize),
                (TermType::Plus(None), 14usize),
                (TermType::Question(None), 15usize),
                (TermType::Right(None), 16usize),
                (TermType::Star(None), 17usize),
                (TermType::Start(None), 18usize),
                (TermType::Token(None), 19usize),
                (TermType::TokenType(None), 20usize),
                (TermType::UserData(None), 21usize),
            ]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([
                ("AnyTokenNoSemi", 22usize),
                ("RustCode", 71usize),
                ("__AnyTokenNoSemi__plus_", 26usize),
            ]);
            let reduce_map = std::collections::HashMap::from([]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 2usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 3usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 4usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 5usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 6usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 7usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 8usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 9usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 10usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 11usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 12usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 13usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 14usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 15usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 16usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 17usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 18usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 19usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 20usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 21usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 52usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_4.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 61usize,
                            shifted: 1usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 63usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_4.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 64usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_4.clone(),
                    ),
                ]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term =
                std::collections::HashMap::from([(TermType::Semicolon(None), 72usize)]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 61usize,
                        shifted: 2usize,
                    },
                    rustylr_macrogenerated_lookaheads_0.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([
                (TermType::Eof, 61usize),
                (TermType::EofDef(None), 61usize),
                (TermType::ErrorType(None), 61usize),
                (TermType::Ident(None), 61usize),
                (TermType::Left(None), 61usize),
                (TermType::ModulePrefix(None), 61usize),
                (TermType::Right(None), 61usize),
                (TermType::Start(None), 61usize),
                (TermType::Token(None), 61usize),
                (TermType::TokenType(None), 61usize),
                (TermType::UserData(None), 61usize),
            ]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 61usize,
                        shifted: 3usize,
                    },
                    rustylr_macrogenerated_lookaheads_0.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([
                (TermType::Colon(None), 2usize),
                (TermType::EofDef(None), 3usize),
                (TermType::Equal(None), 4usize),
                (TermType::ErrorType(None), 5usize),
                (TermType::Group(None), 6usize),
                (TermType::Ident(None), 7usize),
                (TermType::Left(None), 8usize),
                (TermType::Literal(None), 9usize),
                (TermType::ModulePrefix(None), 10usize),
                (TermType::OtherPunct(None), 11usize),
                (TermType::Percent(None), 12usize),
                (TermType::Pipe(None), 13usize),
                (TermType::Plus(None), 14usize),
                (TermType::Question(None), 15usize),
                (TermType::Right(None), 16usize),
                (TermType::Star(None), 17usize),
                (TermType::Start(None), 18usize),
                (TermType::Token(None), 19usize),
                (TermType::TokenType(None), 20usize),
                (TermType::UserData(None), 21usize),
            ]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([
                ("AnyTokenNoSemi", 22usize),
                ("RustCode", 74usize),
                ("__AnyTokenNoSemi__plus_", 26usize),
            ]);
            let reduce_map = std::collections::HashMap::from([]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 2usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 3usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 4usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 5usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 6usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 7usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 8usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 9usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 10usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 11usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 12usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 13usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 14usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 15usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 16usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 17usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 18usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 19usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 20usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 21usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 52usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_4.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 62usize,
                            shifted: 1usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 63usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_4.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 64usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_4.clone(),
                    ),
                ]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term =
                std::collections::HashMap::from([(TermType::Semicolon(None), 75usize)]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 62usize,
                        shifted: 2usize,
                    },
                    rustylr_macrogenerated_lookaheads_0.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([
                (TermType::Eof, 62usize),
                (TermType::EofDef(None), 62usize),
                (TermType::ErrorType(None), 62usize),
                (TermType::Ident(None), 62usize),
                (TermType::Left(None), 62usize),
                (TermType::ModulePrefix(None), 62usize),
                (TermType::Right(None), 62usize),
                (TermType::Start(None), 62usize),
                (TermType::Token(None), 62usize),
                (TermType::TokenType(None), 62usize),
                (TermType::UserData(None), 62usize),
            ]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 62usize,
                        shifted: 3usize,
                    },
                    rustylr_macrogenerated_lookaheads_0.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([
                (TermType::EofDef(None), 1usize),
                (TermType::ErrorType(None), 27usize),
                (TermType::Ident(None), 30usize),
                (TermType::Left(None), 54usize),
                (TermType::ModulePrefix(None), 57usize),
                (TermType::Right(None), 60usize),
                (TermType::Start(None), 63usize),
                (TermType::Token(None), 66usize),
                (TermType::TokenType(None), 70usize),
                (TermType::UserData(None), 73usize),
            ]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([
                ("EofDef", 76usize),
                ("ErrorDef", 77usize),
                ("Grammar", 93usize),
                ("ModulePrefixDef", 79usize),
                ("ReduceDef", 81usize),
                ("Rule", 83usize),
                ("StartDef", 85usize),
                ("TokenDef", 87usize),
                ("TokenTypeDef", 89usize),
                ("UserDataDef", 91usize),
            ]);
            let reduce_map = std::collections::HashMap::from([(TermType::Eof, 31usize)]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 22usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 23usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 24usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 25usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 26usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 27usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 28usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 29usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 30usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 30usize,
                            shifted: 1usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 31usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 31usize,
                            shifted: 1usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 32usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 33usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 34usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 35usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 36usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 37usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 38usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 39usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 40usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 41usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 42usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 43usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 44usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 45usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 53usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 58usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 61usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 62usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                ]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([
                (TermType::EofDef(None), 1usize),
                (TermType::ErrorType(None), 27usize),
                (TermType::Ident(None), 30usize),
                (TermType::Left(None), 54usize),
                (TermType::ModulePrefix(None), 57usize),
                (TermType::Right(None), 60usize),
                (TermType::Start(None), 63usize),
                (TermType::Token(None), 66usize),
                (TermType::TokenType(None), 70usize),
                (TermType::UserData(None), 73usize),
            ]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([
                ("EofDef", 76usize),
                ("ErrorDef", 77usize),
                ("Grammar", 78usize),
                ("ModulePrefixDef", 79usize),
                ("ReduceDef", 81usize),
                ("Rule", 83usize),
                ("StartDef", 85usize),
                ("TokenDef", 87usize),
                ("TokenTypeDef", 89usize),
                ("UserDataDef", 91usize),
            ]);
            let reduce_map = std::collections::HashMap::from([(TermType::Eof, 39usize)]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 22usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 23usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 24usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 25usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 26usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 27usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 28usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 29usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 30usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 31usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 32usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 33usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 34usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 35usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 36usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 37usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 38usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 38usize,
                            shifted: 1usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 39usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 39usize,
                            shifted: 1usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 40usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 41usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 42usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 43usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 44usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 45usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 53usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 58usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 61usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 62usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                ]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([(TermType::Eof, 38usize)]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 38usize,
                        shifted: 2usize,
                    },
                    rustylr_macrogenerated_lookaheads_1.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([
                (TermType::EofDef(None), 1usize),
                (TermType::ErrorType(None), 27usize),
                (TermType::Ident(None), 30usize),
                (TermType::Left(None), 54usize),
                (TermType::ModulePrefix(None), 57usize),
                (TermType::Right(None), 60usize),
                (TermType::Start(None), 63usize),
                (TermType::Token(None), 66usize),
                (TermType::TokenType(None), 70usize),
                (TermType::UserData(None), 73usize),
            ]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([
                ("EofDef", 76usize),
                ("ErrorDef", 77usize),
                ("Grammar", 80usize),
                ("ModulePrefixDef", 79usize),
                ("ReduceDef", 81usize),
                ("Rule", 83usize),
                ("StartDef", 85usize),
                ("TokenDef", 87usize),
                ("TokenTypeDef", 89usize),
                ("UserDataDef", 91usize),
            ]);
            let reduce_map = std::collections::HashMap::from([(TermType::Eof, 41usize)]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 22usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 23usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 24usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 25usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 26usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 27usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 28usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 29usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 30usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 31usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 32usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 33usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 34usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 35usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 36usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 37usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 38usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 39usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 40usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 40usize,
                            shifted: 1usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 41usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 41usize,
                            shifted: 1usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 42usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 43usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 44usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 45usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 53usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 58usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 61usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 62usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                ]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([(TermType::Eof, 40usize)]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 40usize,
                        shifted: 2usize,
                    },
                    rustylr_macrogenerated_lookaheads_1.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([
                (TermType::EofDef(None), 1usize),
                (TermType::ErrorType(None), 27usize),
                (TermType::Ident(None), 30usize),
                (TermType::Left(None), 54usize),
                (TermType::ModulePrefix(None), 57usize),
                (TermType::Right(None), 60usize),
                (TermType::Start(None), 63usize),
                (TermType::Token(None), 66usize),
                (TermType::TokenType(None), 70usize),
                (TermType::UserData(None), 73usize),
            ]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([
                ("EofDef", 76usize),
                ("ErrorDef", 77usize),
                ("Grammar", 82usize),
                ("ModulePrefixDef", 79usize),
                ("ReduceDef", 81usize),
                ("Rule", 83usize),
                ("StartDef", 85usize),
                ("TokenDef", 87usize),
                ("TokenTypeDef", 89usize),
                ("UserDataDef", 91usize),
            ]);
            let reduce_map = std::collections::HashMap::from([(TermType::Eof, 37usize)]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 22usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 23usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 24usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 25usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 26usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 27usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 28usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 29usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 30usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 31usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 32usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 33usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 34usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 35usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 36usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 36usize,
                            shifted: 1usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 37usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 37usize,
                            shifted: 1usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 38usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 39usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 40usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 41usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 42usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 43usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 44usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 45usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 53usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 58usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 61usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 62usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                ]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([(TermType::Eof, 36usize)]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 36usize,
                        shifted: 2usize,
                    },
                    rustylr_macrogenerated_lookaheads_1.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([
                (TermType::EofDef(None), 1usize),
                (TermType::ErrorType(None), 27usize),
                (TermType::Ident(None), 30usize),
                (TermType::Left(None), 54usize),
                (TermType::ModulePrefix(None), 57usize),
                (TermType::Right(None), 60usize),
                (TermType::Start(None), 63usize),
                (TermType::Token(None), 66usize),
                (TermType::TokenType(None), 70usize),
                (TermType::UserData(None), 73usize),
            ]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([
                ("EofDef", 76usize),
                ("ErrorDef", 77usize),
                ("Grammar", 84usize),
                ("ModulePrefixDef", 79usize),
                ("ReduceDef", 81usize),
                ("Rule", 83usize),
                ("StartDef", 85usize),
                ("TokenDef", 87usize),
                ("TokenTypeDef", 89usize),
                ("UserDataDef", 91usize),
            ]);
            let reduce_map = std::collections::HashMap::from([(TermType::Eof, 25usize)]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 22usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 23usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 24usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 24usize,
                            shifted: 1usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 25usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 25usize,
                            shifted: 1usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 26usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 27usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 28usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 29usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 30usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 31usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 32usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 33usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 34usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 35usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 36usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 37usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 38usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 39usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 40usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 41usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 42usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 43usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 44usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 45usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 53usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 58usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 61usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 62usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                ]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([(TermType::Eof, 24usize)]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 24usize,
                        shifted: 2usize,
                    },
                    rustylr_macrogenerated_lookaheads_1.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([
                (TermType::EofDef(None), 1usize),
                (TermType::ErrorType(None), 27usize),
                (TermType::Ident(None), 30usize),
                (TermType::Left(None), 54usize),
                (TermType::ModulePrefix(None), 57usize),
                (TermType::Right(None), 60usize),
                (TermType::Start(None), 63usize),
                (TermType::Token(None), 66usize),
                (TermType::TokenType(None), 70usize),
                (TermType::UserData(None), 73usize),
            ]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([
                ("EofDef", 76usize),
                ("ErrorDef", 77usize),
                ("Grammar", 86usize),
                ("ModulePrefixDef", 79usize),
                ("ReduceDef", 81usize),
                ("Rule", 83usize),
                ("StartDef", 85usize),
                ("TokenDef", 87usize),
                ("TokenTypeDef", 89usize),
                ("UserDataDef", 91usize),
            ]);
            let reduce_map = std::collections::HashMap::from([(TermType::Eof, 29usize)]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 22usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 23usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 24usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 25usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 26usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 27usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 28usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 28usize,
                            shifted: 1usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 29usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 29usize,
                            shifted: 1usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 30usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 31usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 32usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 33usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 34usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 35usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 36usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 37usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 38usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 39usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 40usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 41usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 42usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 43usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 44usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 45usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 53usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 58usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 61usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 62usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                ]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([(TermType::Eof, 28usize)]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 28usize,
                        shifted: 2usize,
                    },
                    rustylr_macrogenerated_lookaheads_1.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([
                (TermType::EofDef(None), 1usize),
                (TermType::ErrorType(None), 27usize),
                (TermType::Ident(None), 30usize),
                (TermType::Left(None), 54usize),
                (TermType::ModulePrefix(None), 57usize),
                (TermType::Right(None), 60usize),
                (TermType::Start(None), 63usize),
                (TermType::Token(None), 66usize),
                (TermType::TokenType(None), 70usize),
                (TermType::UserData(None), 73usize),
            ]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([
                ("EofDef", 76usize),
                ("ErrorDef", 77usize),
                ("Grammar", 88usize),
                ("ModulePrefixDef", 79usize),
                ("ReduceDef", 81usize),
                ("Rule", 83usize),
                ("StartDef", 85usize),
                ("TokenDef", 87usize),
                ("TokenTypeDef", 89usize),
                ("UserDataDef", 91usize),
            ]);
            let reduce_map = std::collections::HashMap::from([(TermType::Eof, 27usize)]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 22usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 23usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 24usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 25usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 26usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 26usize,
                            shifted: 1usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 27usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 27usize,
                            shifted: 1usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 28usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 29usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 30usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 31usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 32usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 33usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 34usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 35usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 36usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 37usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 38usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 39usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 40usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 41usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 42usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 43usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 44usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 45usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 53usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 58usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 61usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 62usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                ]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([(TermType::Eof, 26usize)]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 26usize,
                        shifted: 2usize,
                    },
                    rustylr_macrogenerated_lookaheads_1.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([
                (TermType::EofDef(None), 1usize),
                (TermType::ErrorType(None), 27usize),
                (TermType::Ident(None), 30usize),
                (TermType::Left(None), 54usize),
                (TermType::ModulePrefix(None), 57usize),
                (TermType::Right(None), 60usize),
                (TermType::Start(None), 63usize),
                (TermType::Token(None), 66usize),
                (TermType::TokenType(None), 70usize),
                (TermType::UserData(None), 73usize),
            ]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([
                ("EofDef", 76usize),
                ("ErrorDef", 77usize),
                ("Grammar", 90usize),
                ("ModulePrefixDef", 79usize),
                ("ReduceDef", 81usize),
                ("Rule", 83usize),
                ("StartDef", 85usize),
                ("TokenDef", 87usize),
                ("TokenTypeDef", 89usize),
                ("UserDataDef", 91usize),
            ]);
            let reduce_map = std::collections::HashMap::from([(TermType::Eof, 33usize)]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 22usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 23usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 24usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 25usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 26usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 27usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 28usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 29usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 30usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 31usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 32usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 32usize,
                            shifted: 1usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 33usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 33usize,
                            shifted: 1usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 34usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 35usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 36usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 37usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 38usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 39usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 40usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 41usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 42usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 43usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 44usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 45usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 53usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 58usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 61usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 62usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                ]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([(TermType::Eof, 32usize)]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 32usize,
                        shifted: 2usize,
                    },
                    rustylr_macrogenerated_lookaheads_1.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([
                (TermType::EofDef(None), 1usize),
                (TermType::ErrorType(None), 27usize),
                (TermType::Ident(None), 30usize),
                (TermType::Left(None), 54usize),
                (TermType::ModulePrefix(None), 57usize),
                (TermType::Right(None), 60usize),
                (TermType::Start(None), 63usize),
                (TermType::Token(None), 66usize),
                (TermType::TokenType(None), 70usize),
                (TermType::UserData(None), 73usize),
            ]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([
                ("EofDef", 76usize),
                ("ErrorDef", 77usize),
                ("Grammar", 92usize),
                ("ModulePrefixDef", 79usize),
                ("ReduceDef", 81usize),
                ("Rule", 83usize),
                ("StartDef", 85usize),
                ("TokenDef", 87usize),
                ("TokenTypeDef", 89usize),
                ("UserDataDef", 91usize),
            ]);
            let reduce_map = std::collections::HashMap::from([(TermType::Eof, 35usize)]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 22usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 23usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 24usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 25usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 26usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 27usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 28usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 29usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 30usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 31usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 32usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 33usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 34usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 34usize,
                            shifted: 1usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 35usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 35usize,
                            shifted: 1usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 36usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 37usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 38usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 39usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 40usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 41usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 42usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 43usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 44usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 45usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 53usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 58usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 61usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 62usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                ]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([(TermType::Eof, 34usize)]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 34usize,
                        shifted: 2usize,
                    },
                    rustylr_macrogenerated_lookaheads_1.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([(TermType::Eof, 30usize)]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 30usize,
                        shifted: 2usize,
                    },
                    rustylr_macrogenerated_lookaheads_1.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([(TermType::Eof, 95usize)]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 69usize,
                        shifted: 1usize,
                    },
                    rustylr_macrogenerated_lookaheads_2.clone(),
                )]),
            };
            let state = ::rusty_lr_core::State {
                shift_goto_map_term,
                shift_goto_map_nonterm,
                reduce_map,
                ruleset,
            };
            states.push(state);
        }
        {
            let shift_goto_map_term = std::collections::HashMap::from([]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 69usize,
                        shifted: 2usize,
                    },
                    rustylr_macrogenerated_lookaheads_2.clone(),
                )]),
            };
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
