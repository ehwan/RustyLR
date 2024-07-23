use crate::grammar::TermType;

use super::error::ParseError;
use super::token::Token;
use proc_macro2::TokenStream;
use rusty_lr;

#[derive(Debug)]
pub struct RuleLine {
    pub tokens: Vec<Token>,
    pub reduce_type: rusty_lr::ReduceType,
    pub reduce_action: Option<TokenStream>,
}

impl RuleLine {
    // ReduceType: Left | Right | Error
    //           |
    //           ;
    pub(crate) fn parse_reduce_type(
        tree: &rusty_lr::Tree,
        terms: &[TermType],
    ) -> Result<rusty_lr::ReduceType, ParseError> {
        match tree.slice(terms).get(0) {
            Some(TermType::Left(_)) => Ok(rusty_lr::ReduceType::Left),
            Some(TermType::Right(_)) => Ok(rusty_lr::ReduceType::Right),
            Some(TermType::Error(_)) => Ok(rusty_lr::ReduceType::Error),
            None => Ok(rusty_lr::ReduceType::Error),
            _ => unreachable!(),
        }
    }

    // RuleDef: Tokens ;
    //
    // Tokens: TokensOne
    //       |
    //       ;
    // TokensOne: Token TokensOne
    //          | Token
    //          ;
    // Token: Ident
    pub(crate) fn parse_defs(
        tree: &rusty_lr::Tree,
        terms: &[TermType],
        _parser: &rusty_lr::Parser<TermType, &'static str>,
    ) -> Result<Vec<Token>, ParseError> {
        let mut ret = Vec::with_capacity(terms.len());

        for term in tree.slice(terms) {
            if let TermType::Ident(ident) = term {
                ret.push(Token::Term(ident.as_ref().unwrap().clone()));
            } else {
                unreachable!();
            }
        }
        Ok(ret)
    }
    // RuleLine: RuleDef ReduceType Action ;
    pub(crate) fn parse_tree(
        tree: &rusty_lr::Tree,
        terms: &[TermType],
        parser: &rusty_lr::Parser<TermType, &'static str>,
    ) -> Result<Self, ParseError> {
        match tree {
            rusty_lr::Tree::NonTerminal(_, children) => {
                // children[0] = RuleDef
                // children[1] = ReduceType
                // children[2] = Action

                let tokens = Self::parse_defs(&children[0], terms, parser)?;
                let reduce_type = Self::parse_reduce_type(&children[1], terms)?;
                let reduce_action = children[2].slice(terms);
                let reduce_action = match reduce_action.get(0) {
                    Some(TermType::Group(group)) => {
                        let group = group.as_ref().unwrap();
                        Some(group.stream())
                    }
                    None => {
                        // if not set, use action that returns '()'
                        // @TODO set span here
                        let stream = quote::quote! { () };
                        Some(stream)
                    }
                    _ => unreachable!(),
                };

                Ok(Self {
                    tokens,
                    reduce_type,
                    reduce_action,
                })
            }
            _ => {
                unreachable!();
            }
        }
    }
}

#[derive(Debug)]
pub struct RuleLines {
    pub rule_lines: Vec<RuleLine>,
}

impl RuleLines {
    // RuleLines: RuleLine '|' RuleLines
    //          | RuleLine
    //          ;
    // returned vec is reversed
    pub(crate) fn parse_tree(
        tree: &rusty_lr::Tree,
        terms: &[TermType],
        parser: &rusty_lr::Parser<TermType, &'static str>,
    ) -> Result<Self, ParseError> {
        match tree {
            rusty_lr::Tree::NonTerminal(_, children) => {
                if children.len() == 3 {
                    let rule_line = RuleLine::parse_tree(&children[0], terms, parser)?;
                    let mut rule = Self::parse_tree(&children[2], terms, parser)?;
                    rule.rule_lines.push(rule_line);
                    Ok(rule)
                } else if children.len() == 1 {
                    let rule_line = RuleLine::parse_tree(&children[0], terms, parser)?;
                    Ok(Self {
                        rule_lines: vec![rule_line],
                    })
                } else {
                    unreachable!();
                }
            }
            _ => {
                unreachable!();
            }
        }
    }
}
