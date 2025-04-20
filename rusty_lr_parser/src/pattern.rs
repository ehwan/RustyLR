use super::error::ParseError;
use super::grammar::Grammar;
use super::nonterminal_info::{NonTerminalInfo, Rule};
use super::token::TokenMapped;

use std::collections::BTreeSet;

use proc_macro2::Ident;
use proc_macro2::Span;
use proc_macro2::TokenStream;

use quote::quote;

use rusty_lr_core::HashMap;
use rusty_lr_core::Token;

/// Some regex pattern
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum PatternType {
    Ident(Ident),
    Plus(Box<Pattern>),
    Star(Box<Pattern>),
    Question(Box<Pattern>),
    Exclamation(Box<Pattern>),
    TerminalSet(BTreeSet<usize>),
    Lookaheads(Box<Pattern>, BTreeSet<usize>),
    Group(Vec<Pattern>),
    Literal(syn::Lit),
}

#[derive(Debug, Clone)]
pub struct Pattern {
    pub pattern_type: PatternType,
    pub pretty_name: String,
}
impl std::hash::Hash for Pattern {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.pattern_type.hash(state);
    }
}
impl std::cmp::PartialEq for Pattern {
    fn eq(&self, other: &Self) -> bool {
        self.pattern_type == other.pattern_type
    }
}
impl std::cmp::Eq for Pattern {}

#[derive(Clone)]
pub struct PatternResult {
    pub name: Ident,
    pub token: Token<usize, usize>,
    pub ruletype_map: Option<(TokenStream, Ident)>,
}

impl Pattern {
    /// get rule for the pattern
    /// make new rule if not exists
    ///
    /// *Note*
    /// When converting `PatternArgs` to `Pattern`,
    /// if any exclamation mark `!` is present,
    /// it will be put in the innermost pattern.
    /// e.g. Pattern like `A+?!` will be converted to `A!+?`
    pub(crate) fn to_rule(
        &self,
        grammar: &mut Grammar,
        pattern_map: &mut HashMap<Pattern, PatternResult>,
        root_span_pair: (Span, Span),
    ) -> Result<PatternResult, ParseError> {
        if let Some(existing) = pattern_map.get(self) {
            return Ok(existing.clone());
        }
        match &self.pattern_type {
            PatternType::Ident(ident) => {
                if let Some(term_idx) = grammar.terminals_index.get(ident) {
                    Ok(PatternResult {
                        name: ident.clone(),
                        token: Token::Term(*term_idx),
                        ruletype_map: Some((grammar.token_typename.clone(), ident.clone())),
                    })
                } else if let Some(nonterm_idx) = grammar.nonterminals_index.get(ident) {
                    let nonterminal = &grammar.nonterminals[*nonterm_idx];
                    Ok(PatternResult {
                        name: ident.clone(),
                        token: Token::NonTerm(*nonterm_idx),
                        ruletype_map: nonterminal
                            .ruletype
                            .as_ref()
                            .map(|ruletype| (ruletype.clone(), ident.clone())),
                    })
                } else {
                    Err(ParseError::TerminalNotDefined(ident.clone()))
                }
            }
            PatternType::Plus(pattern) => {
                let base_rule = pattern.to_rule(grammar, pattern_map, root_span_pair)?;
                let newrule_idx = grammar.nonterminals.len();
                let newrule_name = Ident::new(
                    &format!("_{}_Plus{}", base_rule.name, newrule_idx),
                    root_span_pair.0,
                );

                if let Some((base_typename, base_mapto)) = &base_rule.ruletype_map {
                    // typename exist, make new rule with typename Vec<base_typename>
                    // A+ -> A+ A { Ap.push(A); Ap }
                    //     | A    { vec![A] }
                    let line1 = Rule {
                        tokens: vec![TokenMapped {
                            token: base_rule.token,
                            mapto: Some(Ident::new("A", Span::call_site())),
                            begin_span: Span::call_site(),
                            end_span: Span::call_site(),
                        }],
                        reduce_action: Some(quote! {
                            { vec![A] }
                        }),
                        separator_span: Span::call_site(),
                        lookaheads: None,
                        id: 0,
                        prec: None,
                    };
                    let line2 = Rule {
                        tokens: vec![
                            TokenMapped {
                                token: Token::NonTerm(newrule_idx),
                                mapto: Some(Ident::new("Ap", Span::call_site())),
                                begin_span: Span::call_site(),
                                end_span: Span::call_site(),
                            },
                            TokenMapped {
                                token: base_rule.token,
                                mapto: Some(Ident::new("A", Span::call_site())),
                                begin_span: Span::call_site(),
                                end_span: Span::call_site(),
                            },
                        ],
                        reduce_action: Some(quote! {
                            { Ap.push(A); Ap }
                        }),
                        separator_span: Span::call_site(),
                        lookaheads: None,
                        id: 0,
                        prec: None,
                    };

                    let nonterm_info = NonTerminalInfo {
                        name: newrule_name.clone(),
                        pretty_name: self.pretty_name.clone(),
                        ruletype: Some(quote! { Vec<#base_typename> }),
                        rules: vec![line1, line2],
                        regex_span: Some(root_span_pair),
                    };
                    grammar.nonterminals.push(nonterm_info);
                    grammar
                        .nonterminals_index
                        .insert(newrule_name.clone(), newrule_idx);

                    let res = PatternResult {
                        name: newrule_name,
                        token: Token::NonTerm(newrule_idx),
                        ruletype_map: Some((quote! { Vec<#base_typename> }, base_mapto.clone())),
                    };
                    pattern_map.insert(self.clone(), res.clone());
                    Ok(res)
                } else {
                    // typename not exist, make new rule with typename ()
                    // A+ -> A Ap
                    //     | A
                    let line1 = Rule {
                        tokens: vec![TokenMapped {
                            token: base_rule.token,
                            mapto: None,
                            begin_span: Span::call_site(),
                            end_span: Span::call_site(),
                        }],
                        reduce_action: None,
                        separator_span: Span::call_site(),
                        lookaheads: None,
                        id: 0,
                        prec: None,
                    };
                    let line2 = Rule {
                        tokens: vec![
                            TokenMapped {
                                token: base_rule.token,
                                mapto: None,
                                begin_span: Span::call_site(),
                                end_span: Span::call_site(),
                            },
                            TokenMapped {
                                token: Token::NonTerm(newrule_idx),
                                mapto: None,
                                begin_span: Span::call_site(),
                                end_span: Span::call_site(),
                            },
                        ],
                        reduce_action: None,
                        separator_span: Span::call_site(),
                        lookaheads: None,
                        id: 0,
                        prec: None,
                    };

                    let nonterm_info = NonTerminalInfo {
                        name: newrule_name.clone(),
                        pretty_name: self.pretty_name.clone(),
                        ruletype: None,
                        rules: vec![line1, line2],
                        regex_span: Some(root_span_pair),
                    };
                    grammar.nonterminals.push(nonterm_info);
                    grammar
                        .nonterminals_index
                        .insert(newrule_name.clone(), newrule_idx);

                    let res = PatternResult {
                        name: newrule_name,
                        token: Token::NonTerm(newrule_idx),
                        ruletype_map: None,
                    };
                    pattern_map.insert(self.clone(), res.clone());
                    Ok(res)
                }
            }
            PatternType::Star(pattern) => {
                let plus_rule = Pattern {
                    pattern_type: PatternType::Plus(pattern.clone()),
                    pretty_name: format!("{}+", pattern.pretty_name),
                }
                .to_rule(grammar, pattern_map, root_span_pair)?;

                let base_rule = pattern.to_rule(grammar, pattern_map, root_span_pair)?;

                let newrule_idx = grammar.nonterminals.len();
                let newrule_name = Ident::new(
                    &format!("_{}_Star{}", base_rule.name, newrule_idx),
                    root_span_pair.0,
                );

                if let Some((base_typename, base_mapto)) = &base_rule.ruletype_map {
                    // typename exist, make new rule with typename Vec<base_typename>
                    // A* -> A+ { Ap }
                    //     |    { vec![] }
                    let line1 = Rule {
                        tokens: vec![TokenMapped {
                            token: plus_rule.token,
                            mapto: Some(Ident::new("Ap", Span::call_site())),
                            begin_span: Span::call_site(),
                            end_span: Span::call_site(),
                        }],
                        reduce_action: Some(quote! {
                            { Ap }
                        }),
                        separator_span: Span::call_site(),
                        lookaheads: None,
                        id: 0,
                        prec: None,
                    };
                    let line2 = Rule {
                        tokens: vec![],
                        reduce_action: Some(quote! {
                            { vec![] }
                        }),
                        separator_span: Span::call_site(),
                        lookaheads: None,
                        id: 0,
                        prec: None,
                    };

                    let nonterm_info = NonTerminalInfo {
                        name: newrule_name.clone(),
                        pretty_name: self.pretty_name.clone(),
                        ruletype: Some(quote! { Vec<#base_typename> }),
                        rules: vec![line1, line2],
                        regex_span: Some(root_span_pair),
                    };
                    grammar.nonterminals.push(nonterm_info);
                    grammar
                        .nonterminals_index
                        .insert(newrule_name.clone(), newrule_idx);

                    let res = PatternResult {
                        name: newrule_name,
                        token: Token::NonTerm(newrule_idx),
                        ruletype_map: Some((quote! { Vec<#base_typename> }, base_mapto.clone())),
                    };
                    pattern_map.insert(self.clone(), res.clone());
                    Ok(res)
                } else {
                    // typename not exist, make new rule with typename ()
                    // A* -> A+ { Ap }
                    //     |    { vec![] }
                    let line1 = Rule {
                        tokens: vec![TokenMapped {
                            token: plus_rule.token,
                            mapto: None,
                            begin_span: Span::call_site(),
                            end_span: Span::call_site(),
                        }],
                        reduce_action: None,
                        separator_span: Span::call_site(),
                        lookaheads: None,
                        id: 0,
                        prec: None,
                    };
                    let line2 = Rule {
                        tokens: vec![],
                        reduce_action: None,
                        separator_span: Span::call_site(),
                        lookaheads: None,
                        id: 0,
                        prec: None,
                    };
                    let nonterm_info = NonTerminalInfo {
                        name: newrule_name.clone(),
                        pretty_name: self.pretty_name.clone(),
                        ruletype: None,
                        rules: vec![line1, line2],
                        regex_span: Some(root_span_pair),
                    };
                    grammar.nonterminals.push(nonterm_info);
                    grammar
                        .nonterminals_index
                        .insert(newrule_name.clone(), newrule_idx);

                    let res = PatternResult {
                        name: newrule_name,
                        token: Token::NonTerm(newrule_idx),
                        ruletype_map: None,
                    };
                    pattern_map.insert(self.clone(), res.clone());
                    Ok(res)
                }
            }
            PatternType::Question(pattern) => {
                let base_rule = pattern.to_rule(grammar, pattern_map, root_span_pair)?;
                let newrule_idx = grammar.nonterminals.len();
                let newrule_name = Ident::new(
                    &format!("_{}_Question{}", base_rule.name, newrule_idx),
                    root_span_pair.0,
                );

                if let Some((base_typename, base_mapto)) = &base_rule.ruletype_map {
                    // typename exist, make new rule with typename Option<base_typename>
                    // A? -> A { Some(A) }
                    //     |   { None }
                    let line1 = Rule {
                        tokens: vec![TokenMapped {
                            token: base_rule.token,
                            mapto: Some(Ident::new("A", Span::call_site())),
                            begin_span: Span::call_site(),
                            end_span: Span::call_site(),
                        }],
                        reduce_action: Some(quote! {
                            { Some(A) }
                        }),
                        separator_span: Span::call_site(),
                        lookaheads: None,
                        id: 0,
                        prec: None,
                    };
                    let line2 = Rule {
                        tokens: vec![],
                        reduce_action: Some(quote! {
                            { None }
                        }),
                        separator_span: Span::call_site(),
                        lookaheads: None,
                        id: 0,
                        prec: None,
                    };

                    let nonterm_info = NonTerminalInfo {
                        name: newrule_name.clone(),
                        pretty_name: self.pretty_name.clone(),
                        ruletype: Some(quote! {Option<#base_typename>}),
                        rules: vec![line1, line2],
                        regex_span: Some(root_span_pair),
                    };
                    grammar.nonterminals.push(nonterm_info);
                    grammar
                        .nonterminals_index
                        .insert(newrule_name.clone(), newrule_idx);

                    let res = PatternResult {
                        name: newrule_name,
                        token: Token::NonTerm(newrule_idx),
                        ruletype_map: Some((quote! {Option<#base_typename>}, base_mapto.clone())),
                    };
                    pattern_map.insert(self.clone(), res.clone());
                    Ok(res)
                } else {
                    // typename not exist, make new rule with typename ()
                    // A? -> A { Some(A) }
                    //     |   { None }
                    let line1 = Rule {
                        tokens: vec![TokenMapped {
                            token: base_rule.token,
                            mapto: None,
                            begin_span: Span::call_site(),
                            end_span: Span::call_site(),
                        }],
                        reduce_action: None,
                        separator_span: Span::call_site(),
                        lookaheads: None,
                        id: 0,
                        prec: None,
                    };
                    let line2 = Rule {
                        tokens: vec![],
                        reduce_action: None,
                        separator_span: Span::call_site(),
                        lookaheads: None,
                        id: 0,
                        prec: None,
                    };

                    let nonterm_info = NonTerminalInfo {
                        name: newrule_name.clone(),
                        pretty_name: self.pretty_name.clone(),
                        ruletype: None,
                        rules: vec![line1, line2],
                        regex_span: Some(root_span_pair),
                    };
                    grammar.nonterminals.push(nonterm_info);
                    grammar
                        .nonterminals_index
                        .insert(newrule_name.clone(), newrule_idx);

                    let res = PatternResult {
                        name: newrule_name,
                        token: Token::NonTerm(newrule_idx),
                        ruletype_map: None,
                    };
                    pattern_map.insert(self.clone(), res.clone());
                    Ok(res)
                }
            }

            PatternType::Exclamation(pattern) => {
                let mut base_rule = pattern.to_rule(grammar, pattern_map, root_span_pair)?;
                base_rule.ruletype_map = None;
                Ok(base_rule)
            }
            PatternType::TerminalSet(terminal_set) => {
                let newrule_idx = grammar.nonterminals.len();
                let newrule_name =
                    Ident::new(&format!("_TerminalSet{}", newrule_idx), root_span_pair.0);

                let mut rules = Vec::with_capacity(terminal_set.len());
                for terminal in terminal_set.iter() {
                    let rule = Rule {
                        tokens: vec![TokenMapped {
                            token: Token::Term(*terminal),
                            mapto: Some(Ident::new("term", Span::call_site())),
                            begin_span: Span::call_site(),
                            end_span: Span::call_site(),
                        }],
                        reduce_action: Some(quote! {
                            term
                        }),
                        separator_span: Span::call_site(),
                        lookaheads: None,
                        id: 0,
                        prec: None,
                    };
                    rules.push(rule);
                }
                let nonterm_info = NonTerminalInfo {
                    name: newrule_name.clone(),
                    pretty_name: self.pretty_name.clone(),
                    ruletype: Some(grammar.token_typename.clone()),
                    rules,
                    regex_span: Some(root_span_pair),
                };
                grammar.nonterminals.push(nonterm_info);
                grammar
                    .nonterminals_index
                    .insert(newrule_name.clone(), newrule_idx);

                let res = PatternResult {
                    name: newrule_name,
                    token: Token::NonTerm(newrule_idx),
                    ruletype_map: Some((
                        grammar.token_typename.clone(),
                        Ident::new("__rustylr_terminal", Span::call_site()),
                    )),
                };
                pattern_map.insert(self.clone(), res.clone());
                Ok(res)
            }
            PatternType::Lookaheads(pattern, lookaheads) => {
                let base_rule = pattern.to_rule(grammar, pattern_map, root_span_pair)?;

                let newrule_idx = grammar.nonterminals.len();
                let newrule_name = Ident::new(
                    &format!("_{}_Lookaheads{}", base_rule.name, newrule_idx),
                    root_span_pair.0,
                );

                if let Some((base_typename, base_mapto)) = &base_rule.ruletype_map {
                    let rule = Rule {
                        tokens: vec![TokenMapped {
                            token: base_rule.token,
                            mapto: Some(Ident::new("A", Span::call_site())),
                            begin_span: Span::call_site(),
                            end_span: Span::call_site(),
                        }],
                        reduce_action: Some(quote! {
                            A
                        }),
                        separator_span: Span::call_site(),
                        lookaheads: Some(lookaheads.clone()),
                        id: 0,
                        prec: None,
                    };

                    let nonterm_info = NonTerminalInfo {
                        name: newrule_name.clone(),
                        pretty_name: self.pretty_name.clone(),
                        ruletype: Some(base_typename.clone()),
                        rules: vec![rule],
                        regex_span: Some(root_span_pair),
                    };
                    grammar.nonterminals.push(nonterm_info);
                    grammar
                        .nonterminals_index
                        .insert(newrule_name.clone(), newrule_idx);

                    let res = PatternResult {
                        name: newrule_name,
                        token: Token::NonTerm(newrule_idx),
                        ruletype_map: Some((base_typename.clone(), base_mapto.clone())),
                    };
                    pattern_map.insert(self.clone(), res.clone());
                    Ok(res)
                } else {
                    let rule = Rule {
                        tokens: vec![TokenMapped {
                            token: base_rule.token,
                            mapto: None,
                            begin_span: Span::call_site(),
                            end_span: Span::call_site(),
                        }],
                        reduce_action: None,
                        separator_span: Span::call_site(),
                        lookaheads: Some(lookaheads.clone()),
                        id: 0,
                        prec: None,
                    };

                    let nonterm_info = NonTerminalInfo {
                        name: newrule_name.clone(),
                        pretty_name: self.pretty_name.clone(),
                        ruletype: None,
                        rules: vec![rule],
                        regex_span: Some(root_span_pair),
                    };
                    grammar.nonterminals.push(nonterm_info);
                    grammar
                        .nonterminals_index
                        .insert(newrule_name.clone(), newrule_idx);

                    let res = PatternResult {
                        name: newrule_name,
                        token: Token::NonTerm(newrule_idx),
                        ruletype_map: None,
                    };
                    pattern_map.insert(self.clone(), res.clone());
                    Ok(res)
                }
            }

            PatternType::Group(group) => {
                // Consider parenthesis-ed group of patterns
                // ( A B C D ... )
                // if there are no pattern holding a value, then the RuleType of the group is None
                // if there is only one pattern holding a value, T, then the RuleType of the group is T
                // otherwise, the RuleType of the group is (T1, T2, T3, ...) where T1 T2 T3 ... are the RuleType of the patterns holding a value

                let mut elements = Vec::with_capacity(group.len());
                for (child_idx, child) in group.iter().enumerate() {
                    let mut child_rule = child.to_rule(grammar, pattern_map, root_span_pair)?;
                    if let Some((_, mapto)) = &mut child_rule.ruletype_map {
                        *mapto = Ident::new(
                            format!("__rustylr_group_elem{}", child_idx).as_str(),
                            Span::call_site(),
                        );
                    }
                    elements.push(child_rule);
                }
                let mut tokens = Vec::with_capacity(group.len());
                for child in elements.iter() {
                    tokens.push(TokenMapped {
                        token: child.token,
                        mapto: child.ruletype_map.as_ref().map(|(_, ident)| ident.clone()),
                        begin_span: Span::call_site(),
                        end_span: Span::call_site(),
                    });
                }

                // unique child with ruletype
                let mut ruletype_child_idxs = Vec::with_capacity(group.len());
                for (child_idx, child) in elements.iter().enumerate() {
                    if child.ruletype_map.is_some() {
                        ruletype_child_idxs.push(child_idx);
                    }
                }

                let newrule_idx = grammar.nonterminals.len();
                let newrule_name = Ident::new(&format!("_Group{}", newrule_idx), root_span_pair.0);

                match ruletype_child_idxs.len() {
                    0 => {
                        let rule = Rule {
                            tokens,
                            reduce_action: None,
                            separator_span: Span::call_site(),
                            lookaheads: None,
                            id: 0,
                            prec: None,
                        };
                        let nonterm_info = NonTerminalInfo {
                            name: newrule_name.clone(),
                            pretty_name: self.pretty_name.clone(),
                            ruletype: None,
                            rules: vec![rule],
                            regex_span: Some(root_span_pair),
                        };
                        grammar.nonterminals.push(nonterm_info);
                        grammar
                            .nonterminals_index
                            .insert(newrule_name.clone(), newrule_idx);

                        let res = PatternResult {
                            name: newrule_name,
                            token: Token::NonTerm(newrule_idx),
                            ruletype_map: None,
                        };
                        pattern_map.insert(self.clone(), res.clone());
                        Ok(res)
                    }

                    1 => {
                        let unique_child_idx = ruletype_child_idxs[0];
                        let (typename, mapto) =
                            &elements[unique_child_idx].ruletype_map.as_ref().unwrap();
                        let rule = Rule {
                            tokens,
                            reduce_action: Some(quote! { #mapto }),
                            separator_span: Span::call_site(),
                            lookaheads: None,
                            id: 0,
                            prec: None,
                        };
                        let nonterm_info = NonTerminalInfo {
                            name: newrule_name.clone(),
                            pretty_name: self.pretty_name.clone(),
                            ruletype: Some(typename.clone()),
                            rules: vec![rule],
                            regex_span: Some(root_span_pair),
                        };
                        grammar.nonterminals.push(nonterm_info);
                        grammar
                            .nonterminals_index
                            .insert(newrule_name.clone(), newrule_idx);

                        let res = PatternResult {
                            name: newrule_name,
                            token: Token::NonTerm(newrule_idx),
                            ruletype_map: Some((typename.clone(), mapto.clone())),
                        };
                        pattern_map.insert(self.clone(), res.clone());
                        Ok(res)
                    }

                    _ => {
                        let mut typename = TokenStream::new();
                        let mut initializer = TokenStream::new();

                        for child_idx in ruletype_child_idxs.into_iter() {
                            let (child_typename, child_mapto) =
                                &elements[child_idx].ruletype_map.as_ref().unwrap();
                            typename.extend(quote! {#child_typename,});
                            initializer.extend(quote! { #child_mapto, });
                        }
                        let typename = quote! {(#typename)};
                        let initializer = quote! {(#initializer)};
                        let rule = Rule {
                            tokens,
                            reduce_action: Some(initializer),
                            separator_span: Span::call_site(),
                            lookaheads: None,
                            id: 0,
                            prec: None,
                        };
                        let nonterm_info = NonTerminalInfo {
                            name: newrule_name.clone(),
                            pretty_name: self.pretty_name.clone(),
                            ruletype: Some(typename.clone()),
                            rules: vec![rule],
                            regex_span: Some(root_span_pair),
                        };
                        grammar.nonterminals.push(nonterm_info);
                        grammar
                            .nonterminals_index
                            .insert(newrule_name.clone(), newrule_idx);

                        let res = PatternResult {
                            name: newrule_name,
                            token: Token::NonTerm(newrule_idx),
                            ruletype_map: Some((
                                typename,
                                Ident::new("__rustylr_group", Span::call_site()),
                            )),
                        };
                        pattern_map.insert(self.clone(), res.clone());
                        Ok(res)
                    }
                }
            }

            PatternType::Literal(literal) => match literal {
                syn::Lit::Char(_) => {
                    let idx = grammar
                        .add_or_get_literal_character(literal.clone(), None)
                        .unwrap();
                    let info = &grammar.terminals[idx];

                    Ok(PatternResult {
                        name: info.name.clone(),
                        token: Token::Term(idx),
                        ruletype_map: Some((grammar.token_typename.clone(), info.name.clone())),
                    })
                }
                syn::Lit::Byte(_) => {
                    let idx = grammar
                        .add_or_get_literal_character(literal.clone(), None)
                        .unwrap();
                    let info = &grammar.terminals[idx];

                    Ok(PatternResult {
                        name: info.name.clone(),
                        token: Token::Term(idx),
                        ruletype_map: Some((grammar.token_typename.clone(), info.name.clone())),
                    })
                }
                syn::Lit::Str(s) => {
                    let newrule_idx = grammar.nonterminals.len();
                    let str_span = s.span();
                    let newrule_name =
                        Ident::new(&format!("_LiteralString{}", newrule_idx), str_span);

                    let rule = Rule {
                        tokens: s
                            .value()
                            .chars()
                            .map(|ch| {
                                let term_id = grammar
                                    .add_or_get_literal_character(
                                        syn::Lit::Char(syn::LitChar::new(ch, str_span)),
                                        None,
                                    )
                                    .unwrap();

                                TokenMapped {
                                    token: Token::Term(term_id),
                                    mapto: None,
                                    begin_span: str_span,
                                    end_span: str_span,
                                }
                            })
                            .collect(),
                        reduce_action: None,
                        separator_span: Span::call_site(),
                        lookaheads: None,
                        id: 0,
                        prec: None,
                    };

                    let nonterm_info = NonTerminalInfo {
                        name: newrule_name.clone(),
                        pretty_name: s.value(),
                        ruletype: None,
                        rules: vec![rule],
                        regex_span: Some((str_span, str_span)),
                    };
                    grammar.nonterminals.push(nonterm_info);
                    grammar
                        .nonterminals_index
                        .insert(newrule_name.clone(), newrule_idx);

                    let res = PatternResult {
                        name: newrule_name.clone(),
                        token: Token::NonTerm(newrule_idx),
                        ruletype_map: Some((quote! { &'static str }, newrule_name)),
                    };
                    pattern_map.insert(self.clone(), res.clone());
                    Ok(res)
                }
                syn::Lit::ByteStr(s) => {
                    let newrule_idx = grammar.nonterminals.len();
                    let str_span = s.span();
                    let newrule_name =
                        Ident::new(&format!("_LiteralString{}", newrule_idx), str_span);
                    let vec = s.value();

                    let rule = Rule {
                        tokens: vec
                            .iter()
                            .map(|ch| {
                                let term_id = grammar
                                    .add_or_get_literal_character(
                                        syn::Lit::Byte(syn::LitByte::new(*ch, str_span)),
                                        None,
                                    )
                                    .unwrap();

                                TokenMapped {
                                    token: Token::Term(term_id),
                                    mapto: None,
                                    begin_span: str_span,
                                    end_span: str_span,
                                }
                            })
                            .collect(),
                        reduce_action: None,
                        separator_span: Span::call_site(),
                        lookaheads: None,
                        id: 0,
                        prec: None,
                    };

                    let nonterm_info = NonTerminalInfo {
                        name: newrule_name.clone(),
                        pretty_name: String::from_utf8(vec).unwrap(),
                        ruletype: None,
                        rules: vec![rule],
                        regex_span: Some((str_span, str_span)),
                    };
                    grammar.nonterminals.push(nonterm_info);
                    grammar
                        .nonterminals_index
                        .insert(newrule_name.clone(), newrule_idx);

                    let res = PatternResult {
                        name: newrule_name.clone(),
                        token: Token::NonTerm(newrule_idx),
                        ruletype_map: Some((quote! { &'static [u8] }, newrule_name)),
                    };
                    pattern_map.insert(self.clone(), res.clone());
                    Ok(res)
                }
                _ => unreachable!("Only char, byte, str and bytes are supported"),
            },
        }
    }
}
