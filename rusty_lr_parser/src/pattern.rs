use super::error::ParseError;
use super::grammar::Grammar;
use super::rule::{RuleLine, RuleLines};
use super::token::TokenMapped;

use std::collections::BTreeSet;

use proc_macro2::Ident;
use proc_macro2::Span;
use proc_macro2::TokenStream;

use quote::quote;

/// Some regex pattern
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum PatternType {
    Ident(Ident),
    Plus(Box<Pattern>),
    Star(Box<Pattern>),
    Question(Box<Pattern>),
    Exclamation(Box<Pattern>),
    TerminalSet(BTreeSet<Ident>),
    Lookaheads(Box<Pattern>, BTreeSet<Ident>),
    Group(Vec<Pattern>),
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

impl Pattern {
    /// get rule for the pattern
    /// make new rule if not exists
    ///
    /// *Note*
    /// When converting `PatternArgs` to `Pattern`,
    /// if any exclamation mark `!` is present,
    /// it will be put in the innermost pattern.
    /// e.g. Pattern like `A+?!` will be converted to `A!+?`
    pub(crate) fn get_rule(
        &self,
        grammar: &mut Grammar,
        root_span_pair: (Span, Span),
    ) -> Result<Ident, ParseError> {
        if let Some(existing) = grammar.pattern_map.get(self) {
            return Ok(existing.clone());
        }
        match &self.pattern_type {
            PatternType::Ident(ident) => Ok(ident.clone()),
            PatternType::Plus(pattern) => {
                let base_rule = pattern.get_rule(grammar, root_span_pair)?;
                let typename = self.typename(grammar);

                let new_ident = Ident::new(
                    &format!("_{}_Plus{}", base_rule, grammar.pattern_map.len()),
                    root_span_pair.0,
                );
                grammar.pattern_map.insert(self.clone(), new_ident.clone());
                grammar
                    .generated_root_span
                    .insert(new_ident.clone(), root_span_pair);

                if let Some((typename, _)) = typename {
                    // typename exist, make new rule with typename Vec<base_typename>
                    // A+ -> A+ A { Ap.push(A); Ap }
                    //     | A    { vec![A] }
                    let line1 = RuleLine {
                        tokens: vec![TokenMapped {
                            token: base_rule.clone(),
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
                    };
                    let line2 = RuleLine {
                        tokens: vec![
                            TokenMapped {
                                token: new_ident.clone(),
                                mapto: Some(Ident::new("Ap", Span::call_site())),
                                begin_span: Span::call_site(),
                                end_span: Span::call_site(),
                            },
                            TokenMapped {
                                token: base_rule.clone(),
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
                    };
                    let rule_lines = RuleLines {
                        rule_lines: vec![line1, line2],
                        pretty_name: self.pretty_name.clone(),
                    };
                    grammar.rules.insert(new_ident.clone(), rule_lines);
                    grammar.rules_index.push(new_ident.clone());
                    grammar
                        .nonterm_typenames
                        .insert(new_ident.clone(), typename);
                } else {
                    // typename not exist, make new rule with typename ()
                    // A+ -> A Ap
                    //     | A
                    let line1 = RuleLine {
                        tokens: vec![TokenMapped {
                            token: base_rule.clone(),
                            mapto: None,
                            begin_span: Span::call_site(),
                            end_span: Span::call_site(),
                        }],
                        reduce_action: None,
                        separator_span: Span::call_site(),
                        lookaheads: None,
                        id: 0,
                    };
                    let line2 = RuleLine {
                        tokens: vec![
                            TokenMapped {
                                token: base_rule.clone(),
                                mapto: None,
                                begin_span: Span::call_site(),
                                end_span: Span::call_site(),
                            },
                            TokenMapped {
                                token: new_ident.clone(),
                                mapto: None,
                                begin_span: Span::call_site(),
                                end_span: Span::call_site(),
                            },
                        ],
                        reduce_action: None,
                        separator_span: Span::call_site(),
                        lookaheads: None,
                        id: 0,
                    };
                    let rule_lines = RuleLines {
                        rule_lines: vec![line1, line2],
                        pretty_name: self.pretty_name.clone(),
                    };
                    grammar.rules.insert(new_ident.clone(), rule_lines);
                    grammar.rules_index.push(new_ident.clone());
                }

                Ok(new_ident)
            }
            PatternType::Star(pattern) => {
                let plus_pattern = Pattern {
                    pattern_type: PatternType::Plus(pattern.clone()),
                    pretty_name: format!("{}+", pattern.pretty_name),
                };
                let plus_rule = plus_pattern.get_rule(grammar, root_span_pair)?;
                let base_rule = pattern.get_rule(grammar, root_span_pair)?;

                let new_ident = Ident::new(
                    &format!("_{}_Star{}", base_rule, grammar.pattern_map.len()),
                    root_span_pair.0,
                );
                grammar.pattern_map.insert(self.clone(), new_ident.clone());
                grammar
                    .generated_root_span
                    .insert(new_ident.clone(), root_span_pair);
                let typename = self.typename(grammar);

                if let Some((typename, _)) = typename {
                    // typename exist, make new rule with typename Vec<base_typename>
                    // A* -> A+ { Ap }
                    //     |    { vec![] }
                    let line1 = RuleLine {
                        tokens: vec![TokenMapped {
                            token: plus_rule.clone(),
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
                    };
                    let line2 = RuleLine {
                        tokens: vec![],
                        reduce_action: Some(quote! {
                            { vec![] }
                        }),
                        separator_span: Span::call_site(),
                        lookaheads: None,
                        id: 0,
                    };
                    let rule_lines = RuleLines {
                        rule_lines: vec![line1, line2],
                        pretty_name: self.pretty_name.clone(),
                    };
                    grammar.rules.insert(new_ident.clone(), rule_lines);
                    grammar.rules_index.push(new_ident.clone());
                    grammar
                        .nonterm_typenames
                        .insert(new_ident.clone(), typename);
                } else {
                    // typename not exist, make new rule with typename ()
                    // A* -> A+ { Ap }
                    //     |    { vec![] }
                    let line1 = RuleLine {
                        tokens: vec![TokenMapped {
                            token: plus_rule.clone(),
                            mapto: None,
                            begin_span: Span::call_site(),
                            end_span: Span::call_site(),
                        }],
                        reduce_action: None,
                        separator_span: Span::call_site(),
                        lookaheads: None,
                        id: 0,
                    };
                    let line2 = RuleLine {
                        tokens: vec![],
                        reduce_action: None,
                        separator_span: Span::call_site(),
                        lookaheads: None,
                        id: 0,
                    };
                    let rule_lines = RuleLines {
                        rule_lines: vec![line1, line2],
                        pretty_name: self.pretty_name.clone(),
                    };
                    grammar.rules.insert(new_ident.clone(), rule_lines);
                    grammar.rules_index.push(new_ident.clone());
                }

                Ok(new_ident)
            }
            PatternType::Question(pattern) => {
                let base_rule = pattern.get_rule(grammar, root_span_pair)?;

                let new_ident = Ident::new(
                    &format!("_{}_Option{}", base_rule, grammar.pattern_map.len()),
                    root_span_pair.0,
                );
                grammar.pattern_map.insert(self.clone(), new_ident.clone());
                grammar
                    .generated_root_span
                    .insert(new_ident.clone(), root_span_pair);

                let typename = self.typename(grammar);

                if let Some((typename, _)) = typename {
                    // typename exist, make new rule with typename Option<base_typename>
                    // A? -> A { Some(A) }
                    //     |   { None }
                    let line1 = RuleLine {
                        tokens: vec![TokenMapped {
                            token: base_rule.clone(),
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
                    };
                    let line2 = RuleLine {
                        tokens: vec![],
                        reduce_action: Some(quote! {
                            { None }
                        }),
                        separator_span: Span::call_site(),
                        lookaheads: None,
                        id: 0,
                    };
                    let rule_lines = RuleLines {
                        rule_lines: vec![line1, line2],
                        pretty_name: self.pretty_name.clone(),
                    };
                    grammar.rules.insert(new_ident.clone(), rule_lines);
                    grammar.rules_index.push(new_ident.clone());
                    grammar
                        .nonterm_typenames
                        .insert(new_ident.clone(), typename);
                } else {
                    // typename not exist, make new rule with typename ()
                    // A? -> A { Some(A) }
                    //     |   { None }
                    let line1 = RuleLine {
                        tokens: vec![TokenMapped {
                            token: base_rule.clone(),
                            mapto: None,
                            begin_span: Span::call_site(),
                            end_span: Span::call_site(),
                        }],
                        reduce_action: None,
                        separator_span: Span::call_site(),
                        lookaheads: None,
                        id: 0,
                    };
                    let line2 = RuleLine {
                        tokens: vec![],
                        reduce_action: None,
                        separator_span: Span::call_site(),
                        lookaheads: None,
                        id: 0,
                    };
                    let rule_lines = RuleLines {
                        rule_lines: vec![line1, line2],
                        pretty_name: self.pretty_name.clone(),
                    };
                    grammar.rules.insert(new_ident.clone(), rule_lines);
                    grammar.rules_index.push(new_ident.clone());
                }

                Ok(new_ident)
            }

            PatternType::Exclamation(pattern) => pattern.get_rule(grammar, root_span_pair),
            PatternType::TerminalSet(terminal_set) => {
                let new_ident = Ident::new(
                    &format!("_TerminalSet{}", grammar.pattern_map.len()),
                    root_span_pair.0,
                );
                grammar.pattern_map.insert(self.clone(), new_ident.clone());
                grammar
                    .generated_root_span
                    .insert(new_ident.clone(), root_span_pair);

                let mut rule_lines = Vec::new();
                for terminal in terminal_set.iter() {
                    let rule = RuleLine {
                        tokens: vec![TokenMapped {
                            token: terminal.clone(),
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
                    };
                    rule_lines.push(rule);
                }
                grammar.rules.insert(
                    new_ident.clone(),
                    RuleLines {
                        rule_lines,
                        pretty_name: self.pretty_name.clone(),
                    },
                );
                grammar.rules_index.push(new_ident.clone());
                grammar
                    .nonterm_typenames
                    .insert(new_ident.clone(), grammar.token_typename.clone());
                Ok(new_ident)
            }
            PatternType::Lookaheads(pattern, lookaheads) => {
                let base_rule = pattern.get_rule(grammar, root_span_pair)?;
                let new_ident = Ident::new(
                    &format!("_{}_Lookaheads{}", base_rule, grammar.pattern_map.len()),
                    root_span_pair.0,
                );
                grammar.pattern_map.insert(self.clone(), new_ident.clone());
                grammar
                    .generated_root_span
                    .insert(new_ident.clone(), root_span_pair);
                let typename = self.typename(grammar);

                if let Some((typename, _)) = typename {
                    let mut rule_lines = Vec::new();
                    let rule = RuleLine {
                        tokens: vec![TokenMapped {
                            token: base_rule,
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
                    };
                    rule_lines.push(rule);

                    grammar.rules.insert(
                        new_ident.clone(),
                        RuleLines {
                            rule_lines,
                            pretty_name: self.pretty_name.clone(),
                        },
                    );
                    grammar.rules_index.push(new_ident.clone());
                    grammar
                        .nonterm_typenames
                        .insert(new_ident.clone(), typename);
                } else {
                    let mut rule_lines = Vec::new();
                    let rule = RuleLine {
                        tokens: vec![TokenMapped {
                            token: base_rule,
                            mapto: None,
                            begin_span: Span::call_site(),
                            end_span: Span::call_site(),
                        }],
                        reduce_action: None,
                        separator_span: Span::call_site(),
                        lookaheads: Some(lookaheads.clone()),
                        id: 0,
                    };
                    rule_lines.push(rule);

                    grammar.rules.insert(
                        new_ident.clone(),
                        RuleLines {
                            rule_lines,
                            pretty_name: self.pretty_name.clone(),
                        },
                    );
                    grammar.rules_index.push(new_ident.clone());
                }

                Ok(new_ident)
            }

            PatternType::Group(group) => {
                // Consider parenthesis-ed group of patterns
                // ( A B C D ... )
                // if there are no pattern holding a value, then the RuleType of the group is None
                // if there is only one pattern holding a value, T, then the RuleType of the group is T
                // otherwise, the RuleType of the group is (T1, T2, T3, ...) where T1 T2 T3 ... are the RuleType of the patterns holding a value

                let new_ident = Ident::new(
                    &format!("_Group{}", grammar.pattern_map.len()),
                    root_span_pair.0,
                );
                grammar.pattern_map.insert(self.clone(), new_ident.clone());
                grammar
                    .generated_root_span
                    .insert(new_ident.clone(), root_span_pair);

                let mut vars = Vec::with_capacity(group.len());
                let mut typenames = Vec::with_capacity(group.len());
                let mut tokens = Vec::with_capacity(group.len());

                for (idx, child) in group.iter().enumerate() {
                    let child_rule = child.get_rule(grammar, root_span_pair)?;
                    if let Some((child_typename, _)) = child.typename(grammar) {
                        let var_name = Ident::new(format!("v{}", idx).as_str(), Span::call_site());
                        vars.push(var_name.clone());
                        typenames.push(child_typename);
                        tokens.push(TokenMapped {
                            token: child_rule,
                            mapto: Some(var_name),
                            begin_span: Span::call_site(),
                            end_span: Span::call_site(),
                        });
                    } else {
                        tokens.push(TokenMapped {
                            token: child_rule,
                            mapto: None,
                            begin_span: Span::call_site(),
                            end_span: Span::call_site(),
                        });
                    }
                }
                let mut rule_lines = Vec::new();
                let (typename, rule) = match typenames.len() {
                    0 => (
                        None,
                        RuleLine {
                            tokens,
                            reduce_action: None,
                            separator_span: Span::call_site(),
                            lookaheads: None,
                            id: 0,
                        },
                    ),

                    1 => {
                        let v0 = &vars[0];
                        (
                            typenames.into_iter().next(),
                            RuleLine {
                                tokens,
                                reduce_action: Some(quote! {
                                    {#v0}
                                }),
                                separator_span: Span::call_site(),
                                lookaheads: None,
                                id: 0,
                            },
                        )
                    }
                    _ => {
                        let mut reduce_action = TokenStream::new();
                        for var in vars.iter() {
                            reduce_action.extend(quote! { #var, });
                        }
                        let mut typename = TokenStream::new();
                        for child_type in typenames {
                            typename.extend(quote! { #child_type, });
                        }
                        (
                            Some(quote! { (#typename) }),
                            RuleLine {
                                tokens,
                                reduce_action: Some(quote! {
                                    { (#reduce_action) }
                                }),
                                separator_span: Span::call_site(),
                                lookaheads: None,
                                id: 0,
                            },
                        )
                    }
                };
                rule_lines.push(rule);

                grammar.rules.insert(
                    new_ident.clone(),
                    RuleLines {
                        rule_lines,
                        pretty_name: self.pretty_name.clone(),
                    },
                );
                grammar.rules_index.push(new_ident.clone());

                if let Some(typename) = typename {
                    grammar
                        .nonterm_typenames
                        .insert(new_ident.clone(), typename);
                }

                Ok(new_ident)
            }
        }
    }

    pub fn typename(&self, grammar: &Grammar) -> Option<(TokenStream, Ident)> {
        match &self.pattern_type {
            PatternType::Ident(ident) => grammar
                .get_typename(ident)
                .map(|typename| (typename.clone(), ident.clone())),
            PatternType::Plus(pattern) => pattern
                .typename(grammar)
                .map(|(typename, ident)| (quote! { Vec<#typename> }, ident)),
            PatternType::Star(pattern) => pattern
                .typename(grammar)
                .map(|(typename, ident)| (quote! { Vec<#typename> }, ident)),
            PatternType::Question(pattern) => pattern
                .typename(grammar)
                .map(|(typename, ident)| (quote! { Option<#typename> }, ident)),
            PatternType::Exclamation(_) => None,
            PatternType::TerminalSet(_) => Some((
                grammar.token_typename.clone(),
                Ident::new("__rustylr_term", Span::call_site()),
            )),
            PatternType::Lookaheads(pattern, _) => pattern.typename(grammar),
            PatternType::Group(group) => {
                let mut child_types = Vec::with_capacity(group.len());
                for child in group.iter() {
                    if let Some(typename) = child.typename(grammar) {
                        child_types.push(typename);
                    }
                }

                match child_types.len() {
                    0 => None,                           // none of the children have typename
                    1 => child_types.into_iter().next(), // single child with typename
                    _ => {
                        // multiple children with typename
                        let mut typename = TokenStream::new();
                        for (child_type, _) in child_types {
                            typename.extend(quote! { #child_type, });
                        }
                        Some((
                            quote! { (#typename) },
                            Ident::new("__rustylr_group", Span::call_site()),
                        ))
                    }
                }
            }
        }
    }
}
