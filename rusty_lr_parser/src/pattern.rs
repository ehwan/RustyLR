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
pub enum Pattern {
    Ident(Ident),
    Plus(Box<Pattern>),
    Star(Box<Pattern>),
    Question(Box<Pattern>),
    Exclamation(Box<Pattern>),
    TerminalSet(BTreeSet<Ident>),
    Lookaheads(Box<Pattern>, BTreeSet<Ident>),
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
    pub(crate) fn get_rule(
        &self,
        grammar: &mut Grammar,
        root_span_pair: (Span, Span),
    ) -> Result<Ident, ParseError> {
        if let Some(existing) = grammar.pattern_map.get(self) {
            return Ok(existing.clone());
        }
        match self {
            Pattern::Ident(ident) => Ok(ident.clone()),
            Pattern::Plus(pattern) => {
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

                if let Some(typename) = typename {
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
                    };
                    let rule_lines = RuleLines {
                        rule_lines: vec![line1, line2],
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
                    };
                    let rule_lines = RuleLines {
                        rule_lines: vec![line1, line2],
                    };
                    grammar.rules.insert(new_ident.clone(), rule_lines);
                    grammar.rules_index.push(new_ident.clone());
                }

                Ok(new_ident)
            }
            Pattern::Star(pattern) => {
                let plus_pattern = Pattern::Plus(pattern.clone());
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

                if let Some(typename) = typename {
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
                    };
                    let line2 = RuleLine {
                        tokens: vec![],
                        reduce_action: Some(quote! {
                            { vec![] }
                        }),
                        separator_span: Span::call_site(),
                        lookaheads: None,
                    };
                    let rule_lines = RuleLines {
                        rule_lines: vec![line1, line2],
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
                    };
                    let line2 = RuleLine {
                        tokens: vec![],
                        reduce_action: None,
                        separator_span: Span::call_site(),
                        lookaheads: None,
                    };
                    let rule_lines = RuleLines {
                        rule_lines: vec![line1, line2],
                    };
                    grammar.rules.insert(new_ident.clone(), rule_lines);
                    grammar.rules_index.push(new_ident.clone());
                }

                Ok(new_ident)
            }
            Pattern::Question(pattern) => {
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

                if let Some(typename) = typename {
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
                    };
                    let line2 = RuleLine {
                        tokens: vec![],
                        reduce_action: Some(quote! {
                            { None }
                        }),
                        separator_span: Span::call_site(),
                        lookaheads: None,
                    };
                    let rule_lines = RuleLines {
                        rule_lines: vec![line1, line2],
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
                    };
                    let line2 = RuleLine {
                        tokens: vec![],
                        reduce_action: None,
                        separator_span: Span::call_site(),
                        lookaheads: None,
                    };
                    let rule_lines = RuleLines {
                        rule_lines: vec![line1, line2],
                    };
                    grammar.rules.insert(new_ident.clone(), rule_lines);
                    grammar.rules_index.push(new_ident.clone());
                }

                Ok(new_ident)
            }

            Pattern::Exclamation(pattern) => pattern.get_rule(grammar, root_span_pair),
            Pattern::TerminalSet(terminal_set) => {
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
                    };
                    rule_lines.push(rule);
                }
                grammar
                    .rules
                    .insert(new_ident.clone(), RuleLines { rule_lines });
                grammar.rules_index.push(new_ident.clone());
                grammar
                    .nonterm_typenames
                    .insert(new_ident.clone(), grammar.token_typename.clone());
                Ok(new_ident)
            }
            Pattern::Lookaheads(pattern, lookaheads) => {
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

                if let Some(typename) = typename {
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
                    };
                    rule_lines.push(rule);

                    grammar
                        .rules
                        .insert(new_ident.clone(), RuleLines { rule_lines });
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
                    };
                    rule_lines.push(rule);

                    grammar
                        .rules
                        .insert(new_ident.clone(), RuleLines { rule_lines });
                    grammar.rules_index.push(new_ident.clone());
                }

                Ok(new_ident)
            }
        }
    }

    pub fn typename(&self, grammar: &Grammar) -> Option<TokenStream> {
        match self {
            Pattern::Ident(ident) => grammar.get_typename(ident).cloned(),
            Pattern::Plus(pattern) => pattern
                .typename(grammar)
                .map(|typename| quote! { Vec<#typename> }),
            Pattern::Star(pattern) => pattern
                .typename(grammar)
                .map(|typename| quote! { Vec<#typename> }),
            Pattern::Question(pattern) => pattern.typename(grammar).map(|typename| {
                quote! { Option<#typename> }
            }),
            Pattern::Exclamation(_) => None,
            Pattern::TerminalSet(_) => Some(grammar.token_typename.clone()),
            Pattern::Lookaheads(pattern, _) => pattern.typename(grammar),
        }
    }

    /// if explicit mapto is not defined, map to default variable name
    pub fn map_to(&self) -> Option<Ident> {
        match self {
            Pattern::Ident(ident) => Some(ident.clone()),
            Pattern::Plus(pattern) => pattern.map_to(),
            Pattern::Star(pattern) => pattern.map_to(),
            Pattern::Question(pattern) => pattern.map_to(),
            Pattern::Exclamation(_) => None,
            Pattern::TerminalSet(_) => Some(Ident::new("__rustylr_term", Span::call_site())),
            Pattern::Lookaheads(pattern, _) => pattern.map_to(),
        }
    }
}
