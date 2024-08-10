use super::error::ParseError;
use super::grammar::Grammar;
use super::rule::{RuleLine, RuleLines};
use super::token::TokenMapped;

use std::collections::BTreeSet;

use proc_macro2::Ident;
use proc_macro2::Span;

use quote::quote;

/// Some regex pattern
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Pattern {
    Ident(Ident),
    Plus(Box<Pattern>),
    Star(Box<Pattern>),
    Question(Box<Pattern>),
    TerminalSet(BTreeSet<Ident>),
}

impl Pattern {
    /// get rule for the pattern
    /// make new rule if not exists
    pub(crate) fn get_rule(&self, grammar: &mut Grammar) -> Result<Ident, ParseError> {
        if let Some(existing) = grammar.pattern_map.get(self) {
            return Ok(existing.clone());
        }
        match self {
            Pattern::Ident(ident) => Ok(ident.clone()),
            Pattern::Plus(pattern) => {
                let new_ident = Ident::new(
                    &format!("_RustyLRGenerated{}", grammar.pattern_map.len()),
                    Span::call_site(),
                );
                grammar.pattern_map.insert(self.clone(), new_ident.clone());

                let base_rule = pattern.get_rule(grammar)?;
                let base_typename = grammar.get_typename(&base_rule).cloned();

                if let Some(base_typename) = base_typename {
                    // typename exist, make new rule with typename Vec<base_typename>
                    // A+ -> A+ A { Ap.push(A); Ap }
                    //     | A    { vec![A] }
                    let line1 = RuleLine {
                        tokens: vec![TokenMapped {
                            token: base_rule.clone(),
                            mapto: Ident::new("A", Span::call_site()),
                        }],
                        reduce_action: Some(quote! {
                            { vec![A] }
                        }),
                    };
                    let line2 = RuleLine {
                        tokens: vec![
                            TokenMapped {
                                token: new_ident.clone(),
                                mapto: Ident::new("Ap", Span::call_site()),
                            },
                            TokenMapped {
                                token: base_rule.clone(),
                                mapto: Ident::new("A", Span::call_site()),
                            },
                        ],
                        reduce_action: Some(quote! {
                            { Ap.push(A); Ap }
                        }),
                    };
                    let rule_lines = RuleLines {
                        rule_lines: vec![line1, line2],
                    };
                    grammar.rules.insert(new_ident.clone(), rule_lines);
                    grammar
                        .nonterm_typenames
                        .insert(new_ident.clone(), Some(quote! { Vec<#base_typename> }));
                } else {
                    // typename not exist, make new rule with typename ()
                    // A+ -> A Ap
                    //     | A
                    let line1 = RuleLine {
                        tokens: vec![TokenMapped {
                            token: base_rule.clone(),
                            mapto: Ident::new("A", Span::call_site()),
                        }],
                        reduce_action: None,
                    };
                    let line2 = RuleLine {
                        tokens: vec![
                            TokenMapped {
                                token: base_rule.clone(),
                                mapto: Ident::new("A", Span::call_site()),
                            },
                            TokenMapped {
                                token: new_ident.clone(),
                                mapto: Ident::new("A", Span::call_site()),
                            },
                        ],
                        reduce_action: None,
                    };
                    let rule_lines = RuleLines {
                        rule_lines: vec![line1, line2],
                    };
                    grammar.rules.insert(new_ident.clone(), rule_lines);
                    grammar.nonterm_typenames.insert(new_ident.clone(), None);
                }

                Ok(new_ident)
            }
            Pattern::Star(pattern) => {
                let new_ident = Ident::new(
                    &format!("_RustyLRGenerated{}", grammar.pattern_map.len()),
                    Span::call_site(),
                );
                grammar.pattern_map.insert(self.clone(), new_ident.clone());

                let plus_rule = Pattern::Plus(pattern.clone()).get_rule(grammar)?;

                let base_rule = pattern.get_rule(grammar)?;
                let base_typename = grammar.get_typename(&base_rule).cloned();

                if let Some(base_typename) = base_typename {
                    // typename exist, make new rule with typename Vec<base_typename>
                    // A* -> A+ { Ap }
                    //     |    { vec![] }
                    let line1 = RuleLine {
                        tokens: vec![TokenMapped {
                            token: plus_rule.clone(),
                            mapto: Ident::new("Ap", Span::call_site()),
                        }],
                        reduce_action: Some(quote! {
                            { Ap }
                        }),
                    };
                    let line2 = RuleLine {
                        tokens: vec![],
                        reduce_action: Some(quote! {
                            { vec![] }
                        }),
                    };
                    let rule_lines = RuleLines {
                        rule_lines: vec![line1, line2],
                    };
                    grammar.rules.insert(new_ident.clone(), rule_lines);
                    grammar
                        .nonterm_typenames
                        .insert(new_ident.clone(), Some(quote! { Vec<#base_typename> }));
                } else {
                    // typename not exist, make new rule with typename ()
                    // A* -> A+ { Ap }
                    //     |    { vec![] }
                    let line1 = RuleLine {
                        tokens: vec![TokenMapped {
                            token: plus_rule.clone(),
                            mapto: Ident::new("A", Span::call_site()),
                        }],
                        reduce_action: None,
                    };
                    let line2 = RuleLine {
                        tokens: vec![],
                        reduce_action: None,
                    };
                    let rule_lines = RuleLines {
                        rule_lines: vec![line1, line2],
                    };
                    grammar.rules.insert(new_ident.clone(), rule_lines);
                    grammar.nonterm_typenames.insert(new_ident.clone(), None);
                }

                Ok(new_ident)
            }
            Pattern::Question(pattern) => {
                let new_ident = Ident::new(
                    &format!("_RustyLRGenerated{}", grammar.pattern_map.len()),
                    Span::call_site(),
                );
                grammar.pattern_map.insert(self.clone(), new_ident.clone());

                let base_rule = pattern.get_rule(grammar)?;
                let base_typename = grammar.get_typename(&base_rule).cloned();

                if let Some(base_typename) = base_typename {
                    // typename exist, make new rule with typename Option<base_typename>
                    // A? -> A { Some(A) }
                    //     |   { None }
                    let line1 = RuleLine {
                        tokens: vec![TokenMapped {
                            token: base_rule.clone(),
                            mapto: Ident::new("A", Span::call_site()),
                        }],
                        reduce_action: Some(quote! {
                            { Some(A) }
                        }),
                    };
                    let line2 = RuleLine {
                        tokens: vec![],
                        reduce_action: Some(quote! {
                            { None }
                        }),
                    };
                    let rule_lines = RuleLines {
                        rule_lines: vec![line1, line2],
                    };
                    grammar.rules.insert(new_ident.clone(), rule_lines);
                    grammar
                        .nonterm_typenames
                        .insert(new_ident.clone(), Some(quote! { Option<#base_typename> }));
                } else {
                    // typename not exist, make new rule with typename ()
                    // A? -> A { Some(A) }
                    //     |   { None }
                    let line1 = RuleLine {
                        tokens: vec![TokenMapped {
                            token: base_rule.clone(),
                            mapto: Ident::new("A", Span::call_site()),
                        }],
                        reduce_action: None,
                    };
                    let line2 = RuleLine {
                        tokens: vec![],
                        reduce_action: None,
                    };
                    let rule_lines = RuleLines {
                        rule_lines: vec![line1, line2],
                    };
                    grammar.rules.insert(new_ident.clone(), rule_lines);
                    grammar.nonterm_typenames.insert(new_ident.clone(), None);
                }

                Ok(new_ident)
            }
            Pattern::TerminalSet(terminal_set) => {
                let new_ident = Ident::new(
                    &format!("_RustyLRGenerated{}", grammar.pattern_map.len()),
                    Span::call_site(),
                );
                grammar.pattern_map.insert(self.clone(), new_ident.clone());

                let mut rule_lines = Vec::new();
                for terminal in terminal_set.iter() {
                    let rule = RuleLine {
                        tokens: vec![TokenMapped {
                            token: terminal.clone(),
                            mapto: Ident::new("term", Span::call_site()),
                        }],
                        reduce_action: Some(quote! {
                            term
                        }),
                    };
                    rule_lines.push(rule);
                }
                grammar
                    .rules
                    .insert(new_ident.clone(), RuleLines { rule_lines });
                grammar
                    .nonterm_typenames
                    .insert(new_ident.clone(), Some(grammar.token_typename.clone()));
                Ok(new_ident)
            }
        }
    }

    /// get ident for default mapto
    pub(crate) fn base_ident(&self) -> Ident {
        match self {
            Pattern::Ident(ident) => ident.clone(),
            Pattern::Plus(pattern) => pattern.base_ident(),
            Pattern::Star(pattern) => pattern.base_ident(),
            Pattern::Question(pattern) => pattern.base_ident(),
            Pattern::TerminalSet(_) => Ident::new("_rustylr_deafult_ident", Span::call_site()),
        }
    }
}
