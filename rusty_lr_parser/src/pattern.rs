use crate::utils;

use super::error::ParseError;
use super::grammar::Grammar;
use super::rule::{RuleLine, RuleLines};
use super::token::TokenMapped;

use std::collections::BTreeSet;

use proc_macro2::Ident;
use proc_macro2::Span;

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
}

impl Pattern {
    /// get rule for the pattern
    /// make new rule if not exists
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
                let new_ident = Ident::new(
                    &format!(
                        "{}{}",
                        utils::AUTO_GENERATED_RULE_PREFIX,
                        grammar.pattern_map.len()
                    ),
                    Span::call_site(),
                );
                grammar.pattern_map.insert(self.clone(), new_ident.clone());
                grammar
                    .generated_root_span
                    .insert(new_ident.clone(), root_span_pair);

                let base_rule = pattern.get_rule(grammar, root_span_pair)?;
                let base_typename = grammar.get_typename(&base_rule).cloned();

                if let Some(base_typename) = base_typename {
                    // typename exist, make new rule with typename Vec<base_typename>
                    // A+ -> A+ A { Ap.push(A); Ap }
                    //     | A    { vec![A] }
                    let line1 = RuleLine {
                        tokens: vec![TokenMapped {
                            token: base_rule.clone(),
                            mapto: Ident::new("A", Span::call_site()),
                            begin_span: Span::call_site(),
                            end_span: Span::call_site(),
                        }],
                        reduce_action: Some(quote! {
                            { vec![A] }
                        }),
                        separator_span: Span::call_site(),
                    };
                    let line2 = RuleLine {
                        tokens: vec![
                            TokenMapped {
                                token: new_ident.clone(),
                                mapto: Ident::new("Ap", Span::call_site()),
                                begin_span: Span::call_site(),
                                end_span: Span::call_site(),
                            },
                            TokenMapped {
                                token: base_rule.clone(),
                                mapto: Ident::new("A", Span::call_site()),
                                begin_span: Span::call_site(),
                                end_span: Span::call_site(),
                            },
                        ],
                        reduce_action: Some(quote! {
                            { Ap.push(A); Ap }
                        }),
                        separator_span: Span::call_site(),
                    };
                    let rule_lines = RuleLines {
                        rule_lines: vec![line1, line2],
                    };
                    grammar.rules.insert(new_ident.clone(), rule_lines);
                    grammar.rules_index.push(new_ident.clone());
                    grammar
                        .nonterm_typenames
                        .insert(new_ident.clone(), quote! { Vec<#base_typename> });
                } else {
                    // typename not exist, make new rule with typename ()
                    // A+ -> A Ap
                    //     | A
                    let line1 = RuleLine {
                        tokens: vec![TokenMapped {
                            token: base_rule.clone(),
                            mapto: Ident::new("A", Span::call_site()),
                            begin_span: Span::call_site(),
                            end_span: Span::call_site(),
                        }],
                        reduce_action: None,
                        separator_span: Span::call_site(),
                    };
                    let line2 = RuleLine {
                        tokens: vec![
                            TokenMapped {
                                token: base_rule.clone(),
                                mapto: Ident::new("A", Span::call_site()),
                                begin_span: Span::call_site(),
                                end_span: Span::call_site(),
                            },
                            TokenMapped {
                                token: new_ident.clone(),
                                mapto: Ident::new("A", Span::call_site()),
                                begin_span: Span::call_site(),
                                end_span: Span::call_site(),
                            },
                        ],
                        reduce_action: None,
                        separator_span: Span::call_site(),
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
                let new_ident = Ident::new(
                    &format!(
                        "{}{}",
                        utils::AUTO_GENERATED_RULE_PREFIX,
                        grammar.pattern_map.len()
                    ),
                    Span::call_site(),
                );
                grammar.pattern_map.insert(self.clone(), new_ident.clone());
                grammar
                    .generated_root_span
                    .insert(new_ident.clone(), root_span_pair);

                let plus_rule = Pattern::Plus(pattern.clone()).get_rule(grammar, root_span_pair)?;

                let base_rule = pattern.get_rule(grammar, root_span_pair)?;
                let base_typename = grammar.get_typename(&base_rule).cloned();

                if let Some(base_typename) = base_typename {
                    // typename exist, make new rule with typename Vec<base_typename>
                    // A* -> A+ { Ap }
                    //     |    { vec![] }
                    let line1 = RuleLine {
                        tokens: vec![TokenMapped {
                            token: plus_rule.clone(),
                            mapto: Ident::new("Ap", Span::call_site()),
                            begin_span: Span::call_site(),
                            end_span: Span::call_site(),
                        }],
                        reduce_action: Some(quote! {
                            { Ap }
                        }),
                        separator_span: Span::call_site(),
                    };
                    let line2 = RuleLine {
                        tokens: vec![],
                        reduce_action: Some(quote! {
                            { vec![] }
                        }),
                        separator_span: Span::call_site(),
                    };
                    let rule_lines = RuleLines {
                        rule_lines: vec![line1, line2],
                    };
                    grammar.rules.insert(new_ident.clone(), rule_lines);
                    grammar.rules_index.push(new_ident.clone());
                    grammar
                        .nonterm_typenames
                        .insert(new_ident.clone(), quote! { Vec<#base_typename> });
                } else {
                    // typename not exist, make new rule with typename ()
                    // A* -> A+ { Ap }
                    //     |    { vec![] }
                    let line1 = RuleLine {
                        tokens: vec![TokenMapped {
                            token: plus_rule.clone(),
                            mapto: Ident::new("A", Span::call_site()),
                            begin_span: Span::call_site(),
                            end_span: Span::call_site(),
                        }],
                        reduce_action: None,
                        separator_span: Span::call_site(),
                    };
                    let line2 = RuleLine {
                        tokens: vec![],
                        reduce_action: None,
                        separator_span: Span::call_site(),
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
                let new_ident = Ident::new(
                    &format!(
                        "{}{}",
                        utils::AUTO_GENERATED_RULE_PREFIX,
                        grammar.pattern_map.len()
                    ),
                    Span::call_site(),
                );
                grammar.pattern_map.insert(self.clone(), new_ident.clone());
                grammar
                    .generated_root_span
                    .insert(new_ident.clone(), root_span_pair);

                let base_rule = pattern.get_rule(grammar, root_span_pair)?;
                let base_typename = grammar.get_typename(&base_rule).cloned();

                if let Some(base_typename) = base_typename {
                    // typename exist, make new rule with typename Option<base_typename>
                    // A? -> A { Some(A) }
                    //     |   { None }
                    let line1 = RuleLine {
                        tokens: vec![TokenMapped {
                            token: base_rule.clone(),
                            mapto: Ident::new("A", Span::call_site()),
                            begin_span: Span::call_site(),
                            end_span: Span::call_site(),
                        }],
                        reduce_action: Some(quote! {
                            { Some(A) }
                        }),
                        separator_span: Span::call_site(),
                    };
                    let line2 = RuleLine {
                        tokens: vec![],
                        reduce_action: Some(quote! {
                            { None }
                        }),
                        separator_span: Span::call_site(),
                    };
                    let rule_lines = RuleLines {
                        rule_lines: vec![line1, line2],
                    };
                    grammar.rules.insert(new_ident.clone(), rule_lines);
                    grammar.rules_index.push(new_ident.clone());
                    grammar
                        .nonterm_typenames
                        .insert(new_ident.clone(), quote! { Option<#base_typename> });
                } else {
                    // typename not exist, make new rule with typename ()
                    // A? -> A { Some(A) }
                    //     |   { None }
                    let line1 = RuleLine {
                        tokens: vec![TokenMapped {
                            token: base_rule.clone(),
                            mapto: Ident::new("A", Span::call_site()),
                            begin_span: Span::call_site(),
                            end_span: Span::call_site(),
                        }],
                        reduce_action: None,
                        separator_span: Span::call_site(),
                    };
                    let line2 = RuleLine {
                        tokens: vec![],
                        reduce_action: None,
                        separator_span: Span::call_site(),
                    };
                    let rule_lines = RuleLines {
                        rule_lines: vec![line1, line2],
                    };
                    grammar.rules.insert(new_ident.clone(), rule_lines);
                    grammar.rules_index.push(new_ident.clone());
                }

                Ok(new_ident)
            }

            Pattern::Exclamation(pattern) => {
                // if base rule does not have typename, just use base rule
                let base_rule = pattern.get_rule(grammar, root_span_pair)?;
                if grammar.get_typename(&base_rule).is_none() {
                    grammar.pattern_map.insert(self.clone(), base_rule.clone());
                    return Ok(base_rule);
                }

                // else, make new rule with typename ()
                let new_ident = Ident::new(
                    &format!(
                        "{}{}",
                        utils::AUTO_GENERATED_RULE_PREFIX,
                        grammar.pattern_map.len()
                    ),
                    Span::call_site(),
                );
                grammar.pattern_map.insert(self.clone(), new_ident.clone());
                grammar
                    .generated_root_span
                    .insert(new_ident.clone(), root_span_pair);

                let line1 = RuleLine {
                    tokens: vec![TokenMapped {
                        token: base_rule.clone(),
                        mapto: Ident::new("A", Span::call_site()),
                        begin_span: Span::call_site(),
                        end_span: Span::call_site(),
                    }],
                    reduce_action: None,
                    separator_span: Span::call_site(),
                };
                let rule_lines = RuleLines {
                    rule_lines: vec![line1],
                };
                grammar.rules.insert(new_ident.clone(), rule_lines);
                grammar.rules_index.push(new_ident.clone());

                Ok(new_ident)
            }
            Pattern::TerminalSet(terminal_set) => {
                let new_ident = Ident::new(
                    &format!(
                        "{}{}",
                        utils::AUTO_GENERATED_RULE_PREFIX,
                        grammar.pattern_map.len()
                    ),
                    Span::call_site(),
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
                            mapto: Ident::new("term", Span::call_site()),
                            begin_span: Span::call_site(),
                            end_span: Span::call_site(),
                        }],
                        reduce_action: Some(quote! {
                            term
                        }),
                        separator_span: Span::call_site(),
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
        }
    }

    /// Get ident for default mapto
    ///
    /// This is used for mapped variable name
    /// ex) A: plus* <ReduceAction> ;
    /// zero-or-more plus can be accessible by variable name `plus`, so this function returns `plus`
    pub(crate) fn base_ident(&self) -> Ident {
        match self {
            Pattern::Ident(ident) => ident.clone(),
            Pattern::Plus(pattern) => pattern.base_ident(),
            Pattern::Star(pattern) => pattern.base_ident(),
            Pattern::Question(pattern) => pattern.base_ident(),
            Pattern::Exclamation(pattern) => pattern.base_ident(),
            Pattern::TerminalSet(_) => Ident::new("_rustylr_deafult_ident", Span::call_site()),
        }
    }
}
