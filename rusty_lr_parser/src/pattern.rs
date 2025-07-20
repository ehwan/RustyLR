use crate::terminal_info::TerminalName;

use rusty_lr_core::TerminalSymbol;
use rusty_lr_core::Token;

use super::error::ParseError;
use super::grammar::Grammar;
use super::nonterminal_info::{NonTerminalInfo, Rule};
use super::token::TokenMapped;

use std::collections::BTreeSet;

use proc_macro2::Ident;
use proc_macro2::Span;
use proc_macro2::TokenStream;

use quote::format_ident;
use quote::{quote, ToTokens};

/// Some regex pattern
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum PatternInternal {
    Ident(Ident),
    Plus(Box<Pattern>),
    Star(Box<Pattern>),
    Question(Box<Pattern>),
    Exclamation(Box<Pattern>),
    TerminalSet(bool, BTreeSet<usize>),
    Lookaheads(Box<Pattern>, bool, BTreeSet<usize>),
    Group(Vec<Pattern>),
    Literal(syn::Lit),
    Sep(Box<Pattern>, Box<Pattern>, bool),
}

#[derive(Debug, Clone)]
pub struct Pattern {
    pub internal: PatternInternal,
    /// for debug and generating pretty message
    /// A_Star -> A*
    /// A_Exclamation -> A!
    /// A_Question -> A?
    /// ...
    pub pretty_name: String,
}
impl std::hash::Hash for Pattern {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.internal.hash(state);
    }
}
impl std::cmp::PartialEq for Pattern {
    fn eq(&self, other: &Self) -> bool {
        self.internal == other.internal
    }
}
impl std::cmp::Eq for Pattern {}

#[derive(Clone)]
pub struct PatternToToken {
    /// only for internal usage; generating name like A_star, A_plus, A_question
    name: Ident,
    /// actual token for the pattern
    pub token: Token<TerminalSymbol<usize>, usize>,
    /// ruletype for this pattern
    pub ruletype: Option<TokenStream>,
    /// implicit mapto derived from its parent pattern (e.g. 'A' from A+, 'A' from A*!)
    pub mapto: Option<Ident>,
}

impl Pattern {
    /// Cache wrapper for `to_token_impl`.
    pub(crate) fn to_token(
        &self,
        grammar: &mut Grammar,
        pattern_cache: &mut rusty_lr_core::hash::HashMap<Pattern, PatternToToken>,
        root_span_pair: (Span, Span),
    ) -> Result<PatternToToken, ParseError> {
        if let Some(existing) = pattern_cache.get(self) {
            return Ok(existing.clone());
        }
        let ret = self.to_token_impl(grammar, pattern_cache, root_span_pair)?;
        pattern_cache.insert(self.clone(), ret.clone());
        Ok(ret)
    }
    /// Converts the `Pattern` to a `Token`.
    /// This generates a new non-terminal if needed,
    /// and returns a `PatternToToken` which contains the token's ruletype, variable name, and the token itself.
    ///
    /// *Note*
    /// When converting `PatternArgs` to `Pattern`,
    /// if any exclamation mark `!` is present,
    /// it will be put in the innermost pattern.
    /// e.g. Pattern like `A+?!` will be converted to `A!+?`
    fn to_token_impl(
        &self,
        grammar: &mut Grammar,
        pattern_cache: &mut rusty_lr_core::hash::HashMap<Pattern, PatternToToken>,
        root_span_pair: (Span, Span),
    ) -> Result<PatternToToken, ParseError> {
        use crate::nonterminal_info::ReduceAction;
        use rusty_lr_core::nonterminal::NonTerminalType;

        match &self.internal {
            PatternInternal::Ident(ident) => {
                if ident == crate::utils::ERROR_NAME {
                    // special case for error token
                    return Ok(PatternToToken {
                        name: ident.clone(),
                        token: Token::Term(TerminalSymbol::Error),
                        ruletype: None,
                        mapto: Some(ident.clone()),
                    });
                }

                // check if this ident is either name of terminal or nonterminal
                if let Some(term_idx) = grammar
                    .terminals_index
                    .get(&TerminalName::Ident(ident.clone()))
                {
                    // terminal
                    Ok(PatternToToken {
                        name: ident.clone(),
                        token: Token::Term(TerminalSymbol::Term(*term_idx)),
                        ruletype: Some(grammar.token_typename.clone()),
                        mapto: Some(ident.clone()),
                    })
                } else if let Some(nonterm_idx) = grammar.nonterminals_index.get(ident) {
                    // nonterminal
                    let nonterminal = &grammar.nonterminals[*nonterm_idx];
                    Ok(PatternToToken {
                        name: ident.clone(),
                        token: Token::NonTerm(*nonterm_idx),
                        ruletype: nonterminal.ruletype.clone(),
                        mapto: Some(ident.clone()),
                    })
                } else {
                    Err(ParseError::TerminalNotDefined(ident.clone()))
                }
            }
            PatternInternal::Plus(pattern) => {
                let base_rule = pattern.to_token(grammar, pattern_cache, root_span_pair)?;
                let newrule_idx = grammar.nonterminals.len();
                let newrule_name = Ident::new(
                    &format!("_{}Plus{}", base_rule.name, newrule_idx),
                    root_span_pair.0,
                );

                if let Some(base_typename) = &base_rule.ruletype {
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
                        reduce_action: Some(ReduceAction::Custom(quote! {
                            { vec![A] }
                        })),
                        separator_span: Span::call_site(),
                        lookaheads: None,
                        prec: None,
                        dprec: None,
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
                        reduce_action: Some(ReduceAction::Custom(quote! {
                            { Ap.push(A); Ap }
                        })),
                        separator_span: Span::call_site(),
                        lookaheads: None,
                        prec: None,
                        dprec: None,
                    };

                    let nonterm_info = NonTerminalInfo {
                        name: newrule_name.clone(),
                        pretty_name: self.pretty_name.clone(),
                        ruletype: Some(quote! { Vec<#base_typename> }),
                        rules: vec![line1, line2],
                        regex_span: Some(root_span_pair),
                        trace: false,
                        protected: false,
                        nonterm_type: Some(NonTerminalType::PlusLeft),
                    };
                    grammar.nonterminals.push(nonterm_info);
                    grammar
                        .nonterminals_index
                        .insert(newrule_name.clone(), newrule_idx);

                    Ok(PatternToToken {
                        name: newrule_name,
                        token: Token::NonTerm(newrule_idx),
                        ruletype: Some(quote! { Vec<#base_typename> }),
                        mapto: base_rule.mapto.clone(),
                    })
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
                        prec: None,
                        dprec: None,
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
                        prec: None,
                        dprec: None,
                    };

                    let nonterm_info = NonTerminalInfo {
                        name: newrule_name.clone(),
                        pretty_name: self.pretty_name.clone(),
                        ruletype: None,
                        rules: vec![line1, line2],
                        regex_span: Some(root_span_pair),
                        trace: false,
                        protected: false,
                        nonterm_type: Some(NonTerminalType::PlusRight),
                    };
                    grammar.nonterminals.push(nonterm_info);
                    grammar
                        .nonterminals_index
                        .insert(newrule_name.clone(), newrule_idx);

                    Ok(PatternToToken {
                        name: newrule_name,
                        token: Token::NonTerm(newrule_idx),
                        ruletype: None,
                        mapto: base_rule.mapto.clone(),
                    })
                }
            }
            PatternInternal::Star(pattern) => {
                let plus_rule = Pattern {
                    internal: PatternInternal::Plus(pattern.clone()),
                    pretty_name: format!("{}+", pattern.pretty_name),
                }
                .to_token(grammar, pattern_cache, root_span_pair)?;

                let base_rule = pattern.to_token(grammar, pattern_cache, root_span_pair)?;

                let newrule_idx = grammar.nonterminals.len();
                let newrule_name = Ident::new(
                    &format!("_{}Star{}", base_rule.name, newrule_idx),
                    root_span_pair.0,
                );

                if let Some(base_typename) = &base_rule.ruletype {
                    // typename exist, make new rule with typename Vec<base_typename>
                    // A* -> A+ { Ap }
                    //     |    { vec![] }
                    let line1 = Rule {
                        tokens: vec![TokenMapped {
                            token: plus_rule.token,
                            mapto: Some(Ident::new("__token0", Span::call_site())),
                            begin_span: Span::call_site(),
                            end_span: Span::call_site(),
                        }],
                        reduce_action: Some(ReduceAction::Identity(0)),
                        separator_span: Span::call_site(),
                        lookaheads: None,
                        prec: None,
                        dprec: None,
                    };
                    let line2 = Rule {
                        tokens: vec![],
                        reduce_action: Some(ReduceAction::Custom(quote! {
                            { vec![] }
                        })),
                        separator_span: Span::call_site(),
                        lookaheads: None,
                        prec: None,
                        dprec: None,
                    };

                    let nonterm_info = NonTerminalInfo {
                        name: newrule_name.clone(),
                        pretty_name: self.pretty_name.clone(),
                        ruletype: Some(quote! { Vec<#base_typename> }),
                        rules: vec![line1, line2],
                        regex_span: Some(root_span_pair),
                        trace: false,
                        protected: false,
                        nonterm_type: Some(NonTerminalType::Star),
                    };
                    grammar.nonterminals.push(nonterm_info);
                    grammar
                        .nonterminals_index
                        .insert(newrule_name.clone(), newrule_idx);

                    Ok(PatternToToken {
                        name: newrule_name,
                        token: Token::NonTerm(newrule_idx),
                        ruletype: Some(quote! { Vec<#base_typename> }),
                        mapto: base_rule.mapto.clone(),
                    })
                } else {
                    // typename not exist, make new rule with typename ()
                    // A* -> A+
                    //     |
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
                        prec: None,
                        dprec: None,
                    };
                    let line2 = Rule {
                        tokens: vec![],
                        reduce_action: None,
                        separator_span: Span::call_site(),
                        lookaheads: None,
                        prec: None,
                        dprec: None,
                    };
                    let nonterm_info = NonTerminalInfo {
                        name: newrule_name.clone(),
                        pretty_name: self.pretty_name.clone(),
                        ruletype: None,
                        rules: vec![line1, line2],
                        regex_span: Some(root_span_pair),
                        trace: false,
                        protected: false,
                        nonterm_type: Some(NonTerminalType::Star),
                    };
                    grammar.nonterminals.push(nonterm_info);
                    grammar
                        .nonterminals_index
                        .insert(newrule_name.clone(), newrule_idx);

                    Ok(PatternToToken {
                        name: newrule_name,
                        token: Token::NonTerm(newrule_idx),
                        ruletype: None,
                        mapto: base_rule.mapto.clone(),
                    })
                }
            }
            PatternInternal::Question(pattern) => {
                let base_rule = pattern.to_token(grammar, pattern_cache, root_span_pair)?;
                let newrule_idx = grammar.nonterminals.len();
                let newrule_name = Ident::new(
                    &format!("_{}Question{}", base_rule.name, newrule_idx),
                    root_span_pair.0,
                );

                if let Some(base_typename) = &base_rule.ruletype {
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
                        reduce_action: Some(ReduceAction::Custom(quote! { Some(A) })),
                        separator_span: Span::call_site(),
                        lookaheads: None,
                        prec: None,
                        dprec: None,
                    };
                    let line2 = Rule {
                        tokens: vec![],
                        reduce_action: Some(ReduceAction::Custom(quote! {
                            { None }
                        })),
                        separator_span: Span::call_site(),
                        lookaheads: None,
                        prec: None,
                        dprec: None,
                    };

                    let nonterm_info = NonTerminalInfo {
                        name: newrule_name.clone(),
                        pretty_name: self.pretty_name.clone(),
                        ruletype: Some(quote! {Option<#base_typename>}),
                        rules: vec![line1, line2],
                        regex_span: Some(root_span_pair),
                        trace: false,
                        protected: false,
                        nonterm_type: Some(NonTerminalType::Optional),
                    };
                    grammar.nonterminals.push(nonterm_info);
                    grammar
                        .nonterminals_index
                        .insert(newrule_name.clone(), newrule_idx);

                    Ok(PatternToToken {
                        name: newrule_name,
                        token: Token::NonTerm(newrule_idx),
                        ruletype: Some(quote! {Option<#base_typename>}),
                        mapto: base_rule.mapto.clone(),
                    })
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
                        prec: None,
                        dprec: None,
                    };
                    let line2 = Rule {
                        tokens: vec![],
                        reduce_action: None,
                        separator_span: Span::call_site(),
                        lookaheads: None,
                        prec: None,
                        dprec: None,
                    };

                    let nonterm_info = NonTerminalInfo {
                        name: newrule_name.clone(),
                        pretty_name: self.pretty_name.clone(),
                        ruletype: None,
                        rules: vec![line1, line2],
                        regex_span: Some(root_span_pair),
                        trace: false,
                        protected: false,
                        nonterm_type: Some(NonTerminalType::Optional),
                    };
                    grammar.nonterminals.push(nonterm_info);
                    grammar
                        .nonterminals_index
                        .insert(newrule_name.clone(), newrule_idx);

                    Ok(PatternToToken {
                        name: newrule_name,
                        token: Token::NonTerm(newrule_idx),
                        ruletype: None,
                        mapto: base_rule.mapto.clone(),
                    })
                }
            }

            PatternInternal::Exclamation(pattern) => {
                let mut base_rule = pattern.to_token(grammar, pattern_cache, root_span_pair)?;
                base_rule.ruletype = None;
                Ok(base_rule)
            }
            PatternInternal::TerminalSet(negate, terminal_set) => {
                let terminals = if *negate {
                    grammar.negate_terminal_set(terminal_set)
                } else {
                    terminal_set.clone()
                };
                if terminals.len() == 1 {
                    let terminal = terminals.into_iter().next().unwrap();
                    let term_info = &grammar.terminals[terminal];
                    return Ok(PatternToToken {
                        name: term_info.name.clone().name(),
                        token: Token::Term(TerminalSymbol::Term(terminal)),
                        ruletype: Some(grammar.token_typename.clone()),
                        mapto: None,
                    });
                }

                let newrule_idx = grammar.nonterminals.len();
                let newrule_name =
                    Ident::new(&format!("_TermSet{}", newrule_idx), root_span_pair.0);
                let mut rules = Vec::with_capacity(terminal_set.len());
                for terminal in terminals {
                    let rule = Rule {
                        tokens: vec![TokenMapped {
                            token: Token::Term(TerminalSymbol::Term(terminal)),
                            mapto: Some(Ident::new("__token0", Span::call_site())),
                            begin_span: Span::call_site(),
                            end_span: Span::call_site(),
                        }],
                        reduce_action: Some(ReduceAction::Identity(0)),
                        separator_span: Span::call_site(),
                        lookaheads: None,
                        prec: None,
                        dprec: None,
                    };
                    rules.push(rule);
                }
                let nonterm_info = NonTerminalInfo {
                    name: newrule_name.clone(),
                    pretty_name: self.pretty_name.clone(),
                    ruletype: Some(grammar.token_typename.clone()),
                    rules,
                    regex_span: Some(root_span_pair),
                    trace: false,
                    protected: false,
                    nonterm_type: Some(NonTerminalType::TerminalSet),
                };
                grammar.nonterminals.push(nonterm_info);
                grammar
                    .nonterminals_index
                    .insert(newrule_name.clone(), newrule_idx);

                Ok(PatternToToken {
                    name: newrule_name,
                    token: Token::NonTerm(newrule_idx),
                    ruletype: Some(grammar.token_typename.clone()),
                    mapto: None,
                })
            }
            PatternInternal::Lookaheads(pattern, negate, lookaheads) => {
                let lookaheads = if *negate {
                    grammar.negate_terminal_set(lookaheads)
                } else {
                    lookaheads.clone()
                };
                let base_rule = pattern.to_token(grammar, pattern_cache, root_span_pair)?;

                let newrule_idx = grammar.nonterminals.len();
                let newrule_name = Ident::new(
                    &format!("_{}LH{}", base_rule.name, newrule_idx),
                    root_span_pair.0,
                );

                if let Some(base_typename) = &base_rule.ruletype {
                    let rule = Rule {
                        tokens: vec![TokenMapped {
                            token: base_rule.token,
                            mapto: Some(Ident::new("__token0", Span::call_site())),
                            begin_span: Span::call_site(),
                            end_span: Span::call_site(),
                        }],
                        reduce_action: Some(ReduceAction::Identity(0)),
                        separator_span: Span::call_site(),
                        lookaheads: Some(lookaheads),
                        prec: None,
                        dprec: None,
                    };

                    let nonterm_info = NonTerminalInfo {
                        name: newrule_name.clone(),
                        pretty_name: self.pretty_name.clone(),
                        ruletype: Some(base_typename.clone()),
                        rules: vec![rule],
                        regex_span: Some(root_span_pair),
                        trace: false,
                        protected: false,
                        nonterm_type: Some(NonTerminalType::Lookahead),
                    };
                    grammar.nonterminals.push(nonterm_info);
                    grammar
                        .nonterminals_index
                        .insert(newrule_name.clone(), newrule_idx);

                    Ok(PatternToToken {
                        name: newrule_name,
                        token: Token::NonTerm(newrule_idx),
                        ruletype: Some(base_typename.clone()),
                        mapto: base_rule.mapto.clone(),
                    })
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
                        lookaheads: Some(lookaheads),
                        prec: None,
                        dprec: None,
                    };

                    let nonterm_info = NonTerminalInfo {
                        name: newrule_name.clone(),
                        pretty_name: self.pretty_name.clone(),
                        ruletype: None,
                        rules: vec![rule],
                        regex_span: Some(root_span_pair),
                        trace: false,
                        protected: false,
                        nonterm_type: Some(NonTerminalType::Lookahead),
                    };
                    grammar.nonterminals.push(nonterm_info);
                    grammar
                        .nonterminals_index
                        .insert(newrule_name.clone(), newrule_idx);

                    Ok(PatternToToken {
                        name: newrule_name,
                        token: Token::NonTerm(newrule_idx),
                        ruletype: None,
                        mapto: base_rule.mapto.clone(),
                    })
                }
            }

            PatternInternal::Group(group) => {
                // Consider parenthesis-ed group of patterns
                // ( A B C D ... )
                // if there are no pattern holding a value, then the RuleType of the group is None
                // if there is only one pattern holding a value, T, then the RuleType of the group is T
                // otherwise, the RuleType of the group is (T1, T2, T3, ...) where T1 T2 T3 ... are the RuleType of the patterns holding a value

                let mut elements = Vec::with_capacity(group.len());
                for child in group.iter() {
                    elements.push(child.to_token(grammar, pattern_cache, root_span_pair)?);
                }
                let mut tokens = Vec::with_capacity(group.len());
                // indices of children that have ruletype
                let mut ruletype_child_idxs = Vec::with_capacity(group.len());
                for (child_idx, child) in elements.iter().enumerate() {
                    tokens.push(TokenMapped {
                        token: child.token,
                        mapto: Some(format_ident!("__token{child_idx}")),
                        begin_span: Span::call_site(),
                        end_span: Span::call_site(),
                    });
                    if child.ruletype.is_some() {
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
                            prec: None,
                            dprec: None,
                        };
                        let nonterm_info = NonTerminalInfo {
                            name: newrule_name.clone(),
                            pretty_name: self.pretty_name.clone(),
                            ruletype: None,
                            rules: vec![rule],
                            regex_span: Some(root_span_pair),
                            trace: false,
                            protected: false,
                            nonterm_type: Some(NonTerminalType::Group),
                        };
                        grammar.nonterminals.push(nonterm_info);
                        grammar
                            .nonterminals_index
                            .insert(newrule_name.clone(), newrule_idx);

                        Ok(PatternToToken {
                            name: newrule_name,
                            token: Token::NonTerm(newrule_idx),
                            ruletype: None,
                            mapto: None,
                        })
                    }

                    1 => {
                        let unique_child_idx = ruletype_child_idxs[0];
                        // let (typename, mapto) =
                        //     &elements[unique_child_idx].ruletype_map.as_ref().unwrap();
                        let rule = Rule {
                            tokens,
                            reduce_action: Some(ReduceAction::Identity(unique_child_idx)),
                            separator_span: Span::call_site(),
                            lookaheads: None,
                            prec: None,
                            dprec: None,
                        };
                        let nonterm_info = NonTerminalInfo {
                            name: newrule_name.clone(),
                            pretty_name: self.pretty_name.clone(),
                            ruletype: elements[unique_child_idx].ruletype.clone(),
                            rules: vec![rule],
                            regex_span: Some(root_span_pair),
                            trace: false,
                            protected: false,
                            nonterm_type: Some(NonTerminalType::Group),
                        };
                        grammar.nonterminals.push(nonterm_info);
                        grammar
                            .nonterminals_index
                            .insert(newrule_name.clone(), newrule_idx);

                        Ok(PatternToToken {
                            name: newrule_name,
                            token: Token::NonTerm(newrule_idx),
                            ruletype: elements[unique_child_idx].ruletype.clone(),
                            mapto: elements[unique_child_idx].mapto.clone(),
                        })
                    }

                    _ => {
                        let mut typename = TokenStream::new();
                        let mut initializer = TokenStream::new();

                        for child_idx in ruletype_child_idxs {
                            let child_typename = elements[child_idx].ruletype.as_ref().unwrap();
                            let child_mapto = format_ident!("__token{child_idx}");
                            typename.extend(quote! { #child_typename, });
                            initializer.extend(quote! { #child_mapto, });
                        }
                        let typename = quote! {(#typename)};
                        let initializer = quote! {(#initializer)};
                        let rule = Rule {
                            tokens,
                            reduce_action: Some(ReduceAction::Custom(initializer)),
                            separator_span: Span::call_site(),
                            lookaheads: None,
                            prec: None,
                            dprec: None,
                        };
                        let nonterm_info = NonTerminalInfo {
                            name: newrule_name.clone(),
                            pretty_name: self.pretty_name.clone(),
                            ruletype: Some(typename.clone()),
                            rules: vec![rule],
                            regex_span: Some(root_span_pair),
                            trace: false,
                            protected: false,
                            nonterm_type: Some(NonTerminalType::Group),
                        };
                        grammar.nonterminals.push(nonterm_info);
                        grammar
                            .nonterminals_index
                            .insert(newrule_name.clone(), newrule_idx);

                        Ok(PatternToToken {
                            name: newrule_name,
                            token: Token::NonTerm(newrule_idx),
                            ruletype: Some(typename),
                            mapto: None,
                        })
                    }
                }
            }

            PatternInternal::Literal(literal) => match literal {
                syn::Lit::Char(ch) => {
                    let idx = grammar.get_terminal_index_from_char(ch.value());
                    let info = &grammar.terminals[idx];

                    Ok(PatternToToken {
                        name: info.name.clone().name(),
                        token: Token::Term(TerminalSymbol::Term(idx)),
                        ruletype: Some(grammar.token_typename.clone()),
                        mapto: None,
                    })
                }
                syn::Lit::Byte(ch) => {
                    let idx = grammar.get_terminal_index_from_char(ch.value() as char);
                    let info = &grammar.terminals[idx];

                    Ok(PatternToToken {
                        name: info.name.clone().name(),
                        token: Token::Term(TerminalSymbol::Term(idx)),
                        ruletype: Some(grammar.token_typename.clone()),
                        mapto: None,
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
                                let term_id = grammar.get_terminal_index_from_char(ch);
                                TokenMapped {
                                    token: Token::Term(TerminalSymbol::Term(term_id)),
                                    mapto: None,
                                    begin_span: str_span,
                                    end_span: str_span,
                                }
                            })
                            .collect(),
                        reduce_action: None,
                        separator_span: Span::call_site(),
                        lookaheads: None,
                        prec: None,
                        dprec: None,
                    };

                    let nonterm_info = NonTerminalInfo {
                        name: newrule_name.clone(),
                        pretty_name: s.to_token_stream().to_string(),
                        ruletype: None,
                        rules: vec![rule],
                        regex_span: Some((str_span, str_span)),
                        trace: false,
                        protected: false,
                        nonterm_type: Some(NonTerminalType::LiteralString),
                    };
                    grammar.nonterminals.push(nonterm_info);
                    grammar
                        .nonterminals_index
                        .insert(newrule_name.clone(), newrule_idx);

                    Ok(PatternToToken {
                        name: newrule_name.clone(),
                        token: Token::NonTerm(newrule_idx),
                        ruletype: Some(quote! { &'static str }),
                        mapto: None,
                    })
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
                                let term_id = grammar.get_terminal_index_from_char(*ch as char);
                                TokenMapped {
                                    token: Token::Term(TerminalSymbol::Term(term_id)),
                                    mapto: None,
                                    begin_span: str_span,
                                    end_span: str_span,
                                }
                            })
                            .collect(),
                        reduce_action: None,
                        separator_span: Span::call_site(),
                        lookaheads: None,
                        prec: None,
                        dprec: None,
                    };

                    let nonterm_info = NonTerminalInfo {
                        name: newrule_name.clone(),
                        pretty_name: s.to_token_stream().to_string(),
                        ruletype: None,
                        rules: vec![rule],
                        regex_span: Some((str_span, str_span)),
                        trace: false,
                        protected: false,
                        nonterm_type: Some(NonTerminalType::LiteralString),
                    };
                    grammar.nonterminals.push(nonterm_info);
                    grammar
                        .nonterminals_index
                        .insert(newrule_name.clone(), newrule_idx);

                    Ok(PatternToToken {
                        name: newrule_name.clone(),
                        token: Token::NonTerm(newrule_idx),
                        ruletype: Some(quote! { &'static [u8] }),
                        mapto: None,
                    })
                }
                _ => unreachable!("Only char, byte, str and bytes are supported"),
            },

            PatternInternal::Sep(base, del, true) => {
                let base_rule = base.to_token(grammar, pattern_cache, root_span_pair)?;
                let del = del.to_token(grammar, pattern_cache, root_span_pair)?;
                let newrule_idx = grammar.nonterminals.len();
                let newrule_name = Ident::new(
                    &format!("_{}SepPlus{}", base_rule.name, newrule_idx),
                    root_span_pair.0,
                );

                if let Some(base_typename) = &base_rule.ruletype {
                    let rule1 = Rule {
                        tokens: vec![TokenMapped {
                            token: base_rule.token,
                            mapto: Some(Ident::new("__token0", Span::call_site())),
                            begin_span: Span::call_site(),
                            end_span: Span::call_site(),
                        }],
                        reduce_action: Some(ReduceAction::Custom(quote! {
                            { vec![__token0] }
                        })),
                        separator_span: Span::call_site(),
                        lookaheads: None,
                        prec: None,
                        dprec: None,
                    };
                    let rule2 = Rule {
                        tokens: vec![
                            TokenMapped {
                                token: Token::NonTerm(newrule_idx),
                                mapto: Some(Ident::new("__token0", Span::call_site())),
                                begin_span: Span::call_site(),
                                end_span: Span::call_site(),
                            },
                            TokenMapped {
                                token: del.token,
                                mapto: None,
                                begin_span: Span::call_site(),
                                end_span: Span::call_site(),
                            },
                            TokenMapped {
                                token: base_rule.token,
                                mapto: Some(Ident::new("__token1", Span::call_site())),
                                begin_span: Span::call_site(),
                                end_span: Span::call_site(),
                            },
                        ],
                        reduce_action: Some(ReduceAction::Custom(quote! {
                            {
                            __token0.push(__token1);
                            __token0
                            }
                        })),
                        separator_span: Span::call_site(),
                        lookaheads: None,
                        prec: None,
                        dprec: None,
                    };

                    let nonterm_info = NonTerminalInfo {
                        name: newrule_name.clone(),
                        pretty_name: self.pretty_name.clone(),
                        ruletype: Some(quote! { Vec<#base_typename> }),
                        rules: vec![rule1, rule2],
                        regex_span: Some(root_span_pair),
                        trace: false,
                        protected: false,
                        nonterm_type: Some(NonTerminalType::PlusLeft),
                    };
                    grammar.nonterminals.push(nonterm_info);
                    grammar
                        .nonterminals_index
                        .insert(newrule_name.clone(), newrule_idx);

                    Ok(PatternToToken {
                        name: newrule_name,
                        token: Token::NonTerm(newrule_idx),
                        ruletype: Some(quote! { Vec<#base_typename> }),
                        mapto: base_rule.mapto.clone(),
                    })
                } else {
                    let rule1 = Rule {
                        tokens: vec![TokenMapped {
                            token: base_rule.token,
                            mapto: None,
                            begin_span: Span::call_site(),
                            end_span: Span::call_site(),
                        }],
                        reduce_action: None,
                        separator_span: Span::call_site(),
                        lookaheads: None,
                        prec: None,
                        dprec: None,
                    };
                    let rule2 = Rule {
                        tokens: vec![
                            TokenMapped {
                                token: base_rule.token,
                                mapto: None,
                                begin_span: Span::call_site(),
                                end_span: Span::call_site(),
                            },
                            TokenMapped {
                                token: del.token,
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
                        prec: None,
                        dprec: None,
                    };

                    let nonterm_info = NonTerminalInfo {
                        name: newrule_name.clone(),
                        pretty_name: self.pretty_name.clone(),
                        ruletype: None,
                        rules: vec![rule1, rule2],
                        regex_span: Some(root_span_pair),
                        trace: false,
                        protected: false,
                        nonterm_type: Some(NonTerminalType::PlusRight),
                    };
                    grammar.nonterminals.push(nonterm_info);
                    grammar
                        .nonterminals_index
                        .insert(newrule_name.clone(), newrule_idx);

                    Ok(PatternToToken {
                        name: newrule_name,
                        token: Token::NonTerm(newrule_idx),
                        ruletype: None,
                        mapto: base_rule.mapto.clone(),
                    })
                }
            }

            PatternInternal::Sep(base, del, false) => {
                let plus_rule = Pattern {
                    internal: PatternInternal::Sep(base.clone(), del.clone(), true),
                    pretty_name: format!("$sep({}, {}, +)", base.pretty_name, del.pretty_name),
                }
                .to_token(grammar, pattern_cache, root_span_pair)?;

                let base_rule = base.to_token(grammar, pattern_cache, root_span_pair)?;

                let newrule_idx = grammar.nonterminals.len();
                let newrule_name = Ident::new(
                    &format!("_{}SepStar{}", base_rule.name, newrule_idx),
                    root_span_pair.0,
                );

                if let Some(base_typename) = &base_rule.ruletype {
                    // typename exist, make new rule with typename Vec<base_typename>
                    // A* -> A+ { Ap }
                    //     |    { vec![] }
                    let line1 = Rule {
                        tokens: vec![TokenMapped {
                            token: plus_rule.token,
                            mapto: Some(Ident::new("__token0", Span::call_site())),
                            begin_span: Span::call_site(),
                            end_span: Span::call_site(),
                        }],
                        reduce_action: Some(ReduceAction::Identity(0)),
                        separator_span: Span::call_site(),
                        lookaheads: None,
                        prec: None,
                        dprec: None,
                    };
                    let line2 = Rule {
                        tokens: vec![],
                        reduce_action: Some(ReduceAction::Custom(quote! {
                            { vec![] }
                        })),
                        separator_span: Span::call_site(),
                        lookaheads: None,
                        prec: None,
                        dprec: None,
                    };

                    let nonterm_info = NonTerminalInfo {
                        name: newrule_name.clone(),
                        pretty_name: self.pretty_name.clone(),
                        ruletype: Some(quote! { Vec<#base_typename> }),
                        rules: vec![line1, line2],
                        regex_span: Some(root_span_pair),
                        trace: false,
                        protected: false,
                        nonterm_type: Some(NonTerminalType::Star),
                    };
                    grammar.nonterminals.push(nonterm_info);
                    grammar
                        .nonterminals_index
                        .insert(newrule_name.clone(), newrule_idx);

                    Ok(PatternToToken {
                        name: newrule_name,
                        token: Token::NonTerm(newrule_idx),
                        ruletype: Some(quote! { Vec<#base_typename> }),
                        mapto: base_rule.mapto.clone(),
                    })
                } else {
                    // typename not exist, make new rule with typename ()
                    // A* -> A+
                    //     |
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
                        prec: None,
                        dprec: None,
                    };
                    let line2 = Rule {
                        tokens: vec![],
                        reduce_action: None,
                        separator_span: Span::call_site(),
                        lookaheads: None,
                        prec: None,
                        dprec: None,
                    };
                    let nonterm_info = NonTerminalInfo {
                        name: newrule_name.clone(),
                        pretty_name: self.pretty_name.clone(),
                        ruletype: None,
                        rules: vec![line1, line2],
                        regex_span: Some(root_span_pair),
                        trace: false,
                        protected: false,
                        nonterm_type: Some(NonTerminalType::Star),
                    };
                    grammar.nonterminals.push(nonterm_info);
                    grammar
                        .nonterminals_index
                        .insert(newrule_name.clone(), newrule_idx);

                    Ok(PatternToToken {
                        name: newrule_name,
                        token: Token::NonTerm(newrule_idx),
                        ruletype: None,
                        mapto: base_rule.mapto.clone(),
                    })
                }
            }
        }
    }
}
