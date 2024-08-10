// This file was generated by rustylr 0.1.3
//
// Input file: ../rusty_lr_parser/src/parser/terminalset_parser.rs
// Output file: terminalset_expanded.rs
// ================================User Codes Begin================================
use crate::parser::lexer::Lexed;
use crate::terminalset::TerminalSet;
use crate::terminalset::TerminalSetItem;
// =================================User Codes End=================================
/*
====================================Grammar=====================================
0: TerminalSet -> _RustyLRGenerated0 _RustyLRGenerated1
1: TerminalSetItem -> ident
2: TerminalSetItem -> ident minus ident
3: _RustyLRGenerated0 -> caret
4: _RustyLRGenerated0 ->
5: _RustyLRGenerated1 -> TerminalSetItem
6: _RustyLRGenerated1 -> _RustyLRGenerated1 TerminalSetItem
7: Augmented -> TerminalSet eof


*/
// =============================Generated Codes Begin==============================
#[doc = r" An enum that represents non-terminal symbols"]
#[allow(non_camel_case_types)]
#[derive(
    Debug,
    Clone,
    Copy,
    std :: hash :: Hash,
    std :: cmp :: PartialEq,
    std :: cmp :: Eq,
    std :: cmp :: PartialOrd,
    std :: cmp :: Ord,
)]
pub enum TerminalSetNonTerminals {
    TerminalSet,
    TerminalSetItem,
    _RustyLRGenerated0,
    _RustyLRGenerated1,
    Augmented,
}
impl std::fmt::Display for TerminalSetNonTerminals {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TerminalSetNonTerminals::TerminalSet => write!(f, "TerminalSet"),
            TerminalSetNonTerminals::TerminalSetItem => write!(f, "TerminalSetItem"),
            TerminalSetNonTerminals::_RustyLRGenerated0 => write!(f, "_RustyLRGenerated0"),
            TerminalSetNonTerminals::_RustyLRGenerated1 => write!(f, "_RustyLRGenerated1"),
            TerminalSetNonTerminals::Augmented => write!(f, "Augmented"),
        }
    }
}
#[doc = r" struct that holds internal parser data, for reduce action and state transition"]
#[allow(
    unused_braces,
    unused_parens,
    unused_variables,
    non_snake_case,
    unused_mut
)]
pub struct TerminalSetContext {
    #[doc = r" state stack, user must not modify this"]
    pub state_stack: Vec<usize>,
    __rustylr_generated_terminal_stack: Vec<Lexed>,
    _rustylr_generated_TerminalSet_stack: Vec<(TerminalSet)>,
    _rustylr_generated_TerminalSetItem_stack: Vec<(TerminalSetItem)>,
    _rustylr_generated__RustyLRGenerated0_stack: Vec<Option<Lexed>>,
    _rustylr_generated__RustyLRGenerated1_stack: Vec<Vec<(TerminalSetItem)>>,
}
#[allow(
    unused_braces,
    unused_parens,
    unused_variables,
    non_snake_case,
    unused_mut
)]
impl TerminalSetContext {
    pub fn new() -> Self {
        Self {
            state_stack: vec![0],
            __rustylr_generated_terminal_stack: Vec::new(),
            _rustylr_generated_TerminalSet_stack: Vec::new(),
            _rustylr_generated_TerminalSetItem_stack: Vec::new(),
            _rustylr_generated__RustyLRGenerated0_stack: Vec::new(),
            _rustylr_generated__RustyLRGenerated1_stack: Vec::new(),
        }
    }
    fn reduce_TerminalSet_0(&mut self) -> Result<(), String> {
        let mut TerminalSetItem = self
            ._rustylr_generated__RustyLRGenerated1_stack
            .pop()
            .expect("Something wrong! _RustyLRGenerated1 stack is empty");
        let mut caret = self
            ._rustylr_generated__RustyLRGenerated0_stack
            .pop()
            .expect("Something wrong! _RustyLRGenerated0 stack is empty");
        self._rustylr_generated_TerminalSet_stack.push({
            TerminalSet {
                negate: caret.is_some(),
                items: TerminalSetItem,
            }
        });
        Ok(())
    }
    fn reduce_TerminalSetItem_0(&mut self) -> Result<(), String> {
        let mut ident = self
            .__rustylr_generated_terminal_stack
            .pop()
            .expect("Something wrong! term_stack is empty");
        self._rustylr_generated_TerminalSetItem_stack.push({
            let ident = if let Lexed::Ident(ident) = ident {
                ident.expect("TerminalSetItem-Range0")
            } else {
                unreachable!("TerminalSetItem-Range1");
            };
            TerminalSetItem::Terminal(ident)
        });
        Ok(())
    }
    fn reduce_TerminalSetItem_1(&mut self) -> Result<(), String> {
        let mut last = self
            .__rustylr_generated_terminal_stack
            .pop()
            .expect("Something wrong! term_stack is empty");
        let mut minus = self
            .__rustylr_generated_terminal_stack
            .pop()
            .expect("Something wrong! term_stack is empty");
        let mut first = self
            .__rustylr_generated_terminal_stack
            .pop()
            .expect("Something wrong! term_stack is empty");
        self._rustylr_generated_TerminalSetItem_stack.push({
            let first = if let Lexed::Ident(first) = first {
                first.expect("TerminalSetItem-Range0")
            } else {
                unreachable!("TerminalSetItem-Range1");
            };
            let last = if let Lexed::Ident(last) = last {
                last.expect("TerminalSetItem-Range2")
            } else {
                unreachable!("TerminalSetItem-Range3");
            };
            TerminalSetItem::Range(first, last)
        });
        Ok(())
    }
    fn reduce__RustyLRGenerated0_0(&mut self) -> Result<(), String> {
        let mut A = self
            .__rustylr_generated_terminal_stack
            .pop()
            .expect("Something wrong! term_stack is empty");
        self._rustylr_generated__RustyLRGenerated0_stack
            .push({ Some(A) });
        Ok(())
    }
    fn reduce__RustyLRGenerated0_1(&mut self) -> Result<(), String> {
        self._rustylr_generated__RustyLRGenerated0_stack
            .push({ None });
        Ok(())
    }
    fn reduce__RustyLRGenerated1_0(&mut self) -> Result<(), String> {
        let mut A = self
            ._rustylr_generated_TerminalSetItem_stack
            .pop()
            .expect("Something wrong! TerminalSetItem stack is empty");
        self._rustylr_generated__RustyLRGenerated1_stack
            .push({ vec![A] });
        Ok(())
    }
    fn reduce__RustyLRGenerated1_1(&mut self) -> Result<(), String> {
        let mut A = self
            ._rustylr_generated_TerminalSetItem_stack
            .pop()
            .expect("Something wrong! TerminalSetItem stack is empty");
        let mut Ap = self
            ._rustylr_generated__RustyLRGenerated1_stack
            .pop()
            .expect("Something wrong! _RustyLRGenerated1 stack is empty");
        self._rustylr_generated__RustyLRGenerated1_stack.push({
            Ap.push(A);
            Ap
        });
        Ok(())
    }
    #[doc = r" reduce items in stack, this function is called automatically by parser"]
    pub fn reduce(
        &mut self,
        rulelen: usize,
        rustylr_macro_generated_ruleid__: usize,
    ) -> Result<(), String> {
        match rustylr_macro_generated_ruleid__ {
            0usize => {
                self.reduce_TerminalSet_0()?;
            }
            1usize => {
                self.reduce_TerminalSetItem_0()?;
            }
            2usize => {
                self.reduce_TerminalSetItem_1()?;
            }
            3usize => {
                self.reduce__RustyLRGenerated0_0()?;
            }
            4usize => {
                self.reduce__RustyLRGenerated0_1()?;
            }
            5usize => {
                self.reduce__RustyLRGenerated1_0()?;
            }
            6usize => {
                self.reduce__RustyLRGenerated1_1()?;
            }
            _ => {
                unreachable!("Invalid Rule: {}", rustylr_macro_generated_ruleid__);
            }
        }
        Ok(())
    }
    #[doc = r" pop value from start rule"]
    pub fn accept(&mut self) -> (TerminalSet) {
        self._rustylr_generated_TerminalSet_stack
            .pop()
            .expect("Something wrong! start_rule_stack is empty")
    }
    #[doc = r" push terminal symbol to stack, this function is called automatically by parser"]
    pub fn push(&mut self, term: Lexed) {
        self.__rustylr_generated_terminal_stack.push(term);
    }
}
#[doc = r" struct that holds parser data, DFA tables"]
#[allow(
    unused_braces,
    unused_parens,
    unused_variables,
    non_snake_case,
    unused_mut
)]
pub struct TerminalSetParser {
    #[doc = r" production rules"]
    pub rules: Vec<::rusty_lr_core::ProductionRule<Lexed, TerminalSetNonTerminals>>,
    #[doc = r" states"]
    pub states: Vec<::rusty_lr_core::State<Lexed, TerminalSetNonTerminals>>,
}
#[allow(
    unused_braces,
    unused_parens,
    unused_variables,
    non_snake_case,
    unused_mut
)]
impl TerminalSetParser {
    pub fn new() -> Self {
        let rules = vec![
            ::rusty_lr_core::ProductionRule {
                name: TerminalSetNonTerminals::TerminalSet,
                rule: vec![
                    ::rusty_lr_core::Token::NonTerm(TerminalSetNonTerminals::_RustyLRGenerated0),
                    ::rusty_lr_core::Token::NonTerm(TerminalSetNonTerminals::_RustyLRGenerated1),
                ],
            },
            ::rusty_lr_core::ProductionRule {
                name: TerminalSetNonTerminals::TerminalSetItem,
                rule: vec![::rusty_lr_core::Token::Term(Lexed::Ident(None))],
            },
            ::rusty_lr_core::ProductionRule {
                name: TerminalSetNonTerminals::TerminalSetItem,
                rule: vec![
                    ::rusty_lr_core::Token::Term(Lexed::Ident(None)),
                    ::rusty_lr_core::Token::Term(Lexed::Minus(None)),
                    ::rusty_lr_core::Token::Term(Lexed::Ident(None)),
                ],
            },
            ::rusty_lr_core::ProductionRule {
                name: TerminalSetNonTerminals::_RustyLRGenerated0,
                rule: vec![::rusty_lr_core::Token::Term(Lexed::Caret(None))],
            },
            ::rusty_lr_core::ProductionRule {
                name: TerminalSetNonTerminals::_RustyLRGenerated0,
                rule: vec![],
            },
            ::rusty_lr_core::ProductionRule {
                name: TerminalSetNonTerminals::_RustyLRGenerated1,
                rule: vec![::rusty_lr_core::Token::NonTerm(
                    TerminalSetNonTerminals::TerminalSetItem,
                )],
            },
            ::rusty_lr_core::ProductionRule {
                name: TerminalSetNonTerminals::_RustyLRGenerated1,
                rule: vec![
                    ::rusty_lr_core::Token::NonTerm(TerminalSetNonTerminals::_RustyLRGenerated1),
                    ::rusty_lr_core::Token::NonTerm(TerminalSetNonTerminals::TerminalSetItem),
                ],
            },
            ::rusty_lr_core::ProductionRule {
                name: TerminalSetNonTerminals::Augmented,
                rule: vec![
                    ::rusty_lr_core::Token::NonTerm(TerminalSetNonTerminals::TerminalSet),
                    ::rusty_lr_core::Token::Term(Lexed::Eof),
                ],
            },
        ];
        let rustylr_macrogenerated_lookaheads_0 = std::collections::BTreeSet::from([Lexed::Eof]);
        let rustylr_macrogenerated_lookaheads_1 =
            std::collections::BTreeSet::from([Lexed::Ident(None)]);
        let rustylr_macrogenerated_lookaheads_2 = std::collections::BTreeSet::from([]);
        let rustylr_macrogenerated_lookaheads_3 =
            std::collections::BTreeSet::from([Lexed::Eof, Lexed::Ident(None)]);
        let mut states = Vec::with_capacity(11usize);
        {
            let shift_goto_map_term =
                std::collections::HashMap::from([(Lexed::Caret(None), 1usize)]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([
                (TerminalSetNonTerminals::TerminalSet, 2usize),
                (TerminalSetNonTerminals::_RustyLRGenerated0, 4usize),
            ]);
            let reduce_map = std::collections::HashMap::from([(Lexed::Ident(None), 4usize)]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 0usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 3usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 4usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_1.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 7usize,
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
            let shift_goto_map_term = std::collections::HashMap::from([]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([(Lexed::Ident(None), 3usize)]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 3usize,
                        shifted: 1usize,
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
            let shift_goto_map_term = std::collections::HashMap::from([(Lexed::Eof, 3usize)]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 7usize,
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
                        rule: 7usize,
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
        {
            let shift_goto_map_term =
                std::collections::HashMap::from([(Lexed::Ident(None), 5usize)]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([
                (TerminalSetNonTerminals::TerminalSetItem, 8usize),
                (TerminalSetNonTerminals::_RustyLRGenerated1, 9usize),
            ]);
            let reduce_map = std::collections::HashMap::from([]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 0usize,
                            shifted: 1usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 1usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 2usize,
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
                std::collections::HashMap::from([(Lexed::Minus(None), 6usize)]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([
                (Lexed::Eof, 1usize),
                (Lexed::Ident(None), 1usize),
            ]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 1usize,
                            shifted: 1usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 2usize,
                            shifted: 1usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
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
                std::collections::HashMap::from([(Lexed::Ident(None), 7usize)]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([]);
            let reduce_map = std::collections::HashMap::from([]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 2usize,
                        shifted: 2usize,
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
                (Lexed::Eof, 2usize),
                (Lexed::Ident(None), 2usize),
            ]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 2usize,
                        shifted: 3usize,
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
                (Lexed::Eof, 5usize),
                (Lexed::Ident(None), 5usize),
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
            let shift_goto_map_term =
                std::collections::HashMap::from([(Lexed::Ident(None), 5usize)]);
            let shift_goto_map_nonterm = std::collections::HashMap::from([(
                TerminalSetNonTerminals::TerminalSetItem,
                10usize,
            )]);
            let reduce_map = std::collections::HashMap::from([(Lexed::Eof, 0usize)]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 0usize,
                            shifted: 2usize,
                        },
                        rustylr_macrogenerated_lookaheads_0.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 1usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 2usize,
                            shifted: 0usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
                    ),
                    (
                        ::rusty_lr_core::ShiftedRuleRef {
                            rule: 6usize,
                            shifted: 1usize,
                        },
                        rustylr_macrogenerated_lookaheads_3.clone(),
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
                (Lexed::Eof, 6usize),
                (Lexed::Ident(None), 6usize),
            ]);
            let ruleset = ::rusty_lr_core::LookaheadRuleRefSet {
                rules: std::collections::BTreeMap::from([(
                    ::rusty_lr_core::ShiftedRuleRef {
                        rule: 6usize,
                        shifted: 2usize,
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
        Self { rules, states }
    }
    #[doc = r" give lookahead token to parser, and check if there is any reduce action"]
    fn lookahead<'a, C: ::rusty_lr_core::Callback<Lexed, TerminalSetNonTerminals>>(
        &'a self,
        context: &mut TerminalSetContext,
        callback: &mut C,
        term: &Lexed,
    ) -> Result<(), ::rusty_lr_core::ParseError<'a, Lexed, TerminalSetNonTerminals, C::Error, String>>
    {
        let state = &self.states[*context
            .state_stack
            .last()
            .expect("Something wrong! state_stack is empty")];
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
        context: &mut TerminalSetContext,
        term: Lexed,
    ) -> Result<(), ::rusty_lr_core::ParseError<'a, Lexed, TerminalSetNonTerminals, u8, String>>
    {
        self.feed_callback(context, &mut ::rusty_lr_core::DefaultCallback {}, term)
    }
    #[doc = r" feed one terminal to parser, and update state stack"]
    pub fn feed_callback<'a, C: ::rusty_lr_core::Callback<Lexed, TerminalSetNonTerminals>>(
        &'a self,
        context: &mut TerminalSetContext,
        callback: &mut C,
        term: Lexed,
    ) -> Result<(), ::rusty_lr_core::ParseError<'a, Lexed, TerminalSetNonTerminals, C::Error, String>>
    {
        self.lookahead(context, callback, &term)?;
        let state = &self.states[*context
            .state_stack
            .last()
            .expect("Something wrong! state_stack is empty")];
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
    fn feed_nonterm<'a, C: ::rusty_lr_core::Callback<Lexed, TerminalSetNonTerminals>>(
        &'a self,
        context: &mut TerminalSetContext,
        callback: &mut C,
        nonterm: &'a TerminalSetNonTerminals,
    ) -> Result<(), ::rusty_lr_core::ParseError<'a, Lexed, TerminalSetNonTerminals, C::Error, String>>
    {
        let state = &self.states[*context
            .state_stack
            .last()
            .expect("Something wrong! state_stack is empty")];
        if let Some(next_state_id) = state.shift_goto_nonterm(nonterm) {
            context.state_stack.push(next_state_id);
            callback
                .shift_and_goto_nonterm(&self.rules, &self.states, &context.state_stack, nonterm)
                .map_err(|e| ::rusty_lr_core::ParseError::Callback(e))?;
            Ok(())
        } else {
            Err(::rusty_lr_core::ParseError::InvalidNonTerminal(
                nonterm,
                &self.rules,
                &self.states,
                context.state_stack.clone(),
            ))
        }
    }
    pub fn begin(&self) -> TerminalSetContext {
        TerminalSetContext::new()
    }
}
// ==============================Generated Codes End===============================
