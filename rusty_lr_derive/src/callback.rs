use super::error::ParseError;
use super::grammar::Grammar;
use super::rule::RuleLine;
use super::rule::RuleLines;
use super::term::TermType;
use super::token::Token;
use super::token::TokenMapped;

use proc_macro2::Group;
use proc_macro2::TokenStream;
use quote::quote;
use quote::ToTokens;

pub struct Callback {
    pub grammar: Grammar,
    pub termstack: Vec<TermType>,

    pub ruletype_stack: Vec<Option<Group>>,
    pub rulelines_stack: Vec<RuleLines>,
    pub ruleline_stack: Vec<RuleLine>,
    pub tokens_stack: Vec<Vec<TokenMapped>>,
    pub token_stack: Vec<TokenMapped>,
    pub action_stack: Vec<Option<Group>>,
    pub rustcode_stack: Vec<TokenStream>,
}

impl Callback {
    pub fn new() -> Self {
        Self {
            grammar: Grammar::new(),
            termstack: Vec::new(),
            ruletype_stack: Vec::new(),
            ruleline_stack: Vec::new(),
            rulelines_stack: Vec::new(),
            tokens_stack: Vec::new(),
            token_stack: Vec::new(),
            action_stack: Vec::new(),
            rustcode_stack: Vec::new(),
        }
    }
}

impl rusty_lr_core::Callback<TermType, &'static str> for Callback {
    type Error = ParseError;
    fn reduce(
        &mut self,
        _rules: &[rusty_lr_core::ProductionRule<TermType, &'static str>],
        _states: &[rusty_lr_core::State<TermType, &'static str>],
        _state_stack: &[usize],
        rule: usize,
    ) -> Result<(), Self::Error> {
        match rule {
            // Rule: Ident RuleType ':' RuleLines ';'
            0 => {
                // ';'
                self.termstack.pop();

                // ':'
                self.termstack.pop();

                // Ident
                let ident = match self.termstack.pop() {
                    Some(TermType::Ident(ident)) => ident.unwrap(),
                    _ => unreachable!("Rule{} - Ident", rule),
                };

                let ruletype = if let Some(rt) = self.ruletype_stack.pop() {
                    rt
                } else {
                    unreachable!("Rule{} - RuleType", rule);
                };

                let rulelines = if let Some(rl) = self.rulelines_stack.pop() {
                    rl
                } else {
                    unreachable!("Rule{} - RuleLines", rule);
                };

                let span = ident.span();
                let name = ident.to_string();

                if self.grammar.terminals.contains_key(&name) {
                    return Err(ParseError::TermNonTermConflict(span, name));
                }

                if self
                    .grammar
                    .rules
                    .insert(
                        name.clone(),
                        (ident, ruletype.map(|t| t.to_token_stream()), rulelines),
                    )
                    .is_some()
                {
                    return Err(ParseError::MultipleRuleDefinition(span, name));
                }
            }

            // RuleType: Group
            1 => {
                if let Some(TermType::Group(ruletype)) = self.termstack.pop() {
                    if let Some(ruletype) = &ruletype {
                        // check if ruletype is enclosed with '(' and ')'
                        if ruletype.delimiter() != proc_macro2::Delimiter::Parenthesis {
                            return Err(ParseError::InvalidRuletypeDelimiter(ruletype.span()));
                        }
                    } else {
                        unreachable!("Rule{} - Group", rule);
                    }
                    self.ruletype_stack.push(ruletype);
                } else {
                    unreachable!("Rule{}", rule);
                }
            }

            // RuleType:
            2 => {
                self.ruletype_stack.push(None);
            }

            // RuleLines: RuleLine '|' RuleLines
            3 => {
                // '|'
                self.termstack.pop();

                // RuleLine
                if let Some(ruleline) = self.ruleline_stack.pop() {
                    // RuleLines
                    if let Some(mut rulelines) = self.rulelines_stack.pop() {
                        rulelines.rule_lines.push(ruleline);
                        self.rulelines_stack.push(rulelines);
                    } else {
                        unreachable!("Rule{} - 2", rule);
                    }
                } else {
                    unreachable!("Rule{} - 1", rule);
                }
            }

            // RuleLines: RuleLine
            4 => {
                // RuleLine
                if let Some(ruleline) = self.ruleline_stack.pop() {
                    let rulelines = RuleLines {
                        rule_lines: vec![ruleline],
                    };
                    self.rulelines_stack.push(rulelines);
                } else {
                    unreachable!("Rule{}", rule);
                }
            }

            // RuleLine: RuleDef Action
            //      RuleDef -> Tokens
            5 => {
                // Action
                let action = match self.action_stack.pop() {
                    Some(action) => action,
                    None => unreachable!("Rule{} - Action", rule),
                };

                // RuleDef
                let mut tokens = match self.tokens_stack.pop() {
                    Some(tokens) => tokens,
                    _ => unreachable!("Rule{} - RuleDef", rule),
                };
                tokens.reverse();

                self.ruleline_stack.push(RuleLine {
                    tokens,
                    reduce_action: action.map(|a| a.to_token_stream()),
                });
            }

            // RuleDef: Tokens
            6 => {}

            // Tokens: TokensOne
            7 => {}

            // Tokens:
            8 => {
                self.tokens_stack.push(Vec::new());
            }

            // TokensOne: Token TokensOne
            9 => {
                let token = match self.token_stack.pop() {
                    Some(token) => token,
                    _ => unreachable!("Rule{} - Token", rule),
                };

                let mut tokens = match self.tokens_stack.pop() {
                    Some(tokens) => tokens,
                    _ => unreachable!("Rule{} - Tokens", rule),
                };
                tokens.push(token);
                self.tokens_stack.push(tokens);
            }

            // TokensOne: Token
            10 => {
                let token = match self.token_stack.pop() {
                    Some(token) => token,
                    _ => unreachable!("Rule{} - Token", rule),
                };
                self.tokens_stack.push(vec![token]);
            }

            // Token: Ident
            11 => match self.termstack.pop() {
                Some(TermType::Ident(ident)) => {
                    self.token_stack.push(TokenMapped {
                        token: Token::NonTerm(ident.unwrap()),
                        mapped: None,
                    });
                }
                _ => {
                    unreachable!("Rule{} - Ident", rule);
                }
            },
            // Token: Ident '=' Ident
            12 => {
                let ident = if let Some(TermType::Ident(ident)) = self.termstack.pop() {
                    ident.unwrap()
                } else {
                    unreachable!("Rule{} - Ident1", rule);
                };
                // '='
                self.termstack.pop();

                let mapped = if let Some(TermType::Ident(ident)) = self.termstack.pop() {
                    ident.unwrap()
                } else {
                    unreachable!("Rule{} - Ident2", rule);
                };

                self.token_stack.push(TokenMapped {
                    token: Token::NonTerm(ident),
                    mapped: Some(mapped),
                });
            }

            // Action: Group
            13 => match self.termstack.pop() {
                Some(TermType::Group(group)) => {
                    if let Some(action) = &group {
                        // check if action is enclosed with '{' and '}'
                        if action.delimiter() != proc_macro2::Delimiter::Brace {
                            return Err(ParseError::InvliadReduceActionDelimiter(action.span()));
                        }
                    } else {
                        unreachable!("Rule{} - Group", rule);
                    }
                    self.action_stack.push(group);
                }
                _ => {
                    unreachable!("Rule{} - Group", rule);
                }
            },

            // Action:
            14 => {
                self.action_stack.push(None);
            }

            // TokenDef: '%token' Ident RustCode ';'
            15 => {
                // ';'
                self.termstack.pop();

                // RustCode
                // ....

                // Ident
                let ident = match self.termstack.pop() {
                    Some(TermType::Ident(ident)) => ident.unwrap(),
                    _ => unreachable!("Rule{} - Ident", rule),
                };

                let rustcode = match self.rustcode_stack.pop() {
                    Some(rustcode) => rustcode,
                    _ => unreachable!("Rule{} - RustCode", rule),
                };

                // '%token'
                self.termstack.pop();
                let span = ident.span();
                let name = ident.to_string();

                if self.grammar.rules.contains_key(&name) {
                    return Err(ParseError::TermNonTermConflict(span, name));
                }

                if let Some(old) = self
                    .grammar
                    .terminals
                    .insert(name.clone(), (ident, rustcode.clone()))
                {
                    return Err(ParseError::MultipleTokenDefinition(
                        span, old.0, old.1, rustcode,
                    ));
                }
            }

            // AnyTokenNoSemi: <Any Token Except Semicolon>
            16 => {}
            17 => {}
            18 => {}
            19 => {}
            20 => {}
            21 => {}
            22 => {}
            23 => {}
            24 => {}
            25 => {}
            26 => {}
            27 => {}
            28 => {}
            29 => {}
            30 => {}

            // AnyTokens: AnyTokenNoSemi AnyTokens
            31 => {
                // AnyTokenNoSemi
                let token = match self.termstack.pop() {
                    Some(token) => token.stream(),
                    _ => unreachable!("Rule{} - AnyTokenNoSemi", rule),
                };

                // AnyTokens
                let rustcode = match self.rustcode_stack.pop() {
                    Some(tokens) => tokens,
                    _ => unreachable!("Rule{} - AnyTokens", rule),
                };
                self.rustcode_stack.push(quote! { #token #rustcode });
            }

            // AnyTokens: AnyTokenNoSemi
            32 => {
                // AnyTokenNoSemi
                let token = match self.termstack.pop() {
                    Some(token) => token.stream(),
                    _ => unreachable!("Rule{} - AnyTokenNoSemi", rule),
                };
                self.rustcode_stack.push(token);
            }

            // RustCode: AnyTokens
            33 => {}

            // StartDef: '%start' Ident ';'
            34 => {
                // ';'
                self.termstack.pop();

                // Ident
                let ident = match self.termstack.pop() {
                    Some(TermType::Ident(ident)) => ident.unwrap(),
                    _ => unreachable!("Rule{} - Ident", rule),
                };

                // '%start'
                self.termstack.pop();
                let span = ident.span();

                if let Some(old) = &self.grammar.start_rule_name {
                    return Err(ParseError::MultipleStartDefinition(
                        span,
                        old.clone(),
                        ident,
                    ));
                }

                self.grammar.start_rule_name = Some(ident);
            }

            // EofDef: '%eof' RustCode ';'
            35 => {
                // ';'
                self.termstack.pop();

                // RustCode
                let rustcode = match self.rustcode_stack.pop() {
                    Some(rustcode) => rustcode,
                    _ => unreachable!("Rule{} - RustCode", rule),
                };

                // '%eof'
                let span = self.termstack.pop().unwrap().span().unwrap();

                if let Some(old) = &self.grammar.eof {
                    return Err(ParseError::MultipleEofDefinition(
                        span,
                        old.clone(),
                        rustcode,
                    ));
                }

                self.grammar.eof = Some(rustcode);
            }

            // TokenTypeDef: '%tokentype' RustCode ';'
            36 => {
                // ';'
                self.termstack.pop();

                // RustCode
                let rustcode = match self.rustcode_stack.pop() {
                    Some(rustcode) => rustcode,
                    _ => unreachable!("Rule{} - RustCode", rule),
                };

                // '%tokentype'
                let span = self.termstack.pop().unwrap().span().unwrap();

                if let Some(old) = &self.grammar.token_typename {
                    return Err(ParseError::MultipleTokenTypeDefinition(
                        span,
                        old.clone(),
                        rustcode,
                    ));
                }

                self.grammar.token_typename = Some(rustcode);
            }

            // UserDataDef: '%userdata' RustCode ';'
            37 => {
                // ';'
                self.termstack.pop();

                // RustCode
                let rustcode = match self.rustcode_stack.pop() {
                    Some(rustcode) => rustcode,
                    _ => unreachable!("Rule{} - RustCode", rule),
                };

                // '%userdata'
                let span = self.termstack.pop().unwrap().span().unwrap();

                if let Some(old) = &self.grammar.userdata_typename {
                    return Err(ParseError::MultipleUserDataDefinition(
                        span,
                        old.clone(),
                        rustcode,
                    ));
                }

                self.grammar.userdata_typename = Some(rustcode);
            }

            // ReduceDef: '%left' Ident ';'
            38 => {
                // ';'
                self.termstack.pop();

                // Ident
                let ident = match self.termstack.pop() {
                    Some(TermType::Ident(ident)) => ident.unwrap(),
                    _ => unreachable!("Rule{} - Ident", rule),
                };
                self.grammar
                    .reduce_types
                    .insert(ident.to_string(), (ident, rusty_lr_core::ReduceType::Left));

                // '%left'
                self.termstack.pop();
            }

            // ReduceDef: '%right' Ident ';'
            39 => {
                // ';'
                self.termstack.pop();

                // Ident
                let ident = match self.termstack.pop() {
                    Some(TermType::Ident(ident)) => ident.unwrap(),
                    _ => unreachable!("Rule{} - Ident", rule),
                };
                self.grammar
                    .reduce_types
                    .insert(ident.to_string(), (ident, rusty_lr_core::ReduceType::Left));

                // '%right'
                self.termstack.pop();
            }

            // Grammar: Rule Grammar
            40 => {}

            // Grammar: Rule
            41 => {}

            // Grammar: TokenDef Grammar
            42 => {}

            // Grammar: TokenDef
            43 => {}

            // Grammar: StartDef Grammar
            44 => {}

            // Grammar: StartDef
            45 => {}

            // Grammar: EofDef Grammar
            46 => {}

            // Grammar: EofDef
            47 => {}

            // Grammar: TokenTypeDef Grammar
            48 => {}

            // Grammar: TokenTypeDef
            49 => {}

            // Grammar: UserDataDef Grammar
            50 => {}

            // Grammar: UserDataDef
            51 => {}

            // Grammar: ReduceDef Grammar
            52 => {}

            // Grammar: ReduceDef
            53 => {}

            _ => unreachable!("Invalid RuleID: {}", rule),
        }
        Ok(())
    }
    fn shift_and_goto(
        &mut self,
        _rules: &[rusty_lr_core::ProductionRule<TermType, &'static str>],
        _states: &[rusty_lr_core::State<TermType, &'static str>],
        _state_stack: &[usize],
        term: &TermType,
    ) -> Result<(), Self::Error> {
        self.termstack.push(term.clone());
        Ok(())
    }
    fn shift_and_goto_nonterm(
        &mut self,
        _rules: &[rusty_lr_core::ProductionRule<TermType, &'static str>],
        _states: &[rusty_lr_core::State<TermType, &'static str>],
        _state_stack: &[usize],
        _nonterm: &&'static str,
    ) -> Result<(), Self::Error> {
        Ok(())
    }
}
