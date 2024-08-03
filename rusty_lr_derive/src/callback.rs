use super::error::ParseError;
use super::grammar::Grammar;
use super::rule::RuleLine;
use super::rule::RuleLines;
use super::term::TermType;
use super::token::Token;

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
    pub tokens_stack: Vec<Vec<Token>>,
    pub token_stack: Vec<Token>,
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
                    _ => unreachable!("Rule0 - Ident"),
                };

                let ruletype = self.ruletype_stack.pop().expect("Rule0 - RuleType");

                let rulelines = self.rulelines_stack.pop().expect("Rule0 - RuleLines");

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
                        unreachable!("Rule1 - Group");
                    }
                    self.ruletype_stack.push(ruletype);
                } else {
                    unreachable!("Rule1");
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
                        unreachable!("Rule3 - 2");
                    }
                } else {
                    unreachable!("Rule3 - 1");
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
                    unreachable!("Rule4");
                }
            }

            // RuleLine: RuleDef Action
            //      RuleDef -> Tokens
            5 => {
                // Action
                let action = match self.action_stack.pop() {
                    Some(action) => action,
                    None => unreachable!("Rule5 - Action"),
                };

                // RuleDef
                let mut tokens = match self.tokens_stack.pop() {
                    Some(tokens) => tokens,
                    _ => unreachable!("Rule5 - RuleDef"),
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
                    _ => unreachable!("Rule9 - Token"),
                };

                let mut tokens = match self.tokens_stack.pop() {
                    Some(tokens) => tokens,
                    _ => unreachable!("Rule9 - Tokens"),
                };
                tokens.push(token);
                self.tokens_stack.push(tokens);
            }

            // TokensOne: Token
            10 => {
                let token = match self.token_stack.pop() {
                    Some(token) => token,
                    _ => unreachable!("Rule9 - Token"),
                };
                self.tokens_stack.push(vec![token]);
            }

            // Token: Ident
            11 => match self.termstack.pop() {
                Some(TermType::Ident(ident)) => {
                    self.token_stack.push(Token::NonTerm(ident.unwrap()));
                }
                _ => {
                    unreachable!("Rule11 - Ident");
                }
            },

            // Action: Group
            12 => match self.termstack.pop() {
                Some(TermType::Group(group)) => {
                    if let Some(action) = &group {
                        // check if action is enclosed with '{' and '}'
                        if action.delimiter() != proc_macro2::Delimiter::Brace {
                            return Err(ParseError::InvliadReduceActionDelimiter(action.span()));
                        }
                    } else {
                        unreachable!("Rule12 - Group");
                    }
                    self.action_stack.push(group);
                }
                _ => {
                    unreachable!("Rule12 - Group");
                }
            },

            // Action:
            13 => {
                self.action_stack.push(None);
            }

            // TokenDef: '%token' Ident RustCode ';'
            14 => {
                // ';'
                self.termstack.pop();

                // RustCode
                // ....

                // Ident
                let ident = match self.termstack.pop() {
                    Some(TermType::Ident(ident)) => ident.unwrap(),
                    _ => unreachable!("Rule14 - Ident"),
                };

                let rustcode = match self.rustcode_stack.pop() {
                    Some(rustcode) => rustcode,
                    _ => unreachable!("Rule14 - RustCode"),
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
            15 => {}
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

            // AnyTokens: AnyTokenNoSemi AnyTokens
            29 => {
                // AnyTokenNoSemi
                let token = match self.termstack.pop() {
                    Some(token) => token.stream(),
                    _ => unreachable!("Rule29 - AnyTokenNoSemi"),
                };

                // AnyTokens
                let rustcode = match self.rustcode_stack.pop() {
                    Some(tokens) => tokens,
                    _ => unreachable!("Rule29 - AnyTokens"),
                };
                self.rustcode_stack.push(quote! { #token #rustcode });
            }

            // AnyTokens: AnyTokenNoSemi
            30 => {
                // AnyTokenNoSemi
                let token = match self.termstack.pop() {
                    Some(token) => token.stream(),
                    _ => unreachable!("Rule29 - AnyTokenNoSemi"),
                };
                self.rustcode_stack.push(token);
            }

            // RustCode: AnyTokens
            31 => {}

            // StartDef: '%start' Ident ';'
            32 => {
                // ';'
                self.termstack.pop();

                // Ident
                let ident = match self.termstack.pop() {
                    Some(TermType::Ident(ident)) => ident.unwrap(),
                    _ => unreachable!("Rule32 - Ident"),
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
            33 => {
                // ';'
                self.termstack.pop();

                // RustCode
                let rustcode = match self.rustcode_stack.pop() {
                    Some(rustcode) => rustcode,
                    _ => unreachable!("Rule33 - RustCode"),
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
            34 => {
                // ';'
                self.termstack.pop();

                // RustCode
                let rustcode = match self.rustcode_stack.pop() {
                    Some(rustcode) => rustcode,
                    _ => unreachable!("Rule34 - RustCode"),
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
            35 => {
                // ';'
                self.termstack.pop();

                // RustCode
                let rustcode = match self.rustcode_stack.pop() {
                    Some(rustcode) => rustcode,
                    _ => unreachable!("Rule35 - RustCode"),
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
            36 => {
                // ';'
                self.termstack.pop();

                // Ident
                let ident = match self.termstack.pop() {
                    Some(TermType::Ident(ident)) => ident.unwrap(),
                    _ => unreachable!("Rule36 - Ident"),
                };
                self.grammar
                    .reduce_types
                    .insert(ident.to_string(), (ident, rusty_lr_core::ReduceType::Left));

                // '%left'
                self.termstack.pop();
            }

            // ReduceDef: '%right' Ident ';'
            37 => {
                // ';'
                self.termstack.pop();

                // Ident
                let ident = match self.termstack.pop() {
                    Some(TermType::Ident(ident)) => ident.unwrap(),
                    _ => unreachable!("Rule37 - Ident"),
                };
                self.grammar
                    .reduce_types
                    .insert(ident.to_string(), (ident, rusty_lr_core::ReduceType::Left));

                // '%right'
                self.termstack.pop();
            }

            // Grammar: Rule Grammar
            38 => {}

            // Grammar: Rule
            39 => {}

            // Grammar: TokenDef Grammar
            40 => {}

            // Grammar: TokenDef
            41 => {}

            // Grammar: StartDef Grammar
            42 => {}

            // Grammar: StartDef
            43 => {}

            // Grammar: EofDef Grammar
            44 => {}

            // Grammar: EofDef
            45 => {}

            // Grammar: TokenTypeDef Grammar
            46 => {}

            // Grammar: TokenTypeDef
            47 => {}

            // Grammar: UserDataDef Grammar
            48 => {}

            // Grammar: UserDataDef
            49 => {}

            // Grammar: ReduceDef Grammar
            50 => {}

            // Grammar: ReduceDef
            51 => {}

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
