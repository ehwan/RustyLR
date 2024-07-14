use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::vec::Vec;

use thiserror::Error;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Term {
    Id,
    Plus,
    Star,
    LeftParen,
    RightParen,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Token {
    Term(Term),
    NonTerm(String),

    // special token for end of input stream
    End,
}
impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Term(term) => write!(f, "{:?}", term),
            Token::NonTerm(rule) => write!(f, "{}", rule),
            Token::End => write!(f, "$"),
        }
    }
}

/// A struct for single shifted named production rule
/// name -> Token1 Token2 . Token3
///         ^^^^^^^^^^^^^ shifted = 2
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct NamedShiftedRule {
    pub name: String,
    pub rule: Vec<Token>,
    pub shifted: usize,
}
impl NamedShiftedRule {
    pub fn new(name: String, rule: Vec<Token>, shifted: usize) -> Self {
        NamedShiftedRule {
            name,
            rule,
            shifted,
        }
    }

    /// get first token of shifted rule
    pub fn first(&self) -> Option<&Token> {
        self.rule.get(self.shifted)
    }
    /// get rest of the shifted rule (excluding first token)
    pub fn rest(&self) -> &[Token] {
        &self.rule[self.shifted + 1..]
    }
}
impl std::fmt::Display for NamedShiftedRule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} -> ", self.name)?;
        for (id, token) in self.rule.iter().enumerate() {
            if id == self.shifted {
                write!(f, ". ")?;
            }
            write!(f, "{} ", token)?;
        }
        Ok(())
    }
}

/// A struct for single shifted named production rule with lookahead tokens
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct LookaheadRule {
    pub rule: NamedShiftedRule,
    pub lookaheads: BTreeSet<Token>,
}
impl std::fmt::Display for LookaheadRule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} / ", self.rule)?;
        for lookahead in self.lookaheads.iter() {
            write!(f, "{}, ", lookahead)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct LookaheadRuleSet {
    pub rules: BTreeSet<LookaheadRule>,
}
impl LookaheadRuleSet {
    pub fn new() -> Self {
        LookaheadRuleSet {
            rules: BTreeSet::new(),
        }
    }
    pub fn add_rule(&mut self, rule: LookaheadRule) {
        self.rules.insert(rule);
    }
}
impl std::fmt::Display for LookaheadRuleSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for rule in self.rules.iter() {
            rule.fmt(f)?;
            writeln!(f)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum Action {
    Reduce(NamedShiftedRule),
    Goto(usize),
}
impl Action {
    /// if it is reduce action, feeded token should not be shifted; otherwise it should be shifted
    pub fn should_shift(&self) -> bool {
        match self {
            Action::Reduce(_) => false,
            Action::Goto(_) => true,
        }
    }

    /// get production rule if it is reduce action
    pub fn rule(self) -> Option<NamedShiftedRule> {
        match self {
            Action::Reduce(rule) => Some(rule),
            _ => None,
        }
    }
}
impl std::fmt::Display for Action {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Action::Reduce(rule) => write!(f, "Reduce {}", rule)?,
            Action::Goto(state) => write!(f, "Goto {}", state)?,
        }
        Ok(())
    }
}

/// A struct for state in LR(1) parser
#[derive(Debug, Clone)]
pub struct State {
    pub action_map: BTreeMap<Token, Action>,
}
impl State {
    pub fn new() -> Self {
        State {
            action_map: BTreeMap::new(),
        }
    }

    /// feed one token and get action
    pub fn feed<'a>(&'a self, token: &Token) -> Option<&'a Action> {
        self.action_map.get(token)
    }
}
impl std::fmt::Display for State {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (token, action) in self.action_map.iter() {
            write!(f, "{}: {}\n", token, action)?;
        }
        Ok(())
    }
}

#[derive(Error, Debug)]
pub enum BuildError {
    #[error("Rule not found: {0}")]
    RuleNotFound(LookaheadRule),

    /// reduce/reduce conflict
    #[error("Conflict in grammar: {0} and {1}")]
    ReduceReduceConflict(NamedShiftedRule, NamedShiftedRule),

    /// reduce/shift conflict
    #[error("Conflict in grammar:\n{0} and\n{1}")]
    ReduceShiftConflict(NamedShiftedRule, LookaheadRuleSet),

    #[error("Entry rule not set; use .set_main( entry_name: &str )")]
    EntryNotSet,

    #[error("Duplicated rule: {0}")]
    DuplicatedRule(LookaheadRule),
}

/// A set of production rules and main entry point
#[derive(Debug, Clone)]
pub struct Grammar {
    /// set of production rules
    pub rules: BTreeMap<String, Vec<Vec<Token>>>,

    /// name of main entry nonterminal rule
    pub main: Option<String>,

    /// first terminal tokens for each nonterminals
    /// true if it can be empty
    pub firsts: BTreeMap<String, (BTreeSet<Token>, bool)>,
}

impl Grammar {
    pub fn new() -> Self {
        Grammar {
            rules: BTreeMap::new(),
            main: None,
            firsts: BTreeMap::new(),
        }
    }

    /// add new production rule for given nonterminal 'name'
    pub fn add_rule(&mut self, name: &str, rule: Vec<Token>) {
        self.rules
            .entry(name.to_string())
            .or_insert(Vec::new())
            .push(rule);
    }
    /// set main entry nonterminal rule
    pub fn set_main(&mut self, name: &str) {
        self.main = Some(name.to_string());
    }

    /// build LR(1) parser table from given grammar
    pub fn build_main(&mut self) -> Result<Parser, BuildError> {
        for (_name, rules) in self.rules.iter_mut() {
            rules.sort();
        }

        self.calculate_first()?;

        if self.main.is_none() {
            return Err(BuildError::EntryNotSet);
        }
        let main_name = self.main.clone().unwrap();

        // add main augmented rule
        let augmented_rule_set = {
            let augmented_rule = NamedShiftedRule {
                name: main_name.clone() + "'",
                rule: vec![Token::NonTerm(main_name), Token::End],
                shifted: 0,
            };
            let augmented_rule = LookaheadRule {
                rule: augmented_rule,
                lookaheads: BTreeSet::new(),
            };
            LookaheadRuleSet {
                rules: BTreeSet::from([augmented_rule]),
            }
        };

        let mut states = Vec::new();
        let mut state_map = BTreeMap::new();
        let main_state = self.build(augmented_rule_set, &mut states, &mut state_map)?;
        Ok(Parser { states, main_state })
    }

    /// search for every production rules with name 'name'
    fn search_rules<'a>(&'a self, name: &str) -> Option<&'a Vec<Vec<Token>>> {
        self.rules.get(name)
    }
    /// calculate first terminals for each nonterminals
    fn calculate_first(&mut self) -> Result<(), BuildError> {
        loop {
            let mut changed = false;
            for (name, rules) in self.rules.iter() {
                let (mut firsts, mut canbe_empty) = self
                    .firsts
                    .entry(name.clone())
                    .or_insert_with(|| {
                        changed = true;
                        (BTreeSet::new(), false)
                    })
                    .clone();

                let mut this_nonterm_changed = false;
                for rule in rules.iter() {
                    let mut this_rule_canbe_empty = true;
                    'tokenfor: for token in rule.iter() {
                        match token {
                            Token::Term(_) | Token::End => {
                                let insert_result = firsts.insert(token.clone());
                                if insert_result {
                                    this_nonterm_changed = true;
                                }
                                this_rule_canbe_empty = false;
                                break 'tokenfor;
                            }
                            Token::NonTerm(nonterm) => {
                                if let Some((child_firsts, child_canbe_empty)) =
                                    self.firsts.get(nonterm)
                                {
                                    let old_len = firsts.len();
                                    firsts.extend(child_firsts.iter().cloned());
                                    if old_len != firsts.len() {
                                        this_nonterm_changed = true;
                                    }
                                    if !child_canbe_empty {
                                        this_rule_canbe_empty = false;
                                        break 'tokenfor;
                                    }
                                } else {
                                    this_rule_canbe_empty = false;
                                    break 'tokenfor;
                                }
                            }
                        }
                    }
                    if this_rule_canbe_empty && !canbe_empty {
                        canbe_empty = true;
                        this_nonterm_changed = true;
                    }
                }
                if this_nonterm_changed {
                    changed = true;
                    self.firsts.insert(name.clone(), (firsts, canbe_empty));
                }
            }
            if !changed {
                break;
            }
        }
        Ok(())
    }

    /// calculate lookahead tokens for given follow tokens and lookahead tokens
    /// this is equivalent to FIRST( follow_tokens lookahead )
    fn lookahead(
        &self,
        follow_tokens: &[Token],
        lookahead: &BTreeSet<Token>,
    ) -> Result<BTreeSet<Token>, BuildError> {
        let mut ret = BTreeSet::new();
        for token in follow_tokens.iter() {
            match token {
                Token::Term(_) | Token::End => {
                    ret.insert(token.clone());
                    return Ok(ret);
                }
                Token::NonTerm(nonterm) => {
                    let (firsts, canbe_empty) = self.firsts.get(nonterm).unwrap();
                    ret.extend(firsts.iter().cloned());
                    if !canbe_empty {
                        return Ok(ret);
                    }
                }
            }
        }
        ret.extend(lookahead.iter().cloned());
        Ok(ret)
    }

    /// for given set of production rules,
    /// if the first token of each rule is nonterminal, attach its production rules
    fn expand(&self, rules: &mut LookaheadRuleSet) -> Result<(), BuildError> {
        loop {
            let mut new_rules = Vec::new();
            for rule in rules.rules.iter() {
                if let Some(Token::NonTerm(ref rule_name)) = rule.rule.first() {
                    if let Some(searched_rules) = self.search_rules(rule_name) {
                        // calculate lookaheads
                        let lookaheads = self.lookahead(&rule.rule.rest(), &rule.lookaheads)?;

                        for searched_rule in searched_rules.iter() {
                            let rule_with_ahead = LookaheadRule {
                                rule: NamedShiftedRule {
                                    name: rule_name.clone(),
                                    rule: searched_rule.clone(),
                                    shifted: 0,
                                },
                                lookaheads: lookaheads.clone(),
                            };
                            new_rules.push(rule_with_ahead);
                        }
                    } else {
                        // rule not found
                        return Err(BuildError::RuleNotFound(rule.clone()));
                    }
                }
            }

            let old_len = rules.rules.len();
            rules.rules.extend(new_rules.into_iter());
            if rules.rules.len() == old_len {
                // nothing added newly; stop loop
                return Ok(());
            }
        }
    }

    /// build new state with given production rules
    fn build(
        &self,
        mut rules: LookaheadRuleSet,
        states: &mut Vec<State>,
        state_map: &mut BTreeMap<LookaheadRuleSet, usize>,
    ) -> Result<usize, BuildError> {
        // expand
        self.expand(&mut rules)?;

        // check if this set of production rules already exists
        if let Some(state_id) = state_map.get(&rules) {
            return Ok(*state_id);
        }

        // new state id
        let state_id = states.len();
        state_map.insert(rules.clone(), state_id);
        states.push(State::new());
        println!("State{}", state_id,);
        println!("Rules:\n{}", rules);

        let mut next_rules = BTreeMap::new();
        let mut empty_rules = Vec::new();
        for mut rule in rules.rules.into_iter() {
            match rule.rule.first().cloned() {
                Some(token) => {
                    rule.rule.shifted += 1;
                    let next_rule_set = next_rules.entry(token).or_insert(LookaheadRuleSet::new());
                    let insert_result = next_rule_set.rules.insert(rule.clone());
                    if insert_result == false {
                        return Err(BuildError::DuplicatedRule(rule));
                    }
                }
                None => {
                    empty_rules.push(rule);
                }
            }
        }

        // process empty rules
        // if next token is one of lookahead, add reduce action
        // if there are multiple recude rules for same lookahead, it is a reduce/reduce conflict
        for mut empty_rule in empty_rules.into_iter() {
            empty_rule.rule.shifted = 0;
            let action = Action::Reduce(empty_rule.rule);
            let lookaheads = empty_rule.lookaheads;
            let state = &mut states[state_id];
            for lookahead in lookaheads.into_iter() {
                if let Some(old) = state.action_map.get_mut(&lookahead) {
                    // conflict
                    return Err(BuildError::ReduceReduceConflict(
                        old.clone().rule().unwrap(),
                        action.clone().rule().unwrap(),
                    ));
                } else {
                    state.action_map.insert(lookahead, action.clone());
                }
            }
        }

        // process next rules with token
        for (next_token, next_rule) in next_rules.into_iter() {
            let next_state_id = self.build(next_rule.clone(), states, state_map)?;
            let action = Action::Goto(next_state_id);
            if let Some(old) = states[state_id].action_map.insert(next_token, action) {
                // reduce/shift conflict
                return Err(BuildError::ReduceShiftConflict(
                    old.clone().rule().unwrap(),
                    next_rule,
                ));
            }
        }

        Ok(state_id)
    }
}

pub struct Parser {
    pub states: Vec<State>,
    pub main_state: usize,
}

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("Invalid Token: {0}")]
    InvalidToken(Token),

    #[error("State Stack is empty")]
    StateStackEmpty,

    #[error("Stack is empty")]
    StackEmpty,

    #[error("Stack is not enough to reduce")]
    StackNotEnough,

    #[error("Invalid State: {0}")]
    InvalidState(usize),
}
impl Parser {
    /// feed one token to parser, and update state stack and stack
    fn feed(
        &self,
        state_stack: &mut Vec<usize>,
        stack: &mut Vec<Token>,
        token: &Token,
    ) -> Result<(), ParseError> {
        println!(
            "Feed: {}\nState stack: {:?}\nStack: {:?}",
            token, state_stack, stack
        );
        // fetch state from state stack
        let state = if let Some(state_id) = state_stack.last() {
            if let Some(state) = self.states.get(*state_id) {
                state
            } else {
                return Err(ParseError::InvalidState(*state_id));
            }
        } else {
            return Err(ParseError::StateStackEmpty);
        };

        // feed token to current state and get action
        let action = if let Some(action) = state.feed(token) {
            action
        } else {
            // TODO add curret context or production rule to error
            return Err(ParseError::InvalidToken(token.clone()));
        };

        if action.should_shift() {
            stack.push(token.clone());
        }
        match action {
            Action::Reduce(rule) => {
                // reduce items in stack
                println!("Reduce: {}", rule);
                if stack.len() < rule.rule.len() {
                    return Err(ParseError::StackNotEnough);
                }
                stack.truncate(stack.len() - rule.rule.len());

                // pop state from stack, number of tokens reduced
                state_stack.truncate(state_stack.len() - rule.rule.len());

                // feed reduced token
                let reduced_token = Token::NonTerm(rule.name.clone());
                self.feed(state_stack, stack, &reduced_token)?;
                self.feed(state_stack, stack, token)?;
            }
            Action::Goto(next_state_id) => {
                state_stack.push(*next_state_id);
            }
        }

        Ok(())
    }

    /// parse given tokens and return result
    pub fn parse(&self, tokens: &[Token]) -> Result<Token, ParseError> {
        let mut state_stack = Vec::new();
        state_stack.push(self.main_state);
        let mut stack = Vec::new();
        for token in tokens.iter() {
            self.feed(&mut state_stack, &mut stack, token)?;
        }

        // feed End token
        self.feed(&mut state_stack, &mut stack, &Token::End)?;

        let result = if let Some(result) = stack.get(0) {
            result
        } else {
            return Err(ParseError::StackEmpty);
        };
        Ok(result.clone())
    }
}

fn main() {
    let mut grammar = Grammar::new();

    grammar.add_rule(
        "A",
        vec![
            Token::NonTerm("A".to_string()),
            Token::Term(Term::Plus),
            Token::NonTerm("M".to_string()),
        ],
    );
    grammar.add_rule("A", vec![Token::NonTerm("M".to_string())]);

    grammar.add_rule(
        "M",
        vec![
            Token::NonTerm("M".to_string()),
            Token::Term(Term::Star),
            Token::NonTerm("P".to_string()),
        ],
    );
    grammar.add_rule("M", vec![Token::NonTerm("P".to_string())]);

    grammar.add_rule("P", vec![Token::Term(Term::Id)]);
    grammar.add_rule(
        "P",
        vec![
            Token::Term(Term::LeftParen),
            Token::NonTerm("E".to_string()),
            Token::Term(Term::RightParen),
        ],
    );
    grammar.add_rule("E", vec![Token::NonTerm("A".to_string())]);

    grammar.set_main("E");

    let parser = match grammar.build_main() {
        Ok(result) => result,
        Err(err) => {
            eprintln!("{}", err);
            return;
        }
    };
    println!("Number of states: {}", parser.states.len());
    println!("Main state: {}", parser.main_state);

    for (state_id, state) in parser.states.iter().enumerate() {
        println!("State{}", state_id);
        println!("{}", state);
    }

    let tokens = vec![
        Token::Term(Term::Id),
        Token::Term(Term::Plus),
        Token::Term(Term::Id),
        Token::Term(Term::Star),
        Token::Term(Term::LeftParen),
        Token::Term(Term::Id),
        Token::Term(Term::Plus),
        Token::Term(Term::Id),
        Token::Term(Term::RightParen),
    ];
    match parser.parse(&tokens) {
        Ok(result) => println!("Result: {}", result),
        Err(err) => eprintln!("{}", err),
    }
}
