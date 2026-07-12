use proc_macro2::TokenStream;
use rusty_lr_core::Symbol;
use rusty_lr_core::TerminalSymbol;
use std::collections::BTreeMap;
use std::collections::BTreeSet;

use super::CustomSingleReduceAction;
use super::Grammar;
use super::TerminalClass;
use super::TerminalClassDefinition;
use crate::error::Info;
use crate::error::Warning;
use crate::nonterminal_info::ReduceAction;
use crate::nonterminal_info::Rule;
use crate::parser::location::Located;
use crate::terminal_info::TerminalName;
use crate::utils;

impl Grammar {
    fn ruletype_key(ruletype: &TokenStream, boxed: bool) -> (String, bool) {
        (
            ruletype
                .to_string()
                .chars()
                .filter(|c| !c.is_whitespace())
                .collect(),
            boxed,
        )
    }

    fn symbol_data_key(
        &self,
        symbol: Symbol<TerminalSymbol<TerminalClass>, usize>,
    ) -> Option<(String, bool)> {
        match symbol {
            Symbol::Terminal(TerminalSymbol::Terminal(term)) => self.terminal_classes[term]
                .data_used
                .then(|| Self::ruletype_key(&self.token_typename, self.is_tokentype_boxed)),
            Symbol::Terminal(
                TerminalSymbol::Error | TerminalSymbol::Eof | TerminalSymbol::VirtualStart(_),
            ) => None,
            Symbol::NonTerminal(nonterm) => {
                let nonterminal = &self.nonterminals[nonterm];
                nonterminal
                    .ruletype
                    .as_ref()
                    .map(|ruletype| Self::ruletype_key(ruletype, nonterminal.ruletype_boxed))
            }
        }
    }

    pub(super) fn reduce_only_state_bypass_push(
        &self,
        nonterm_idx: usize,
        rule: &Rule,
    ) -> Option<bool> {
        match &rule.reduce_action {
            None => Some(false),
            Some(ReduceAction::Identity(identity_idx)) => {
                // Identity actions may still be required to rewrap the semantic value into
                // a different Data enum variant. Bypass only when both sides use the same
                // generated storage variant, approximated by the normalized Rust type plus
                // boxedness.
                let lhs_key = self.symbol_data_key(Symbol::NonTerminal(nonterm_idx))?;
                let rhs_key = self.symbol_data_key(rule.tokens[*identity_idx].symbol)?;
                (lhs_key == rhs_key).then_some(true)
            }
            Some(ReduceAction::Custom(_)) => None,
        }
    }

    /// optimize grammar
    fn optimize_iterate(&mut self) -> bool {
        // for early stopping optimization loop
        let mut something_changed = false;

        // We are trying to find the 'minimum partitioning' of terminals
        // First we collect all the *groups* of terminals
        // Then we calculate the *minimal partitioning* to compress the groups
        let mut term_sets = BTreeSet::new();
        term_sets.insert((0..self.terminal_classes.len()).collect());

        // collect precedence orders
        // all terminals in one class must have same precedence order
        let mut precedence_sets: BTreeMap<_, BTreeSet<usize>> = Default::default();
        for (term_idx, term) in self.terminals.iter().enumerate() {
            let class = self.terminal_class_id[term_idx];
            let level = term.precedence.map(Located::into_value);
            precedence_sets.entry(level).or_default().insert(class);
        }
        term_sets.extend(precedence_sets.into_values());

        // collect {set of terminals} that have same prefix-suffix-reduce_action in the production rules
        // so we can merge those terminals into one class
        // e.g.
        // consider the following state:
        //      A -> X x Y
        //      A -> X y Y
        //      A -> X z Y
        // here, we can group {x, y, z} into one class and merge them
        //      A -> X <class> Y
        for nonterm_def in self.nonterminals.iter() {
            let mut same_ruleset = BTreeMap::new();
            for rule in &nonterm_def.rules {
                for (token_idx, term_mapped) in rule.tokens.iter().enumerate() {
                    if let Symbol::Terminal(TerminalSymbol::Terminal(term)) = term_mapped.symbol {
                        // if this rule has reduce action, and it is not auto-generated,
                        // this terminal should be completely distinct from others (for user-defined inspection action)
                        // so put this terminal into separate class
                        if rule.reduce_action.is_some()
                            && !rule.reduce_action.as_ref().unwrap().is_identity()
                        {
                            term_sets.insert(BTreeSet::from([term]));
                            continue;
                        }

                        // tokens before this token
                        let prefix = rule
                            .tokens
                            .iter()
                            .take(token_idx)
                            .map(|token| (token.symbol, &token.reduce_action_chains))
                            .collect::<Vec<_>>();
                        // tokens after this token
                        let suffix = rule
                            .tokens
                            .iter()
                            .skip(token_idx + 1)
                            .map(|token| (token.symbol, &token.reduce_action_chains))
                            .collect::<Vec<_>>();
                        let reduce_chains = &term_mapped.reduce_action_chains;
                        let prec = rule.prec.map(Located::into_value);
                        let dprec = rule.dprec.map_or(0, Located::into_value);
                        let reduce_action_token_index =
                            rule.reduce_action
                                .as_ref()
                                .map(|reduce_action| match reduce_action {
                                    ReduceAction::Identity(idx) => *idx,
                                    _ => unreachable!("only identity reduce action should be here"),
                                });

                        if !same_ruleset
                            .entry((
                                prefix,
                                suffix,
                                reduce_chains,
                                prec,
                                dprec,
                                reduce_action_token_index,
                            ))
                            .or_insert_with(BTreeSet::new)
                            .insert(term)
                        {
                            // if it is false, it is reduce/reduce conflict (duplicated rule)
                            // so stop optimization
                            return false;
                        }
                    }
                }
            }
            term_sets.extend(same_ruleset.into_values());
        }

        let term_partition = crate::partition::minimal_partition(
            term_sets.into_iter().map(|terms| terms.into_iter()),
        );

        if term_partition.len() != self.terminal_classes.len() {
            something_changed = true;

            // convert all terminals using terminal class
            // delete all rules that using non-first terminals in that class
            // e.g. delete
            //      A -> X y Y
            //      A -> X z Y
            // and convert x to class so that only
            //      A -> X <class> Y
            // remains
            let mut is_first_oldclass_in_newclass = Vec::new();
            is_first_oldclass_in_newclass.resize(self.terminal_classes.len(), false);

            let mut old_class_to_new_class = vec![0; self.terminal_classes.len()];

            let mut new_class_defs = Vec::with_capacity(term_partition.len());
            let mut new_term_class_id = vec![0; self.terminals.len()];
            let mut multiterm_counter = 0;
            for (new_class_id, (_setids, old_classes)) in term_partition.into_iter().enumerate() {
                let mut terms = Vec::new();
                // terms.len() != len because single terminal could be range-based optimized into multiple characters
                let mut len = 0;
                for &old_class in old_classes.iter() {
                    for &term in &self.terminal_classes[old_class].terminals {
                        new_term_class_id[term] = new_class_id;
                        terms.push(term);

                        let name = &self.terminals[term].name;
                        len += name.count();
                    }
                    old_class_to_new_class[old_class] = new_class_id;
                }
                is_first_oldclass_in_newclass[old_classes[0]] = true;
                if len > 1 {
                    multiterm_counter += 1;
                }
                let class_def = TerminalClassDefinition {
                    terminals: terms,
                    multiterm_counter,
                    ranges: Vec::new(),
                    data_used: self.terminal_classes[old_classes[0]].data_used,
                };
                new_class_defs.push(class_def);
            }

            self.terminal_class_id = new_term_class_id;
            self.terminal_classes = new_class_defs;
            self.other_terminal_class_id = self.terminal_class_id[self.other_terminal_index];
            // terminal class optimization ends

            let mut other_was_used = false;
            for nonterm in &mut self.nonterminals {
                let rules = std::mem::take(&mut nonterm.rules);
                let mut new_rules = Vec::new();
                for mut rule in rules {
                    // check if this rule contains any terminal that is not the first terminal in the class
                    let mut remove_this_rule = false;
                    for token in &rule.tokens {
                        if let Symbol::Terminal(TerminalSymbol::Terminal(old_class)) = token.symbol
                        {
                            if !is_first_oldclass_in_newclass[old_class] {
                                remove_this_rule = true;
                                break;
                            }
                        }
                    }

                    if remove_this_rule {
                        // this rule contains terminal that is not the first terminal in the class
                        // so remove this rule
                        // add to diags only if it was not auto-generated
                        if !nonterm.is_auto_generated() {
                            self.infos.push(Info::RedundantRuleRemoved {
                                rule_location: rule.location(),
                            });
                        }
                        continue;
                    }

                    // change any terminal to its class id

                    //  - tokens in the rule
                    for token in &mut rule.tokens {
                        if let Symbol::Terminal(TerminalSymbol::Terminal(old_class)) = token.symbol
                        {
                            let new_class = old_class_to_new_class[old_class];
                            if new_class == self.other_terminal_class_id {
                                other_was_used = true;
                            }
                            token.symbol = Symbol::Terminal(TerminalSymbol::Terminal(new_class));
                        }
                    }
                    new_rules.push(rule);
                }
                nonterm.rules = new_rules;
            }

            self.other_used = other_was_used;
        }

        // Reachability analysis from the start symbols
        let mut nonterm_used = vec![false; self.nonterminals.len()];
        let mut queue = Vec::new();

        for start_rule_name in &self.start_rule_names {
            let start_idx_opt = self.nonterminals_index.get(start_rule_name.value());
            if let Some(&start_idx) = start_idx_opt {
                nonterm_used[start_idx] = true;
                queue.push(start_idx);
            }
        }

        // Also queue protected non-terminals
        for (i, nonterm) in self.nonterminals.iter().enumerate() {
            if nonterm.is_protected() && !nonterm_used[i] {
                nonterm_used[i] = true;
                queue.push(i);
            }
        }

        // DFS reachability
        while let Some(nt_idx) = queue.pop() {
            for rule in &self.nonterminals[nt_idx].rules {
                for token in &rule.tokens {
                    if let Symbol::NonTerminal(next_nt) = token.symbol {
                        if !nonterm_used[next_nt] {
                            nonterm_used[next_nt] = true;
                            queue.push(next_nt);
                        }
                    }
                }
            }
        }

        for (nonterm_idx, nonterm) in self.nonterminals.iter_mut().enumerate() {
            // do not delete protected non-terminals
            if nonterm.is_protected() {
                continue;
            }
            if nonterm.rules.is_empty() {
                continue;
            }

            if !nonterm_used[nonterm_idx] {
                // this rule was not used
                nonterm.rules.clear();
                something_changed = true;
                if !nonterm.is_auto_generated() {
                    self.warnings.push(Warning::NonTermUnreachable {
                        nonterm_name: nonterm.name.clone(),
                    });
                }
            }
        }

        // Productivity Analysis
        // A non-terminal is productive if it has at least one rule that contains only productive symbols (terminals, or productive non-terminals).
        let mut productive = vec![false; self.nonterminals.len()];
        let mut prod_changed = true;

        while prod_changed {
            prod_changed = false;
            for (i, nonterm) in self.nonterminals.iter().enumerate() {
                if productive[i] {
                    continue;
                }

                let mut is_productive = false;
                for rule in &nonterm.rules {
                    let mut rule_productive = true;
                    for token in &rule.tokens {
                        if let Symbol::NonTerminal(next_nt) = token.symbol {
                            if !productive[next_nt] {
                                rule_productive = false;
                                break;
                            }
                        }
                    }
                    if rule_productive {
                        is_productive = true;
                        break;
                    }
                }

                if is_productive {
                    productive[i] = true;
                    prod_changed = true;
                }
            }
        }

        // Remove rules containing unproductive non-terminals
        for (nonterm_idx, nonterm) in self.nonterminals.iter_mut().enumerate() {
            if nonterm.is_protected() && nonterm.rules.is_empty() {
                continue;
            }

            let original_rule_count = nonterm.rules.len();
            if original_rule_count == 0 {
                continue;
            }

            nonterm.rules.retain(|rule| {
                rule.tokens.iter().all(|token| {
                    if let Symbol::NonTerminal(next_nt) = token.symbol {
                        productive[next_nt]
                    } else {
                        true
                    }
                })
            });

            if nonterm.rules.len() < original_rule_count {
                something_changed = true;
            }

            // If it lost all its rules and is now unproductive
            if nonterm.rules.is_empty() && !productive[nonterm_idx] && !nonterm.is_auto_generated()
            {
                self.warnings.push(Warning::NonTermUnproductive {
                    nonterm_name: nonterm.name.clone(),
                });
            }
        }

        // remove rules that have single production rule and single token
        // e.g. A -> B, then fix all occurrences of A to B
        let mut nonterm_replace = BTreeMap::new();

        // in one optimize iteration, do not allow optimize-chains (e.g. A -> B, B -> C)
        let mut optimize_related_nonterminals = BTreeSet::new();

        for (nonterm_id, nonterm) in self.nonterminals.iter().enumerate() {
            // do not delete protected non-terminals
            if nonterm.is_protected() {
                continue;
            }
            if nonterm.rules.len() != 1 {
                continue;
            }
            let rule = &nonterm.rules[0];
            if rule.dprec.is_some() {
                // this rule has %dprec, so do not optimize
                continue;
            }
            if rule.prec.is_some() {
                // this rule has %prec, so do not optimize
                continue;
            }
            if rule.tokens.len() != 1 {
                continue;
            }
            if optimize_related_nonterminals.contains(&nonterm_id) {
                continue;
            }
            let totoken = rule.tokens[0].symbol;
            if let Symbol::NonTerminal(to_nonterm_id) = totoken {
                if optimize_related_nonterminals.contains(&to_nonterm_id) {
                    continue;
                }
            }

            if totoken == Symbol::NonTerminal(nonterm_id) {
                // A -> A cycle, do not optimize
                continue;
            }

            let mut reduce_action_chain = rule.tokens[0].reduce_action_chains.clone();

            if let Some(ReduceAction::Custom(body)) = &rule.reduce_action {
                if rule.reduce_action_contains_ident(utils::LOOKAHEAD_PARAMETER_NAME) {
                    continue;
                }
                // if this rule has custom reduce action, save it
                let output_type = nonterm.ruletype.clone();
                let mapto = &rule.tokens[0].mapto;
                let location_mapto = if let Some(mapto) = mapto {
                    let location_varname = utils::location_variable_name(mapto.value().as_str());
                    if rule.reduce_action_contains_ident(&location_varname) {
                        Some(location_varname)
                    } else {
                        None
                    }
                } else {
                    None
                };

                let input_type = if let Some(mapto) = mapto {
                    let ruletype = match totoken {
                        Symbol::Terminal(_) => Some(self.token_typename.clone()),
                        Symbol::NonTerminal(to_nonterm_id) => {
                            self.nonterminals[to_nonterm_id].ruletype.clone()
                        }
                    };

                    if let Some(ruletype) = ruletype {
                        if rule.reduce_action_contains_ident(mapto.value().as_str()) {
                            Some((mapto.value().clone(), ruletype))
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                } else {
                    None
                };
                let idx = self.custom_reduce_actions.len();
                self.custom_reduce_actions.push(CustomSingleReduceAction {
                    body: body.clone(),
                    input_type,
                    input_location: location_mapto,
                    output_type,
                });
                reduce_action_chain.push(idx);
            }

            optimize_related_nonterminals.insert(nonterm_id);
            if let Symbol::NonTerminal(to_nonterm_id) = totoken {
                optimize_related_nonterminals.insert(to_nonterm_id);
            }

            nonterm_replace.insert(nonterm_id, (totoken, reduce_action_chain));
        }

        // replace all Symbol::NonTerminal that can be replaced into Symbol::Terminal calculated above
        for nonterm in self.nonterminals.iter_mut() {
            for rule in &mut nonterm.rules {
                for token in &mut rule.tokens {
                    if let Symbol::NonTerminal(nonterm_id) = token.symbol {
                        if let Some((newclass, custom_reduce_action)) =
                            nonterm_replace.get(&nonterm_id)
                        {
                            token.symbol = *newclass;
                            let mut new_reduce_chain = custom_reduce_action.clone();
                            new_reduce_chain
                                .append(&mut std::mem::take(&mut token.reduce_action_chains));
                            token.reduce_action_chains = new_reduce_chain;
                        }
                    }
                }
            }
        }

        // delete rules - keys of nonterm_replace
        something_changed |= !nonterm_replace.is_empty();
        for &nonterm_id in nonterm_replace.keys() {
            let nonterm = &mut self.nonterminals[nonterm_id];
            let rules = std::mem::take(&mut nonterm.rules);
            let rule = rules.into_iter().next().unwrap();
            // add to diags only if it was not auto-generated
            if !nonterm.is_auto_generated() {
                self.infos.push(Info::UnitProductionEliminated {
                    nonterm_name: nonterm.name.clone(),
                    rule_location: rule.location(),
                });
            }
        }

        something_changed
    }

    pub fn optimize(&mut self, max_iter: usize) {
        if !self.optimize {
            return;
        }

        // check if RuleType and ReduceAction can be removed from certain non-terminals
        let mut add_to_diags = BTreeSet::new();
        loop {
            let start_rule_indices: std::collections::HashSet<usize> = self
                .start_rule_names
                .iter()
                .map(|name| *self.nonterminals_index.get(name.value()).unwrap())
                .collect();
            let mut changed = false;
            let mut can_removes = Vec::new();

            for (i, nonterm) in self.nonterminals.iter().enumerate() {
                if start_rule_indices.contains(&i) {
                    // do not remove ruletype from start rules
                    continue;
                }

                if nonterm.ruletype.is_none() {
                    continue;
                }

                let mut can_remove = true;

                // check for every production rules,
                // if it is still compilable without this nonterminal's ruletype
                // if it is possible, we can remove this nonterminal's ruletype (and reduce action)
                for (j, nonterm_j) in self.nonterminals.iter().enumerate() {
                    if i == j {
                        if nonterm_j.is_auto_generated() {
                            // if nonterm_i is auto-generated, do not check self rules
                            continue;
                        }
                    }
                    for rule in nonterm_j.rules.iter() {
                        for token in rule.tokens.iter() {
                            if token.symbol != Symbol::NonTerminal(i) {
                                continue;
                            }

                            // nonterm_i's data was used in this rule
                            let used = if let Some(mapto) = &token.mapto {
                                rule.reduce_action_contains_ident(mapto.value().as_str())
                            } else {
                                false
                            };

                            if used {
                                // nonterm_i's data cannot be removed
                                can_remove = false;
                                break;
                            }
                        }
                        if !can_remove {
                            break;
                        }
                    }
                    if !can_remove {
                        break;
                    }
                }

                if can_remove {
                    can_removes.push(i);
                }
            }

            for i in can_removes {
                let nonterm = &mut self.nonterminals[i];
                if nonterm.ruletype.is_some() {
                    changed = true;
                    nonterm.ruletype = None;
                }
                if nonterm.is_auto_generated() {
                    for rule in &mut nonterm.rules {
                        if rule.reduce_action.is_some() {
                            changed = true;
                            rule.reduce_action = None;
                        }
                    }
                } else {
                    for rule in &mut nonterm.rules {
                        if let Some(reduce_action) = &rule.reduce_action {
                            match reduce_action {
                                ReduceAction::Custom(_) => {
                                    // cannot remove custom reduce action;
                                    // add to diag
                                    add_to_diags.insert(i);
                                }
                                ReduceAction::Identity(_) => {
                                    changed = true;
                                    rule.reduce_action = None;
                                }
                            }
                        }
                    }
                }
            }

            if !changed {
                break;
            }
        }
        for i in add_to_diags {
            let nonterm = &self.nonterminals[i];
            self.warnings.push(Warning::UnusedNonTermData {
                nonterm_name: nonterm.name.clone(),
            });
        }

        // check for any data of terminal symbol was used in any reduce action
        for class_def in &mut self.terminal_classes {
            class_def.data_used = false;
        }
        for nonterm in &self.nonterminals {
            for rule in &nonterm.rules {
                for token in &rule.tokens {
                    if let Symbol::Terminal(TerminalSymbol::Terminal(term)) = token.symbol {
                        if let Some(mapto) = &token.mapto {
                            if rule.reduce_action_contains_ident(mapto.value().as_str()) {
                                self.terminal_classes[term].data_used = true;
                            }
                        }
                    }
                }
            }
        }

        for _ in 0..max_iter {
            if !self.optimize_iterate() {
                break;
            }
        }

        // remove nonterminals from Vecs which are deleted in the optimization
        // nonterm idx remapping
        let mut nonterm_old_to_new = vec![0; self.nonterminals.len()];
        let mut new_idx = 0;
        for (old_idx, nonterm) in self.nonterminals.iter().enumerate() {
            if nonterm.rules.is_empty() && !nonterm.is_protected() {
                continue;
            }
            nonterm_old_to_new[old_idx] = new_idx;
            new_idx += 1;
        }
        self.nonterminals = std::mem::take(&mut self.nonterminals)
            .into_iter()
            .filter(|nonterm| nonterm.is_protected() || !nonterm.rules.is_empty())
            .collect();
        self.nonterminals_index.clear();
        for (idx, nonterm) in self.nonterminals.iter().enumerate() {
            self.nonterminals_index
                .insert(nonterm.name.value().clone(), idx);
        }
        for nonterm in &mut self.nonterminals {
            for rule in &mut nonterm.rules {
                for token in &mut rule.tokens {
                    if let Symbol::NonTerminal(nonterm_idx) = token.symbol {
                        token.symbol = Symbol::NonTerminal(nonterm_old_to_new[nonterm_idx]);
                    }
                }
            }
        }

        if self.is_char || self.is_u8 {
            // calculate ranges
            for class in self.terminal_classes.iter_mut() {
                let mut ranges0 = class
                    .terminals
                    .iter()
                    .filter_map(|&term_idx| {
                        if term_idx == self.other_terminal_index {
                            None
                        } else {
                            let name = &self.terminals[term_idx].name;
                            if let TerminalName::CharRange(start, last) = name {
                                Some((*start as u32, *last as u32))
                            } else {
                                unreachable!("terminal name should be char range");
                            }
                        }
                    })
                    .collect::<Vec<_>>();
                ranges0.sort();

                let mut ranges: Vec<(u32, u32)> = Vec::new();
                for (s, l) in ranges0 {
                    match ranges.last_mut() {
                        Some((_, last)) if *last + 1 == s => {
                            *last = l;
                        }
                        _ => {
                            ranges.push((s, l));
                        }
                    }
                }
                class.ranges = ranges;
            }
        }

        // Collect optimization warnings and notes/infos
        for (class_idx, class_def) in self.terminal_classes.iter().enumerate() {
            let len: usize = class_def
                .terminals
                .iter()
                .map(|term| self.terminals[*term].name.count())
                .sum();
            if len == 1 {
                continue;
            }
            self.infos.push(Info::TerminalsMerged { class_idx });
        }

        // if other terminals were not used, print warning about removing them
        let other_terminal_class = &self.terminal_classes[self.other_terminal_class_id];
        if !self.other_used && other_terminal_class.terminals.len() > 1 {
            self.warnings.push(Warning::UnusedTerminals {
                class_idx: self.other_terminal_class_id,
            });
        }
    }
}
