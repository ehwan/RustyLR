use rusty_lr_core::TerminalSymbol;

use super::Grammar;
use super::ResolvedAllowTarget;
use super::Terminal;
use super::TerminalClass;
use crate::error::Info;
use crate::error::Warning;

impl Grammar {
    fn is_terminal_allowed_by_target(&self, term: Terminal, target: &ResolvedAllowTarget) -> bool {
        match target {
            ResolvedAllowTarget::Name(name) => {
                let t_name = self.term_pretty_name(term);
                if &t_name == name {
                    return true;
                }
                let class_idx = self.terminal_class_id[term];
                let class_name = self.class_pretty_name_abbr(class_idx);
                if &class_name == name {
                    return true;
                }
                false
            }
            ResolvedAllowTarget::Terminals(set) => set.contains(&term),
        }
    }

    fn is_symbol_allowed(
        &self,
        term: &TerminalSymbol<TerminalClass>,
        scopes: &[Option<ResolvedAllowTarget>],
    ) -> bool {
        match term {
            TerminalSymbol::VirtualStart(_) => return true,
            TerminalSymbol::Error => {
                for opt_target in scopes {
                    match opt_target {
                        None => return true,
                        Some(ResolvedAllowTarget::Name(name)) if name == "error" => return true,
                        _ => {}
                    }
                }
            }
            TerminalSymbol::Eof => {
                for opt_target in scopes {
                    match opt_target {
                        None => return true,
                        Some(ResolvedAllowTarget::Name(name)) if name == "$" => return true,
                        _ => {}
                    }
                }
            }
            TerminalSymbol::Terminal(class_idx) => {
                let class_name = self.class_pretty_name_abbr(*class_idx);
                for opt_target in scopes {
                    match opt_target {
                        None => return true,
                        Some(target) => {
                            if let ResolvedAllowTarget::Name(name) = target {
                                if name == &class_name {
                                    return true;
                                }
                            }
                            let terminals = &self.terminal_classes[*class_idx].terminals;
                            for &t in terminals {
                                if self.is_terminal_allowed_by_target(t, target) {
                                    return true;
                                }
                            }
                        }
                    }
                }
            }
        }
        false
    }

    pub fn is_warning_allowed(&self, warning: &Warning) -> bool {
        if let Some(scopes) = self.allowed_diagnostics.get(warning.name()) {
            for opt_target in scopes {
                match opt_target {
                    None => return true,
                    Some(target) => match warning {
                        Warning::NonTermUnreachable { nonterm_name }
                        | Warning::UnusedNonTermData { nonterm_name }
                        | Warning::NonTermUnproductive { nonterm_name } => {
                            if let ResolvedAllowTarget::Name(name) = target {
                                if name == nonterm_name.value() {
                                    return true;
                                }
                            }
                        }
                        Warning::UnusedTerminals { class_idx } => {
                            if let ResolvedAllowTarget::Name(name) = target {
                                let class_name = self.class_pretty_name_abbr(*class_idx);
                                if name == &class_name {
                                    return true;
                                }
                            }
                        }
                    },
                }
            }

            if let Warning::UnusedTerminals { class_idx } = warning {
                let class_def = &self.terminal_classes[*class_idx];
                let unused_terms: Vec<Terminal> = class_def
                    .terminals
                    .iter()
                    .copied()
                    .filter(|&t| t != self.other_terminal_index)
                    .collect();

                if unused_terms.is_empty() {
                    return true;
                }

                let mut all_unused_ignored = true;
                for &t in &unused_terms {
                    let mut t_ignored = false;
                    for opt_target in scopes {
                        match opt_target {
                            None => {
                                t_ignored = true;
                                break;
                            }
                            Some(target) => {
                                if self.is_terminal_allowed_by_target(t, target) {
                                    t_ignored = true;
                                    break;
                                }
                            }
                        }
                    }
                    if !t_ignored {
                        all_unused_ignored = false;
                        break;
                    }
                }
                if all_unused_ignored {
                    return true;
                }
            }
        }
        false
    }

    pub fn is_info_allowed(&self, info: &Info) -> bool {
        if let Some(scopes) = self.allowed_diagnostics.get(info.name()) {
            for opt_target in scopes {
                if opt_target.is_none() {
                    return true;
                }
            }
            match info {
                Info::UnitProductionEliminated { nonterm_name, .. } => {
                    for opt_target in scopes {
                        if let Some(ResolvedAllowTarget::Name(name)) = opt_target {
                            if name == nonterm_name.value() {
                                return true;
                            }
                        }
                    }
                }
                Info::GlrOptionalExpanded { nonterm_name, .. } => {
                    for opt_target in scopes {
                        if let Some(ResolvedAllowTarget::Name(name)) = opt_target {
                            if name == nonterm_name.value() {
                                return true;
                            }
                        }
                    }
                }
                Info::RedundantRuleRemoved { rule_location } => {
                    for nonterm in &self.nonterminals {
                        for rule in &nonterm.rules {
                            if rule.location() == *rule_location {
                                for opt_target in scopes {
                                    if let Some(ResolvedAllowTarget::Name(name)) = opt_target {
                                        if name == nonterm.name.value() {
                                            return true;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                Info::TerminalsMerged { class_idx } => {
                    let class_name = self.class_pretty_name_abbr(*class_idx);
                    for opt_target in scopes {
                        if let Some(target) = opt_target {
                            if let ResolvedAllowTarget::Name(name) = target {
                                if name == &class_name {
                                    return true;
                                }
                            }
                            let terminals = &self.terminal_classes[*class_idx].terminals;
                            for &t in terminals {
                                if self.is_terminal_allowed_by_target(t, target) {
                                    return true;
                                }
                            }
                        }
                    }
                }
                Info::ReduceReduceConflictResolved { reduce_rules, .. } => {
                    for &r in reduce_rules {
                        if let Some((nonterm, _)) = self.get_rule_by_id(r) {
                            for opt_target in scopes {
                                if let Some(ResolvedAllowTarget::Name(name)) = opt_target {
                                    if name == nonterm.name.value() {
                                        return true;
                                    }
                                }
                            }
                        }
                    }
                }
                Info::ShiftReduceConflictResolvedShift { term, .. }
                | Info::ShiftReduceConflictResolvedReduce { term, .. }
                | Info::ShiftReduceConflictGLR { term, .. } => {
                    if self.is_symbol_allowed(term, scopes) {
                        return true;
                    }
                }
                Info::ReduceReduceConflictGLR { terms, .. } => {
                    for term in terms {
                        if self.is_symbol_allowed(term, scopes) {
                            return true;
                        }
                    }
                }
            }
        }
        false
    }
}
