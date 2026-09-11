use proc_macro2::TokenStream;
use rusty_lr_core::Symbol;
use rusty_lr_core::hash::{HashMap, HashSet};

use super::Grammar;
use crate::error::ParseError;
use crate::nonterminal_info::ReduceAction;
use crate::parser::location::Location;

// A resolved type of None means the nonterminal carries no semantic value.
enum TypeInference {
    Pending,
    Resolved(Option<TokenStream>),
}

impl Grammar {
    pub(super) fn resolve_placeholder_types(&mut self) -> Result<(), ParseError> {
        let mut resolved = HashMap::default();
        let mut unresolved = HashSet::default();
        for nonterminal in &self.nonterminals {
            if let Some(name) = get_placeholder_name(&nonterminal.ruletype) {
                unresolved.insert(name);
            }
        }

        loop {
            // Apply results together so all productions in a round see the same types.
            let resolved_this_round: Vec<_> = unresolved
                .iter()
                .filter_map(|name| {
                    let nonterminal_idx = self.placeholder_nonterminal(name)?;
                    match self.infer_identity_type(nonterminal_idx, &resolved) {
                        TypeInference::Pending => None,
                        TypeInference::Resolved(typename) => Some((name.clone(), typename)),
                    }
                })
                .collect();
            if resolved_this_round.is_empty() {
                break;
            }
            for (name, typename) in resolved_this_round {
                unresolved.remove(&name);
                resolved.insert(name, typename);
            }
        }

        if let Some(name) = unresolved.iter().next() {
            let location = self
                .placeholder_nonterminal(name)
                .map(|idx| self.nonterminals[idx].name.location())
                .unwrap_or(Location::CallSite);
            return Err(ParseError::TypeInferenceFailed(location));
        }

        // Generated helper nonterminals can contain placeholders inside compound types.
        for nonterminal in &mut self.nonterminals {
            nonterminal.ruletype = nonterminal
                .ruletype
                .as_ref()
                .and_then(|typename| substitute_placeholders(typename.clone(), &resolved));
        }
        Ok(())
    }

    fn placeholder_nonterminal(&self, name: &str) -> Option<usize> {
        self.nonterminals.iter().position(|nonterminal| {
            nonterminal
                .ruletype
                .as_ref()
                .is_some_and(|typename| typename.to_string() == name)
        })
    }

    fn infer_identity_type(
        &self,
        nonterminal_idx: usize,
        resolved: &HashMap<String, Option<TokenStream>>,
    ) -> TypeInference {
        for production in &self.nonterminals[nonterminal_idx].rules {
            let Some(ReduceAction::Identity(symbol_idx)) = &production.reduce_action else {
                continue;
            };
            let Some(symbol) = production.tokens.get(*symbol_idx) else {
                continue;
            };
            match symbol.symbol {
                Symbol::Terminal(_) => {
                    return TypeInference::Resolved(Some(self.token_typename.clone()));
                }
                Symbol::NonTerminal(target_idx) => {
                    let substituted = self.nonterminals[target_idx]
                        .ruletype
                        .as_ref()
                        .and_then(|typename| substitute_placeholders(typename.clone(), resolved));
                    if get_placeholder_name(&substituted).is_none() {
                        return TypeInference::Resolved(substituted);
                    }
                }
            }
        }
        TypeInference::Pending
    }
}

fn get_placeholder_name(ruletype: &Option<TokenStream>) -> Option<String> {
    if let Some(ts) = ruletype {
        for token in ts.clone() {
            match token {
                proc_macro2::TokenTree::Ident(ident) => {
                    let s = ident.to_string();
                    if s.starts_with("__rustylr_placeholder_") {
                        return Some(s);
                    }
                }
                proc_macro2::TokenTree::Group(group) => {
                    if let Some(name) = get_placeholder_name(&Some(group.stream())) {
                        return Some(name);
                    }
                }
                _ => {}
            }
        }
    }
    None
}

fn substitute_placeholders(
    ts: TokenStream,
    resolved: &HashMap<String, Option<TokenStream>>,
) -> Option<TokenStream> {
    let mut new_ts = TokenStream::new();
    for token in ts {
        match token {
            proc_macro2::TokenTree::Ident(ident) => {
                let s = ident.to_string();
                if s.starts_with("__rustylr_placeholder_") {
                    if let Some(replacement_opt) = resolved.get(&s) {
                        if let Some(replacement) = replacement_opt {
                            new_ts.extend(replacement.clone());
                        }
                    } else {
                        new_ts.extend([proc_macro2::TokenTree::Ident(ident)]);
                    }
                } else {
                    new_ts.extend([proc_macro2::TokenTree::Ident(ident)]);
                }
            }
            proc_macro2::TokenTree::Group(group) => {
                if let Some(sub) = substitute_placeholders(group.stream(), resolved) {
                    let new_group = proc_macro2::Group::new(group.delimiter(), sub);
                    new_ts.extend([proc_macro2::TokenTree::Group(new_group)]);
                }
            }
            other => {
                new_ts.extend([other]);
            }
        }
    }
    if new_ts.is_empty() {
        None
    } else {
        Some(new_ts)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use quote::quote;

    #[test]
    fn substitution_preserves_unknown_types_and_resolves_nested_groups() {
        let resolved = HashMap::from_iter([
            ("__rustylr_placeholder_A".to_owned(), Some(quote! { i32 })),
            ("__rustylr_placeholder_B".to_owned(), None),
        ]);
        let substituted = substitute_placeholders(
            quote! {
                (__rustylr_placeholder_A, [__rustylr_placeholder_A; 2], __rustylr_placeholder_C)
            },
            &resolved,
        )
        .unwrap();
        assert_eq!(
            substituted.to_string(),
            quote! { (i32, [i32; 2], __rustylr_placeholder_C) }.to_string()
        );
        assert_eq!(
            get_placeholder_name(&Some(substituted)).as_deref(),
            Some("__rustylr_placeholder_C")
        );
        assert!(substitute_placeholders(quote! { (__rustylr_placeholder_B) }, &resolved).is_none());
    }

    #[test]
    fn identity_inference_distinguishes_pending_and_valueless_types() {
        let args = Grammar::parse_args(quote! {
            %tokentype char;
            %start Expr;
            Expr(_) : Atom;
            Atom(_) : 'a';
        })
        .unwrap();
        let mut grammar = Grammar::from_grammar_args(args).unwrap();
        let expr = grammar.nonterminals_index["Expr"];
        let atom = grammar.nonterminals_index["Atom"];
        grammar.nonterminals[atom].ruletype = Some(quote! { __rustylr_placeholder_Atom });
        assert!(matches!(
            grammar.infer_identity_type(expr, &HashMap::default()),
            TypeInference::Pending
        ));
        let resolved = HashMap::from_iter([("__rustylr_placeholder_Atom".to_owned(), None)]);
        assert!(matches!(
            grammar.infer_identity_type(expr, &resolved),
            TypeInference::Resolved(None)
        ));
    }
}
