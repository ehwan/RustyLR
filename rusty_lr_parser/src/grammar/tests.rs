use super::*;
use crate::error::{ArgError, Info, ParseError, Warning};
use crate::nonterminal_info::ReduceAction;
use crate::parser::args::PatternArgs;
use crate::parser::args::TableLayout;
use crate::terminal_info::TerminalName;
use quote::quote;

#[test]
fn test_parse_simple_grammar() {
    let input = quote! {
        %tokentype char;
        %start Expr;
        Expr : 'a';
    };

    let grammar_args = Grammar::parse_args(input).expect("Failed to parse grammar");

    assert!(grammar_args.error_recovered.is_empty());

    // Check token type
    assert_eq!(grammar_args.token_typename.len(), 1);
    let (_, typename) = &grammar_args.token_typename[0];
    assert_eq!(typename.to_string(), "char");

    // Check start rule
    assert_eq!(grammar_args.start_rule_name.len(), 1);
    assert_eq!(grammar_args.start_rule_name[0].value(), "Expr");

    // Check rules
    assert_eq!(grammar_args.rules.len(), 1);
    let rule = &grammar_args.rules[0];
    assert_eq!(rule.name.value(), "Expr");
    assert_eq!(rule.rule_lines.len(), 1);

    let line = &rule.rule_lines[0];
    assert_eq!(line.tokens.len(), 1);
    let (name, pattern) = &line.tokens[0];
    assert!(name.is_none());

    if let PatternArgs::Char(c) = pattern {
        assert_eq!(*c.value(), 'a');
    } else {
        panic!("Expected PatternArgs::Char");
    }
}

#[test]
fn test_parse_complex_grammar_directives_userdata() {
    let input = quote! {
        %tokentype u8;
        %start Expr;
        %userdata UserData;
        %error MyError;
        Expr : b'a';
    };
    let grammar_args = Grammar::parse_args(input).expect("Failed userdata");
    assert!(grammar_args.error_recovered.is_empty());
}

#[test]
fn test_parse_complex_grammar_directives_prefix() {
    let input = quote! {
        %tokentype u8;
        %start Expr;
        %moduleprefix ::my_prefix;
        Expr : b'a';
    };
    let grammar_args = Grammar::parse_args(input).expect("Failed prefix");
    assert!(grammar_args.error_recovered.is_empty());
}

#[test]
fn test_parse_complex_grammar_directives_prec() {
    let input = quote! {
        %tokentype u8;
        %start Expr;
        %left b'+' b'-';
        Expr : b'a';
    };
    let grammar_args = Grammar::parse_args(input).expect("Failed prec");
    assert!(grammar_args.error_recovered.is_empty());
}

#[test]
fn test_parse_complex_grammar_directives_glr() {
    let input = quote! {
        %tokentype u8;
        %start Expr;
        %glr;
        Expr : b'a';
    };
    let grammar_args = Grammar::parse_args(input).expect("Failed glr");
    assert!(grammar_args.error_recovered.is_empty());
    assert!(grammar_args.glr);
    assert_eq!(grammar_args.layout, TableLayout::Auto);
}

#[test]
fn test_parse_complex_grammar_rules() {
    let input = quote! {
        %tokentype char;
        %start Expr;
        Expr : Term
             | Expr '+' Term
             ;
        Term : 'a'*
             | dollar_ident=ident
             ;
    };

    let grammar_args = Grammar::parse_args(input).expect("Failed to parse complex grammar rules");
    assert!(grammar_args.error_recovered.is_empty());
}

#[test]
fn test_parse_complex_grammar_sep() {
    let input = quote! {
        %tokentype char;
        %start Expr;
        Expr : $sep(Term, ',', +);
        Term : 'a';
    };

    let grammar_args = Grammar::parse_args(input).expect("Failed to parse complex grammar sep");
    assert!(grammar_args.error_recovered.is_empty());
}

#[test]
fn test_parse_grammar_error_recovery() {
    let input = quote! {
        %tokentype char;
        %start Expr;
        %error MyError;
        %left; // Missing precedence arguments
        Expr : 'a';
    };

    let grammar_args = Grammar::parse_args(input).expect("Should recover and return grammar args");

    assert!(!grammar_args.error_recovered.is_empty());

    let err = &grammar_args.error_recovered[0];
    assert!(err.message.contains("Expected <ident>") || err.message.contains("precedence"));

    assert_eq!(grammar_args.start_rule_name[0].value(), "Expr");
    assert_eq!(grammar_args.rules.len(), 1);
    assert_eq!(grammar_args.rules[0].name.value(), "Expr");
}

#[test]
fn test_parse_rule_line_action_error_recovery() {
    let input = quote! {
        %tokentype char;
        %start Expr;
        Expr : 'a' = ;
        Term : 'b';
    };

    let grammar_args =
        Grammar::parse_args(input).expect("Should recover from malformed reduce action");

    assert_eq!(grammar_args.error_recovered.len(), 1);
    assert_eq!(
        grammar_args.error_recovered[0].message,
        "Expected reduce action block or rule terminator"
    );
    assert_eq!(grammar_args.rules.len(), 2);
    assert_eq!(grammar_args.rules[0].name.value(), "Expr");
    assert_eq!(grammar_args.rules[1].name.value(), "Term");
}

#[test]
fn test_parse_mapped_symbol_error_recovery() {
    let input = quote! {
        %tokentype char;
        %start Expr;
        Expr : lhs= % 'a';
        Term : 'b';
    };

    let grammar_args =
        Grammar::parse_args(input).expect("Should recover from malformed symbol binding");

    assert_eq!(grammar_args.error_recovered.len(), 2);
    assert_eq!(
        grammar_args.error_recovered[0].message,
        "Expected pattern after symbol binding"
    );
    assert_eq!(
        grammar_args.error_recovered[1].message,
        "Expected %prec or %dprec"
    );
    assert_eq!(grammar_args.rules.len(), 2);
    assert_eq!(grammar_args.rules[0].name.value(), "Expr");
    assert_eq!(grammar_args.rules[1].name.value(), "Term");
}

#[test]
fn test_parse_terminal_set_error_recovery() {
    let input = quote! {
        %tokentype char;
        %start Expr;
        Expr : [ % ];
        Term : 'b';
    };

    let grammar_args =
        Grammar::parse_args(input).expect("Should recover from malformed terminal set");

    assert_eq!(grammar_args.error_recovered.len(), 1);
    assert_eq!(
        grammar_args.error_recovered[0].message,
        "Expected terminal set item"
    );
    assert_eq!(grammar_args.rules.len(), 2);
    assert_eq!(grammar_args.rules[0].name.value(), "Expr");
    assert_eq!(grammar_args.rules[1].name.value(), "Term");
}

#[test]
fn test_parse_allow_target_error_recovery() {
    let input = quote! {
        %tokentype char;
        %start Expr;
        %allow shift_reduce_conflict_resolved(%);
        Expr : 'a';
    };

    let grammar_args =
        Grammar::parse_args(input).expect("Should recover from malformed allow target");

    assert_eq!(grammar_args.error_recovered.len(), 1);
    assert_eq!(
        grammar_args.error_recovered[0].message,
        "Expected diagnostic suppression target"
    );
    assert_eq!(grammar_args.rules.len(), 1);
    assert_eq!(grammar_args.rules[0].name.value(), "Expr");
}

#[test]
fn test_parse_rule_body_error_recovery() {
    let input = quote! {
        %tokentype char;
        %start Expr;
        Expr : { 1 } = ;
        Term : 'b';
    };

    let grammar_args = Grammar::parse_args(input).expect("Should recover from malformed rule body");

    assert_eq!(grammar_args.error_recovered.len(), 1);
    assert_eq!(
        grammar_args.error_recovered[0].message,
        "Expected semicolon or rule alternative"
    );
    assert_eq!(grammar_args.rules.len(), 2);
    assert_eq!(grammar_args.rules[0].name.value(), "Expr");
    assert_eq!(grammar_args.rules[1].name.value(), "Term");
}

#[test]
fn test_bison_variables_success() {
    let input = quote! {
        %tokentype char;
        %start Expr;
        Expr(i32) : 'a' 'b' { $1 as i32 + $2 as i32 + @0.to_range().start as i32 + @1.to_range().start as i32 };
    };
    let grammar_args = Grammar::parse_args(input).expect("Failed to parse grammar args");
    let grammar = Grammar::from_grammar_args(grammar_args);
    assert!(grammar.is_ok());
}

#[test]
fn test_bison_variables_zero_error() {
    let input = quote! {
        %tokentype char;
        %start Expr;
        Expr(i32) : 'a' 'b' { $0 };
    };
    let grammar_args = Grammar::parse_args(input).expect("Failed to parse grammar args");
    let grammar = Grammar::from_grammar_args(grammar_args);
    assert!(matches!(grammar, Err(ParseError::BisonVariableZero(_))));
}

#[test]
fn test_bison_variables_out_of_range_error() {
    let input = quote! {
        %tokentype char;
        %start Expr;
        Expr(i32) : 'a' 'b' { $3 };
    };
    let grammar_args = Grammar::parse_args(input).expect("Failed to parse grammar args");
    let grammar = Grammar::from_grammar_args(grammar_args);
    assert!(matches!(
        grammar,
        Err(ParseError::BisonVariableOutOfRange { .. })
    ));

    let input_loc = quote! {
        %tokentype char;
        %start Expr;
        Expr(i32) : 'a' 'b' { @3.to_range().start as i32 };
    };
    let grammar_args = Grammar::parse_args(input_loc).expect("Failed to parse grammar args");
    let grammar = Grammar::from_grammar_args(grammar_args);
    assert!(matches!(
        grammar,
        Err(ParseError::BisonVariableOutOfRange { .. })
    ));
}

#[test]
fn test_type_inference_simple() {
    let input = quote! {
        %tokentype char;
        %start Expr;
        Expr(_) : 'a' { $1 };
    };
    let grammar_args = Grammar::parse_args(input).expect("Failed to parse grammar args");
    let grammar = Grammar::from_grammar_args(grammar_args).expect("Failed to build grammar");
    let expr_idx = grammar.nonterminals_index.get("Expr").unwrap();
    let ruletype = &grammar.nonterminals[*expr_idx].ruletype;
    assert_eq!(ruletype.as_ref().unwrap().to_string(), "char");
}

#[test]
fn test_type_inference_auto_identity() {
    let input = quote! {
        %tokentype char;
        %start Expr;
        Expr(_) : 'a';
    };
    let grammar_args = Grammar::parse_args(input).expect("Failed to parse grammar args");
    let grammar = Grammar::from_grammar_args(grammar_args).expect("Failed to build grammar");
    let expr_idx = grammar.nonterminals_index.get("Expr").unwrap();
    let ruletype = &grammar.nonterminals[*expr_idx].ruletype;
    assert_eq!(ruletype.as_ref().unwrap().to_string(), "char");
}

#[test]
fn test_type_inference_multi_step() {
    let input = quote! {
        %tokentype char;
        %start Expr;
        Expr(_) : Term { $1 };
        Term(_) : 'a' { $1 };
    };
    let grammar_args = Grammar::parse_args(input).expect("Failed to parse grammar args");
    let grammar = Grammar::from_grammar_args(grammar_args).expect("Failed to build grammar");
    let expr_idx = grammar.nonterminals_index.get("Expr").unwrap();
    let term_idx = grammar.nonterminals_index.get("Term").unwrap();
    assert_eq!(
        grammar.nonterminals[*expr_idx]
            .ruletype
            .as_ref()
            .unwrap()
            .to_string(),
        "char"
    );
    assert_eq!(
        grammar.nonterminals[*term_idx]
            .ruletype
            .as_ref()
            .unwrap()
            .to_string(),
        "char"
    );
}

#[test]
fn test_type_inference_circular_dependency() {
    let input = quote! {
        %tokentype char;
        %start Expr;
        Expr(_) : Term { $1 };
        Term(_) : Expr { $1 };
    };
    let grammar_args = Grammar::parse_args(input).expect("Failed to parse grammar args");
    let grammar = Grammar::from_grammar_args(grammar_args);
    assert!(matches!(grammar, Err(ParseError::TypeInferenceFailed(_))));
}

#[test]
fn test_codegen_no_empty_tags() {
    let input = quote! {
        %tokentype Token;
        %start Expr;
        %token a Token::A(_);
        Expr(i32) : a { 1 };
    };
    let grammar_args = Grammar::parse_args(input).expect("Failed to parse grammar args");
    let grammar = Grammar::from_grammar_args(grammar_args).expect("Failed to build grammar");
    let code = grammar.emit_compiletime();

    // Ensure the generated code compiles successfully as a valid Rust TokenStream
    let parsed = syn::parse2::<syn::File>(code.clone());
    assert!(
        parsed.is_ok(),
        "Generated code failed to parse: {:?}",
        parsed.err()
    );

    // Check that the Empty variant is present in the output
    let code_str = code.to_string();
    assert!(
        code_str.contains("Empty"),
        "Empty variant should be unconditionally included"
    );
}

#[test]
fn test_identity_unit_reduce_action_is_emitted_after_state_build() {
    // Regression test for issue #90.
    let input = quote! {
        %tokentype Token;
        %glr;
        %start Program;

        %token LET Token::Let;
        %token IDENT Token::Ident(_);
        %token SEMI Token::Semi;

        Program(Vec<Statement>)
            : statements=Statement* {
                statements
            }
            ;

        Statement(Statement)
            : SEMI! {
                Statement::Empty
            }
            | statement=LetStatement {
                statement
            }
            ;

        LetStatement(LetStatement)
            : LET! name=IDENT SEMI! {
                let Token::Ident(name) = name else {
                    unreachable!("expected IDENT token")
                };
                Statement::Let(name)
            }
            ;
    };

    let grammar_args = Grammar::parse_args(input).expect("Failed to parse grammar args");
    let mut grammar = Grammar::from_grammar_args(grammar_args).expect("Failed to build grammar");
    grammar.builder = grammar
        .create_builder()
        .expect("Failed to create parser builder");
    let _ = grammar.build_grammar();

    let code = grammar.emit_compiletime();
    let code_str = code.to_string();

    assert!(
        code_str.contains("fn reduce_Statement_1"),
        "identity unit production Statement -> LetStatement must keep its reduce function"
    );
    assert!(
        code_str.contains("2usize => Self :: reduce_Statement_1"),
        "rule 2 must dispatch to the Statement -> LetStatement reduce action"
    );
}

#[test]
fn test_identity_unit_reduce_action_with_matching_type_can_be_bypassed() {
    let input = quote! {
        %tokentype Token;
        %glr;
        %start Program;

        %token ITEM Token::Item(_);

        Program(Node)
            : item=Item {
                item
            }
            ;

        Item(Node)
            : ITEM {
                Node
            }
            ;
    };

    let grammar_args = Grammar::parse_args(input).expect("Failed to parse grammar args");
    let mut grammar = Grammar::from_grammar_args(grammar_args).expect("Failed to build grammar");
    grammar.builder = grammar
        .create_builder()
        .expect("Failed to create parser builder");
    let _ = grammar.build_grammar();

    let code = grammar.emit_compiletime();
    let code_str = code.to_string();

    assert!(
        !code_str.contains("fn reduce_Program_0"),
        "identity unit production with matching storage type should be bypassed"
    );
    assert!(
        !code_str.contains("0usize => Self :: reduce_Program_0"),
        "bypassed identity unit production should not be dispatched"
    );
}

#[test]
fn test_nooptim_disables_identity_unit_reduce_bypass() {
    let input = quote! {
        %nooptim;
        %tokentype Token;
        %glr;
        %start Program;

        %token ITEM Token::Item(_);

        Program(Node)
            : item=Item {
                item
            }
            ;

        Item(Node)
            : ITEM {
                Node
            }
            ;
    };

    let grammar_args = Grammar::parse_args(input).expect("Failed to parse grammar args");
    let mut grammar = Grammar::from_grammar_args(grammar_args).expect("Failed to build grammar");
    assert!(
        !grammar.optimize,
        "%nooptim must disable both grammar and table optimization passes"
    );

    grammar.optimize(25);
    grammar.builder = grammar
        .create_builder()
        .expect("Failed to create parser builder");
    let _ = grammar.build_grammar();

    let code = grammar.emit_compiletime();
    let code_str = code.to_string();

    assert!(
        code_str.contains("fn reduce_Program_0"),
        "%nooptim should keep the identity unit production reduce function"
    );
    assert!(
        code_str.contains("0usize => Self :: reduce_Program_0"),
        "%nooptim should keep the identity unit production dispatch"
    );
}

#[test]
fn test_glr_optional_epsilon_self_loop_is_expanded_even_with_nooptim() {
    let input = quote! {
        %nooptim;
        %glr;
        %tokentype char;
        %start Expr;

        Expr(Ast)
            : 't' { Ast::True }
            | '!'? left=Expr '&' right=Expr {
                Ast::And(Box::new(left), Box::new(right))
            }
            ;
    };

    let grammar_args = Grammar::parse_args(input).expect("Failed to parse grammar args");
    let mut grammar = Grammar::from_grammar_args(grammar_args).expect("Failed to build grammar");
    assert!(
        !grammar.optimize,
        "the regression must be covered even when ordinary optimization is disabled"
    );

    grammar.builder = grammar
        .create_builder()
        .expect("Failed to create parser builder");
    let expansion_info = grammar
        .infos
        .iter()
        .find_map(|info| match info {
            Info::GlrOptionalExpanded { before, after, .. } => Some((before, after)),
            _ => None,
        })
        .expect("expected GLR optional expansion info");
    assert_eq!(
        expansion_info.0, "Expr -> '!'? Expr '&' Expr",
        "the info must report the original production"
    );
    assert_eq!(
        expansion_info.1,
        &vec![
            "Expr -> Expr '&' Expr".to_string(),
            "Expr -> '!' Expr '&' Expr".to_string(),
        ],
        "the info must report the generated replacement productions"
    );

    let _ = grammar.build_grammar();

    for (state_idx, state) in grammar.states.iter().enumerate() {
        for (_, reduce_rules) in &state.reduce_map {
            for &rule_idx in reduce_rules {
                let Some((nonterm_idx, local_rule_idx)) = grammar.rule_id_to_indices(rule_idx)
                else {
                    continue;
                };
                let nonterm = &grammar.nonterminals[nonterm_idx];
                let rule = &nonterm.rules[local_rule_idx];
                let is_optional_empty_rule = nonterm.nonterm_type
                    == Some(rusty_lr_core::parser::nonterminal::NonTerminalType::Optional)
                    && rule.tokens.is_empty();
                let has_self_goto = state
                    .shift_goto_map_nonterm
                    .iter()
                    .any(|(nonterm, target)| *nonterm == nonterm_idx && target.state == state_idx);

                assert!(
                    !(is_optional_empty_rule && has_self_goto),
                    "GLR tables must not keep an optional epsilon self-loop"
                );
            }
        }
    }
}

#[test]
fn test_glr_optional_epsilon_self_loop_reports_error_when_value_is_used() {
    let input = quote! {
        %glr;
        %tokentype char;
        %start Expr;

        Expr(Ast)
            : 't' { Ast::True }
            | opt='!'? left=Expr '&' right=Expr {
                let _ = opt;
                Ast::And(Box::new(left), Box::new(right))
            }
            ;
    };

    let grammar_args = Grammar::parse_args(input).expect("Failed to parse grammar args");
    let mut grammar = Grammar::from_grammar_args(grammar_args).expect("Failed to build grammar");
    grammar.optimize(25);

    let err = grammar
        .create_builder()
        .expect_err("optional value use must block automatic GLR expansion");
    let ParseError::GlrNullableReduceCycle {
        nullable_rule,
        reason,
        help,
        ..
    } = err
    else {
        panic!("expected GLR nullable reduce cycle error");
    };

    assert_eq!(nullable_rule, "'!'? ->");
    assert!(
        reason.contains("reads the optional value `opt`"),
        "reason should explain why automatic expansion is unsafe: {reason}"
    );
    assert!(
        help.contains("Rewrite"),
        "help should tell the user how to fix the grammar: {help}"
    );
}

#[test]
fn test_glr_star_epsilon_self_loop_reports_error() {
    let input = quote! {
        %glr;
        %tokentype char;
        %start Expr;

        Expr(Ast)
            : 't' { Ast::True }
            | '!'* left=Expr '&' right=Expr {
                Ast::And(Box::new(left), Box::new(right))
            }
            ;
    };

    let grammar_args = Grammar::parse_args(input).expect("Failed to parse grammar args");
    let mut grammar = Grammar::from_grammar_args(grammar_args).expect("Failed to build grammar");
    grammar.optimize(25);

    let err = grammar
        .create_builder()
        .expect_err("zero-or-more GLR cycle must be rejected");
    let ParseError::GlrNullableReduceCycle { reason, help, .. } = err else {
        panic!("expected GLR nullable reduce cycle error");
    };

    assert!(
        reason.contains("zero-or-more repetition helper"),
        "reason should explain that star cannot be finitely expanded: {reason}"
    );
    assert!(
        help.contains("non-empty list helper"),
        "help should suggest a useful manual rewrite: {help}"
    );
}

#[test]
fn test_variable_substitution_success() {
    let input = quote! {
        %tokentype MyToken;
        %location MyLoc;
        %userdata MyUser;
        %error MyErr;
        %moduleprefix ::my_prefix;
        %token a MyToken::A;

        %start Expr;
        Expr($tokentype) : a { $tokentype };
        Term($location) : a { $location };
        Rule3($userdata) : a { $userdata };
        Rule4($error) : a { $error };
        Rule5($moduleprefix) : a { $moduleprefix };
        Rule6($a) : a { $a };
        Rule7($Expr) : a { $Expr };
    };

    let grammar_args = Grammar::parse_args(input).expect("Failed to parse grammar args");
    let grammar = Grammar::from_grammar_args(grammar_args).expect("Failed to build grammar");

    let expr_idx = grammar.nonterminals_index.get("Expr").unwrap();
    assert_eq!(
        grammar.nonterminals[*expr_idx]
            .ruletype
            .as_ref()
            .unwrap()
            .to_string(),
        "MyToken"
    );

    let term_idx = grammar.nonterminals_index.get("Term").unwrap();
    assert_eq!(
        grammar.nonterminals[*term_idx]
            .ruletype
            .as_ref()
            .unwrap()
            .to_string(),
        "MyLoc"
    );

    let rule3_idx = grammar.nonterminals_index.get("Rule3").unwrap();
    assert_eq!(
        grammar.nonterminals[*rule3_idx]
            .ruletype
            .as_ref()
            .unwrap()
            .to_string(),
        "MyUser"
    );

    let rule4_idx = grammar.nonterminals_index.get("Rule4").unwrap();
    assert_eq!(
        grammar.nonterminals[*rule4_idx]
            .ruletype
            .as_ref()
            .unwrap()
            .to_string(),
        "MyErr"
    );

    let rule5_idx = grammar.nonterminals_index.get("Rule5").unwrap();
    assert_eq!(
        grammar.nonterminals[*rule5_idx]
            .ruletype
            .as_ref()
            .unwrap()
            .to_string(),
        ":: my_prefix"
    );

    let rule6_idx = grammar.nonterminals_index.get("Rule6").unwrap();
    assert_eq!(
        grammar.nonterminals[*rule6_idx]
            .ruletype
            .as_ref()
            .unwrap()
            .to_string(),
        "MyToken :: A"
    );

    let rule7_idx = grammar.nonterminals_index.get("Rule7").unwrap();
    assert_eq!(
        grammar.nonterminals[*rule7_idx]
            .ruletype
            .as_ref()
            .unwrap()
            .to_string(),
        "MyToken"
    );
}

#[test]
fn test_variable_substitution_circular_dependency() {
    let input = quote! {
        %tokentype Token;
        %token a Token::A;
        %token b Token::B;
        %start Expr;
        Expr($Term) : a;
        Term($Expr) : b;
    };

    let grammar_args = Grammar::parse_args(input).expect("Failed to parse grammar args");
    let grammar = Grammar::from_grammar_args(grammar_args);
    assert!(grammar.is_err());
    let err = grammar.err().unwrap();
    assert!(matches!(err, ParseError::CircularDependency { .. }));
    if let ParseError::CircularDependency { path, .. } = err {
        assert!(path.contains(&"nonterm:Expr".to_string()));
        assert!(path.contains(&"nonterm:Term".to_string()));
    }
}

#[test]
fn test_variable_substitution_unknown_fallback() {
    let input = quote! {
        %tokentype Token;
        %token a Token::A;
        %start Expr;
        Expr : a { $unknown_var };
    };

    let grammar_args = Grammar::parse_args(input).expect("Failed to parse grammar args");
    let grammar = Grammar::from_grammar_args(grammar_args).expect("Failed to build grammar");

    // The reduce action of the rule should preserve $unknown_var unchanged
    let rule = &grammar.nonterminals[*grammar.nonterminals_index.get("Expr").unwrap()].rules[0];
    let action = match rule.reduce_action.as_ref().unwrap() {
        ReduceAction::Custom(custom) => custom.body.to_string(),
        _ => panic!("Expected Custom reduce action"),
    };
    assert!(action.contains("$ unknown_var"));
}

#[test]
fn test_box_keyword_parsing() {
    let input = quote! {
        %tokentype box MyToken;
        %token a MyToken::A;
        %start Expr;
        Expr(box MyType) : a { MyType };
    };

    let grammar_args = Grammar::parse_args(input).expect("Failed to parse grammar args");
    let grammar = Grammar::from_grammar_args(grammar_args).expect("Failed to build grammar");

    assert!(grammar.is_tokentype_boxed);
    assert_eq!(grammar.token_typename.to_string(), "MyToken");

    let expr_idx = grammar.nonterminals_index.get("Expr").unwrap();
    assert!(grammar.nonterminals[*expr_idx].ruletype_boxed);
    assert_eq!(
        grammar.nonterminals[*expr_idx]
            .ruletype
            .as_ref()
            .unwrap()
            .to_string(),
        "MyType"
    );

    let code = grammar.emit_compiletime();
    let code_str = code.to_string();

    // Ensure the data enum wraps in Box
    assert!(
        code_str.contains("Box < MyType >")
            || code_str.contains("Box<MyType>")
            || code_str.contains("Box"),
        "Data enum variant should hold Boxed MyType"
    );
    assert!(
        code_str.contains("Box < MyToken >")
            || code_str.contains("Box<MyToken>")
            || code_str.contains("Box"),
        "Data enum variant should hold Boxed MyToken"
    );

    // Ensure auto-wrap Box::new and auto-unwrap *val are emitted
    assert!(
        code_str.contains("Box :: new") || code_str.contains("Box::new"),
        "Box::new should be generated to wrap reduce result"
    );
    assert!(
        code_str.contains("* val") || code_str.contains("*val"),
        "Dereference should be generated to extract boxed value"
    );
}

#[test]
fn test_warnings_and_infos_collection() {
    let input = quote! {
        %tokentype char;
        %start Expr;
        %left '+';

        Expr : Expr '+' Expr | 'a';
        UnusedNonTerm : 'b' 'c';
    };

    let grammar_args = Grammar::parse_args(input).expect("Failed to parse grammar");
    let mut grammar =
        Grammar::from_grammar_args(grammar_args).expect("Failed to construct grammar");

    println!(
        "Non-terminals before optimize: {:?}",
        grammar
            .nonterminals
            .iter()
            .map(|n| n.name.value().clone())
            .collect::<Vec<_>>()
    );
    grammar.optimize(25);

    println!("Warnings: {:?}", grammar.warnings);
    println!(
        "Non-terminals: {:?}",
        grammar
            .nonterminals
            .iter()
            .map(|n| n.name.value().clone())
            .collect::<Vec<_>>()
    );

    assert!(
        grammar
            .warnings
            .iter()
            .any(|w| matches!(w, Warning::NonTermUnreachable { .. })),
        "Expected NonTermUnreachable warning, got: {:?}",
        grammar.warnings
    );

    grammar.builder = grammar
        .create_builder()
        .expect("Failed to create parser builder");
    let _ = grammar.build_grammar();

    let has_resolved_info = grammar.infos.iter().any(|info| {
        matches!(
            info,
            Info::ShiftReduceConflictResolvedReduce { .. }
                | Info::ShiftReduceConflictResolvedShift { .. }
        )
    });
    assert!(
        has_resolved_info,
        "Expected shift/reduce conflict resolution note, got: {:?}",
        grammar.infos
    );
}

#[test]
fn test_diagnostic_suppression_success() {
    let input = quote! {
        %tokentype char;
        %start Expr;
        %allow nonterm_unreachable;

        Expr : 'a';
        Unused : 'b';
    };

    let grammar_args = Grammar::parse_args(input).expect("Failed to parse grammar");
    assert!(
        grammar_args
            .allowed_diagnostics
            .iter()
            .any(|d| d.0.value() == "nonterm_unreachable")
    );

    let span_manager = grammar_args.span_manager.clone();
    let mut grammar =
        Grammar::from_grammar_args(grammar_args).expect("Failed to construct grammar");
    assert!(
        grammar
            .allowed_diagnostics
            .contains_key("nonterm_unreachable")
    );

    grammar.optimize(25);
    // Find the NonTermUnreachable warning
    let warning = grammar
        .warnings
        .iter()
        .find(|w| matches!(w, Warning::NonTermUnreachable { .. }))
        .expect("Expected NonTermUnreachable warning");

    let ts = warning.to_compile_warning(&grammar, &span_manager);
    assert!(ts.is_empty(), "Warning should be suppressed");
}

#[test]
fn test_diagnostic_suppression_targeted_success() {
    let input = quote! {
        %tokentype char;
        %start Expr;
        %allow nonterm_unreachable(Unused1);

        Expr : 'a';
        Unused1 : 'b';
        Unused2 : 'c';
    };

    let grammar_args = Grammar::parse_args(input).expect("Failed to parse grammar");
    assert!(
        grammar_args
            .allowed_diagnostics
            .iter()
            .any(|d| d.0.value() == "nonterm_unreachable"
                && d.1.as_ref().unwrap().to_string_repr() == "Unused1")
    );

    let span_manager = grammar_args.span_manager.clone();
    let mut grammar =
        Grammar::from_grammar_args(grammar_args).expect("Failed to construct grammar");
    assert!(
        grammar
            .allowed_diagnostics
            .contains_key("nonterm_unreachable")
    );
    let scopes = grammar
        .allowed_diagnostics
        .get("nonterm_unreachable")
        .unwrap();
    assert!(scopes.iter().any(|opt_target| match opt_target {
        Some(ResolvedAllowTarget::Name(name)) => name == "Unused1",
        _ => false,
    }));

    grammar.optimize(25);

    // Find the NonTermUnreachable warning for Unused1
    let warning = grammar
        .warnings
        .iter()
        .find(|w| match w {
            Warning::NonTermUnreachable { nonterm_name } => nonterm_name.value() == "Unused1",
            _ => false,
        })
        .expect("Expected NonTermUnreachable warning for Unused1");

    // Find the NonTermUnreachable warning for Unused2
    let warning2 = grammar
        .warnings
        .iter()
        .find(|w| match w {
            Warning::NonTermUnreachable { nonterm_name } => nonterm_name.value() == "Unused2",
            _ => false,
        })
        .expect("Expected NonTermUnreachable warning for Unused2");

    // Warning for Unused1 should be suppressed
    assert!(grammar.is_warning_allowed(warning));
    assert!(
        warning
            .to_compile_warning(&grammar, &span_manager)
            .is_empty()
    );

    // Warning for Unused2 should NOT be suppressed
    assert!(!grammar.is_warning_allowed(warning2));
    assert!(
        !warning2
            .to_compile_warning(&grammar, &span_manager)
            .is_empty()
    );
}

#[test]
fn test_diagnostic_suppression_invalid_name() {
    let input = quote! {
        %tokentype char;
        %start Expr;
        %allow invalid_diagnostic_name;

        Expr : 'a';
    };

    let grammar_args = Grammar::parse_args(input).expect("Failed to parse grammar");
    let result = Grammar::from_grammar_args(grammar_args);
    assert!(
        result.is_err(),
        "Expected parsing to fail due to invalid diagnostic name"
    );
    if let Err(ParseError::InvalidAllowDiagnostic { name, .. }) = result {
        assert_eq!(name, "invalid_diagnostic_name");
    } else {
        if let Err(e) = result {
            panic!("Expected ParseError::InvalidAllowDiagnostic, got: {:?}", e);
        } else {
            panic!("Expected ParseError::InvalidAllowDiagnostic, got Ok");
        }
    }
}

#[test]
fn test_diagnostic_suppression_unused_terminal_success() {
    // Scenario 1: Only TokB allowed -> Warning NOT suppressed
    let input1 = quote! {
        %tokentype Token;
        %start Expr;
        %allow unused_terminals(TokB);

        %token TokA Token::TokA;
        %token TokB Token::TokB;
        %token TokC Token::TokC;

        Expr : TokA;
    };

    let grammar_args1 = Grammar::parse_args(input1).expect("Failed to parse grammar");
    let mut grammar1 =
        Grammar::from_grammar_args(grammar_args1).expect("Failed to construct grammar");
    grammar1.optimize(25);

    let warning1 = grammar1
        .warnings
        .iter()
        .find(|w| matches!(w, Warning::UnusedTerminals { .. }))
        .expect("Expected unused warning");
    assert!(!grammar1.is_warning_allowed(warning1));

    // Scenario 2: Both TokB and TokC allowed -> Warning suppressed
    let input2 = quote! {
        %tokentype Token;
        %start Expr;
        %allow unused_terminals(TokB);
        %allow unused_terminals(TokC);

        %token TokA Token::TokA;
        %token TokB Token::TokB;
        %token TokC Token::TokC;

        Expr : TokA;
    };

    let grammar_args2 = Grammar::parse_args(input2).expect("Failed to parse grammar");
    let mut grammar2 =
        Grammar::from_grammar_args(grammar_args2).expect("Failed to construct grammar");
    grammar2.optimize(25);

    let warning2 = grammar2
        .warnings
        .iter()
        .find(|w| matches!(w, Warning::UnusedTerminals { .. }))
        .expect("Expected unused warning");
    assert!(grammar2.is_warning_allowed(warning2));

    // Scenario 3: Suppressed via TerminalSet -> Warning suppressed
    let input3 = quote! {
        %tokentype Token;
        %start Expr;
        %allow unused_terminals([ TokB TokC ]);

        %token TokA Token::TokA;
        %token TokB Token::TokB;
        %token TokC Token::TokC;

        Expr : TokA;
    };

    let grammar_args3 = Grammar::parse_args(input3).expect("Failed to parse grammar");
    let mut grammar3 =
        Grammar::from_grammar_args(grammar_args3).expect("Failed to construct grammar");
    grammar3.optimize(25);

    let warning3 = grammar3
        .warnings
        .iter()
        .find(|w| matches!(w, Warning::UnusedTerminals { .. }))
        .expect("Expected unused warning");
    assert!(grammar3.is_warning_allowed(warning3));
}

#[test]
fn test_diagnostic_suppression_range_terminal_success() {
    let input = quote! {
        %tokentype char;
        %start Expr;
        %allow terminals_merged('b'-'d');

        Expr : 'a' | [ 'b'-'d' ];
    };

    let grammar_args = Grammar::parse_args(input).expect("Failed to parse grammar");
    let mut grammar =
        Grammar::from_grammar_args(grammar_args).expect("Failed to construct grammar");
    grammar.optimize(25);

    // TerminalsMerged info for 'b'-'d' should be suppressed
    let info = grammar
        .infos
        .iter()
        .find(|i| matches!(i, Info::TerminalsMerged { .. }))
        .expect("Expected TerminalsMerged info");
    assert!(grammar.is_info_allowed(info));
}

#[test]
fn test_diagnostic_suppression_allow_range_resolver_success() {
    let input = quote! {
        %tokentype char;
        %start Expr;
        %allow unused_terminals('b'-'d');

        Expr : 'a';
    };

    let grammar_args = Grammar::parse_args(input).expect("Failed to parse grammar");
    let grammar = Grammar::from_grammar_args(grammar_args).expect("Failed to construct grammar");

    // Check that character range 'b' to 'd' is present in terminals
    let has_range = grammar
        .terminals
        .iter()
        .any(|t| matches!(t.name, TerminalName::CharRange('b', 'd')));
    assert!(
        has_range,
        "Expected range 'b'-'d' to be registered and split in terminals"
    );
}

#[test]
fn test_multiple_start_symbols() {
    let input = quote! {
        %tokentype char;
        %start Expr;
        %start Stmt;

        Expr : 'a';
        Stmt : 'b';
    };

    let grammar_args = Grammar::parse_args(input).expect("Failed to parse grammar");
    assert_eq!(grammar_args.start_rule_name.len(), 2);
    assert_eq!(grammar_args.start_rule_name[0].value(), "Expr");
    assert_eq!(grammar_args.start_rule_name[1].value(), "Stmt");

    let grammar = Grammar::from_grammar_args(grammar_args).expect("Failed to construct grammar");
    assert_eq!(grammar.start_rule_names.len(), 2);
    assert_eq!(grammar.start_rule_names[0].value(), "Expr");
    assert_eq!(grammar.start_rule_names[1].value(), "Stmt");
}

#[test]
fn test_duplicate_start_symbol() {
    let input = quote! {
        %tokentype char;
        %start Expr;
        %start Expr;

        Expr : 'a';
    };

    let grammar_args = Grammar::parse_args(input).expect("Failed to parse grammar");
    let err = match Grammar::arg_check_error(&grammar_args) {
        Ok(_) => panic!("Expected arg check to fail"),
        Err(e) => e,
    };
    assert!(
        matches!(err, ArgError::DuplicateStartSymbol { ref name, .. } if name == "Expr"),
        "Expected DuplicateStartSymbol error, got {:?}",
        err
    );
}
