mod deterministic_precedence {
    use rusty_lr::lr1;

    lr1! {
        %tokentype char;
        %userdata Vec<&'static str>;
        %start Expr;

        %left '+';
        %left '*';
        %right '^';

        Digit(i32): d=['0'-'9'] { d.to_digit(10).unwrap() as i32 };

        Expr(i32)
            : left=Expr '+' right=Expr {
                data.push("add");
                left + right
            }
            | left=Expr '*' right=Expr {
                data.push("mul");
                left * right
            }
            | left=Expr '^' right=Expr {
                data.push("pow");
                left.pow(right as u32)
            }
            | '(' Expr ')' { Expr }
            | Digit
            ;
    }

    fn parse(input: &str) -> (i32, Vec<&'static str>) {
        let mut ctx = ExprContext::new(Vec::new());
        for ch in input.chars() {
            ctx.feed(ch).unwrap();
        }
        ctx.accept().unwrap()
    }

    #[test]
    fn deterministic_state_stack_starts_at_virtual_start_branch() {
        let ctx = ExprContext::new(Vec::new());
        assert_eq!(ctx.state_stack().count(), 1);
    }

    #[test]
    fn precedence_and_associativity_drive_deterministic_lr_reductions() {
        assert_eq!(parse("2+3*4+5").0, 19);
        assert_eq!(parse("2^3^2").0, 512);

        let (_, reductions) = parse("2+3*4");
        assert_eq!(reductions, ["mul", "add"]);
    }

    #[test]
    fn malformed_expression_reports_a_parse_error() {
        let mut ctx = ExprContext::new(Vec::new());
        ctx.feed('2').unwrap();
        ctx.feed('+').unwrap();
        assert!(ctx.feed('*').is_err());
    }
}

mod precedence_malformed_input_errors {
    use rusty_lr::lr1;

    lr1! {
        %tokentype char;
        %start Expr;

        %precedence '=';

        Atom(&'static str): 'a' { "a" };

        Expr(&'static str)
            : Expr '=' Expr { "eq" }
            | Atom
            ;
    }

    #[test]
    fn precedence_grammar_rejects_incomplete_operator_chain() {
        let mut ctx = ExprContext::with_default_userdata();
        ctx.feed('a').unwrap();
        ctx.feed('=').unwrap();
        ctx.feed('a').unwrap();
        ctx.feed('=').unwrap();
        assert!(ctx.accept().is_err());
    }
}

mod pattern_operators {
    use rusty_lr::lr1;

    lr1! {
        %tokentype char;
        %start Csv;

        Digit(i32): d=['0'-'9'] { d.to_digit(10).unwrap() as i32 };

        Signed(i32): sign='-'? Digit {
            if sign.is_some() {
                -Digit
            } else {
                Digit
            }
        };

        Csv(i32): values=$sep(Signed, ',', +) {
            values.into_iter().sum()
        };
    }

    #[test]
    fn optional_and_separated_patterns_produce_semantic_values() {
        let mut ctx = CsvContext::with_default_userdata();
        for ch in "-1,2,3".chars() {
            ctx.feed(ch).unwrap();
        }
        assert_eq!(ctx.accept().unwrap(), (4, ()));
    }

    #[test]
    fn separated_pattern_rejects_missing_required_item() {
        let mut ctx = CsvContext::with_default_userdata();
        assert!(ctx.accept().is_err());

        let mut ctx = CsvContext::with_default_userdata();
        ctx.feed('1').unwrap();
        ctx.feed(',').unwrap();
        assert!(ctx.accept().is_err());
    }
}

mod token_userdata_and_recovery {
    use rusty_lr::lr1;

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum Token {
        Num(i32),
        Semi,
        Junk,
    }

    lr1! {
        %tokentype Token;
        %userdata Vec<std::ops::Range<usize>>;
        %location std::ops::Range<usize>;
        %start Items;
        %allow unused_terminals(junk);

        %token num Token::Num(_);
        %token semi Token::Semi;
        %token junk Token::Junk;

        Items(Vec<i32>): items=Item* { items };

        Item(i32)
            : num semi {
                if let Token::Num(n) = num {
                    n
                } else {
                    unreachable!()
                }
            }
            | error semi {
                data.push(@error.clone());
                -1
            }
            ;
    }

    #[test]
    fn token_patterns_userdata_and_panic_mode_recovery_work_together() {
        let mut ctx = ItemsContext::new(Vec::new());
        ctx.feed_location(Token::Num(7), 0..1).unwrap();
        ctx.feed_location(Token::Semi, 1..2).unwrap();
        ctx.feed_location(Token::Junk, 2..3).unwrap();
        ctx.feed_location(Token::Junk, 3..4).unwrap();
        ctx.feed_location(Token::Semi, 4..5).unwrap();

        let (items, recovered_spans) = ctx.accept().unwrap();
        assert_eq!(items, [7, -1]);
        assert_eq!(recovered_spans, [2..4]);
    }

    #[test]
    fn recovery_rule_still_errors_without_a_synchronizing_token() {
        let mut ctx = ItemsContext::new(Vec::new());
        ctx.feed_location(Token::Junk, 0..1).unwrap();
        assert!(ctx.accept().is_err());
    }
}

mod recovery_reduce_before_sync_token {
    mod deterministic {
        use rusty_lr::lr1;

        lr1! {
            %nooptim;
            %tokentype char;
            %location std::ops::Range<usize>;
            %userdata Vec<std::ops::Range<usize>>;
            %start S;

            S(&'static str): error Tail {
                data.push(@error.clone());
                "recovered"
            };

            Tail(()): Empty 's' { () };
            Empty(()): "" { () };
        }

        #[test]
        fn deterministic_recovery_feeds_sync_token_through_reduce_chain() {
            let mut ctx = SContext::new(Vec::new());
            ctx.feed_location('s', 0..1).unwrap();

            let (value, recovered_spans) = ctx.accept().unwrap();
            assert_eq!(value, "recovered");
            assert_eq!(recovered_spans, [0..0]);
        }
    }

    mod glr {
        use rusty_lr::lr1;

        lr1! {
            %glr;
            %nooptim;
            %tokentype char;
            %location std::ops::Range<usize>;
            %userdata Vec<std::ops::Range<usize>>;
            %start S;

            S(&'static str): error Tail {
                data.push(@error.clone());
                "recovered"
            };

            Tail(()): Empty 's' { () };
            Empty(()): "" { () };
        }

        #[test]
        fn glr_recovery_feeds_sync_token_through_reduce_chain() {
            let mut ctx = SContext::new(Vec::new());
            ctx.feed_location('s', 0..1).unwrap();

            let (value, recovered_spans) = ctx.accept().unwrap();
            assert_eq!(value, "recovered");
            assert_eq!(recovered_spans, [0..0]);
        }
    }
}

mod left_recursive_nullable_list {
    use rusty_lr::lr1;

    lr1! {
        %lalr;
        %tokentype char;
        %start List;

        Elem(char): ch=['a'-'c'] { ch };

        List(Vec<char>)
            : List Elem {
                let mut list = List;
                list.push(Elem);
                list
            }
            | "" { Vec::new() }
            ;
    }

    #[test]
    fn left_recursive_nullable_production_accumulates_in_input_order() {
        let mut ctx = ListContext::with_default_userdata();
        for ch in "abca".chars() {
            ctx.feed(ch).unwrap();
        }
        assert_eq!(ctx.accept().unwrap(), (vec!['a', 'b', 'c', 'a'], ()));
    }
}

mod grammar_with_productive_cycle {
    use rusty_lr::lr1;

    lr1! {
        %lalr;
        %tokentype char;
        %start S;

        S(String): A;

        A(String)
            : 'a' B {
                format!("a{}", B)
            }
            | 'x' {
                "x".to_string()
            }
            ;

        B(String)
            : 'b' A {
                format!("b{}", A)
            }
            | 'y' {
                "y".to_string()
            }
            ;
    }

    #[test]
    fn productive_symbol_cycle_parses_after_consuming_terminals() {
        let mut ctx = SContext::with_default_userdata();
        for ch in "abx".chars() {
            ctx.feed(ch).unwrap();
        }
        assert_eq!(ctx.accept().unwrap(), ("abx".to_string(), ()));

        let mut ctx = SContext::with_default_userdata();
        for ch in "ay".chars() {
            ctx.feed(ch).unwrap();
        }
        assert_eq!(ctx.accept().unwrap(), ("ay".to_string(), ()));
    }
}

mod glr_ambiguous_concatenation {
    use rusty_lr::lr1;

    lr1! {
        %glr;
        %tokentype char;
        %start S;

        S(String)
            : S S {
                format!("({}{})", $1, $2)
            }
            | 'a' {
                "a".to_string()
            }
            ;
    }

    #[test]
    fn ambiguous_non_lr_grammar_keeps_all_glr_parse_trees() {
        let mut ctx = SContext::with_default_userdata();
        for ch in "aaa".chars() {
            ctx.feed(ch).unwrap();
        }

        let mut results = ctx
            .accept_all()
            .unwrap()
            .map(|(value, _)| value)
            .collect::<Vec<_>>();
        results.sort();

        assert_eq!(results, ["((aa)a)", "(a(aa))"]);
    }

    #[test]
    fn glr_state_stack_reporting_starts_at_virtual_start_branch() {
        let ctx = SContext::with_default_userdata();
        let counts = ctx
            .state_stacks()
            .map(|state_stack| state_stack.count())
            .collect::<Vec<_>>();
        assert_eq!(counts, [1]);
    }

    #[test]
    fn glr_parser_reports_an_error_when_all_branches_die() {
        let mut ctx = SContext::with_default_userdata();
        assert!(ctx.feed('b').is_err());
    }
}

mod glr_can_feed_planning {
    use rusty_lr::lr1;

    lr1! {
        %glr;
        %nooptim;
        %tokentype char;
        %start S;

        S(&'static str)
            : A 'x' {
                "x"
            }
            | B 'y' {
                "y"
            }
            | Empty {
                "empty"
            }
            ;

        A(()): Empty { () };
        B(()): Empty { () };
        Empty(()): "" { () };
    }

    #[test]
    fn glr_can_feed_and_can_accept_reuse_feed_planning_through_reduce_chains() {
        let ctx = SContext::with_default_userdata();

        assert!(ctx.can_feed(&'x'));
        assert!(ctx.can_feed(&'y'));
        assert!(!ctx.can_feed(&'z'));
        assert!(ctx.can_accept());
    }
}

mod glr_dynamic_precedence {
    use rusty_lr::lr1;

    lr1! {
        %glr;
        %tokentype char;
        %start S;

        S(i32)
            : 'x' %dprec 1 { 1 }
            | 'x' %dprec 2 { 2 }
            ;
    }

    #[test]
    fn dprec_selects_the_higher_priority_reduce_reduce_branch() {
        let mut ctx = SContext::with_default_userdata();
        ctx.feed('x').unwrap();
        assert_eq!(ctx.accept().unwrap(), (2, ()));
    }
}

mod reduce_action_errors {
    use rusty_lr::lr1;

    lr1! {
        %tokentype char;
        %error &'static str;
        %start Number;

        Digit(u32): d=['0'-'9'] { d.to_digit(10).unwrap() };

        Number(u32): digits=Digit+ {
            let value = digits.into_iter().fold(0, |acc, digit| acc * 10 + digit);
            if value <= 255 {
                value
            } else {
                return Err("number is larger than u8");
            }
        };
    }

    #[test]
    fn reduce_action_result_can_reject_semantic_values() {
        let mut ok = NumberContext::with_default_userdata();
        for ch in "255".chars() {
            ok.feed(ch).unwrap();
        }
        assert_eq!(ok.accept().unwrap(), (255, ()));

        let mut too_large = NumberContext::with_default_userdata();
        for ch in "256".chars() {
            too_large.feed(ch).unwrap();
        }
        assert!(too_large.accept().is_err());
    }

    #[test]
    fn deterministic_accept_no_action_is_reusable_but_success_consumes_context() {
        let mut ctx = NumberContext::with_default_userdata();

        let too_early = ctx.accept().unwrap_err();
        assert_eq!(too_early.reduce_action_error(), None);
        assert!(!too_early.is_consumed_context());

        ctx.feed('7').unwrap();
        assert_eq!(ctx.accept().unwrap(), (7, ()));

        let consumed = ctx.feed('8').unwrap_err();
        assert!(consumed.is_consumed_context());
    }

    mod deterministic_error_userdata {
        use rusty_lr::lr1;

        lr1! {
            %nooptim;
            %tokentype char;
            %userdata Vec<&'static str>;
            %error &'static str;
            %start S;

            S(()): A 'y' { () };

            A(()): 'x' {
                data.push("reduced a");
                if true {
                    return Err("bad a");
                }
                ()
            };
        }

        #[test]
        fn deterministic_reduce_action_error_returns_userdata() {
            let mut ctx = SContext::new(vec!["seed"]);
            ctx.feed('x').unwrap();

            let err = ctx.feed('y').unwrap_err();

            assert_eq!(err.reduce_action_error().unwrap(), &"bad a");
            assert_eq!(err.userdata(), Some(&vec!["seed", "reduced a"]));
            assert!(ctx.userdata_all().next().is_none());
            assert!(!ctx.can_feed(&'y'));
            assert!(!ctx.can_accept());

            let consumed = ctx.feed('y').unwrap_err();
            assert!(consumed.is_consumed_context());
            assert!(consumed.userdata().is_none());
        }
    }

    mod glr_accept_consume {
        use rusty_lr::lr1;

        lr1! {
            %glr;
            %tokentype char;
            %start S;

            S(char): 'x' { 'x' };
        }

        #[test]
        fn glr_accept_success_consumes_context() {
            let mut ctx = SContext::with_default_userdata();
            ctx.feed('x').unwrap();
            assert_eq!(ctx.accept().unwrap(), ('x', ()));

            let consumed = ctx.feed('x').unwrap_err();
            assert!(consumed.is_consumed_context());
            assert!(!ctx.can_accept());
        }

        #[test]
        fn glr_feed_no_action_is_reusable() {
            let mut ctx = SContext::with_default_userdata();

            let err = ctx.feed('y').unwrap_err();
            assert_eq!(err.branch_errors.len(), 1);
            assert!(err.branch_errors[0].reduce_action_error().is_none());
            assert!(!err.is_consumed_context());
            assert!(ctx.can_feed(&'x'));

            ctx.feed('x').unwrap();
            assert_eq!(ctx.accept().unwrap(), ('x', ()));
        }

        #[test]
        fn glr_accept_no_action_is_reusable() {
            let mut ctx = SContext::with_default_userdata();

            let err = ctx.accept().unwrap_err();
            assert_eq!(err.branch_errors.len(), 1);
            assert!(err.branch_errors[0].reduce_action_error().is_none());
            assert!(!err.is_consumed_context());
            assert!(!ctx.can_accept());
            assert!(ctx.can_feed(&'x'));

            ctx.feed('x').unwrap();
            assert_eq!(ctx.accept().unwrap(), ('x', ()));
        }
    }

    mod glr_recovery {
        use rusty_lr::lr1;

        // Keep optimization disabled so `Rejected` remains a distinct reduction that fails before
        // the following terminal is shifted. This makes the recovery boundary observable.
        lr1! {
            %glr;
            %nooptim;
            %tokentype char;
            %userdata Vec<&'static str>;
            %error &'static str;
            %start Item;

            Item(char)
                : Rejected 'y' {
                    Rejected
                }
                | error 'y' {
                    data.push("recovered");
                    '?'
                }
                ;

            Rejected(char): 'x' {
                data.push("rejected branch");
                if true {
                    return Err("rejected token");
                }
                'x'
            };
        }

        #[test]
        fn glr_reduce_action_error_does_not_enter_recovery_mode() {
            let mut ctx = ItemContext::new(Vec::new());
            ctx.feed('x').unwrap();

            // `error 'y'` could recover from a grammatical `NoAction`, but the failing branch is
            // grammatically valid and dies from a reduce-action error instead.
            let err = ctx.feed('y').unwrap_err();

            assert_eq!(err.branch_errors.len(), 1);
            assert_eq!(
                err.branch_errors[0].reduce_action_error(),
                Some(&"rejected token")
            );
            assert_eq!(err.branch_errors[0].userdata(), &vec!["rejected branch"]);
            assert!(ctx.userdata_all().all(|userdata| userdata.is_empty()));
            assert!(!ctx.can_feed(&'y'));
            assert!(!ctx.can_accept());

            let consumed = ctx.feed('y').unwrap_err();
            assert!(consumed.is_consumed_context());
            let consumed = ctx.accept().unwrap_err();
            assert!(consumed.is_consumed_context());
        }
    }

    mod glr_partial_errors {
        use rusty_lr::lr1;

        // This grammar keeps one shift path alive while a competing reduce path fails
        // semantically, exercising successful feeds with pruned-branch diagnostics.
        lr1! {
            %glr;
            %nooptim;
            %tokentype char;
            %error &'static str;
            %start S;

            S(&'static str)
                : A 'y' {
                    "reduce path"
                }
                | 'x' 'y' {
                    "shift path"
                }
                ;

            A(char): 'x' {
                if true {
                    return Err("bad reduce path");
                }
                'x'
            };
        }

        #[test]
        fn glr_feed_success_reports_pruned_branch_errors() {
            let mut ctx = SContext::with_default_userdata();
            assert!(ctx.feed('x').unwrap().errors.is_none());

            // Feeding `y` succeeds through the direct shift branch, while the reduce branch reports
            // its semantic failure through `FeedSuccess::errors`.
            let success = ctx.feed('y').unwrap();
            let errors = success.errors.expect("reduce branch should be reported");

            assert_eq!(errors.branch_errors.len(), 1);
            assert_eq!(
                errors.branch_errors[0].reduce_action_error(),
                Some(&"bad reduce path")
            );
            assert_eq!(ctx.accept().unwrap(), ("shift path", ()));
        }
    }

    mod glr_multiple_branch_errors {
        use rusty_lr::lr1;

        // Both reduce alternatives are grammatically valid for the lookahead, but each semantic
        // action rejects its own GLR branch. The resulting parse error should preserve that branch
        // boundary instead of flattening the reduce-action errors into one parser-wide list.
        lr1! {
            %glr;
            %nooptim;
            %tokentype char;
            %error &'static str;
            %start S;

            S(&'static str)
                : A 'y' {
                    "a"
                }
                | B 'y' {
                    "b"
                }
                ;

            A(char): 'x' {
                if true {
                    return Err("bad a");
                }
                'a'
            };

            B(char): 'x' {
                if true {
                    return Err("bad b");
                }
                'b'
            };
        }

        #[test]
        fn glr_parse_error_keeps_reduce_errors_grouped_by_branch() {
            let mut ctx = SContext::with_default_userdata();
            ctx.feed('x').unwrap();

            let err = ctx.feed('y').unwrap_err();

            assert_eq!(err.branch_errors.len(), 2);
            for branch in &err.branch_errors {
                assert_eq!(branch.states().count(), 1);
                assert!(branch.reduce_action_error().is_some());
            }

            let mut reduce_errors: Vec<_> = err
                .branch_errors
                .iter()
                .map(|branch| *branch.reduce_action_error().unwrap())
                .collect();
            reduce_errors.sort();
            assert_eq!(reduce_errors, ["bad a", "bad b"]);

            assert!(!err.is_consumed_context());
            assert!(!ctx.can_feed(&'y'));
            assert!(!ctx.can_accept());

            let consumed = ctx.feed('y').unwrap_err();
            assert!(consumed.is_consumed_context());
            let consumed = ctx.accept().unwrap_err();
            assert!(consumed.is_consumed_context());
        }
    }

    mod glr_mixed_branch_failure_reuse {
        mod feed {
            use rusty_lr::lr1;

            lr1! {
                %glr;
                %nooptim;
                %tokentype char;
                %error &'static str;
                %start S;

                S(&'static str)
                    : A 'q' {
                        "a"
                    }
                    | B 'r' {
                        "b"
                    }
                    ;

                A(()): X 'p' {
                    if true {
                        return Err("bad reduce");
                    }
                    ()
                };

                B(()): Y 'p' { () };
                X(()): 'x' { () };
                Y(()): 'x' { () };
            }

            #[test]
            fn glr_feed_restores_no_action_branch_when_sibling_reduce_fails() {
                let mut ctx = SContext::with_default_userdata();
                ctx.feed('x').unwrap();
                ctx.feed('p').unwrap();

                let err = ctx.feed('q').unwrap_err();

                assert!(!err.is_consumed_context());
                assert_eq!(err.branch_errors.len(), 2);
                assert_eq!(
                    err.branch_errors
                        .iter()
                        .filter(|branch| branch.reduce_action_error().is_none())
                        .count(),
                    1
                );
                assert_eq!(
                    err.branch_errors
                        .iter()
                        .filter(|branch| branch.reduce_action_error() == Some(&"bad reduce"))
                        .count(),
                    1
                );
                assert!(ctx.can_feed(&'r'));

                ctx.feed('r').unwrap();
                assert_eq!(ctx.accept().unwrap(), ("b", ()));
            }
        }

        mod accept {
            use rusty_lr::lr1;

            lr1! {
                %glr;
                %nooptim;
                %tokentype char;
                %error &'static str;
                %start S;

                S(&'static str)
                    : A {
                        "a"
                    }
                    | B 'r' {
                        "b"
                    }
                    ;

                A(()): X 'p' {
                    if true {
                        return Err("bad reduce");
                    }
                    ()
                };

                B(()): Y 'p' { () };
                X(()): 'x' { () };
                Y(()): 'x' { () };
            }

            #[test]
            fn glr_accept_restores_no_action_branch_when_sibling_reduce_fails() {
                let mut ctx = SContext::with_default_userdata();
                ctx.feed('x').unwrap();
                ctx.feed('p').unwrap();

                let err = ctx.accept().unwrap_err();

                assert!(!err.is_consumed_context());
                assert_eq!(err.branch_errors.len(), 2);
                assert_eq!(
                    err.branch_errors
                        .iter()
                        .filter(|branch| branch.reduce_action_error().is_none())
                        .count(),
                    1
                );
                assert_eq!(
                    err.branch_errors
                        .iter()
                        .filter(|branch| branch.reduce_action_error() == Some(&"bad reduce"))
                        .count(),
                    1
                );
                assert!(ctx.can_feed(&'r'));

                ctx.feed('r').unwrap();
                assert_eq!(ctx.accept().unwrap(), ("b", ()));
            }
        }
    }

    mod glr_recovery_partial_errors {
        use rusty_lr::lr1;

        // Recovery can have GLR alternatives too. A semantic failure in one recovery branch should
        // be reported without killing sibling recovery branches that successfully shifted `error`.
        lr1! {
            %glr;
            %nooptim;
            %tokentype char;
            %error &'static str;
            %start S;

            S(&'static str)
                : Good error 's' {
                    "recovered"
                }
                | Bad error 's' {
                    "bad"
                }
                ;

            Good(()): "" { () };

            Bad(()): "" {
                if true {
                    return Err("bad recovery branch");
                }
                ()
            };
        }

        #[test]
        fn glr_recovery_success_reports_pruned_recovery_branch_errors() {
            let mut ctx = SContext::with_default_userdata();

            let success = ctx.feed('x').unwrap();
            let errors = success
                .errors
                .expect("failed recovery branch should be reported");
            assert_eq!(errors.branch_errors.len(), 1);
            assert_eq!(
                errors.branch_errors[0].reduce_action_error(),
                Some(&"bad recovery branch")
            );
            ctx.debug_check();

            ctx.feed('s').unwrap();
            ctx.debug_check();
            assert_eq!(ctx.accept().unwrap(), ("recovered", ()));
        }
    }

    mod glr_failed_sibling_cleanup {
        use rusty_lr::lr1;

        // One nullable sibling succeeds, while another reaches a deeper reduce-action failure.
        // The failed branch must be detached from the GSS when the successful sibling survives.
        lr1! {
            %glr;
            %nooptim;
            %tokentype char;
            %error &'static str;
            %start S;

            S(&'static str)
                : Good GoodTail {
                    "good"
                }
                | Bad BadTail {
                    "bad"
                }
                ;

            Good(()): "" { () };
            GoodTail(()): 'x' { () };

            Bad(()): "" { () };
            BadTail(()): BadReduce 'x' { () };
            BadReduce(()): "" {
                if true {
                    return Err("bad nested branch");
                }
                ()
            };
        }

        #[test]
        fn glr_feed_cleans_failed_sibling_nodes_when_another_sibling_survives() {
            let mut ctx = SContext::with_default_userdata();
            let success = ctx.feed('x').unwrap();

            let errors = success
                .errors
                .expect("failed sibling branch should be reported");
            assert_eq!(errors.branch_errors.len(), 1);
            assert_eq!(
                errors.branch_errors[0].reduce_action_error(),
                Some(&"bad nested branch")
            );

            ctx.debug_check();
            assert_eq!(ctx.accept().unwrap(), ("good", ()));
        }
    }

    mod glr_no_action_sibling_cleanup {
        use rusty_lr::lr1;

        // Feeding `p` creates two live GLR branches. The following `q` keeps only the `A`
        // branch, so the `B` branch is a grammatical NoAction sibling and must be removed.
        lr1! {
            %glr;
            %nooptim;
            %tokentype char;
            %start S;

            S(&'static str)
                : A 'q' {
                    "a"
                }
                | B 'r' {
                    "b"
                }
                ;

            A(()): X 'p' { () };
            B(()): Y 'p' { () };
            X(()): 'x' { () };
            Y(()): 'x' { () };
        }

        #[test]
        fn glr_feed_cleans_no_action_sibling_nodes_when_another_branch_survives() {
            let mut ctx = SContext::with_default_userdata();

            ctx.feed('x').unwrap();
            ctx.feed('p').unwrap();
            ctx.debug_check();

            let success = ctx.feed('q').unwrap();
            assert!(success.errors.is_some());

            ctx.debug_check();
            assert_eq!(ctx.accept().unwrap(), ("a", ()));
        }
    }

    mod glr_panic_mode_node_cleanup {
        use rusty_lr::lr1;

        // The `abc` branch lets `b` be shifted normally. When the next token is invalid,
        // recovery must pop that shifted `b` node back to the state after `a`, where
        // `error 's'` is available. The popped node must be removed from the GSS.
        lr1! {
            %glr;
            %nooptim;
            %tokentype char;
            %start S;

            S(&'static str)
                : 'a' error 's' {
                    "recovered"
                }
                | 'a' 'b' 'c' {
                    "abc"
                }
                ;
        }

        #[test]
        fn glr_panic_mode_cleans_nodes_popped_during_recovery() {
            let mut ctx = SContext::with_default_userdata();

            ctx.feed('a').unwrap();
            ctx.feed('b').unwrap();
            ctx.debug_check();
            assert!(ctx.can_panic());

            ctx.feed('x').unwrap();
            ctx.debug_check();

            ctx.feed('s').unwrap();
            ctx.debug_check();
            assert_eq!(ctx.accept().unwrap(), ("recovered", ()));
        }
    }

    mod glr_conflict_free_reduce_reuse {
        use rusty_lr::{Location, lr1};
        use std::sync::atomic::{AtomicUsize, Ordering};

        static DATA_CLONES: AtomicUsize = AtomicUsize::new(0);
        static WATCHED_LOCATION_CLONES: AtomicUsize = AtomicUsize::new(0);

        #[derive(Debug, PartialEq, Eq)]
        struct CloneWatchedValue(&'static str);

        impl Clone for CloneWatchedValue {
            fn clone(&self) -> Self {
                DATA_CLONES.fetch_add(1, Ordering::SeqCst);
                Self(self.0)
            }
        }

        #[derive(Debug, PartialEq, Eq)]
        struct CloneWatchedLocation {
            reduced: bool,
        }

        impl Clone for CloneWatchedLocation {
            fn clone(&self) -> Self {
                if self.reduced {
                    WATCHED_LOCATION_CLONES.fetch_add(1, Ordering::SeqCst);
                }
                Self {
                    reduced: self.reduced,
                }
            }
        }

        impl Location for CloneWatchedLocation {
            fn new<'a>(_stack: impl Iterator<Item = &'a Self> + Clone, len: usize) -> Self
            where
                Self: 'a,
            {
                Self { reduced: len > 0 }
            }
        }

        lr1! {
            %glr;
            %nooptim;
            %tokentype char;
            %location CloneWatchedLocation;
            %start S;

            S(CloneWatchedValue): Pair 'c' { Pair };
            Pair(CloneWatchedValue): Item 'b' { Item };
            Item(CloneWatchedValue): 'a' { CloneWatchedValue("item") };
        }

        #[test]
        fn glr_conflict_free_reduce_does_not_clone_semantic_or_location_stacks() {
            DATA_CLONES.store(0, Ordering::SeqCst);
            WATCHED_LOCATION_CLONES.store(0, Ordering::SeqCst);

            let mut ctx = SContext::with_default_userdata();
            ctx.feed_location('a', CloneWatchedLocation { reduced: false })
                .unwrap();
            ctx.feed_location('b', CloneWatchedLocation { reduced: false })
                .unwrap();
            ctx.feed_location('c', CloneWatchedLocation { reduced: false })
                .unwrap();

            let (value, _) = ctx.accept().unwrap();
            assert_eq!(value, CloneWatchedValue("item"));
            assert_eq!(DATA_CLONES.load(Ordering::SeqCst), 0);
            assert_eq!(WATCHED_LOCATION_CLONES.load(Ordering::SeqCst), 0);
        }
    }
}
