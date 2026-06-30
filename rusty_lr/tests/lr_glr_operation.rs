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
        let ctx = CsvContext::with_default_userdata();
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
    fn glr_parser_reports_an_error_when_all_branches_die() {
        let mut ctx = SContext::with_default_userdata();
        assert!(ctx.feed('b').is_err());
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
}
