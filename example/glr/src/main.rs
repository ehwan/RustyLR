pub mod parser;

fn main() {
    let mut context = parser::EContext::with_default_userdata();

    let input = "1+2*3+4";
    for ch in input.chars() {
        println!("feed: {}, possible: {}", ch, context.can_feed(&ch));
        match context.feed(ch) {
            Ok(_) => {
                println!("nodes: {}", context.len_paths());
            }
            Err(e) => {
                println!("Error: {}", e);
                return;
            }
        }
        context.debug_check();
    }
    let result = match context.accept() {
        Ok((result, _)) => result,
        Err(e) => {
            println!("Error: {}", e);
            return;
        }
    };
    println!("Result: {}", result);

    let input = "1+2**3+4";
    let mut context = parser::EContext::with_default_userdata();
    for ch in input.chars() {
        println!("feed: {}, can_feed(): {}", ch, context.can_feed(&ch));
        match context.feed(ch) {
            Ok(_) => {
                println!("nodes: {}", context.len_paths());
            }
            Err(e) => {
                println!("Error: {}", e);
                return;
            }
        }
    }

    for (result, _) in context.accept_all().unwrap() {
        println!("Result: {}", result);
    }

    // for mut n in c.current_nodes.nodes.into_iter() {
    //     loop {
    //         println!("{}", n.state());
    //         if let Some(par) = n.parent() {
    //             n = std::rc::Rc::clone(par);
    //         } else {
    //             break;
    //         }
    //     }
    //     println!("---");
    // }
}

#[test]
fn test_parser() {
    let mut context = parser::EContext::with_default_userdata();
    let input1 = "  1 + 2 * 3 * 4 + 5 * 6 + 7 ";
    for ch in input1.chars() {
        context.feed(ch).unwrap();
    }

    let answer = 1 + 2 * 3 * 4 + 5 * 6 + 7;

    let mut results = context
        .accept_all()
        .unwrap()
        .map(|(result, _)| result)
        .collect::<Vec<_>>();
    results.sort();
    assert_eq!(results.len(), 1);
    assert_eq!(results, [answer]);
}

#[test]
fn test_multiple_start_symbols() {
    // Test parsing with the EContext entry point
    {
        let mut context = parser::EContext::with_default_userdata();
        for ch in "12+34".chars() {
            context.feed(ch).unwrap();
        }
        let (val, _) = context.accept().unwrap();
        assert_eq!(val, 46);
    }

    // Test parsing with the NumberContext entry point
    {
        let mut context = parser::NumberContext::with_default_userdata();
        for ch in " 567 ".chars() {
            context.feed(ch).unwrap();
        }
        let (val, _) = context.accept().unwrap();
        assert_eq!(val, 567);
    }
}

#[cfg(test)]
mod userdata_branch_tests {
    use rusty_lr::lr1;

    lr1! {
        %glr;
        %tokentype char;
        %userdata Vec<&'static str>;
        %start S;

        S(i32): A;
        A(i32): 'a' {
            data.push("left");
            1
        }
        | 'a' {
            data.push("right");
            2
        };
    }

    #[test]
    fn userdata_is_cloned_for_glr_branches() {
        let mut context = SContext::new(Vec::new());
        context.feed('a').unwrap();

        let mut results = context.accept_all().unwrap().collect::<Vec<_>>();
        results.sort_by_key(|(value, _)| *value);

        assert_eq!(results.len(), 2);
        assert_eq!(results[0], (1, vec!["left"]));
        assert_eq!(results[1], (2, vec!["right"]));
    }
}

/// Regression test for <https://github.com/ehwan/RustyLR/issues/89>:
/// GLR parser panicked on an optional branch `(A B)?` when the shift and the
/// empty-reduce were both active for the same lookahead token.
#[cfg(test)]
mod issue_89_optional_empty_branch {
    use rusty_lr::lr1;

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum Token {
        Amp,
        SelfKw,
        Bool,
        Ident,
        Colon,
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum Param {
        RefSelf,
        RefBool,
    }

    lr1! {
        %glr;
        %tokentype Token;
        %start Param;

        %token amp   Token::Amp;
        %token self_kw Token::SelfKw;
        %token bool_kw Token::Bool;
        %token ident Token::Ident;
        %token colon Token::Colon;

        Param(Param)
            : amp self_kw { Param::RefSelf }
            | (ident colon)? amp bool_kw { Param::RefBool }
            ;
    }

    #[test]
    fn refself_no_panic() {
        // `amp self_kw` — previously caused a panic inside feed_location_impl
        // due to a non-leaf node being pushed to next_nodes after an empty
        // reduce produced a child of the original node.
        let mut ctx = ParamContext::with_default_userdata();
        ctx.feed(Token::Amp).unwrap();
        ctx.feed(Token::SelfKw).unwrap();
        let mut results: Vec<_> = ctx.accept_all().unwrap().collect();
        results.sort_by_key(|(p, _)| matches!(p, Param::RefBool) as u8);
        assert_eq!(results, vec![(Param::RefSelf, ())]);
    }

    #[test]
    fn refbool_without_optional() {
        // `amp bool_kw` — optional part is absent
        let mut ctx = ParamContext::with_default_userdata();
        ctx.feed(Token::Amp).unwrap();
        ctx.feed(Token::Bool).unwrap();
        let results: Vec<_> = ctx.accept_all().unwrap().collect();
        assert_eq!(results, vec![(Param::RefBool, ())]);
    }

    #[test]
    fn refbool_with_optional() {
        // `ident colon amp bool_kw` — optional part is present
        let mut ctx = ParamContext::with_default_userdata();
        ctx.feed(Token::Ident).unwrap();
        ctx.feed(Token::Colon).unwrap();
        ctx.feed(Token::Amp).unwrap();
        ctx.feed(Token::Bool).unwrap();
        let results: Vec<_> = ctx.accept_all().unwrap().collect();
        assert_eq!(results, vec![(Param::RefBool, ())]);
    }
}
