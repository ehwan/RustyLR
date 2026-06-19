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
        Ok(mut results) => results.next().unwrap().0,
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

    for (result, _) in context.accept().unwrap() {
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
        .accept()
        .unwrap()
        .map(|(result, _)| result)
        .collect::<Vec<_>>();
    results.sort();
    assert_eq!(results.len(), 1);
    assert_eq!(results, [answer]);
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

        let mut results = context.accept().unwrap().collect::<Vec<_>>();
        results.sort_by_key(|(value, _)| *value);

        assert_eq!(results.len(), 2);
        assert_eq!(results[0], (1, vec!["left"]));
        assert_eq!(results[1], (2, vec!["right"]));
    }
}
