pub mod parser;

fn main() {
    let parser = parser::EParser::new();
    let mut context = parser::EContext::new();

    let input = "1+2*3+4";
    for ch in input.chars() {
        println!("feed: {}, possible: {}", ch, context.can_feed(&parser, &ch));
        match context.feed(&parser, ch, &mut ()) {
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
    let result = match context.accept(&parser, &mut ()) {
        Ok(mut results) => results.next().unwrap(),
        Err(e) => {
            println!("Error: {}", e);
            return;
        }
    };
    println!("Result: {}", result);

    let input = "1+2**3+4";
    let mut context = parser::EContext::new();
    for ch in input.chars() {
        println!(
            "feed: {}, can_feed(): {}",
            ch,
            context.can_feed(&parser, &ch)
        );
        match context.feed(&parser, ch, &mut ()) {
            Ok(_) => {
                println!("nodes: {}", context.len_paths());
            }
            Err(e) => {
                println!("Error: {}", e);
                return;
            }
        }
    }

    for result in context.accept(&parser, &mut ()).unwrap() {
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
