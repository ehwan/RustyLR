pub mod parser;
// pub mod parser_expanded;

fn main() {
    let p = parser::EParser::new();
    let mut c = parser::EContext::new();

    let input = "1+2*3+4";
    for ch in input.chars() {
        println!("feed: {}, possible: {}", ch, c.can_feed(&p, &ch));
        match c.feed(&p, ch, &mut ()) {
            Ok(_) => {
                println!("nodes: {}", c.len_paths());
            }
            Err(e) => {
                println!("Error: {}", e);
                return;
            }
        }
    }
    println!("feed eof");
    match c.feed(&p, '\0', &mut ()) {
        Ok(_) => {
            println!("nodes: {}", c.len_paths());
        }
        Err(e) => {
            println!("Error: {}", e);
        }
    }
    let result = c.accept().next().unwrap();
    println!("Result: {}", result);

    let input = "1+2**3+4";
    let mut c = parser::EContext::new();
    for ch in input.chars() {
        println!("feed: {}, can_feed(): {}", ch, c.can_feed(&p, &ch));
        match c.feed(&p, ch, &mut ()) {
            Ok(_) => {
                println!("nodes: {}", c.len_paths());
            }
            Err(e) => {
                println!("Error: {}", e);
                return;
            }
        }
    }
    println!("feed eof");
    match c.feed(&p, '\0', &mut ()) {
        Ok(_) => {
            println!("nodes: {}", c.len_paths());
        }
        Err(e) => {
            println!("Error: {}", e);
        }
    }

    for result in c.accept() {
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
