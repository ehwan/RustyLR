pub mod parser;
// pub mod parser_expanded;

fn main() {
    let p = parser::EParser::new();
    let mut c = p.begin();

    let input = "1+2*3+4";
    for ch in input.chars() {
        println!("feed: {}", ch);
        match p.feed(&mut c, ch) {
            Ok(_) => {
                println!("nodes: {}", c.current_nodes.len());
                c.backtrace(8, &p);
            }
            Err(e) => {
                println!("Error: {}", e);
                return;
            }
        }
    }
    println!("feed eof");
    match p.feed(&mut c, '\0') {
        Ok(_) => {
            println!("nodes: {}", c.current_nodes.len());
        }
        Err(e) => {
            println!("Error: {}", e);
        }
    }

    let result = match c.accept(&p) {
        Ok(n) => n,
        Err(e) => {
            println!("Error: {}", e);
            return;
        }
    };
    println!("Result: {}", result);

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
