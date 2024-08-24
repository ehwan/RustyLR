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
                println!("nodes: {}", c.current_nodes.nodes.len());
            }
            Err(e) => {
                println!("Error: {:?}", e);
                break;
            }
        }
    }
    println!("feed eof");
    match p.feed(&mut c, '\0') {
        Ok(_) => {
            println!("nodes: {}", c.current_nodes.nodes.len());
        }
        Err(e) => {
            println!("Error: {:?}", e);
        }
    }

    println!("{}", c.accept());

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
