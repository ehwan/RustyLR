pub mod parser;

fn main() {
    let input = "  1 +  2 *   (3 + 4 )   ";

    let parser = parser::EParser::new();
    let mut context = parser.begin();
    for b in input.as_bytes().iter() {
        match parser.feed(&mut context, *b) {
            Ok(_) => {}
            Err(e) => {
                eprintln!("error: {:?}", e);
                return;
            }
        }
    }
    parser.feed(&mut context, 0).unwrap();

    let result = context.accept();
    println!("result: {}", result);
}
