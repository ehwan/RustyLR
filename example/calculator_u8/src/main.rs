pub mod parser;

fn main() {
    let input = "  1 +  20 *   (3 + 4 )   ";

    let parser = parser::EParser::new();
    let mut context = parser.begin();
    let mut userdata: i32 = 0;
    for b in input.as_bytes().iter() {
        match parser.feed(&mut context, *b, &mut userdata) {
            // feed userdata here
            Ok(_) => {}
            Err(e) => {
                eprintln!("error: {:?}", e);
                return;
            }
        }
    }
    parser.feed(&mut context, 0, &mut userdata).unwrap(); // feed EOF

    let result = context.accept(); // get value of start 'E'
    println!("result: {}", result);
    println!("userdata: {}", userdata);
}
