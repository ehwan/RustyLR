pub mod parser;

fn main() {
    let input = "  1 +  20 *   (3 + 4 )   ";

    let parser = parser::EParser::new();
    let mut context = parser.begin();
    let mut userdata: i32 = 0;
    for b in input.chars() {
        match parser.feed(&mut context, b, &mut userdata) {
            // feed userdata here
            Ok(_) => {}
            Err(e) => {
                eprintln!("error: {:?}", e);
                return;
            }
        }
    }
    context.to_tree_list().pretty_print_debug(2);
    // println!("{:?}", context.to_tree_list());
    parser.feed(&mut context, 0 as char, &mut userdata).unwrap(); // feed EOF

    let result = context.accept(); // get value of start 'E'
    println!("result: {}", result);
    println!("userdata: {}", userdata);

    // invalid input, expect error
    let error_input = "1+2**(3+4)";
    let mut context = parser.begin();
    let mut userdata: i32 = 0;
    for b in error_input.chars() {
        match parser.feed(&mut context, b, &mut userdata) {
            // feed userdata here
            Ok(_) => {}
            Err(e) => {
                // this will print error messages
                eprintln!("error: {}", e);
                let message = e.long_message(&parser, &context);
                eprintln!("long message: {}", message);
                return;
            }
        }
    }
    parser.feed(&mut context, 0 as char, &mut userdata).unwrap(); // feed EOF
}
