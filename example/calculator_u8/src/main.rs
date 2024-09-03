pub mod parser;

fn main() {
    let input = "  1 +  20 *   (3 + 4 )   ";

    let parser = parser::EParser::new();
    let mut context = parser::EContext::new();
    let mut userdata: i32 = 0;
    for b in input.chars() {
        match context.feed(&parser, b, &mut userdata) {
            // feed userdata here
            Ok(_) => {}
            Err(e) => {
                eprintln!("error: {:?}", e);
                return;
            }
        }
    }
    println!("{:?}", context);
    context.feed(&parser, 0 as char, &mut userdata).unwrap(); // feed EOF

    let result = context.accept(); // get value of start 'E'
    println!("result: {}", result);
    println!("userdata: {}", userdata);

    // invalid input, expect error
    let error_input = "1+2**(3+4)";
    let mut context = parser::EContext::new();
    let mut userdata: i32 = 0;
    for b in error_input.chars() {
        match context.feed(&parser, b, &mut userdata) {
            // feed userdata here
            Ok(_) => {}
            Err(e) => {
                // this will print error messages
                eprintln!("error: {:?}", e);

                // eprintln!("{:?}", context.backtrace(&parser));
                return;
            }
        }
    }
    context.feed(&parser, 0 as char, &mut userdata).unwrap(); // feed EOF
}
