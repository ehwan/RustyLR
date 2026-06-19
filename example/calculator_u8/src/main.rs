pub mod parser_expanded;
use parser_expanded as parser;

fn main() {
    let input = "  1 +  -20 *   (3 + 4 )   ";

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

    let result = context.accept(&parser, &mut userdata).unwrap(); // get value of start 'E'
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_calculator_u8_success() {
        let input = "  1 +  -20 *   (3 + 4 )   ";
        let parser = parser::EParser::new();
        let mut context = parser::EContext::new();
        let mut userdata: i32 = 0;
        for b in input.chars() {
            context
                .feed(&parser, b, &mut userdata)
                .expect("Failed to feed char");
        }
        let result = context
            .accept(&parser, &mut userdata)
            .expect("Failed to accept");
        assert_eq!(result, -139.0);
    }

    #[test]
    fn test_calculator_u8_invalid_input() {
        let error_input = "1+2**(3+4)";
        let parser = parser::EParser::new();
        let mut context = parser::EContext::new();
        let mut userdata: i32 = 0;

        let mut has_err = false;
        for b in error_input.chars() {
            if context.feed(&parser, b, &mut userdata).is_err() {
                has_err = true;
                break;
            }
        }
        assert!(has_err);
    }
}
