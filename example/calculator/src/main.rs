mod parser_expanded;

use parser_expanded as parser;

fn main() {
    use parser::Token;
    let input = vec![
        Token::Num(1),
        Token::Plus,
        Token::Num(2),
        Token::Star,
        Token::LParen,
        Token::Num(3),
        Token::Plus,
        Token::Num(4),
        Token::RParen,
        Token::Eof,
    ];

    let parser = parser::EParser::new();
    let mut context = parser::EContext::new();
    let mut userdata: i32 = 0;
    for token in input {
        match context.feed(&parser, token, &mut userdata) {
            //                          ^^^^^   ^^^^^^^^^^^^ userdata passed here as `&mut i32`
            //                           |- feed token
            Ok(_) => {}
            Err(e) => {
                println!("{:?}", e);
                return;
            }
        }
    }
    // res = value of start symbol ( E(i32) )
    let res = context.accept();
    println!("{}", res);
    println!("userdata: {}", userdata);
}
