mod parser;

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
    let mut context = parser.begin();
    let mut userdata: i32 = 0;
    for token in input {
        match parser.feed(&mut context, token, &mut userdata) {
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
