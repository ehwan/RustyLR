mod parser;
mod parser2;

fn main() {
    let input = " 1  + 2*(3   + 4)  ";

    // version1: parse with string
    let p = parser::EParser::new();
    let mut number_of_num: i32 = 0;
    // `number_of_num` passed to parser as user_data
    let res = match p.parse_str(input, 0 as char, &mut number_of_num) {
        Ok(res) => res,
        Err(e) => {
            println!("Error: {}", e);
            return;
        }
    };
    println!("Result: {}", res);
    println!("Number of 'Num' in {}: {}", input, number_of_num);

    // version2: tokenizing and parse with tokens
    let mut tokens = parser2::TokensParser::new()
        .parse_str(input, 0 as char)
        .unwrap();
    tokens.reverse();
    println!("{:?}", tokens);

    let res = parser2::EParser::new()
        .parse(&tokens, parser2::Token::Eof)
        .unwrap();
    println!("{}", res);
}
