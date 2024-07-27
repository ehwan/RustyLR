mod parser;
mod parser2;

fn main() {
    let p = parser::EParser::new();

    let input = " 1  + 2*(3   + 4)  ";
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
}
