mod parser;

fn main() {
    let p = parser::EParser::new();

    let input = "1+2*(3+4)";
    let res = match p.parse_str(input, 0 as char) {
        Ok(res) => res,
        Err(e) => {
            println!("Error: {}", e);
            return;
        }
    };
    println!("Result: {}", res);
}
