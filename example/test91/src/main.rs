mod parser;

fn main() {
    let mut context = parser::ExprContext::with_default_userdata();
    context.feed('t').unwrap();

    let parses: Vec<_> = context.accept_all().unwrap().collect();
    println!("accepted {parses:#?}");
}
