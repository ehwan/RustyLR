use rusty_lr::lr1;

struct NotClone;

lr1! {
    %tokentype char;
    %start S;

    S(NotClone): { NotClone };
}

fn main() {
    let mut context = SContext::with_default_userdata();
    let (_value, _userdata) = context.accept().unwrap();
}
