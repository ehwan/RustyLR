use rusty_lr::lr1;

#[derive(Clone)]
struct CloneValue;

lr1! {
    %tokentype char;
    %start S;

    S(CloneValue): { CloneValue };
}

fn main() {
    let context = SContext::with_default_userdata();
    let _cloned = context.clone();
}
