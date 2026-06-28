use rusty_lr::lr1;

struct NotClone;

lr1! {
    %tokentype char;
    %start S;

    S(NotClone): { NotClone };
}

fn assert_clone<T: Clone>() {}

fn main() {
    assert_clone::<SContext>();
}
