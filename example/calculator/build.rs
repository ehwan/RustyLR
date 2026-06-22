use rusty_lr::build;

fn main() {
    println!("cargo::rerun-if-changed=src/parser.rustylr");
    let output = format!("{}/parser.rs", std::env::var("OUT_DIR").unwrap());

    build::Builder::new()
        .file("src/parser.rustylr")
        .build(&output);
}
