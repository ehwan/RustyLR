#!/bin/sh

# Booststrap rusty_lr_parser/src/parser/parser.rs with different configurations
# and check if the output files are identical.

compare_files() {
    # Check if both arguments are provided
    if [ $# -ne 2 ]; then
        echo "Usage: compare_files <file1> <file2>"
        return 1
    fi

    local file1="$1"
    local file2="$2"

    # Check if both files exist
    if [ ! -f "$file1" ]; then
        echo "Error: File '$file1' does not exist"
        return 1
    fi

    if [ ! -f "$file2" ]; then
        echo "Error: File '$file2' does not exist"
        return 1
    fi

    # Run diff and capture the exit code
    if diff "$file1" "$file2" > /dev/null; then
        echo "Files are identical"
        return 0
    else
        echo "Error: Files are different"
        diff "$file1" "$file2"
        return 1
    fi
}

# Get the directory where this script is located and go up one level
script_dir="$(dirname "$0")"
rustylr_path="$(realpath "$script_dir/..")"

echo "RustyLR path: $rustylr_path"

echo "Setting GLR = true"
cargo run --bin rustylr -- "$rustylr_path/rusty_lr_parser/src/parser/parser.rs" out.tab.rs --glr true > /dev/null
mv out.tab.rs "$rustylr_path/rusty_lr_parser/src/parser/parser_expanded.rs"
cargo run --bin rustylr -- "$rustylr_path/rusty_lr_parser/src/parser/parser.rs" out.tab.rs --glr true > /dev/null
compare_files "$rustylr_path/rusty_lr_parser/src/parser/parser_expanded.rs" out.tab.rs

echo "Setting GLR = false"
cargo run --bin rustylr -- "$rustylr_path/rusty_lr_parser/src/parser/parser.rs" out.tab.rs --glr false > /dev/null
mv out.tab.rs "$rustylr_path/rusty_lr_parser/src/parser/parser_expanded.rs"
cargo run --bin rustylr -- "$rustylr_path/rusty_lr_parser/src/parser/parser.rs" out.tab.rs --glr false > /dev/null
compare_files "$rustylr_path/rusty_lr_parser/src/parser/parser_expanded.rs" out.tab.rs

echo "Setting Runtime = true"
cargo run --bin rustylr -- "$rustylr_path/rusty_lr_parser/src/parser/parser.rs" out.tab.rs --runtime true > /dev/null
mv out.tab.rs "$rustylr_path/rusty_lr_parser/src/parser/parser_expanded.rs"
cargo run --bin rustylr -- "$rustylr_path/rusty_lr_parser/src/parser/parser.rs" out.tab.rs --runtime true > /dev/null
compare_files "$rustylr_path/rusty_lr_parser/src/parser/parser_expanded.rs" out.tab.rs

echo "Setting Runtime = false"
cargo run --bin rustylr -- "$rustylr_path/rusty_lr_parser/src/parser/parser.rs" out.tab.rs --runtime false > /dev/null
mv out.tab.rs "$rustylr_path/rusty_lr_parser/src/parser/parser_expanded.rs"
cargo run --bin rustylr -- "$rustylr_path/rusty_lr_parser/src/parser/parser.rs" out.tab.rs --runtime false > /dev/null
compare_files "$rustylr_path/rusty_lr_parser/src/parser/parser_expanded.rs" out.tab.rs

echo "Setting Dense = true"
cargo run --bin rustylr -- "$rustylr_path/rusty_lr_parser/src/parser/parser.rs" out.tab.rs --dense true > /dev/null
mv out.tab.rs "$rustylr_path/rusty_lr_parser/src/parser/parser_expanded.rs"
cargo run --bin rustylr -- "$rustylr_path/rusty_lr_parser/src/parser/parser.rs" out.tab.rs --dense true > /dev/null
compare_files "$rustylr_path/rusty_lr_parser/src/parser/parser_expanded.rs" out.tab.rs

echo "Setting Dense = false"
cargo run --bin rustylr -- "$rustylr_path/rusty_lr_parser/src/parser/parser.rs" out.tab.rs --dense false > /dev/null
mv out.tab.rs "$rustylr_path/rusty_lr_parser/src/parser/parser_expanded.rs"
cargo run --bin rustylr -- "$rustylr_path/rusty_lr_parser/src/parser/parser.rs" out.tab.rs --dense false > /dev/null
compare_files "$rustylr_path/rusty_lr_parser/src/parser/parser_expanded.rs" out.tab.rs

# boostrap itself with normal configuration
cargo run --bin rustylr -- "$rustylr_path/rusty_lr_parser/src/parser/parser.rs" out.tab.rs > /dev/null
mv out.tab.rs "$rustylr_path/rusty_lr_parser/src/parser/parser_expanded.rs"