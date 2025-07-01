#!/bin/sh

# Bootstrap rusty_lr_parser/src/parser/parser.rs with different configurations
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

process_and_compare() {
    local config="$1"
    cargo run --bin rustylr -- "$rustylr_path/rusty_lr_parser/src/parser/parser.rs" out.tab.rs $config > /dev/null
    mv out.tab.rs "$rustylr_path/rusty_lr_parser/src/parser/parser_expanded.rs"
    cargo run --bin rustylr -- "$rustylr_path/rusty_lr_parser/src/parser/parser.rs" out.tab.rs $config > /dev/null
    compare_files "$rustylr_path/rusty_lr_parser/src/parser/parser_expanded.rs" out.tab.rs
    if [ $? -ne 0 ]; then
        exit 1
    fi
}

echo "RustyLR path: $rustylr_path"

echo "Setting GLR = true"
process_and_compare "--glr true"

echo "Setting GLR = false"
process_and_compare "--glr false"

echo "Setting Runtime = true"
process_and_compare "--runtime true"
compare_files "$rustylr_path/rusty_lr_parser/src/parser/parser_expanded.rs" out.tab.rs
if [ $? -ne 0 ]; then
    exit 1
fi

echo "Setting Runtime = false"
cargo run --bin rustylr -- "$rustylr_path/rusty_lr_parser/src/parser/parser.rs" out.tab.rs --runtime false > /dev/null
mv out.tab.rs "$rustylr_path/rusty_lr_parser/src/parser/parser_expanded.rs"
cargo run --bin rustylr -- "$rustylr_path/rusty_lr_parser/src/parser/parser.rs" out.tab.rs --runtime false > /dev/null
compare_files "$rustylr_path/rusty_lr_parser/src/parser/parser_expanded.rs" out.tab.rs
if [ $? -ne 0 ]; then
    exit 1
fi

echo "Setting Dense = true"
cargo run --bin rustylr -- "$rustylr_path/rusty_lr_parser/src/parser/parser.rs" out.tab.rs --dense true > /dev/null
mv out.tab.rs "$rustylr_path/rusty_lr_parser/src/parser/parser_expanded.rs"
cargo run --bin rustylr -- "$rustylr_path/rusty_lr_parser/src/parser/parser.rs" out.tab.rs --dense true > /dev/null
compare_files "$rustylr_path/rusty_lr_parser/src/parser/parser_expanded.rs" out.tab.rs
if [ $? -ne 0 ]; then
    exit 1
fi

echo "Setting Dense = false"
cargo run --bin rustylr -- "$rustylr_path/rusty_lr_parser/src/parser/parser.rs" out.tab.rs --dense false > /dev/null
mv out.tab.rs "$rustylr_path/rusty_lr_parser/src/parser/parser_expanded.rs"
cargo run --bin rustylr -- "$rustylr_path/rusty_lr_parser/src/parser/parser.rs" out.tab.rs --dense false > /dev/null
compare_files "$rustylr_path/rusty_lr_parser/src/parser/parser_expanded.rs" out.tab.rs
if [ $? -ne 0 ]; then
    exit 1
fi

# boostrap itself with normal configuration
cargo run --bin rustylr -- "$rustylr_path/rusty_lr_parser/src/parser/parser.rs" out.tab.rs > /dev/null
mv out.tab.rs "$rustylr_path/rusty_lr_parser/src/parser/parser_expanded.rs"
cargo run --bin rustylr -- "$rustylr_path/rusty_lr_parser/src/parser/parser.rs" out.tab.rs > /dev/null
compare_files "$rustylr_path/rusty_lr_parser/src/parser/parser_expanded.rs" out.tab.rs
if [ $? -ne 0 ]; then
    exit 1
fi
mv out.tab.rs "$rustylr_path/rusty_lr_parser/src/parser/parser_expanded.rs"