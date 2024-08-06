use proc_macro2::TokenStream;
use proc_macro2::TokenTree;
use quote::TokenStreamExt;

use std::env::args;
use std::fs::read;
use std::fs::write;

/*
very early version of rusty_lr_expand

same as 'yacc',
this find for '%%' and parse-expand the grammar
*/

fn is_percent(token: &TokenTree) -> bool {
    if let TokenTree::Punct(punct) = token {
        punct.as_char() == '%'
    } else {
        false
    }
}

// split stream by '%%'
fn split_stream(token_stream: TokenStream) -> (TokenStream, TokenStream) {
    // input stream
    let mut token_stream = token_stream.into_iter().peekable();
    // before '%%'
    let mut output_stream = TokenStream::new();
    // after '%%'
    let mut macro_stream = TokenStream::new();

    while let Some(token) = token_stream.next() {
        let next = token_stream.peek().clone();
        if next.is_none() {
            output_stream.append(token);
        } else {
            let next = next.unwrap();
            if is_percent(&token) && is_percent(next) {
                token_stream.next();
                macro_stream.extend(token_stream);
                break;
            } else {
                output_stream.append(token);
            }
        }
    }
    (output_stream, macro_stream)
}

fn main() {
    let mut args = args();
    args.next();

    let input_name = args.next().expect("Expected input file");
    let output_name = args.next().expect("Expected output file");
    let bytes = match read(input_name) {
        Ok(bytes) => bytes,
        Err(e) => {
            eprintln!("Error reading file: {}", e);
            return;
        }
    };

    let str = match String::from_utf8(bytes) {
        Ok(str) => str,
        Err(e) => {
            eprintln!("Error reading utf-8: {}", e);
            return;
        }
    };

    let token_stream: TokenStream = match str.parse() {
        Ok(token_stream) => token_stream,
        Err(e) => {
            eprintln!("Error parsing token stream: {}", e);
            return;
        }
    };

    let (output_stream, macro_stream) = split_stream(token_stream);

    let g = match rusty_lr_parser::grammar::Grammar::parse(macro_stream) {
        Ok(grammar) => grammar,
        Err(e) => {
            println!("{}", e.compile_error());
            return;
        }
    };

    let expanded_stream = match g.emit_runtime(false) {
        Ok(parser) => parser,
        Err(e) => {
            println!("{}", e.compile_error());
            return;
        }
    };

    let output_string = format!("{}{}", output_stream, expanded_stream);
    write(output_name, output_string).expect("Error writing output file");
}
