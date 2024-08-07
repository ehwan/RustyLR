use clap::Parser;
use proc_macro2::TokenStream;
use proc_macro2::TokenTree;
use quote::TokenStreamExt;

use std::fs::read;
use std::fs::write;
use std::process::Command;

mod arg;

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
fn split_stream(token_stream: TokenStream) -> Result<(TokenStream, TokenStream), String> {
    // input stream
    let mut token_stream = token_stream.into_iter().peekable();
    // before '%%'
    let mut output_stream = TokenStream::new();

    while let Some(token) = token_stream.next() {
        let next = token_stream.peek().clone();
        if next.is_none() {
            output_stream.append(token);
        } else {
            let next = next.unwrap();
            if is_percent(&token) && is_percent(next) {
                token_stream.next();
                let macro_stream: TokenStream = token_stream.collect();
                return Ok((output_stream, macro_stream));
            } else {
                output_stream.append(token);
            }
        }
    }
    Err("No '%%' found".to_string())
}

fn main() {
    let args = match arg::Args::try_parse() {
        Ok(args) => args,
        Err(e) => {
            eprintln!("{}", e);
            return;
        }
    };

    let input_bytes = match read(args.input_file) {
        Ok(bytes) => bytes,
        Err(e) => {
            eprintln!("Error reading file: {}", e);
            return;
        }
    };
    let str = match String::from_utf8(input_bytes) {
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
    let (output_stream, macro_stream) = match split_stream(token_stream) {
        Ok((output_stream, macro_stream)) => (output_stream, macro_stream),
        Err(e) => {
            eprintln!("{}", e);
            return;
        }
    };

    let grammar = match rusty_lr_parser::grammar::Grammar::parse(macro_stream) {
        Ok(grammar) => grammar,
        Err(e) => {
            println!("{}", e);
            return;
        }
    };

    let expanded_stream = if args.runtime {
        grammar.emit_runtime(args.lalr)
    } else {
        grammar.emit_compiletime(args.lalr)
    };

    let expanded_stream = match expanded_stream {
        Ok(expanded_stream) => expanded_stream,
        Err(e) => {
            eprintln!("{}", e);
            return;
        }
    };

    let output_string = format!("{}{}", output_stream, expanded_stream);
    match write(args.output_file.clone(), output_string) {
        Ok(_) => {}
        Err(e) => {
            eprintln!("Error writing output file: {}", e);
            return;
        }
    }

    if !args.no_format {
        let mut child = Command::new("rustfmt")
            .arg(args.output_file)
            .spawn()
            .expect("Failed to run rustfmt");
        child.wait().expect("Failed to wait on rustfmt");
    }
}
