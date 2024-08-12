use proc_macro2::Spacing;
use proc_macro2::TokenStream;
use proc_macro2::TokenTree;
use quote::TokenStreamExt;

// split stream by '%%'
pub fn split_stream(token_stream: TokenStream) -> Result<(TokenStream, TokenStream), &'static str> {
    // input stream
    let mut token_stream = token_stream.into_iter().peekable();

    // before '%%'
    let mut output_stream = TokenStream::new();

    while let Some(token) = token_stream.next() {
        if let TokenTree::Punct(token) = token {
            if token.as_char() == '%' && token.spacing() == Spacing::Joint {
                if let Some(TokenTree::Punct(next)) = token_stream.peek() {
                    if next.as_char() == '%' && next.spacing() == Spacing::Alone {
                        token_stream.next();
                        let macro_stream: TokenStream = token_stream.collect();
                        return Ok((output_stream, macro_stream));
                    } else {
                        output_stream.append(token);
                    }
                } else {
                    output_stream.append(token);
                }
            } else {
                output_stream.append(token);
            }
        } else {
            output_stream.append(token);
        }
    }

    Err("No '%%' found")
}
