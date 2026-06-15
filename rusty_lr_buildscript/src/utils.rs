use proc_macro2::TokenStream;

use std::ops::Range;

pub fn tokenstream_range(stream: TokenStream) -> Range<usize> {
    if stream.is_empty() {
        return 0..0;
    }
    let mut stream = stream.into_iter();
    let first = stream.next().unwrap().span().byte_range();
    let last = if let Some(last) = stream.last() {
        last.span().byte_range()
    } else {
        first.clone()
    };

    first.start..last.end
}
