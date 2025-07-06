use proc_macro2::Span;

/// type for %location for each token
/// since `Span::join()` is only for nightly,
/// we collect the first and last span pair of the token in the parsing tree.
#[derive(Clone, Debug, Copy)]
pub struct SpanPair {
    /// `None` if this is a zero-length span
    pub pair: Option<(Span, Span)>,
}
impl Default for SpanPair {
    fn default() -> Self {
        SpanPair { pair: None }
    }
}
impl SpanPair {
    pub fn new_single(span: Span) -> Self {
        SpanPair {
            pair: Some((span, span)),
        }
    }
    pub fn span(&self) -> Span {
        self.pair
            .as_ref()
            .map_or(Span::call_site(), |(first, last)| {
                if let Some(joined) = first.join(*last) {
                    joined
                } else {
                    *first
                }
            })
    }
}
impl rusty_lr_core::Location for SpanPair {
    fn new<'a>(stack: impl Iterator<Item = &'a Self> + Clone, len: usize) -> Self
    where
        Self: 'a,
    {
        let mut take = stack.take(len).filter_map(|x| x.pair);
        let pair = if let Some(last) = take.next() {
            let first = take.last().unwrap_or(last);

            Some((first.0, last.1))
        } else {
            None
        };
        SpanPair { pair }
    }
}
