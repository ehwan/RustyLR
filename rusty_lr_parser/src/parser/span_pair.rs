use proc_macro2::Span;

/// type for %location for each token
/// stores byte offset range [start, end) in the source file.
#[derive(Clone, Debug, Copy)]
pub struct SpanPair {
    /// byte range `[start, end)` of the token span, or `None` if this is a zero-length span
    pub pair: Option<(usize, usize)>,
}
impl Default for SpanPair {
    fn default() -> Self {
        SpanPair { pair: None }
    }
}
impl SpanPair {
    pub fn new_single(span: Span) -> Self {
        let range = span.byte_range();
        SpanPair {
            pair: Some((range.start, range.end)),
        }
    }
    /// Returns the byte range `[start, end)` of this span, or `0..0` if empty.
    pub fn to_range(&self) -> std::ops::Range<usize> {
        self.pair.map(|(s, e)| s..e).unwrap_or(0..0)
    }
    /// Returns a `proc_macro2::Span` for use in proc-macro error reporting.
    /// Since byte offsets cannot be converted back to `Span`, this always returns `Span::call_site()`.
    pub fn span(&self) -> Span {
        Span::call_site()
    }
}
impl rusty_lr_core::Location for SpanPair {
    fn new<'a>(mut stack: impl Iterator<Item = &'a Self> + Clone, len: usize) -> Self
    where
        Self: 'a,
    {
        if len == 0 {
            // zero-length: point to position after the most recent token
            if let Some(after_pos) = stack.next() {
                let pair = after_pos.pair.map(|(_, e)| (e, e));
                return SpanPair { pair };
            }
            return SpanPair { pair: None };
        }
        // The iterator yields items most-recent-first, so the first item is the end span
        // and the last item taken is the start span.
        let mut take = stack.take(len).filter_map(|x| x.pair);
        let pair = if let Some(end_span) = take.next() {
            let start_span = take.last().unwrap_or(end_span);
            Some((start_span.0, end_span.1))
        } else {
            None
        };
        SpanPair { pair }
    }
}
