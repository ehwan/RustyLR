use proc_macro2::Span;

/// Converts a byte range `[start, end)` into a `proc_macro2::Span`.
/// Currently, byte offsets cannot be mapped back precisely, so this returns `Span::call_site()`.
pub fn byte_range_to_span(_range: std::ops::Range<usize>) -> Span {
    Span::call_site()
}

/// type for %location for each token
/// stores byte offset range [start, end) in the source file.
#[derive(Clone, Debug, Copy)]
pub struct SpanPair {
    /// byte range `[start, end)` of the token span.
    /// zero-length spans are represented with equal values `(pos, pos)`.
    pub pair: (usize, usize),
}
impl Default for SpanPair {
    fn default() -> Self {
        SpanPair { pair: (0, 0) }
    }
}
impl SpanPair {
    pub fn new_single(span: Span) -> Self {
        let range = span.byte_range();
        SpanPair {
            pair: (range.start, range.end),
        }
    }
    /// Returns the byte range `[start, end)` of this span.
    pub fn to_range(&self) -> std::ops::Range<usize> {
        let (s, e) = self.pair;
        s..e
    }
    /// Returns a `proc_macro2::Span` for use in proc-macro error reporting.
    /// Since byte offsets cannot be converted back to `Span`, this falls back to `byte_range_to_span`.
    pub fn span(&self) -> Span {
        byte_range_to_span(self.to_range())
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
                let (_, e) = after_pos.pair;
                return SpanPair { pair: (e, e) };
            }
            return SpanPair::default();
        }
        // The iterator yields items most-recent-first, so the first item is the end span
        // and the last item taken is the start span.
        let mut take = stack.take(len).map(|x| x.pair);
        let pair = if let Some(end_span) = take.next() {
            let start_span = take.last().unwrap_or(end_span);
            (start_span.0, end_span.1)
        } else {
            (0, 0)
        };
        SpanPair { pair }
    }
}
