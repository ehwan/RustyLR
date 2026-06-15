use proc_macro2::Span;

/// Converts a byte range `[start, end)` into a `proc_macro2::Span`.
/// Currently, byte offsets cannot be mapped back precisely, so this returns `Span::call_site()`.
pub fn byte_range_to_span(_range: std::ops::Range<usize>) -> Span {
    Span::call_site()
}

/// type for %location for each token
/// stores byte offset range [start, end) in the source file.
#[derive(Clone, Debug, Copy)]
pub enum SpanPair {
    /// byte range `[start, end)` of the token span.
    /// zero-length spans are represented with equal values `(pos, pos)`.
    Range(usize, usize),

    /// Generated
    Generated,
}
impl Default for SpanPair {
    fn default() -> Self {
        SpanPair::Range(0, 0)
    }
}
impl SpanPair {
    /// Returns the byte range `[start, end)` of this span.
    pub fn to_range(&self) -> std::ops::Range<usize> {
        match self {
            SpanPair::Range(s, e) => *s..*e,
            SpanPair::Generated => 0..0, // TODO
        }
    }
    /// Returns a `proc_macro2::Span` for use in proc-macro error reporting.
    /// Since byte offsets cannot be converted back to `Span`, this falls back to `byte_range_to_span`.
    pub fn span(&self) -> Span {
        // TODO
        // unimplemented!("Cannot convert byte range back to Span, using call_site() instead");
        byte_range_to_span(self.to_range())
    }

    pub fn merge(&self, other: &SpanPair) -> SpanPair {
        match (self, other) {
            (&SpanPair::Range(s1, e1), &SpanPair::Range(s2, e2)) => {
                let start = s1.min(s2);
                let end = e1.max(e2);
                SpanPair::Range(start, end)
            }
            _ => SpanPair::Generated, // TODO: handle merging with Generated
        }
    }
}
impl From<Span> for SpanPair {
    fn from(span: Span) -> Self {
        let range = span.byte_range();
        SpanPair::Range(range.start, range.end)
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
                let e = after_pos.to_range().end;
                return SpanPair::Range(e, e);
            }
            return SpanPair::default();
        }
        // The iterator yields items most-recent-first, so the first item is the end span
        // and the last item taken is the start span.
        let mut take = stack.take(len).map(|x| x.to_range());
        let pair = if let Some(end_span) = take.next() {
            let start_span = take.last().unwrap_or(end_span.clone());
            (start_span.start, end_span.end)
        } else {
            (0, 0)
        };
        SpanPair::Range(pair.0, pair.1)
    }
}
