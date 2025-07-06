use proc_macro2::Span;

/// type for %location for each token
/// since `Span::join()` is only for nightly,
/// we collect the first and last span pair of the token in the parsing tree.
#[derive(Clone, Debug, Copy)]
pub struct SpanPair {
    pub span_first: Span,
    pub span_last: Span,
}
impl Default for SpanPair {
    fn default() -> Self {
        SpanPair {
            span_first: Span::call_site(),
            span_last: Span::call_site(),
        }
    }
}
impl SpanPair {
    pub fn new(span_first: Span, span_last: Span) -> Self {
        SpanPair {
            span_first,
            span_last,
        }
    }
    pub fn new_single(span: Span) -> Self {
        SpanPair {
            span_first: span,
            span_last: span,
        }
    }
    pub fn span(&self) -> Span {
        self.span_first
    }
}
impl rusty_lr_core::Location for SpanPair {
    fn new<'a>(stack: impl Iterator<Item = &'a Self> + Clone, len: usize) -> Self
    where
        Self: 'a,
    {
        if len == 0 {
            SpanPair {
                span_first: Span::call_site(),
                span_last: Span::call_site(),
            }
        } else {
            let mut stack = stack.take(len);
            let first = stack.next().unwrap();
            if let Some(last) = stack.last() {
                SpanPair {
                    span_first: first.span_first,
                    span_last: last.span_last,
                }
            } else {
                SpanPair {
                    span_first: first.span_first,
                    span_last: first.span_last,
                }
            }
        }
    }
}
