use proc_macro2::Span;
use std::collections::BTreeMap;

/// `SpanManager` is used to map a byte range `(start, end]` back to `proc_macro2::Span`.
/// Since it is generally impossible to directly instantiate a `Span` from an arbitrary byte range,
/// `SpanManager` constructs a mapping from `byte_range()` to `Span` for all token spans
/// collected during the parsing phase (`feed_recursive`). Using this map, it provides
/// a method to retrieve all `Span`s that are fully contained within a given range `(start, end]`.
#[derive(Clone, Default, Debug)]
pub struct SpanManager {
    pub spans: BTreeMap<(usize, usize), Span>,
}

impl SpanManager {
    pub fn new() -> Self {
        Self {
            spans: BTreeMap::new(),
        }
    }

    pub fn clear(&mut self) {
        self.spans.clear();
    }

    pub fn add_span(&mut self, span: Span) {
        let range = span.byte_range();
        self.spans.insert((range.start, range.end), span);
    }

    #[allow(dead_code)]
    pub fn get_spans_in_range(&self, start: usize, end: usize) -> Vec<Span> {
        let mut result = Vec::new();
        // Since it is sorted by key, we look up ranges starting from (start, 0)
        // to collect all spans contained within (start, end].
        for (&(span_start, span_end), span) in self.spans.range((start, 0)..) {
            if span_start >= end {
                break;
            }
            if span_end <= end {
                result.push(span.clone());
            }
        }
        if result.is_empty() {
            // If no spans are found, return the call_site span as a fallback.
            // result.push(Span::call_site());
        }
        result
    }

    pub fn get_spans_in_location(&self, location: &Location) -> Vec<Span> {
        match location {
            Location::Range(start, end) => self.get_spans_in_range(*start, *end),
            Location::Generated => vec![Span::call_site()], // TODO: handle Generated case
        }
    }
    pub fn get_span_in_location(&self, location: &Location) -> Span {
        self.get_spans_in_location(location)
            .first()
            .copied()
            .unwrap_or_else(Span::call_site)
    }
}

/// type for %location for each token
/// stores byte offset range [start, end) in the source file.
#[derive(Clone, Debug, Copy)]
pub enum Location {
    /// byte range `[start, end)` of the token span.
    /// zero-length spans are represented with equal values `(pos, pos)`.
    Range(usize, usize),

    /// Generated
    Generated,
}
impl Default for Location {
    fn default() -> Self {
        Location::Range(0, 0)
    }
}
impl Location {
    /// Returns a `Location` representing the Span::call_site() of the macro.
    pub fn call_site() -> Self {
        Span::call_site().into()
    }
    /// Returns the byte range `[start, end)` of this span.
    pub fn to_range(&self) -> std::ops::Range<usize> {
        match self {
            Location::Range(s, e) => *s..*e,
            Location::Generated => 0..0, // TODO
        }
    }

    pub fn merge(&self, other: &Location) -> Location {
        match (self, other) {
            (&Location::Range(s1, e1), &Location::Range(s2, e2)) => {
                let start = s1.min(s2);
                let end = e1.max(e2);
                Location::Range(start, end)
            }
            _ => Location::Generated, // TODO: handle merging with Generated
        }
    }
}
impl From<Span> for Location {
    fn from(span: Span) -> Self {
        let range = span.byte_range();
        Location::Range(range.start, range.end)
    }
}
impl rusty_lr_core::Location for Location {
    fn new<'a>(mut stack: impl Iterator<Item = &'a Self> + Clone, len: usize) -> Self
    where
        Self: 'a,
    {
        if len == 0 {
            // zero-length: point to position after the most recent token
            if let Some(after_pos) = stack.next() {
                let e = after_pos.to_range().end;
                return Location::Range(e, e);
            }
            return Location::default();
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
        Location::Range(pair.0, pair.1)
    }
}
