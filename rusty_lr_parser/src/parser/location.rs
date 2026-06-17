use std::hash::Hash;

use proc_macro2::Span;

/// `SpanManager` is used to map a byte range `(start, end]` back to `proc_macro2::Span`.
/// Since it is generally impossible to directly instantiate a `Span` from an arbitrary byte range,
/// `SpanManager` constructs a mapping from `byte_range()` to `Span` for all token spans
/// collected during the parsing phase (`feed_recursive`). Using this map, it provides
/// a method to retrieve all `Span`s that are fully contained within a given range `(start, end]`.
#[derive(Clone, Default, Debug)]
pub struct SpanManager {
    pub spans: Vec<Span>,
}

impl SpanManager {
    pub fn new() -> Self {
        Self { spans: Vec::new() }
    }

    pub fn add_span(&mut self, span: Span) -> usize {
        let i = self.spans.len();
        self.spans.push(span);
        i
    }

    pub fn get_spans_in_location(&self, location: &Location) -> Vec<Span> {
        match location {
            &Location::Range(start, end) => self.spans[start..end].to_vec(),
            Location::CallSite => vec![Span::call_site()], // TODO: handle CallSite case
        }
    }
    pub fn get_span_in_location(&self, location: &Location) -> Span {
        self.get_spans_in_location(location)
            .first()
            .copied()
            .unwrap_or_else(Span::call_site)
    }
    pub fn get_byterange(&self, location: &Location) -> Option<std::ops::Range<usize>> {
        match location {
            &Location::Range(start, end) => {
                if start > end {
                    return None;
                }
                if start == end {
                    if start == 0 {
                        return None;
                    }
                    // zero-length span: point to position after the most recent token
                    let prev_span = self.spans.get(start - 1)?;
                    let pos = prev_span.byte_range().end;
                    return Some(pos..pos);
                }
                let start_span = self.spans.get(start)?;
                let end_span = self.spans.get(end - 1)?;
                Some((start_span.byte_range().start)..(end_span.byte_range().end))
            }
            Location::CallSite => {
                let span = Span::call_site();
                Some(span.byte_range().start..span.byte_range().end)
            }
        }
    }
}

/// type for %location for each token
/// stores index range `[start, end)` in the `SpanManager::spans` vector, where `start` is the index of the first token in the span and `end` is the index of the last token + 1.
#[derive(Clone, Debug, Copy)]
pub enum Location {
    /// index range `[start, end)` of the token span.
    /// zero-length spans are represented with equal values `(pos, pos)`.
    Range(usize, usize),

    CallSite,
}
impl Default for Location {
    fn default() -> Self {
        Location::CallSite
    }
}
impl Location {
    pub fn merge(&self, other: &Location) -> Location {
        match (self, other) {
            (&Location::Range(s1, e1), &Location::Range(s2, e2)) => {
                let start = s1.min(s2);
                let end = e1.max(e2);
                Location::Range(start, end)
            }
            _ => Location::CallSite, // TODO: handle merging with Generated
        }
    }

    /// only used in `new`, Location must be Range
    fn to_range(&self) -> std::ops::Range<usize> {
        match self {
            Location::Range(start, end) => *start..*end,
            _ => 0..0, // this should not happen, but we return a dummy range for simplicity
        }
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

#[derive(Debug, Clone, Copy, Default)]
pub struct Located<T> {
    pub value: T,
    location: Location,
}

impl<T: std::fmt::Display> std::fmt::Display for Located<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.value.fmt(f)
    }
}
impl<T: PartialEq> PartialEq for Located<T> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}
impl<T: Eq> Eq for Located<T> {}
impl<T: PartialOrd> PartialOrd for Located<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.value.partial_cmp(&other.value)
    }
}
impl<T: Ord> Ord for Located<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.value.cmp(&other.value)
    }
}
impl<T: Hash> Hash for Located<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.value.hash(state);
    }
}
impl<T> std::ops::Deref for Located<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.value
    }
}
impl<T> std::ops::DerefMut for Located<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

impl<T> Located<T> {
    pub fn new(value: T, location: Location) -> Self {
        Self { value, location }
    }
    pub fn location(&self) -> Location {
        self.location
    }
    pub fn value(&self) -> &T {
        &self.value
    }
    pub fn into_value(self) -> T {
        self.value
    }

    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Located<U> {
        Located {
            value: f(self.value),
            location: self.location,
        }
    }
    pub fn map_location<F: FnOnce(Location) -> Location>(self, f: F) -> Located<T> {
        Located {
            value: self.value,
            location: f(self.location),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proc_macro2::Span;
    use rusty_lr_core::Location as CoreLocation;

    #[test]
    fn test_location_default() {
        let loc = Location::default();
        assert!(matches!(loc, Location::CallSite));
    }

    #[test]
    fn test_location_merge() {
        let loc1 = Location::Range(1, 3);
        let loc2 = Location::Range(2, 5);
        let merged = loc1.merge(&loc2);
        if let Location::Range(start, end) = merged {
            assert_eq!(start, 1);
            assert_eq!(end, 5);
        } else {
            panic!("Expected Location::Range");
        }

        let loc3 = Location::Range(1, 3);
        let loc4 = Location::CallSite;
        let merged_gen = loc3.merge(&loc4);
        assert!(matches!(merged_gen, Location::CallSite));
    }

    #[test]
    fn test_location_new_empty_stack() {
        let stack: Vec<Location> = vec![];
        let loc = Location::new(stack.iter(), 0);
        assert!(matches!(loc, Location::CallSite));
    }

    #[test]
    fn test_location_new_zero_len() {
        let stack = vec![Location::Range(2, 4)];
        let loc = Location::new(stack.iter(), 0);
        if let Location::Range(start, end) = loc {
            assert_eq!(start, 4);
            assert_eq!(end, 4);
        } else {
            panic!("Expected Location::Range");
        }
    }

    #[test]
    fn test_location_new_with_len() {
        let stack = vec![Location::Range(4, 6), Location::Range(1, 3)];
        let loc = Location::new(stack.iter(), 2);
        if let Location::Range(start, end) = loc {
            assert_eq!(start, 1);
            assert_eq!(end, 6);
        } else {
            panic!("Expected Location::Range");
        }
    }

    #[test]
    fn test_location_new_single_item() {
        let stack = vec![Location::Range(1, 3)];
        let loc = Location::new(stack.iter(), 1);
        if let Location::Range(start, end) = loc {
            assert_eq!(start, 1);
            assert_eq!(end, 3);
        } else {
            panic!("Expected Location::Range");
        }
    }

    #[test]
    fn test_span_manager() {
        let mut manager = SpanManager::new();
        let span1 = Span::call_site();
        let span2 = Span::call_site();

        let idx1 = manager.add_span(span1);
        let idx2 = manager.add_span(span2);

        assert_eq!(idx1, 0);
        assert_eq!(idx2, 1);

        let loc = Location::Range(0, 2);
        let spans = manager.get_spans_in_location(&loc);
        assert_eq!(spans.len(), 2);

        let single_span = manager.get_span_in_location(&loc);
        assert_eq!(
            manager.spans.first().unwrap().byte_range(),
            single_span.byte_range()
        );

        let empty_loc = Location::CallSite;
        assert!(manager.get_spans_in_location(&empty_loc).len() == 1);
    }

    #[test]
    fn test_span_manager_byterange() {
        let mut manager = SpanManager::new();
        let span = Span::call_site();
        manager.add_span(span);

        let loc = Location::Range(0, 1);
        let byte_range = manager.get_byterange(&loc);
        assert!(byte_range.is_some());
    }
}
