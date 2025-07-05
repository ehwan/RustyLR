/// trait for token location in the input stream
pub trait Location: Clone {
    /// Create a new merged location from `len` elements in the stack.
    /// The `stack` iterator should yield child locations in reverse order.
    /// That is, if the input [a, b, c, ..., z] is fed and `len` is 3,
    /// `stack.next()` will yield `z`, then `y`, then `x`,
    /// and this function should return the merged location of `[x, y, z]`.
    fn new<'a>(stack: impl Iterator<Item = &'a Self>, len: usize) -> Self
    where
        Self: 'a;
}

/// Default location type that does nothing.
#[derive(Clone, Default, Debug, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct DefaultLocation;
impl Location for DefaultLocation {
    fn new<'a>(_stack: impl Iterator<Item = &'a Self>, _len: usize) -> Self {
        DefaultLocation
    }
}

impl<T> Location for std::ops::Range<T>
where
    T: Clone + Default + Ord,
{
    fn new<'a>(mut stack: impl Iterator<Item = &'a Self>, len: usize) -> Self
    where
        Self: 'a,
    {
        if len == 0 {
            if let Some(last) = stack.next() {
                let end = last.end.clone();
                end.clone()..end
            } else {
                T::default()..T::default()
            }
        } else {
            stack
                .take(len)
                .cloned()
                .reduce(|acc, loc| {
                    // stack is in reverse order, so acc is behind loc

                    let start = loc.start;
                    let end = acc.end;
                    start..end
                })
                .unwrap()
        }
    }
}
