/// trait for token location in the input stream
pub trait Location: Clone + Default {
    /// Merge two locations and return the smallest range that covers both locations.
    /// `self` is guaranteed to be left side of `other`.
    fn merge(self, other: Self) -> Self;

    /// Create a new zero-length location that is next to the current location.
    /// So that `self.next_zero().merge( other )` does not mistakenly cover the entire range.
    fn next_zero(&self) -> Self;
}

/// Default location type that does nothing.
#[derive(Clone, Default, Debug, Copy)]
pub struct DefaultLocation;
impl Location for DefaultLocation {
    fn merge(self, _: Self) -> Self {
        DefaultLocation
    }

    fn next_zero(&self) -> Self {
        DefaultLocation
    }
}
