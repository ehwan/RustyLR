/// trait for token location in the input stream
pub trait Location: Clone + Default {
    /// returns the smallest range that covers both locations
    fn merge(self, other: Self) -> Self;

    /// create a new zero-length location that is next to the current location
    fn next_zero(&self) -> Self;
}
