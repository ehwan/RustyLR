/// trait for token location in the input stream
pub trait Location {
    /// returns the smallest range that covers both locations
    fn merge(self, other: Self) -> Self;
}
