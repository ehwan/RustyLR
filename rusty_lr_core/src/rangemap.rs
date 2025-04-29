pub struct RangeMap {
    /// A sorted list of ranges, where each range is represented as a tuple (start, end)
    pub range_starts: Vec<u32>,
    pub range_lasts: Vec<u32>,

    /// A list of values corresponding to each range
    pub values: Vec<usize>,
}

impl RangeMap {
    pub fn new(ranges: Vec<(u32, u32)>, values: Vec<usize>) -> Self {
        let mut starts = Vec::with_capacity(ranges.len());
        let mut lasts = Vec::with_capacity(ranges.len());
        for (start, last) in &ranges {
            starts.push(*start);
            lasts.push(*last);
        }
        RangeMap {
            range_starts: starts,
            range_lasts: lasts,
            values,
        }
    }

    /// Searches the ranges that contain the key, and returns the corresponding value.
    pub fn get(&self, key: u32) -> Option<usize> {
        match self.range_starts.binary_search(&key) {
            Ok(idx) => {
                // range start key found
                if self.range_lasts[idx] >= key {
                    Some(self.values[idx])
                } else {
                    None
                }
            }
            Err(idx) => {
                // starts[idx] > key
                // starts[idx] < key
                if idx == 0 {
                    None
                } else if self.range_lasts[idx - 1] >= key {
                    Some(self.values[idx - 1])
                } else {
                    None
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn zero_test() {
        let m = RangeMap::new(Vec::new(), Vec::new());
        assert_eq!(m.get(0), None);
        assert_eq!(m.get(1), None);
        assert_eq!(m.get(2), None);
        assert_eq!(m.get(3), None);
    }

    #[test]
    fn single_test() {
        let m = RangeMap::new(vec![(10, 20)], vec![2]);
        assert_eq!(m.get(9), None);
        assert_eq!(m.get(10), Some(2));
        assert_eq!(m.get(15), Some(2));
        assert_eq!(m.get(20), Some(2));
        assert_eq!(m.get(21), None);
    }

    #[test]
    fn double_test() {
        let m = RangeMap::new(vec![(10, 20), (30, 40)], vec![2, 3]);
        assert_eq!(m.get(9), None);
        assert_eq!(m.get(10), Some(2));
        assert_eq!(m.get(15), Some(2));
        assert_eq!(m.get(20), Some(2));
        assert_eq!(m.get(21), None);
        assert_eq!(m.get(29), None);
        assert_eq!(m.get(30), Some(3));
        assert_eq!(m.get(35), Some(3));
        assert_eq!(m.get(40), Some(3));
        assert_eq!(m.get(41), None);
    }
}
