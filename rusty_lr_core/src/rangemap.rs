pub struct RangeMap {
    /// A sorted list of ranges, where each range is represented as a tuple (start, end)
    pub ranges: Vec<(u32, u32)>,

    /// A list of values corresponding to each range
    pub values: Vec<usize>,
}

impl RangeMap {
    pub fn new(ranges: Vec<(u32, u32)>, values: Vec<usize>) -> Self {
        RangeMap { ranges, values }
    }

    /// Searches the ranges that contain the key, and returns the corresponding value.
    pub fn get(&self, key: u32) -> Option<usize> {
        if self.ranges.is_empty() {
            return None;
        }
        let mut low = 0;
        let mut high = self.ranges.len() - 1;
        while high - low > 1 {
            let mid = (low + high) / 2;
            if self.ranges[mid].0 <= key {
                low = mid;
            } else {
                high = mid - 1;
            }
        }
        if self.ranges[high].0 <= key && key <= self.ranges[high].1 {
            Some(self.values[high])
        } else if self.ranges[low].0 <= key && key <= self.ranges[low].1 {
            Some(self.values[low])
        } else {
            None
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
