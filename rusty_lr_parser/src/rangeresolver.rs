pub struct RangeResolver {
    ranges: Vec<(u32, u32)>,
}

impl RangeResolver {
    pub fn new() -> Self {
        RangeResolver { ranges: Vec::new() }
    }
    pub fn insert(&mut self, mut start: u32, last: u32) {
        let mut new_ranges = Vec::new();
        let mut added = false;
        for (idx, &(s, l)) in self.ranges.iter().enumerate() {
            if l < start {
                new_ranges.push((s, l));
                continue;
            }
            if s > last {
                if !added {
                    new_ranges.push((start, last));
                }
                new_ranges.extend_from_slice(&self.ranges[idx..]);
                added = true;
                break;
            }

            // must overlap here

            use std::cmp::Ordering;
            match start.cmp(&s) {
                Ordering::Less => {
                    match last.cmp(&l) {
                        Ordering::Less => {
                            // nnnnnnn
                            //     rrrrr
                            new_ranges.push((start, s - 1));
                            new_ranges.push((s, last));
                            new_ranges.push((last + 1, l));
                            added = true;
                        }
                        Ordering::Equal => {
                            // nnnnnn
                            //    rrr
                            new_ranges.push((start, s - 1));
                            new_ranges.push((s, l));
                            added = true;
                        }
                        Ordering::Greater => {
                            // nnnnnnn
                            //   rrr
                            new_ranges.push((start, s - 1));
                            new_ranges.push((s, l));
                            start = l + 1;
                        }
                    }
                }
                Ordering::Equal => {
                    match last.cmp(&l) {
                        Ordering::Less => {
                            // nnnn
                            // rrrrrr
                            new_ranges.push((start, last));
                            added = true;
                            new_ranges.push((last + 1, l));
                        }
                        Ordering::Equal => {
                            // nnnn
                            // rrrr
                            new_ranges.push((start, last));
                            added = true;
                        }
                        Ordering::Greater => {
                            // nnnnnnnn
                            // rrrr
                            new_ranges.push((s, l));
                            start = l + 1;
                        }
                    }
                }
                Ordering::Greater => {
                    // start > s
                    match last.cmp(&l) {
                        Ordering::Less => {
                            //    nnnnn
                            //  rrrrrrrrr
                            new_ranges.push((s, start - 1));
                            new_ranges.push((start, last));
                            new_ranges.push((last + 1, l));
                            added = true;
                        }
                        Ordering::Equal => {
                            //    nnnnnnn
                            //  rrrrrrrrr
                            new_ranges.push((s, start - 1));
                            new_ranges.push((start, last));
                            added = true;
                        }
                        Ordering::Greater => {
                            //    nnnnnnnnn
                            //  rrrrrrrrr
                            new_ranges.push((s, start - 1));
                            new_ranges.push((start, l));
                            start = l + 1;
                        }
                    }
                }
            }
        }
        if !added {
            new_ranges.push((start, last));
        }

        self.ranges = new_ranges;
    }

    pub fn get_ranges(&self, start: u32, last: u32) -> impl Iterator<Item = usize> {
        let first_idx = match self.ranges.binary_search(&(start, start)) {
            Ok(idx) => idx,
            Err(idx) => idx,
        };
        let end_idx = match self.ranges.binary_search(&(last, last)) {
            Ok(idx) => idx + 1,
            Err(idx) => idx,
        };
        first_idx..end_idx
    }

    pub fn iter(&self) -> impl Iterator<Item = (u32, u32)> + '_ {
        self.ranges.iter().copied()
    }
}
