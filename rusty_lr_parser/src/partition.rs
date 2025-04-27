use std::collections::BTreeMap;

/// return {setids} -> {values} map
pub fn minimal_partition<T>(
    sets: impl Iterator<Item = impl Iterator<Item = T>>,
) -> BTreeMap<Vec<usize>, Vec<T>>
where
    T: Ord,
{
    let mut val_setids_map: BTreeMap<T, _> = Default::default();
    for (set_id, val_set) in sets.enumerate() {
        for val in val_set {
            val_setids_map
                .entry(val)
                .or_insert_with(Vec::new)
                .push(set_id);
        }
    }

    let mut setids_val_map: BTreeMap<_, Vec<T>> = Default::default();
    for (val, setids) in val_setids_map {
        setids_val_map
            .entry(setids)
            .or_insert_with(Vec::new)
            .push(val);
    }

    setids_val_map
}
