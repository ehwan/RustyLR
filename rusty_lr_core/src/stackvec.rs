// stack allocated Vec for small lists ( ~ 8 elements )
// currently only for reduce_rules in GLR parser

pub trait ToUsizeList {
    fn to_usize_list(&self) -> impl Iterator<Item = usize> + Clone;
}

pub type SmallVecU8 = smallvec::SmallVec<[u8; 16]>;
pub type SmallVecU16 = smallvec::SmallVec<[u16; 8]>;
pub type SmallVecU32 = smallvec::SmallVec<[u32; 4]>;
pub type SmallVecUsize = smallvec::SmallVec<[usize; 2]>;
impl ToUsizeList for SmallVecU8 {
    fn to_usize_list(&self) -> impl Iterator<Item = usize> + Clone {
        self.iter().map(|&x| x as usize)
    }
}
impl ToUsizeList for SmallVecU16 {
    fn to_usize_list(&self) -> impl Iterator<Item = usize> + Clone {
        self.iter().map(|&x| x as usize)
    }
}
impl ToUsizeList for SmallVecU32 {
    fn to_usize_list(&self) -> impl Iterator<Item = usize> + Clone {
        self.iter().map(|&x| x as usize)
    }
}
impl ToUsizeList for SmallVecUsize {
    fn to_usize_list(&self) -> impl Iterator<Item = usize> + Clone {
        self.iter().copied()
    }
}
