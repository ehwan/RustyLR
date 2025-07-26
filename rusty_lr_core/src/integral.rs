pub trait IntoUsize {
    fn into_usize(self) -> usize;
}
impl IntoUsize for usize {
    fn into_usize(self) -> usize {
        self
    }
}
impl IntoUsize for u8 {
    fn into_usize(self) -> usize {
        self as usize
    }
}
impl IntoUsize for u16 {
    fn into_usize(self) -> usize {
        self as usize
    }
}
impl IntoUsize for u32 {
    fn into_usize(self) -> usize {
        self as usize
    }
}
#[cfg(target_pointer_width = "64")]
impl IntoUsize for u64 {
    fn into_usize(self) -> usize {
        self as usize
    }
}
