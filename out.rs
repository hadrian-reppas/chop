#![allow(warnings)]
#[repr(transparent)]
struct hglobal<T: Copy>(T);
unsafe impl<T: Copy> Sync for hglobal<T> {}
macro_rules! zeroed { ($ty:ty) => {unsafe { std::mem::transmute([0u8; std::mem::size_of::<$ty>()]) }}; }
macro_rules! uninit { () => {std::mem::MaybeUninit::uninit().assume_init()}; }
macro_rules! alloc { ($len:expr, $ty:ty, $alloc:ident) => {{
    let layout = std::alloc::Layout::array::<$ty>($len as usize).unwrap();
    let ptr = std::alloc::$alloc(layout);
    hallocs.as_mut().unwrap().insert(ptr as usize, layout);
    ptr as *mut $ty
}}; }
static mut hstart: std::time::Instant = zeroed!(std::time::Instant);
static mut hallocs: Option<std::collections::HashMap<usize, std::alloc::Layout>> = None;
fn hhashu32(mut x: u32) -> u32 {
    x = ((x >> 16) ^ x).wrapping_mul(0x45d9f3b);
    x = ((x >> 16) ^ x).wrapping_mul(0x45d9f3b);
    (x >> 16) ^ x
}
fn hhash(mut x: u64) -> u64 {
    let a = (x >> 32) as u32;
    let b = x as u32;
    let c = hhashu32(a) ^ hhashu32(a.reverse_bits() ^ b);
    let d = hhashu32(b) ^ hhashu32(b.reverse_bits() ^ a);
    (c as u64) << 32 | d as u64
}
unsafe fn hrandf() -> f64 {
    let x = std::time::Instant::now().duration_since(hstart).as_nanos() as u64;
    hhash(x).saturating_sub(1) as f64 / u64::MAX as f64
}
#[derive(Clone, Copy)]
struct hs_A {
    hm_b: hs_C,
    hm_ptr: *mut u8,
}
fn hf_A_0(hv0: hs_C, hv1: *mut u8, hr0: &mut hs_A) {
    *hr0 = hs_A {
    hm_b: hv0,
    hm_ptr: hv1,
    };
}
#[derive(Clone, Copy)]
struct hs_list<T0> {
    hm_val: T0,
    hm_next: *mut hs_list<T0>,
}
fn hf_list_0<T0>(hv0: T0, hv1: *mut hs_list<T0>, hr0: &mut hs_list<T0>) {
    *hr0 = hs_list {
    hm_val: hv0,
    hm_next: hv1,
    };
}
#[derive(Clone, Copy)]
struct hs_C {
    hm_val: *mut hs_A,
}
fn hf_C_0(hv0: *mut hs_A, hr0: &mut hs_C) {
    *hr0 = hs_C {
    hm_val: hv0,
    };
}
unsafe fn hf_swap_0<T0: Copy, T1: Copy>(mut hv0: T0, mut hv1: T1, hr0: &mut T1, hr1: &mut T0) {
    *hr0 = hv1;
    *hr1 = hv0;
}
unsafe fn hf_byte__test_0(hr0: &mut u8) {
    let mut hv0: *mut u8 = uninit!();
    let mut hv1: u8 = uninit!();
    hv0 = "hello".0;
    hv1 = 255;
    *hr0 = hv1;
}
unsafe fn hf_main_0() {
    let mut hv4: bool = uninit!();
    let mut hv0: i64 = uninit!();
    let mut hv1: i64 = uninit!();
    let mut hv2: i64 = uninit!();
    let mut hv3: i64 = uninit!();
    hv0 = 1;
    hv1 = 2;
    hf_swap_0(hv0, hv1, &mut hv0, &mut hv1);
    hf__2e__0(hv1, &mut hv1, &mut hv2);
    hv3 = 1;
    hf__3d__3d__4(hv2, hv3, &mut hv4);
    if hv4 {
        hf_swap_0(hv0, hv1, &mut hv0, &mut hv1);
    } else {
    }
}
fn hf__3d__3d__4<T>(hv0: *mut T, hv1: *mut T, hr0: &mut bool) { *hr0 = hv0 == hv1; }
fn hf__2e__0<T: Copy>(hv0: T, hr0: &mut T, hr1: &mut T) { *hr0 = hv0; *hr1 = hv0; }
fn main() {
    unsafe { hallocs = Some(std::collections::HashMap::new()); }
    let mut code: i64 = 0;
    unsafe { hf_main_0(); }
    std::process::exit(code as i32);
}
