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
struct hs_vector<T0> {
    hm_ptr: *mut T0,
    hm_length: i64,
    hm_capacity: i64,
}
fn hf_vector_0<T0>(hv0: *mut T0, hv1: i64, hv2: i64, hr0: &mut hs_vector<T0>) {
    *hr0 = hs_vector {
    hm_ptr: hv0,
    hm_length: hv1,
    hm_capacity: hv2,
    };
}
unsafe fn hf_swap_0<T0: Copy, T1: Copy>(mut hv0: T0, mut hv1: T1, hr0: &mut T1, hr1: &mut T0) {
    *hr0 = hv1;
    *hr1 = hv0;
}
unsafe fn hf_rot_0<T0: Copy, T1: Copy, T2: Copy>(mut hv0: T0, mut hv1: T1, mut hv2: T2, hr0: &mut T1, hr1: &mut T2, hr2: &mut T0) {
    *hr0 = hv1;
    *hr1 = hv2;
    *hr2 = hv0;
}
unsafe fn hf_under_0<T0: Copy, T1: Copy>(mut hv0: T0, mut hv1: T1, hr0: &mut T0, hr1: &mut T1, hr2: &mut T0) {
    *hr0 = hv0;
    *hr1 = hv1;
    *hr2 = hv0;
}
unsafe fn hf_carve_0<T0: Copy, T1: Copy>(mut hv0: T0, mut hv1: T1, hr0: &mut T1) {
    *hr0 = hv1;
}
unsafe fn hf_pump_0<T0: Copy, T1: Copy>(mut hv0: T0, mut hv1: T1, hr0: &mut T0, hr1: &mut T0, hr2: &mut T1) {
    *hr0 = hv0;
    *hr1 = hv0;
    *hr2 = hv1;
}
unsafe fn hf_vector__from__value_0<T0: Copy>(mut hv0: T0, hr0: &mut hs_vector<T0>) {
    let mut hv1: i64 = uninit!();
    let mut hv4: i64 = uninit!();
    let mut hv5: i64 = uninit!();
    let mut hv2: *mut T0 = uninit!();
    let mut hv3: *mut T0 = uninit!();
    let mut hv6: hs_vector<T0> = uninit!();
    hv1 = 10;
    hv2 = alloc!(hv1, T0, alloc);
    hf__2e__0(hv2, &mut hv2, &mut hv3);
    hf_rot_0(hv0, hv2, hv3, &mut hv2, &mut hv3, &mut hv0);
    hf_write_0(hv3, hv0);
    hv4 = 1;
    hv5 = 10;
    hf_vector_0(hv2, hv4, hv5, &mut hv6);
    *hr0 = hv6;
}
unsafe fn hf_vector__from__null_0<T0: Copy>(mut hv0: *mut T0, hr0: &mut hs_vector<T0>) {
    let mut hv3: hs_vector<T0> = uninit!();
    let mut hv1: i64 = uninit!();
    let mut hv2: i64 = uninit!();
    hv1 = 0;
    hv2 = 0;
    hf_vector_0(hv0, hv1, hv2, &mut hv3);
    *hr0 = hv3;
}
unsafe fn hf_push_0<T0: Copy>(mut hv0: *mut hs_vector<T0>, mut hv1: T0) {
    let mut hv3: i64 = uninit!();
    let mut hv4: i64 = uninit!();
    let mut hv2: *mut i64 = uninit!();
    let mut hv9: *mut i64 = uninit!();
    let mut hv5: bool = uninit!();
    let mut hv6: *mut hs_vector<T0> = uninit!();
    let mut hv7: *mut *mut T0 = uninit!();
    let mut hv8: *mut T0 = uninit!();
    hf_swap_0(hv0, hv1, &mut hv1, &mut hv0);
    hf__2e__2e_length_1(hv0, &mut hv0, &mut hv2);
    hf_read_0(hv2, &mut hv3);
    hf_swap_0(hv0, hv3, &mut hv3, &mut hv0);
    hf__2e__2e_capacity_1(hv0, &mut hv0, &mut hv2);
    hf_read_0(hv2, &mut hv4);
    hf_rot_0(hv3, hv0, hv4, &mut hv0, &mut hv3, &mut hv4);
    hf__3d__3d__1(hv3, hv4, &mut hv5);
    if hv5 {
        hf__2e__0(hv0, &mut hv0, &mut hv6);
        hf_std__grow__vector_0(hv6);
    } else {
    }
    hf__2e__2e_ptr_1(hv0, &mut hv0, &mut hv7);
    hf_read_0(hv7, &mut hv8);
    hf_swap_0(hv0, hv8, &mut hv8, &mut hv0);
    hf__2e__2e_length_1(hv0, &mut hv0, &mut hv2);
    hf_read_0(hv2, &mut hv3);
    hf_rot_0(hv8, hv0, hv3, &mut hv0, &mut hv3, &mut hv8);
    hf__2b__a(hv3, hv8, &mut hv8);
    hf_rot_0(hv1, hv0, hv8, &mut hv0, &mut hv8, &mut hv1);
    hf_write_0(hv8, hv1);
    hf__2e_length_1(hv0, &mut hv2);
    hf__2e__0(hv2, &mut hv2, &mut hv9);
    hf_read_0(hv9, &mut hv3);
    hv4 = 1;
    hf__2b__4(hv3, hv4, &mut hv3);
    hf_write_0(hv2, hv3);
}
unsafe fn hf_pop_0<T0: Copy>(mut hv0: *mut hs_vector<T0>, hr0: &mut T0) {
    let mut hv1: *mut i64 = uninit!();
    let mut hv5: *mut i64 = uninit!();
    let mut hv4: bool = uninit!();
    let mut hv7: *mut T0 = uninit!();
    let mut hv2: i64 = uninit!();
    let mut hv3: i64 = uninit!();
    let mut hv8: T0 = uninit!();
    let mut hv6: *mut *mut T0 = uninit!();
    hf__2e__2e_length_1(hv0, &mut hv0, &mut hv1);
    hf_read_0(hv1, &mut hv2);
    hv3 = 0;
    hf__3e__1(hv2, hv3, &mut hv4);
    if !hv4 {
        println!("assertion failed at std/vector.hop:27:23");
        std::process::exit(1);
    }
    hf__2e__2e_length_1(hv0, &mut hv0, &mut hv1);
    hf__2e__0(hv1, &mut hv1, &mut hv5);
    hf_read_0(hv5, &mut hv2);
    hv3 = 1;
    hf__2d__4(hv2, hv3, &mut hv2);
    hf_write_0(hv1, hv2);
    hf__2e__2e_ptr_1(hv0, &mut hv0, &mut hv6);
    hf_read_0(hv6, &mut hv7);
    hf_swap_0(hv0, hv7, &mut hv7, &mut hv0);
    hf__2e_length_1(hv0, &mut hv1);
    hf_read_0(hv1, &mut hv2);
    hf__2b__9(hv7, hv2, &mut hv7);
    hf_read_0(hv7, &mut hv8);
    *hr0 = hv8;
}
unsafe fn hf_index_0<T0: Copy>(mut hv0: *mut hs_vector<T0>, mut hv1: i64, hr0: &mut T0) {
    let mut hv2: i64 = uninit!();
    let mut hv3: i64 = uninit!();
    let mut hv8: T0 = uninit!();
    let mut hv4: bool = uninit!();
    let mut hv5: *mut i64 = uninit!();
    let mut hv7: *mut T0 = uninit!();
    let mut hv6: *mut *mut T0 = uninit!();
    hf__2e__0(hv1, &mut hv1, &mut hv2);
    hv3 = 0;
    hf__3e__3d__1(hv2, hv3, &mut hv4);
    if !hv4 {
        println!("assertion failed at std/vector.hop:33:12");
        std::process::exit(1);
    }
    hf_swap_0(hv0, hv1, &mut hv1, &mut hv0);
    hf__2e__2e_length_1(hv0, &mut hv0, &mut hv5);
    hf_read_0(hv5, &mut hv2);
    hf_rot_0(hv1, hv0, hv2, &mut hv0, &mut hv1, &mut hv2);
    hf__2e__0(hv2, &mut hv2, &mut hv3);
    hf_rot_0(hv1, hv2, hv3, &mut hv1, &mut hv2, &mut hv3);
    hf__3e__1(hv2, hv3, &mut hv4);
    if !hv4 {
        println!("assertion failed at std/vector.hop:34:36");
        std::process::exit(1);
    }
    hf_swap_0(hv0, hv1, &mut hv1, &mut hv0);
    hf__2e_ptr_1(hv0, &mut hv6);
    hf_read_0(hv6, &mut hv7);
    hf__2b__a(hv1, hv7, &mut hv7);
    hf_read_0(hv7, &mut hv8);
    *hr0 = hv8;
}
unsafe fn hf_free__vector_0<T0: Copy>(mut hv0: hs_vector<T0>) {
    let mut hv1: *mut T0 = uninit!();
    let mut hv2: i64 = uninit!();
    let mut hv3: i64 = uninit!();
    let mut hv4: bool = uninit!();
    hf__2e__2e_ptr_0(hv0, &mut hv0, &mut hv1);
    hf_swap_0(hv0, hv1, &mut hv1, &mut hv0);
    hf__2e_capacity_0(hv0, &mut hv2);
    hv3 = 0;
    hf__3e__1(hv2, hv3, &mut hv4);
    if hv4 {
        hf_free_0(hv1);
    } else {
    }
}
unsafe fn hf_std__grow__vector_0<T0: Copy>(mut hv0: *mut hs_vector<T0>) {
    let mut hv4: bool = uninit!();
    let mut hv9: bool = uninit!();
    let mut hv1: *mut i64 = uninit!();
    let mut hv2: i64 = uninit!();
    let mut hv3: i64 = uninit!();
    let mut hv8: i64 = uninit!();
    let mut hv6: *mut T0 = uninit!();
    let mut hv7: *mut T0 = uninit!();
    let mut hvb: *mut T0 = uninit!();
    let mut hv5: *mut *mut T0 = uninit!();
    let mut hva: *mut *mut T0 = uninit!();
    hf__2e__2e_length_1(hv0, &mut hv0, &mut hv1);
    hf_read_0(hv1, &mut hv2);
    hv3 = 0;
    hf__3d__3d__1(hv2, hv3, &mut hv4);
    if hv4 {
        hf__2e__2e_capacity_1(hv0, &mut hv0, &mut hv1);
        hv2 = 10;
        hf_write_0(hv1, hv2);
        hf__2e_ptr_1(hv0, &mut hv5);
        hv2 = 10;
        hv6 = alloc!(hv2, T0, alloc);
        hf__2e__0(hv6, &mut hv6, &mut hv7);
        hv3 = hv7 as usize as i64;
        hv8 = 0;
        hf__3d__3d__1(hv3, hv8, &mut hv9);
        if hv9 {
            println!("program aborted at std/vector.hop:52:13");
            std::process::exit(1);
        } else {
        }
        hf_write_0(hv5, hv6);
    } else {
        hf__2e__2e_capacity_1(hv0, &mut hv0, &mut hv1);
        hf_read_0(hv1, &mut hv3);
        hv8 = 2;
        hf__2a__4(hv3, hv8, &mut hv3);
        hf_swap_0(hv0, hv3, &mut hv3, &mut hv0);
        hf__2e_ptr_1(hv0, &mut hv5);
        hf__2e__0(hv5, &mut hv5, &mut hva);
        hf_read_0(hva, &mut hv6);
        hf_rot_0(hv3, hv5, hv6, &mut hv5, &mut hv6, &mut hv3);
        hf_realloc_0(hv6, hv3, &mut hv6);
        hf__2e__0(hv6, &mut hv6, &mut hvb);
        hv3 = hvb as usize as i64;
        hv8 = 0;
        hf__3d__3d__1(hv3, hv8, &mut hv9);
        if hv9 {
            println!("program aborted at std/vector.hop:59:13");
            std::process::exit(1);
        } else {
        }
        hf_write_0(hv5, hv6);
    }
}
unsafe fn hf_sort__vector_0(mut hv0: *mut hs_vector<f64>) {
}
unsafe fn hf_main_0() {
    let mut hv7: f64 = uninit!();
    let mut hv0: i64 = uninit!();
    let mut hv3: i64 = uninit!();
    let mut hv4: i64 = uninit!();
    let mut hv5: i64 = uninit!();
    let mut hv6: *mut hs_vector<f64> = uninit!();
    let mut hv2: hs_vector<f64> = uninit!();
    let mut hv1: *mut f64 = uninit!();
    hv0 = 0;
    hv1 = hv0 as usize as *mut f64;
    hf_vector__from__null_0(hv1, &mut hv2);
    hv3 = 0;
    hv4 = 20;
    for hi in hv3..hv4 {
        hv5 = hi;
        hv6 = &mut hv2 as *mut hs_vector<f64>;
        hf_random_0(&mut hv7);
        hf_push_0(hv6, hv7);
    }
    hv6 = &mut hv2 as *mut hs_vector<f64>;
    hf_sort__vector_0(hv6);
    hv4 = 0;
    hv3 = 20;
    for hi in hv4..hv3 {
        hv5 = hi;
        hf_swap_0(hv2, hv5, &mut hv5, &mut hv2);
        hv6 = &mut hv2 as *mut hs_vector<f64>;
        hf_rot_0(hv5, hv2, hv6, &mut hv2, &mut hv6, &mut hv5);
        hf_index_0(hv6, hv5, &mut hv7);
        hf_putln_1(hv7);
    }
    hf_free__vector_0(hv2);
}
unsafe fn hf_free_0<T>(hv0: *mut T) { std::alloc::dealloc(hv0 as *mut u8, *hallocs.as_ref().unwrap().get(&(hv0 as usize)).unwrap()); }
unsafe fn hf_read_0<T>(hv0: *mut T, hr0: &mut T) { *hr0 = hv0.read(); }
unsafe fn hf__2e_ptr_1<T0: Copy>(mut hv0: *mut hs_vector<T0>, hr0: &mut *mut *mut T0) {
    *hr0 = &mut (*hv0).hm_ptr;
}
fn hf__2e__0<T: Copy>(hv0: T, hr0: &mut T, hr1: &mut T) { *hr0 = hv0; *hr1 = hv0; }
fn hf__2b__4(hv0: i64, hv1: i64, hr0: &mut i64) { *hr0 = hv0.wrapping_add(hv1); }
unsafe fn hf_realloc_0<T>(hv0: *mut T, hv1: i64, hr0: &mut *mut T) {
    let new_size = hv1 as usize * std::mem::size_of::<T>();
    let old_layout = *hallocs.as_mut().unwrap().get(&(hv0 as usize)).unwrap();
    let ptr = std::alloc::realloc(hv0 as *mut u8, old_layout, new_size);
    let layout = std::alloc::Layout::array::<T>(hv1 as usize).unwrap();
    hallocs.as_mut().unwrap().insert(ptr as usize, layout);
    *hr0 = ptr as *mut T;
}
fn hf__2a__4(hv0: i64, hv1: i64, hr0: &mut i64) { *hr0 = hv0.wrapping_mul(hv1); }
unsafe fn hf__2e__2e_length_1<T0: Copy>(mut hv0: *mut hs_vector<T0>, hr0: &mut *mut hs_vector<T0>, hr1: &mut *mut i64) {
    *hr0 = hv0;
    *hr1 = &mut (*hv0).hm_length;
}
unsafe fn hf__2e__2e_ptr_1<T0: Copy>(mut hv0: *mut hs_vector<T0>, hr0: &mut *mut hs_vector<T0>, hr1: &mut *mut *mut T0) {
    *hr0 = hv0;
    *hr1 = &mut (*hv0).hm_ptr;
}
unsafe fn hf__2e__2e_ptr_0<T0: Copy>(mut hv0: hs_vector<T0>, hr0: &mut hs_vector<T0>, hr1: &mut *mut T0) {
    *hr0 = hv0;
    *hr1 = hv0.hm_ptr;
}
unsafe fn hf__2e_length_1<T0: Copy>(mut hv0: *mut hs_vector<T0>, hr0: &mut *mut i64) {
    *hr0 = &mut (*hv0).hm_length;
}
fn hf__3d__3d__1(hv0: i64, hv1: i64, hr0: &mut bool) { *hr0 = hv0 == hv1; }
unsafe fn hf_write_0<T>(hv0: *mut T, hv1: T) { std::ptr::write(hv0, hv1); }
unsafe fn hf__2e__2e_capacity_1<T0: Copy>(mut hv0: *mut hs_vector<T0>, hr0: &mut *mut hs_vector<T0>, hr1: &mut *mut i64) {
    *hr0 = hv0;
    *hr1 = &mut (*hv0).hm_capacity;
}
unsafe fn hf__2b__a<T>(hv0: i64, hv1: *mut T, hr0: &mut *mut T) { *hr0 = hv1.offset(hv0 as isize);  }
fn hf__3e__3d__1(hv0: i64, hv1: i64, hr0: &mut bool) { *hr0 = hv0 >= hv1; }
unsafe fn hf__2e_capacity_0<T0: Copy>(mut hv0: hs_vector<T0>, hr0: &mut i64) {
    *hr0 = hv0.hm_capacity;
}
unsafe fn hf_random_0(hr0: &mut f64) { *hr0 = hrandf(); }
fn hf__3e__1(hv0: i64, hv1: i64, hr0: &mut bool) { *hr0 = hv0 > hv1; }
unsafe fn hf__2b__9<T>(hv0: *mut T, hv1: i64, hr0: &mut *mut T) { *hr0 = hv0.offset(hv1 as isize); }
fn hf_putln_1(hv0: f64) { println!("{hv0:?}"); }
fn hf__2d__4(hv0: i64, hv1: i64, hr0: &mut i64) { *hr0 = hv0.wrapping_sub(hv1); }
fn main() {
    unsafe { hallocs = Some(std::collections::HashMap::new()); }
    let mut code: i64 = 0;
    unsafe { hf_main_0(); }
    std::process::exit(code as i32);
}
