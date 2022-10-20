#![allow(warnings)]
#[repr(transparent)]
struct hglobal<T: Copy>(T);
unsafe impl<T: Copy> Sync for hglobal<T> {}
static hg_count: hglobal<i64> = unsafe { std::mem::transmute([0u8; std::mem::size_of::<i64>()]) };
static hg_flag: hglobal<bool> = unsafe { std::mem::transmute([0u8; std::mem::size_of::<bool>()]) };
static hg_my__pair: hglobal<hs_pair<i64, u8>> = unsafe { std::mem::transmute([0u8; std::mem::size_of::<hs_pair<i64, u8>>()]) };
static hl0: hglobal<*mut u8> = hglobal(0usize as *mut u8);
#[derive(Clone, Copy)]
struct hs_pair<T0, T1> {
    hm_fst: *mut T0,
    hm_snd: *mut T1,
}
unsafe impl<T0, T1> Sync for hs_pair<T0, T1> {}
#[derive(Clone, Copy)]
struct hs_single<T0> {
    hm_value: T0,
}
unsafe impl<T0> Sync for hs_single<T0> {}
#[derive(Clone, Copy)]
struct hs_test {
}
unsafe impl Sync for hs_test {}
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
unsafe fn hf_alloc__test_0(mut hv0: i64, hr0: &mut *mut *mut *mut i64) {
    let mut hv2: *mut i64 = std::mem::MaybeUninit::uninit().assume_init();
    let mut hv8: *mut *mut *mut i64 = std::mem::MaybeUninit::uninit().assume_init();
    let mut hv6: u8 = std::mem::MaybeUninit::uninit().assume_init();
    let mut hv7: u8 = std::mem::MaybeUninit::uninit().assume_init();
    let mut hv4: bool = std::mem::MaybeUninit::uninit().assume_init();
    let mut hv1: *mut hs_pair<i64, u8> = std::mem::MaybeUninit::uninit().assume_init();
    let mut hv5: *mut u8 = std::mem::MaybeUninit::uninit().assume_init();
    let mut hv3: i64 = std::mem::MaybeUninit::uninit().assume_init();
    hv1 = std::alloc::alloc(std::alloc::Layout::array::<hs_pair<i64, u8>>(hv0 as usize).unwrap()) as *mut hs_pair<i64, u8>;
    hf__7e__0(hv1);
    hv2 = std::alloc::alloc_zeroed(std::alloc::Layout::new::<i64>()) as *mut i64;
    hf__2a__9(hv2, &mut hv3);
    hv3 = 0;
    hf__3d__3d__1(hv3, hv3, &mut hv4);
    if !hv4 {
        println!("assertion failed at test.hop:20:24");
        std::process::exit(1);
    }
    hv3 = 5;
    hv5 = std::alloc::alloc_zeroed(std::alloc::Layout::array::<u8>(hv3 as usize).unwrap()) as *mut u8;
    hv3 = 2;
    hf__2b__9(hv5, hv3, &mut hv5);
    hf__2a__9(hv5, &mut hv6);
    hv3 = 0;
    hf_to__byte_0(hv3, &mut hv7);
    hf__3d__3d__0(hv6, hv7, &mut hv4);
    if !hv4 {
        println!("assertion failed at test.hop:21:43");
        std::process::exit(1);
    }
    hv8 = std::alloc::alloc(std::alloc::Layout::new::<*mut *mut i64>()) as *mut *mut *mut i64;
    *hr0 = hv8;
}
unsafe fn hf_cast__to__int__ptr__ptr_0<T0: Copy>(mut hv0: *mut T0, hr0: &mut *mut *mut i64) {
    let mut hv1: *mut *mut i64 = std::mem::MaybeUninit::uninit().assume_init();
    hv1 = std::mem::transmute(hv0 as usize);
    *hr0 = hv1;
}
unsafe fn hf_cast__to__byte__ptr__ptr_0(mut hv0: *mut u8, hr0: &mut *mut *mut u8) {
    let mut hv1: *mut *mut u8 = std::mem::MaybeUninit::uninit().assume_init();
    hv1 = std::mem::transmute(hv0 as usize);
    *hr0 = hv1;
}
unsafe fn hf_test__if_0() {
    let mut hv1: bool = std::mem::MaybeUninit::uninit().assume_init();
    let mut hv2: bool = std::mem::MaybeUninit::uninit().assume_init();
    let mut hv0: i64 = std::mem::MaybeUninit::uninit().assume_init();
    let mut hv3: i64 = std::mem::MaybeUninit::uninit().assume_init();
    hv0 = 1;
    hf__7e__0(hv0);
    hv1 = true;
    if hv1 {
        hv0 = 1;
    } else {
        hv2 = false;
        if hv2 {
            hv0 = 2;
        } else {
            hv0 = 3;
            hv3 = 4;
            hv0 = hv3;
        }
    }
    hf_put_0(hv0);
}
unsafe fn hf_test__while_0() {
    let mut hv0: i64 = std::mem::MaybeUninit::uninit().assume_init();
    let mut hv1: i64 = std::mem::MaybeUninit::uninit().assume_init();
    let mut hv2: bool = std::mem::MaybeUninit::uninit().assume_init();
    let mut hv3: bool = std::mem::MaybeUninit::uninit().assume_init();
    hv0 = 1;
    hv1 = 2;
    hv1 = 3;
    hv2 = true;
    while hv2 {
        hf__7e__0(hv1);
        hv1 = 2;
        hv3 = true;
        hv2 = hv3;
    }
    hf__7e__0(hv1);
    hv1 = 1;
    hv2 = false;
    while hv2 {
        hf__21__2(hv1, &mut hv1);
        hf__21__2(hv1, &mut hv1);
        hf__7e__0(hv1);
        hv1 = 1;
        hv3 = false;
        hv2 = hv3;
    }
    hf__7e__0(hv1);
    hf__7e__0(hv1);
    hf__7e__0(hv0);
}
unsafe fn hf_test__abort_0() {
    println!("program aborted at test.hop:56:5");
    std::process::exit(1);
}
unsafe fn hf_member__test_0() {
    let mut hv0: *mut i64 = std::mem::MaybeUninit::uninit().assume_init();
    let mut hv2: hs_pair<i64, u8> = std::mem::MaybeUninit::uninit().assume_init();
    let mut hv1: *mut u8 = std::mem::MaybeUninit::uninit().assume_init();
    hv0 = std::alloc::alloc(std::alloc::Layout::new::<i64>()) as *mut i64;
    hv1 = std::alloc::alloc(std::alloc::Layout::new::<u8>()) as *mut u8;
    hf_pair_0(hv0, hv1, &mut hv2);
    hf_DEBUG__STACK_0();
    hf__2e_fst_0(hv2, &mut hv0);
    hf_DEBUG__STACK_0();
    hf__7e__0(hv0);
}
unsafe fn hf_test__for_0(hr0: &mut f64, hr1: &mut f64) {
    let mut hv0: f64 = std::mem::MaybeUninit::uninit().assume_init();
    let mut hv1: f64 = std::mem::MaybeUninit::uninit().assume_init();
    let mut hv2: i64 = std::mem::MaybeUninit::uninit().assume_init();
    let mut hv3: i64 = std::mem::MaybeUninit::uninit().assume_init();
    hv0 = 1.0;
    hv1 = 2.0;
    hv2 = 1;
    hv3 = 10;
    for hi in hv2..hv3 {
        hv3 = hi;
        [hv0, hv1] = [hv1, hv0];
    }
    *hr0 = hv0;
    *hr1 = hv1;
}
unsafe fn hf_size__of__test_0<T0: Copy>(mut hv0: T0, hr0: &mut i64) {
    let mut hv1: i64 = std::mem::MaybeUninit::uninit().assume_init();
    hf__7e__0(hv0);
    hv1 = std::mem::size_of::<T0>() as i64;
    *hr0 = hv1;
}
unsafe fn hf_main_0(hr0: &mut i64) {
    let mut hv0: *mut u8 = std::mem::MaybeUninit::uninit().assume_init();
    let mut hv1: hs_test = std::mem::MaybeUninit::uninit().assume_init();
    let mut hv2: hs_test = std::mem::MaybeUninit::uninit().assume_init();
    let mut hv5: bool = std::mem::MaybeUninit::uninit().assume_init();
    let mut hv3: i64 = std::mem::MaybeUninit::uninit().assume_init();
    let mut hv4: i64 = std::mem::MaybeUninit::uninit().assume_init();
    hv0 = hl0.0;
    hf_putlns_0(hv0);
    hf_test_0(&mut hv1);
    hf_test_0(&mut hv2);
    hf_neg_2(hv2, &mut hv3);
    hf__2b__b(hv1, hv3, &mut hv3);
    hf_putln_0(hv3);
    hv3 = 1;
    hv4 = 2;
    hf__2b__4(hv3, hv4, &mut hv3);
    hv4 = 3;
    hf__3d__3d__1(hv3, hv4, &mut hv5);
    if !hv5 {
        println!("assertion failed at test.hop:86:16");
        std::process::exit(1);
    }
    hv3 = 0;
    *hr0 = hv3;
}
unsafe fn hf__2b__b(mut hv0: hs_test, mut hv1: i64, hr0: &mut i64) {
    hf__7e__0(hv1);
    hf__7e__0(hv0);
    hv1 = 1;
    *hr0 = hv1;
}
unsafe fn hf_neg_2(mut hv0: hs_test, hr0: &mut i64) {
    let mut hv1: i64 = std::mem::MaybeUninit::uninit().assume_init();
    hf__7e__0(hv0);
    hv1 = 1;
    *hr0 = hv1;
}
unsafe fn hf_f_0(mut hv0: i64, hr0: &mut i64) {
    *hr0 = hv0;
}
unsafe fn hf_f_1(mut hv0: bool, hr0: &mut bool) {
    *hr0 = hv0;
}
unsafe fn hf__2b__9<T>(hv0: *mut T, hv1: i64, hr0: &mut *mut T) { *hr0 = hv0.offset(hv1 as isize); }
fn hf__2b__4(hv0: i64, hv1: i64, hr0: &mut i64) { *hr0 = hv0.wrapping_add(hv1); }
fn hf__3d__3d__1(hv0: i64, hv1: i64, hr0: &mut bool) { *hr0 = hv0 == hv1; }
unsafe fn hf__2a__9<T>(hv0: *mut T, hr0: &mut T) { *hr0 = hv0.read(); }
fn hf__3d__3d__0(hv0: u8, hv1: u8, hr0: &mut bool) { *hr0 = hv0 == hv1; }
unsafe fn hf_pair_0<T0: Copy, T1: Copy>(mut hv0: *mut T0, mut hv1: *mut T1, hr0: &mut hs_pair<T0, T1>) {
    todo!();
}
fn hf_to__byte_0(hv0: i64, hr0: &mut u8) { *hr0 = hv0 as u8; }
fn hf_put_0(hv0: i64) { print!("{hv0}"); }
unsafe fn hf__2e_fst_0<T0: Copy, T1: Copy>(mut hv0: hs_pair<T0, T1>, hr0: &mut *mut T0) {
    todo!();
}
unsafe fn hf_test_0(hr0: &mut hs_test) {
    todo!();
}
fn hf__21__2(hv0: i64, hr0: &mut i64) { *hr0 = !hv0; }
fn hf__7e__0<T>(hv0: T) {}
fn hf_putln_0(hv0: i64) { println!("{hv0}"); }
fn hf_DEBUG__STACK_0() {}
unsafe fn hf_putc_0(hv0: i64) { print!("{}", char::from_u32_unchecked(hv0 as u32)); }
fn main() {}
