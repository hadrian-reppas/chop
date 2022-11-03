use std::collections::{HashMap, HashSet};
use std::mem;

use lazy_static::lazy_static;

use crate::program::{
    Program, ProgramFn, ProgramGlobal, ProgramMember, ProgramOp, ProgramStmt, ProgramStruct,
};
use crate::typecheck::{CallInfo, FnInfo, ProgramInfo};
use crate::types::{GSignature, Kind, Types};

const PRELUDE: &str = "#![allow(warnings)]
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
";

lazy_static! {
    static ref BUILTIN_MAP: HashMap<&'static str, Vec<&'static str>> = HashMap::from([
        (
            "+",
            vec![
                "fn hf__2b__0(hv0: u8, hv1: u8, hr0: &mut u8) { *hr0 = hv0.wrapping_add(hv1); }\n",
                "fn hf__2b__1(hv0: u8, hv1: i64, hr0: &mut i64) { *hr0 = (hv0 as i64).wrapping_add(hv1); }\n",
                "fn hf__2b__2(hv0: u8, hv1: f64, hr0: &mut f64) { *hr0 = hv0 as f64 + hv1; }\n",
                "fn hf__2b__3(hv0: i64, hv1: u8, hr0: &mut i64) { *hr0 = hv0.wrapping_add(hv1 as i64); }\n",
                "fn hf__2b__4(hv0: i64, hv1: i64, hr0: &mut i64) { *hr0 = hv0.wrapping_add(hv1); }\n",
                "fn hf__2b__5(hv0: i64, hv1: f64, hr0: &mut f64) { *hr0 = hv0 as f64 + hv1; }\n",
                "fn hf__2b__6(hv0: f64, hv1: u8, hr0: &mut f64) { *hr0 = hv0 + hv1 as f64; }\n",
                "fn hf__2b__7(hv0: f64, hv1: i64, hr0: &mut f64) { *hr0 = hv0 + hv1 as f64; }\n",
                "fn hf__2b__8(hv0: f64, hv1: f64, hr0: &mut f64) { *hr0 = hv0 + hv1; }\n",
                "unsafe fn hf__2b__9<T>(hv0: *mut T, hv1: i64, hr0: &mut *mut T) { *hr0 = hv0.offset(hv1 as isize); }\n",
                "unsafe fn hf__2b__a<T>(hv0: i64, hv1: *mut T, hr0: &mut *mut T) { *hr0 = hv1.offset(hv0 as isize);  }\n",
            ]
        ),
        (
            "-",
            vec![
                "fn hf__2d__0(hv0: u8, hv1: u8, hr0: &mut u8) { *hr0 = hv0.wrapping_sub(hv1); }\n",
                "fn hf__2d__1(hv0: u8, hv1: i64, hr0: &mut i64) { *hr0 = (hv0 as i64).wrapping_sub(hv1); }\n",
                "fn hf__2d__2(hv0: u8, hv1: f64, hr0: &mut f64) { *hr0 = hv0 as f64 - hv1; }\n",
                "fn hf__2d__3(hv0: i64, hv1: u8, hr0: &mut i64) { *hr0 = hv0.wrapping_sub(hv1 as i64); }\n",
                "fn hf__2d__4(hv0: i64, hv1: i64, hr0: &mut i64) { *hr0 = hv0.wrapping_sub(hv1); }\n",
                "fn hf__2d__5(hv0: i64, hv1: f64, hr0: &mut f64) { *hr0 = hv0 as f64 - hv1; }\n",
                "fn hf__2d__6(hv0: f64, hv1: u8, hr0: &mut f64) { *hr0 = hv0 - hv1 as f64; }\n",
                "fn hf__2d__7(hv0: f64, hv1: i64, hr0: &mut f64) { *hr0 = hv0 - hv1 as f64; }\n",
                "fn hf__2d__8(hv0: f64, hv1: f64, hr0: &mut f64) { *hr0 = hv0 - hv1; }\n",
                "unsafe fn hf__2d__9<T>(hv0: *mut T, hv1: i64, hr0: &mut *mut T) { *hr0 = hv0.offset(-hv1 as isize); }\n",
                "unsafe fn hf__2d__a<T>(hv0: *mut T, hv1: *mut T, hr0: &mut i64) { *hr0 = hv0.offset_from(hv1); }\n",
            ],
        ),
        (
            "*",
            vec![
                "fn hf__2a__0(hv0: u8, hv1: u8, hr0: &mut u8) { *hr0 = hv0.wrapping_mul(hv1); }\n",
                "fn hf__2a__1(hv0: u8, hv1: i64, hr0: &mut i64) { *hr0 = (hv0 as i64).wrapping_mul(hv1); }\n",
                "fn hf__2a__2(hv0: u8, hv1: f64, hr0: &mut f64) { *hr0 = hv0 as f64 * hv1; }\n",
                "fn hf__2a__3(hv0: i64, hv1: u8, hr0: &mut i64) { *hr0 = hv0.wrapping_mul(hv1 as i64); }\n",
                "fn hf__2a__4(hv0: i64, hv1: i64, hr0: &mut i64) { *hr0 = hv0.wrapping_mul(hv1); }\n",
                "fn hf__2a__5(hv0: i64, hv1: f64, hr0: &mut f64) { *hr0 = hv0 as f64 * hv1; }\n",
                "fn hf__2a__6(hv0: f64, hv1: u8, hr0: &mut f64) { *hr0 = hv0 * hv1 as f64; }\n",
                "fn hf__2a__7(hv0: f64, hv1: i64, hr0: &mut f64) { *hr0 = hv0 * hv1 as f64; }\n",
                "fn hf__2a__8(hv0: f64, hv1: f64, hr0: &mut f64) { *hr0 = hv0 * hv1; }\n",
            ],
        ),
        (
            "/",
            vec![
                "fn hf__2f__0(hv0: u8, hv1: u8, hr0: &mut u8) { *hr0 = hv0.wrapping_div(hv1); }\n",
                "fn hf__2f__1(hv0: u8, hv1: i64, hr0: &mut i64) { *hr0 = (hv0 as i64).wrapping_div(hv1); }\n",
                "fn hf__2f__2(hv0: u8, hv1: f64, hr0: &mut f64) { *hr0 = hv0 as f64 / hv1; }\n",
                "fn hf__2f__3(hv0: i64, hv1: u8, hr0: &mut i64) { *hr0 = hv0.wrapping_div(hv1 as i64); }\n",
                "fn hf__2f__4(hv0: i64, hv1: i64, hr0: &mut i64) { *hr0 = hv0.wrapping_div(hv1); }\n",
                "fn hf__2f__5(hv0: i64, hv1: f64, hr0: &mut f64) { *hr0 = hv0 as f64 / hv1; }\n",
                "fn hf__2f__6(hv0: f64, hv1: u8, hr0: &mut f64) { *hr0 = hv0 / hv1 as f64; }\n",
                "fn hf__2f__7(hv0: f64, hv1: i64, hr0: &mut f64) { *hr0 = hv0 / hv1 as f64; }\n",
                "fn hf__2f__8(hv0: f64, hv1: f64, hr0: &mut f64) { *hr0 = hv0 / hv1; }\n",
            ],
        ),
        (
            "%",
            vec![
                "fn hf__25__0(hv0: u8, hv1: u8, hr0: &mut u8) { *hr0 = hv0.wrapping_rem(hv1); }\n",
                "fn hf__25__1(hv0: u8, hv1: i64, hr0: &mut i64) { *hr0 = (hv0 as i64).wrapping_rem(hv1); }\n",
                "fn hf__25__2(hv0: u8, hv1: f64, hr0: &mut f64) { *hr0 = hv0 as f64 % hv1; }\n",
                "fn hf__25__3(hv0: i64, hv1: u8, hr0: &mut i64) { *hr0 = hv0.wrapping_rem(hv1 as i64); }\n",
                "fn hf__25__4(hv0: i64, hv1: i64, hr0: &mut i64) { *hr0 = hv0.wrapping_rem(hv1); }\n",
                "fn hf__25__5(hv0: i64, hv1: f64, hr0: &mut f64) { *hr0 = hv0 as f64 % hv1; }\n",
                "fn hf__25__6(hv0: f64, hv1: u8, hr0: &mut f64) { *hr0 = hv0 % hv1 as f64; }\n",
                "fn hf__25__7(hv0: f64, hv1: i64, hr0: &mut f64) { *hr0 = hv0 % hv1 as f64; }\n",
                "fn hf__25__8(hv0: f64, hv1: f64, hr0: &mut f64) { *hr0 = hv0 % hv1; }\n",
            ],
        ),
        (
            "&",
            vec![
                "fn hf__26__0(hv0: u8, hv1: u8, hr0: &mut u8) { *hr0 = hv0 & hv1; }\n",
                "fn hf__26__1(hv0: i64, hv1: i64, hr0: &mut i64) { *hr0 = hv0 & hv1; }\n",
                "fn hf__26__2(hv0: bool, hv1: bool, hr0: &mut bool) { *hr0 = hv0 & hv1; }\n",
            ],
        ),
        (
            "^",
            vec![
                "fn hf__5e__0(hv0: u8, hv1: u8, hr0: &mut u8) { *hr0 = hv0 ^ hv1; }\n",
                "fn hf__5e__1(hv0: i64, hv1: i64, hr0: &mut i64) { *hr0 = hv0 ^ hv1; }\n",
                "fn hf__5e__2(hv0: bool, hv1: bool, hr0: &mut bool) { *hr0 = hv0 ^ hv1; }\n",
            ],
        ),
        (
            "|",
            vec![
                "fn hf__7c__0(hv0: u8, hv1: u8, hr0: &mut u8) { *hr0 = hv0 | hv1; }\n",
                "fn hf__7c__1(hv0: i64, hv1: i64, hr0: &mut i64) { *hr0 = hv0 | hv1; }\n",
                "fn hf__7c__2(hv0: bool, hv1: bool, hr0: &mut bool) { *hr0 = hv0 | hv1; }\n",
            ],
        ),
        (
            "<<",
            vec![
                "fn hf__3c__3c__0(hv0: u8, hv1: u8, hr0: &mut u8) { *hr0 = hv0 << hv1; }\n",
                "fn hf__3c__3c__1(hv0: u8, hv1: i64, hr0: &mut u8) { *hr0 = hv0 << (hv1 as u8); }\n",
                "fn hf__3c__3c__2(hv0: i64, hv1: u8, hr0: &mut i64) { *hr0 = hv0 << (hv1 as i64); }\n",
                "fn hf__3c__3c__3(hv0: i64, hv1: i64, hr0: &mut i64) { *hr0 = hv0 << hv1; }\n",
            ],
        ),
        (
            ">>",
            vec![
                "fn hf__3e__3e__0(hv0: u8, hv1: u8, hr0: &mut u8) { *hr0 = hv0 >> hv1; }\n",
                "fn hf__3e__3e__1(hv0: u8, hv1: i64, hr0: &mut u8) { *hr0 = hv0 >> (hv1 as u8); }\n",
                "fn hf__3e__3e__2(hv0: i64, hv1: u8, hr0: &mut i64) { *hr0 = hv0 >> (hv1 as i64); }\n",
                "fn hf__3e__3e__3(hv0: i64, hv1: i64, hr0: &mut i64) { *hr0 = hv0 >> hv1; }\n",
            ],
        ),
        (
            "==",
            vec![
                "fn hf__3d__3d__0(hv0: u8, hv1: u8, hr0: &mut bool) { *hr0 = hv0 == hv1; }\n",
                "fn hf__3d__3d__1(hv0: i64, hv1: i64, hr0: &mut bool) { *hr0 = hv0 == hv1; }\n",
                "fn hf__3d__3d__2(hv0: f64, hv1: f64, hr0: &mut bool) { *hr0 = hv0 == hv1; }\n",
                "fn hf__3d__3d__3(hv0: bool, hv1: bool, hr0: &mut bool) { *hr0 = hv0 == hv1; }\n",
                "fn hf__3d__3d__4<T>(hv0: *mut T, hv1: *mut T, hr0: &mut bool) { *hr0 = hv0 == hv1; }\n",
            ],
        ),
        (
            "!=",
            vec![
                "fn hf__21__3d__0(hv0: u8, hv1: u8, hr0: &mut bool) { *hr0 = hv0 != hv1; }\n",
                "fn hf__21__3d__1(hv0: i64, hv1: i64, hr0: &mut bool) { *hr0 = hv0 != hv1; }\n",
                "fn hf__21__3d__2(hv0: f64, hv1: f64, hr0: &mut bool) { *hr0 = hv0 != hv1; }\n",
                "fn hf__21__3d__3(hv0: bool, hv1: bool, hr0: &mut bool) { *hr0 = hv0 != hv1; }\n",
                "fn hf__21__3d__4<T>(hv0: *mut T, hv1: *mut T, hr0: &mut bool) { *hr0 = hv0 != hv1; }\n",
            ],
        ),
        (
            "<",
            vec![
                "fn hf__3c__0(hv0: u8, hv1: u8, hr0: &mut bool) { *hr0 = hv0 < hv1; }\n",
                "fn hf__3c__1(hv0: i64, hv1: i64, hr0: &mut bool) { *hr0 = hv0 < hv1; }\n",
                "fn hf__3c__2(hv0: f64, hv1: f64, hr0: &mut bool) { *hr0 = hv0 < hv1; }\n",
                "fn hf__3c__3<T>(hv0: *mut T, hv1: *mut T, hr0: &mut bool) { *hr0 = hv0 < hv1; }\n",
            ],
        ),
        (
            "<=",
            vec![
                "fn hf__3c__3d__0(hv0: u8, hv1: u8, hr0: &mut bool) { *hr0 = hv0 <= hv1; }\n",
                "fn hf__3c__3d__1(hv0: i64, hv1: i64, hr0: &mut bool) { *hr0 = hv0 <= hv1; }\n",
                "fn hf__3c__3d__2(hv0: f64, hv1: f64, hr0: &mut bool) { *hr0 = hv0 <= hv1; }\n",
                "fn hf__3c__3d__3<T>(hv0: *mut T, hv1: *mut T, hr0: &mut bool) { *hr0 = hv0 <= hv1; }\n",
            ],
        ),
        (
            ">",
            vec![
                "fn hf__3e__0(hv0: u8, hv1: u8, hr0: &mut bool) { *hr0 = hv0 > hv1; }\n",
                "fn hf__3e__1(hv0: i64, hv1: i64, hr0: &mut bool) { *hr0 = hv0 > hv1; }\n",
                "fn hf__3e__2(hv0: f64, hv1: f64, hr0: &mut bool) { *hr0 = hv0 > hv1; }\n",
                "fn hf__3e__3<T>(hv0: *mut T, hv1: *mut T, hr0: &mut bool) { *hr0 = hv0 > hv1; }\n",
            ],
        ),
        (
            ">=",
            vec![
                "fn hf__3e__3d__0(hv0: u8, hv1: u8, hr0: &mut bool) { *hr0 = hv0 >= hv1; }\n",
                "fn hf__3e__3d__1(hv0: i64, hv1: i64, hr0: &mut bool) { *hr0 = hv0 >= hv1; }\n",
                "fn hf__3e__3d__2(hv0: f64, hv1: f64, hr0: &mut bool) { *hr0 = hv0 >= hv1; }\n",
                "fn hf__3e__3d__3<T>(hv0: *mut T, hv1: *mut T, hr0: &mut bool) { *hr0 = hv0 >= hv1; }\n",
            ],
        ),
        (
            "!",
            vec![
                "fn hf__21__0(hv0: bool, hr0: &mut bool) { *hr0 = !hv0; }\n",
                "fn hf__21__1(hv0: u8, hr0: &mut u8) { *hr0 = !hv0; }\n",
                "fn hf__21__2(hv0: i64, hr0: &mut i64) { *hr0 = !hv0; }\n",
            ],
        ),
        (
            "neg",
            vec![
                "fn hf_neg_0(hv0: i64, hr0: &mut i64) { *hr0 = hv0.wrapping_neg(); }\n",
                "fn hf_neg_1(hv0: f64, hr0: &mut f64) { *hr0 = -hv0; }\n",
            ],
        ),
        (
            ".",
            vec![
                "fn hf__2e__0<T: Copy>(hv0: T, hr0: &mut T, hr1: &mut T) { *hr0 = hv0; *hr1 = hv0; }\n"
            ],
        ),
        // ("~", vec!["fn hf__7e__0<T>(hv0: T) {}\n"]),
        (
            "to_byte",
            vec![
                "fn hf_to__byte_0(hv0: i64, hr0: &mut u8) { *hr0 = hv0 as u8; }\n",
                "fn hf_to__byte_1(hv0: f64, hr0: &mut u8) { *hr0 = hv0 as u8; }\n",
            ],
        ),
        (
            "to_int",
            vec![
                "fn hf_to__int_0(hv0: u8, hr0: &mut i64) { *hr0 = hv0 as i64; }\n",
                "fn hf_to__int_1(hv0: f64, hr0: &mut i64) { *hr0 = hv0 as i64; }\n",
                "fn hf_to__int_2<T>(hv0: *mut T, hr0: &mut i64) { *hr0 = hv0 as i64; }\n",
            ],
        ),
        (
            "to_float",
            vec![
                "fn hf_to__float_0(hv0: u8, hr0: &mut f64) { *hr0 = hv0 as f64; }\n",
                "fn hf_to__float_1(hv0: i64, hr0: &mut f64) { *hr0 = hv0 as f64; }\n",
            ],
        ),
        (
            "put",
            vec![
                "fn hf_put_0(hv0: i64) { print!(\"{hv0}\"); }\n",
                "fn hf_put_1(hv0: f64) { print!(\"{hv0:?}\"); }\n",
                "fn hf_put_2(hv0: u8) { print!(\"{hv0}\"); }\n",
                "fn hf_put_3(hv0: bool) { print!(\"{hv0}\"); }\n",
            ],
        ),
        (
            "putln",
            vec![
                "fn hf_putln_0(hv0: i64) { println!(\"{hv0}\"); }\n",
                "fn hf_putln_1(hv0: f64) { println!(\"{hv0:?}\"); }\n",
                "fn hf_putln_2(hv0: u8) { println!(\"{hv0}\"); }\n",
                "fn hf_putln_3(hv0: bool) { println!(\"{hv0}\"); }\n",
            ],
        ),
        (
            "puts",
            vec![
                "unsafe fn hf_puts_0(hv0: *mut u8) { print!(\"{}\", std::ffi::CStr::from_ptr(hv0 as *const i8).to_str().unwrap()); }\n",
            ],
        ),
        (
            "putlns",
            vec![
                "unsafe fn hf_putlns_0(hv0: *mut u8) { println!(\"{}\", std::ffi::CStr::from_ptr(hv0 as *const i8).to_str().unwrap()); }\n",
            ],
        ),
        (
            "putc",
            vec![
                "unsafe fn hf_putc_0(hv0: i64) { print!(\"{}\", char::from_u32_unchecked(hv0 as u32)); }\n",
            ],
        ),
        (
            "putlnc",
            vec![
                "unsafe fn hf_putlnc_0(hv0: i64) { println!(\"{}\", char::from_u32_unchecked(hv0 as u32)); }\n",
            ],
        ),
        (
            "putp",
            vec![
                "fn hf_putp_0<T>(hv0: *mut T) { print!(\"{hv0:p}\"); }\n",
            ],
        ),
        (
            "putlnp",
            vec![
                "fn hf_putlnp_0<T>(hv0: *mut T) { println!(\"{hv0:p}\"); }\n",
            ],
        ),
        (
            "ln",
            vec![
                "fn hf_ln_0() { println!(); }\n",
            ],
        ),
        (
            "read",
            vec![
                "unsafe fn hf_read_0<T>(hv0: *mut T, hr0: &mut T) { *hr0 = hv0.read(); }\n",
            ],
        ),
        (
            "write",
            vec![
                "unsafe fn hf_write_0<T>(hv0: *mut T, hv1: T) { std::ptr::write(hv0, hv1); }\n",
            ],
        ),
        (
            "exit",
            vec![
                "fn hf_exit_0(hv0: i64) { std::process::exit(hv0 as i32); }\n",
            ],
        ),
        (
            "realloc",
            vec![
                "unsafe fn hf_realloc_0<T>(hv0: *mut T, hv1: i64, hr0: &mut *mut T) {
    let new_size = hv1 as usize * std::mem::size_of::<T>();
    let old_layout = *hallocs.as_mut().unwrap().get(&(hv0 as usize)).unwrap();
    let ptr = std::alloc::realloc(hv0 as *mut u8, old_layout, new_size);
    let layout = std::alloc::Layout::array::<T>(hv1 as usize).unwrap();
    hallocs.as_mut().unwrap().insert(ptr as usize, layout);
    *hr0 = ptr as *mut T;
}\n",
            ],
        ),
        (
            "free",
            vec![
                "unsafe fn hf_free_0<T>(hv0: *mut T) { std::alloc::dealloc(hv0 as *mut u8, *hallocs.as_ref().unwrap().get(&(hv0 as usize)).unwrap()); }\n",
            ],
        ),
        (
            "copy",
            vec![
                "unsafe fn hf_copy_0<T>(hv0: *mut T, hv1: *mut T, hv2: i64) { std::ptr::copy(hv1, hv0, hv2 as usize); }\n"
            ],
        ),
        (
            "pow",
            vec![
                "fn hf_pow_0(hv0: f64, hv1: f64, hr0: &mut f64) { *hr0 = hv0.powf(hv1); }\n",
            ],
        ),
        (
            "random",
            vec![
                "unsafe fn hf_random_0(hr0: &mut f64) { *hr0 = hrandf(); }\n",
            ],
        ),
        (
            "strcmp",
            vec![
                "fn hf_strcmp_0(hv0: *mut u8, hv1: *mut u8, hr0: &mut i64) { *hr0 = todo!(); }\n",
            ],
        ),
        (
            "streq",
            vec![
                "fn hf_streq_0(hv0: *mut u8, hv1: *mut u8, hr0: &mut bool) { *hr0 = todo!(); }\n"
            ],
        ),
        (
            "strcpy",
            vec![
                "fn hf_strcpy_0(hv0: *mut u8, hv1: *mut u8) { todo!(); }\n",
            ],
        ),
        (
            "strlen",
            vec![
                "fn hf_strlen_0(hv1: *mut u8, hr0: &mut i64) { *hr0 = todo!(); }\n",
            ],
        ),
        (
            "read_file",
            vec![
                "fn hf_read__file_0(hv0: *mut u8, hr0: &mut *mut u8) { *hr0 = todo!(); }\n",
            ],
        ),
        (
            "write_to_file",
            vec![
                "fn hf_write__to__file_0(hv0: *mut u8, hv1: *mut u8) { todo!(); }\n",
            ],
        ),
        (
            "DEBUG_STACK",
            vec![
                "fn hf_DEBUG__STACK_0() {}\n"
            ],
        ),
    ]);
}

struct Context<'a> {
    program: &'a Program,
    call_graph: &'a HashMap<FnInfo, HashSet<CallInfo>>,
    signatures: &'a HashMap<&'static str, Vec<GSignature>>,
    types: &'a Types,
    non_custom: HashSet<(&'static str, usize)>,
    code: String,
    depth: usize,
}

pub fn generate(info: &ProgramInfo) -> String {
    let mut context = Context {
        program: &info.program,
        call_graph: &info.call_graph,
        signatures: &info.signatures,
        types: &info.types,
        non_custom: HashSet::new(),
        code: PRELUDE.to_string(),
        depth: 0,
    };
    context.generate();
    context.code
}

impl<'a> Context<'a> {
    fn generate(&mut self) {
        for global in &self.program.globals {
            self.generate_global(global);
        }
        for struct_def in &self.program.structs {
            self.generate_struct(struct_def);
        }
        for function in &self.program.functions {
            self.generate_function(function);
        }
        let non_custom = mem::take(&mut self.non_custom);
        for (name, index) in non_custom {
            self.generate_non_custom(name, index);
        }
        self.generate_main();
    }

    fn escape_name(&mut self, name: &str) {
        escape_name(name, &mut self.code);
    }

    fn generate_global(&mut self, global: &ProgramGlobal) {
        self.code.push_str("static mut hg_");
        self.escape_name(global.name);
        self.code.push_str(&format!(
            ": hglobal<{}> = zeroed!({});\n",
            self.types.rust_type_concrete(global.type_id),
            self.types.rust_type_concrete(global.type_id),
        ));
    }

    fn generate_struct(&mut self, struct_def: &ProgramStruct) {
        self.code.push_str("#[derive(Clone, Copy)]\nstruct hs_");
        self.escape_name(struct_def.name);
        self.declare_generics(struct_def.generic_count);
        self.code.push_str(" {\n");
        for member in &struct_def.members {
            self.code.push_str("    hm_");
            self.escape_name(member.name);
            self.code
                .push_str(&format!(": {},\n", self.types.rust_type(member.type_id),));
        }
        self.code.push_str("}\nfn hf_");
        self.escape_name(struct_def.name);
        let (index, signature) = self.signatures[struct_def.name]
            .iter()
            .enumerate()
            .find(|(_, sig)| matches!(sig.kind, Kind::Constructor(_)))
            .unwrap();
        self.code.push_str(&format!("_{index:x}"));
        self.declare_generics(struct_def.generic_count);
        self.code.push('(');
        for (i, param) in signature.params.iter().enumerate() {
            if i > 0 {
                self.code.push_str(", ");
            }
            self.code
                .push_str(&format!("hv{i:x}: {}", self.types.rust_type(*param)));
        }
        if !signature.params.is_empty() {
            self.code.push_str(", ");
        }
        self.code.push_str(&format!(
            "hr0: &mut {}",
            self.types.rust_type(signature.returns[0]),
        ));
        self.code.push_str(") {\n    *hr0 = hs_");
        self.escape_name(struct_def.name);
        self.code.push_str(" {\n");
        for (i, ProgramMember { name, .. }) in struct_def.members.iter().enumerate() {
            self.code.push_str("    hm_");
            self.escape_name(name);
            self.code.push_str(&format!(": hv{i:x},\n"));
        }
        self.code.push_str("    };\n}\n");
    }

    fn generate_function(&mut self, function: &ProgramFn) {
        for (name, index, _) in self
            .call_graph
            .get(&(function.name, function.index))
            .unwrap()
        {
            if !self.signatures[name][*index].kind.is_custom() {
                self.non_custom.insert((name, *index));
            }
        }
        self.code.push_str("unsafe fn hf_");
        self.escape_name(function.name);
        self.code.push_str(&format!("_{:x}", function.index));
        self.declare_copy_generics(function.generic_count);
        self.code.push('(');
        for (i, param) in function.params.iter().enumerate() {
            if i > 0 {
                self.code.push_str(", ");
            }
            self.code
                .push_str(&format!("mut hv{i:x}: {}", self.types.rust_type(*param)));
        }
        for (i, ret) in function.returns.iter().enumerate() {
            if i > 0 || !function.params.is_empty() {
                self.code.push_str(", ");
            }
            self.code
                .push_str(&format!("hr{i:x}: &mut {}", self.types.rust_type(*ret)));
        }
        self.code.push_str(") {\n");
        self.depth += 1;
        for (type_id, vars) in &function.vars {
            for var in vars {
                self.tabs();
                self.code.push_str(&format!(
                    "let mut hv{var:x}: {} = uninit!();\n",
                    self.types.rust_type(*type_id),
                ));
            }
        }

        for stmt in &function.body {
            self.generate_stmt(stmt);
        }

        for (i, var) in function.return_vars.iter().enumerate() {
            self.tabs();
            self.code.push_str(&format!("*hr{i:x} = hv{var:x};\n"));
        }
        self.depth -= 1;
        self.code.push_str("}\n");
    }

    fn generate_stmt(&mut self, stmt: &ProgramStmt) {
        match stmt {
            ProgramStmt::Op(op) => self.generate_op(op),
            ProgramStmt::If {
                test_var,
                body,
                else_body,
                resolve,
            } => self.generate_if(*test_var, body, else_body, resolve),
            ProgramStmt::For {
                iter_var,
                low_var,
                high_var,
                body,
                resolve,
            } => self.generate_for(*iter_var, *low_var, *high_var, body, resolve),
            ProgramStmt::While {
                test_var,
                body,
                resolve,
            } => self.generate_while(*test_var, body, resolve),
        }
    }

    fn generate_op(&mut self, op: &ProgramOp) {
        match op {
            ProgramOp::Int(var, val) => {
                self.tabs();
                self.code.push_str(&format!("hv{var:x} = {val};\n"));
            }
            ProgramOp::Float(var, val) => {
                self.tabs();
                self.code.push_str(&format!("hv{var:x} = {val:?};\n"));
            }
            ProgramOp::Bool(var, val) => {
                self.tabs();
                self.code.push_str(&format!("hv{var:x} = {val:?};\n"));
            }
            ProgramOp::Byte(var, val) => {
                self.tabs();
                self.code.push_str(&format!("hv{var:x} = {val:?};\n"));
            }
            ProgramOp::String(var, val) => {
                self.tabs();
                self.code.push_str(&format!("hv{var:x} = {val:?}.0;\n"));
            }
            ProgramOp::Call(name, index, params, returns) => {
                if *name == "~" {
                    return;
                }
                self.tabs();
                self.code.push_str("hf_");
                self.escape_name(name);
                self.code.push_str(&format!("_{index:x}("));
                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        self.code.push_str(", ");
                    }
                    self.code.push_str(&format!("hv{param:x}"));
                }
                for (i, ret) in returns.iter().enumerate() {
                    if i > 0 || !params.is_empty() {
                        self.code.push_str(", ");
                    }
                    self.code.push_str(&format!("&mut hv{ret:x}"));
                }
                self.code.push_str(");\n");
            }
            ProgramOp::Ref(var, old_var, type_id) => {
                self.tabs();
                self.code.push_str(&format!(
                    "hv{var:x} = &mut hv{old_var:x} as *mut {};\n",
                    self.types.rust_type(*type_id),
                ));
            }
            ProgramOp::Abort(span) => {
                self.tabs();
                self.code.push_str(&format!(
                    "println!(\"program aborted at {}\");\n",
                    span.location(),
                ));
                self.tabs();
                self.code.push_str("std::process::exit(1);\n");
            }
            ProgramOp::Assert(var, span) => {
                self.tabs();
                self.code.push_str(&format!("if !hv{var:x} {{\n"));
                self.depth += 1;
                self.tabs();
                self.code.push_str(&format!(
                    "println!(\"assertion failed at {}\");\n",
                    span.location(),
                ));
                self.tabs();
                self.code.push_str("std::process::exit(1);\n");
                self.depth -= 1;
                self.tabs();
                self.code.push_str("}\n");
            }
            ProgramOp::SizeOf(var, type_id) => {
                self.tabs();
                self.code.push_str(&format!(
                    "hv{var:x} = std::mem::size_of::<{}>() as i64;\n",
                    self.types.rust_type(*type_id)
                ));
            }
            ProgramOp::Alloc(var, type_id) => {
                self.tabs();
                self.code.push_str(&format!(
                    "hv{var:x} = alloc!(1, {}, alloc);\n",
                    self.types.rust_type(*type_id),
                ));
            }
            ProgramOp::Zalloc(var, type_id) => {
                self.tabs();
                self.code.push_str(&format!(
                    "hv{var:x} = alloc!(1, {}, alloc_zeroed);\n",
                    self.types.rust_type(*type_id),
                ));
            }
            ProgramOp::AllocArr(var, len_var, type_id) => {
                self.tabs();
                self.code.push_str(&format!(
                    "hv{var:x} = alloc!(hv{len_var:x}, {}, alloc);\n",
                    self.types.rust_type(*type_id),
                ));
            }
            ProgramOp::ZallocArr(var, len_var, type_id) => {
                self.tabs();
                self.code.push_str(&format!(
                    "hv{var:x} = alloc!(hv{len_var:x}, {}, alloc_zeroed);\n",
                    self.types.rust_type(*type_id),
                ));
            }
            ProgramOp::CastTo(var, old_var, type_id) => {
                self.tabs();
                self.code.push_str(&format!(
                    "hv{var:x} = hv{old_var:x} as usize as {};\n",
                    self.types.rust_type(*type_id),
                ));
            }
        }
    }

    fn generate_if(
        &mut self,
        test_var: usize,
        body: &[ProgramStmt],
        else_body: &[ProgramStmt],
        resolve: &[(usize, usize)],
    ) {
        self.tabs();
        self.code.push_str(&format!("if hv{test_var:x} {{\n"));
        self.depth += 1;
        for stmt in body {
            self.generate_stmt(stmt);
        }
        self.depth -= 1;
        self.tabs();
        self.code.push_str("} else {\n");
        self.depth += 1;
        for stmt in else_body {
            self.generate_stmt(stmt);
        }
        self.generate_resolve(resolve);
        self.depth -= 1;
        self.tabs();
        self.code.push_str("}\n");
    }

    fn generate_for(
        &mut self,
        iter_var: usize,
        low_var: usize,
        high_var: usize,
        body: &[ProgramStmt],
        resolve: &[(usize, usize)],
    ) {
        self.tabs();
        self.code
            .push_str(&format!("for hi in hv{low_var:x}..hv{high_var:x} {{\n"));
        self.depth += 1;
        self.tabs();
        self.code.push_str(&format!("hv{iter_var:x} = hi;\n"));
        for stmt in body {
            self.generate_stmt(stmt);
        }
        self.generate_resolve(resolve);
        self.depth -= 1;
        self.tabs();
        self.code.push_str("}\n");
    }

    fn generate_while(
        &mut self,
        test_var: usize,
        body: &[ProgramStmt],
        resolve: &[(usize, usize)],
    ) {
        self.tabs();
        self.code.push_str(&format!("while hv{test_var:x} {{\n"));
        self.depth += 1;
        for stmt in body {
            self.generate_stmt(stmt);
        }
        self.generate_resolve(resolve);
        self.depth -= 1;
        self.tabs();
        self.code.push_str("}\n");
    }

    fn generate_resolve(&mut self, resolve: &[(usize, usize)]) {
        match resolve.len() {
            0 => {}
            1 => {
                self.tabs();
                self.code
                    .push_str(&format!("hv{:x} = hv{:x};\n", resolve[0].0, resolve[0].1));
            }
            _ => {
                self.tabs();
                self.code.push('[');
                for (i, (var, _)) in resolve.iter().enumerate() {
                    if i > 0 {
                        self.code.push_str(", ");
                    }
                    self.code.push_str(&format!("hv{var:x}"));
                }
                self.code.push_str("] = [");
                for (i, (_, var)) in resolve.iter().enumerate() {
                    if i > 0 {
                        self.code.push_str(", ");
                    }
                    self.code.push_str(&format!("hv{var:x}"));
                }
                self.code.push_str("];\n")
            }
        }
    }

    fn generate_non_custom(&mut self, name: &str, index: usize) {
        if name == "~" {
            return;
        }
        let signature = &self.signatures[name][index];
        if matches!(signature.kind, Kind::Builtin) {
            if name != "@" && name != "abort" && name != "assert" {
                self.code.push_str(BUILTIN_MAP[name][index]);
            }
            return;
        } else if matches!(signature.kind, Kind::Constructor(_)) {
            return;
        }
        let mut generic_indices = HashSet::new();
        for type_id in &signature.params {
            generic_indices.extend(&self.types.generic_indices(*type_id));
        }
        let generic_count = generic_indices
            .into_iter()
            .max()
            .map(|n: usize| n + 1)
            .unwrap_or(0);
        self.code.push_str("unsafe fn hf_");
        self.escape_name(name);
        self.code.push_str(&format!("_{index:x}"));
        self.declare_copy_generics(generic_count);
        self.code.push('(');
        for (i, param) in signature.params.iter().enumerate() {
            if i > 0 {
                self.code.push_str(", ");
            }
            self.code
                .push_str(&format!("mut hv{i:x}: {}", self.types.rust_type(*param)));
        }
        for (i, ret) in signature.returns.iter().enumerate() {
            if i > 0 || !signature.params.is_empty() {
                self.code.push_str(", ");
            }
            self.code
                .push_str(&format!("hr{i:x}: &mut {}", self.types.rust_type(*ret)));
        }
        self.code.push_str(") {\n");
        match signature.kind {
            Kind::Member(_) => {
                let ptr = self.types.depth(signature.params[0]) > 0;
                if let Some(name) = name.strip_prefix("..") {
                    if ptr {
                        self.code.push_str("    *hr0 = hv0;\n");
                        self.code.push_str("    *hr1 = &mut (*hv0).hm_");
                        self.escape_name(name);
                        self.code.push_str(";\n");
                    } else {
                        self.code.push_str("    *hr0 = hv0;\n");
                        self.code.push_str("    *hr1 = hv0.hm_");
                        self.escape_name(name);
                        self.code.push_str(";\n");
                    }
                } else {
                    let name = &name[1..];
                    if ptr {
                        self.code.push_str("    *hr0 = &mut (*hv0).hm_");
                        self.escape_name(name);
                        self.code.push_str(";\n");
                    } else {
                        self.code.push_str("    *hr0 = hv0.hm_");
                        self.escape_name(name);
                        self.code.push_str(";\n");
                    }
                }
            }
            Kind::Global(_) => {
                self.code.push_str("    *hr0 = hg_");
                self.escape_name(name);
                self.code.push_str(".0;\n");
            }
            Kind::GlobalWrite(_) => {
                let name = name.strip_prefix("write_").unwrap();
                self.code.push_str("    hg_");
                self.escape_name(name);
                self.code.push_str(" = hglobal(hv0);\n")
            }
            Kind::GlobalPtr(_) => {
                let name = name.strip_suffix("_ptr").unwrap();
                self.code.push_str("    *hr0 = &mut hg_");
                self.escape_name(name);
                self.code.push_str(";\n")
            }
            _ => unreachable!(),
        }
        self.code.push_str("}\n");
    }

    fn generate_main(&mut self) {
        self.code.push_str("fn main() {\n");
        self.code
            .push_str("    unsafe { hallocs = Some(std::collections::HashMap::new()); }\n");
        for global in &self.program.globals {
            if let Some(init) = &global.init {
                self.code.push_str("    unsafe {\n");
                self.depth = 2;
                for (type_id, vars) in &init.vars {
                    for var in vars {
                        self.tabs();
                        self.code.push_str(&format!(
                            "let mut hv{var:x}: {} = std::mem::MaybeUninit::uninit().assume_init();\n",
                            self.types.rust_type(*type_id),
                        ));
                    }
                }
                for op in &init.ops {
                    self.generate_op(op);
                }
                self.tabs();
                self.code.push_str("hg_");
                self.escape_name(global.name);
                self.code
                    .push_str(&format!(" = hglobal(hv{:x});\n", init.var));
                self.depth = 0;
                self.code.push_str("    }\n");
            }
        }
        let main_signature = &self.signatures["main"][0];
        if !main_signature.params.is_empty() {
            self.code.push_str("    let args: Vec<_> = std::env::args().map(|arg| std::ffi::CString::new(arg).unwrap()).collect();\n");
            self.code.push_str("    let mut c_args = args.iter().map(|arg| arg.as_ptr() as *mut u8).collect::<Vec<*mut u8>>();\n");
            self.code.push_str("    let argc = c_args.len() as i64;\n");
            self.code
                .push_str("    let argv: *mut *mut u8 = c_args.as_mut_ptr();\n");
        }
        self.code.push_str("    let mut code: i64 = 0;\n");
        self.code.push_str("    unsafe { hf_main_0(");
        if !main_signature.params.is_empty() {
            self.code.push_str("argc, argv");
        }
        if !main_signature.returns.is_empty() {
            if !main_signature.params.is_empty() {
                self.code.push_str(", ");
            }
            self.code.push_str("&mut code");
        }
        self.code
            .push_str("); }\n    std::process::exit(code as i32);\n}\n");
    }

    fn declare_generics(&mut self, n: usize) {
        if n > 0 {
            self.code.push('<');
            for i in 0..n {
                if i > 0 {
                    self.code.push_str(", ");
                }
                self.code.push_str(&format!("T{}", i));
            }
            self.code.push('>');
        }
    }

    fn declare_copy_generics(&mut self, n: usize) {
        if n > 0 {
            self.code.push('<');
            for i in 0..n {
                if i > 0 {
                    self.code.push_str(", ");
                }
                self.code.push_str(&format!("T{}: Copy", i));
            }
            self.code.push('>');
        }
    }

    fn tabs(&mut self) {
        self.code.push_str(&"    ".repeat(self.depth));
    }
}

pub fn escape_name(name: &str, buf: &mut String) {
    for c in name.chars() {
        if c.is_ascii_alphanumeric() {
            buf.push(c);
        } else if c == '_' {
            buf.push_str("__");
        } else {
            buf.push_str(&format!("_{:x}_", c as u32));
        }
    }
}
