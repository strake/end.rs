#![no_std]

use core::mem::transmute as xmut;

macro_rules! to_uns {
    ($n:expr, $bs:expr, $ns:expr) => ({
        assert_eq!($bs.len() << $n, $ns.len());
        for (bs, np) in Iterator::zip($bs.chunks(1 << $n), $ns.iter_mut()) {
            *np = Self::read_u(bs) as _;
        }
    })
}

macro_rules! from_uns {
    ($n:expr, $ns:expr, $bs:expr) => ({
        assert_eq!($bs.len() << $n, $ns.len());
        for (np, bs) in Iterator::zip($ns.iter(), $bs.chunks_mut(1 << $n)) {
            Self::write_u(bs, *np as _);
        }
    })
}

pub trait Endian: private::Sealed {
    fn read_u(&[u8]) -> u64;
    #[inline]
    fn read_i(bs: &[u8]) -> i64 { Self::read_u(bs) as _ }
    fn write_u(&mut [u8], n: u64);
    #[inline]
    fn write_i(bs: &mut [u8], n: i64) { Self::write_u(bs, n as _) }
    #[inline]
    fn to_u16s(bs: &[u8], ns: &mut [u16]) { to_uns!(2, bs, ns) }
    #[inline]
    fn to_i16s(bs: &[u8], ns: &mut [i16]) { Self::to_u16s(bs, unsafe { xmut(ns) }) }
    #[inline]
    fn from_u16s(ns: &[u16], bs: &mut [u8]) { from_uns!(2, ns, bs) }
    #[inline]
    fn from_i16s(ns: &[i16], bs: &mut [u8]) { Self::from_u16s(unsafe { xmut(ns) }, bs) }
    #[inline]
    fn to_u32s(bs: &[u8], ns: &mut [u32]) { to_uns!(4, bs, ns) }
    #[inline]
    fn to_i32s(bs: &[u8], ns: &mut [i32]) { Self::to_u32s(bs, unsafe { xmut(ns) }) }
    #[inline]
    fn from_u32s(ns: &[u32], bs: &mut [u8]) { from_uns!(4, ns, bs) }
    #[inline]
    fn from_i32s(ns: &[i32], bs: &mut [u8]) { Self::from_u32s(unsafe { xmut(ns) }, bs) }
    #[inline]
    fn to_u64s(bs: &[u8], ns: &mut [u64]) { to_uns!(8, bs, ns) }
    #[inline]
    fn to_i64s(bs: &[u8], ns: &mut [i64]) { Self::to_u64s(bs, unsafe { xmut(ns) }) }
    #[inline]
    fn from_u64s(ns: &[u64], bs: &mut [u8]) { from_uns!(8, ns, bs) }
    #[inline]
    fn from_i64s(ns: &[i64], bs: &mut [u8]) { Self::from_u64s(unsafe { xmut(ns) }, bs) }
}

mod private {
    pub trait Sealed {}
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Big;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Lil;

impl private::Sealed for Big {}
impl private::Sealed for Lil {}

impl Endian for Big {
    #[inline]
    fn read_u(bs: &[u8]) -> u64 {
        assert!(8 >= bs.len());
        let mut n = 0;
        for &b in bs {
            n <<= 8;
            n += b as u64;
        }
        n
    }

    #[inline]
    fn write_u(bs: &mut [u8], mut n: u64) {
        assert!(8 >= bs.len());
        for bp in bs.iter_mut().rev() {
            *bp = n as _;
            n >>= 8;
        }
    }
}

impl Endian for Lil {
    #[inline]
    fn read_u(bs: &[u8]) -> u64 {
        assert!(8 >= bs.len());
        let mut n = 0;
        for &b in bs.iter().rev() {
            n <<= 8;
            n += b as u64;
        }
        n
    }

    #[inline]
    fn write_u(bs: &mut [u8], mut n: u64) {
        assert!(8 >= bs.len());
        for bp in bs {
            *bp = n as _;
            n >>= 8;
        }
    }
}
