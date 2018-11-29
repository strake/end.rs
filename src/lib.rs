#![no_std]

use core::{cmp::Ordering, hash::{Hash, Hasher}, marker::PhantomData};

macro_rules! to_ns {
    ($n:expr, $bs:expr, $ns:expr, $f:expr) => ({
        assert_eq!($bs.len() << $n, $ns.len());
        for (bs, np) in Iterator::zip($bs.chunks(1 << $n), $ns.iter_mut()) {
            *np = Self::read_u(bs) as _;
        }
    })
}

macro_rules! from_ns {
    ($n:expr, $ns:expr, $bs:expr, $f:expr) => ({
        assert_eq!($bs.len() << $n, $ns.len());
        for (np, bs) in Iterator::zip($ns.iter(), $bs.chunks_mut(1 << $n)) {
            $f(bs, *np as _);
        }
    })
}

pub trait Endian: private::Sealed {
    const is_big: bool = !Self::is_lil;
    const is_lil: bool = !Self::is_big;
    fn read_u(&[u8]) -> u64;
    #[inline]
    fn read_i(bs: &[u8]) -> i64 { Self::read_u(bs) as _ }
    fn write_u(&mut [u8], n: u64);
    #[inline]
    fn write_i(bs: &mut [u8], n: i64) { Self::write_u(bs, n as _) }
    #[inline]
    fn to_u16s(bs: &[u8], ns: &mut [u16]) { to_ns!(2, bs, ns, Self::read_u) }
    #[inline]
    fn to_i16s(bs: &[u8], ns: &mut [i16]) { to_ns!(2, bs, ns, Self::write_i) }
    #[inline]
    fn from_u16s(ns: &[u16], bs: &mut [u8]) { from_ns!(2, ns, bs, Self::write_u) }
    #[inline]
    fn from_i16s(ns: &[i16], bs: &mut [u8]) { from_ns!(2, ns, bs, Self::write_i) }
    #[inline]
    fn to_u32s(bs: &[u8], ns: &mut [u32]) { to_ns!(4, bs, ns, Self::read_u) }
    #[inline]
    fn to_i32s(bs: &[u8], ns: &mut [i32]) { to_ns!(4, bs, ns, Self::read_i) }
    #[inline]
    fn from_u32s(ns: &[u32], bs: &mut [u8]) { from_ns!(4, ns, bs, Self::write_u) }
    #[inline]
    fn from_i32s(ns: &[i32], bs: &mut [u8]) { from_ns!(4, ns, bs, Self::write_i) }
    #[inline]
    fn to_u64s(bs: &[u8], ns: &mut [u64]) { to_ns!(8, bs, ns, Self::read_u) }
    #[inline]
    fn to_i64s(bs: &[u8], ns: &mut [i64]) { to_ns!(8, bs, ns, Self::read_i) }
    #[inline]
    fn from_u64s(ns: &[u64], bs: &mut [u8]) { from_ns!(8, ns, bs, Self::write_u) }
    #[inline]
    fn from_i64s(ns: &[i64], bs: &mut [u8]) { from_ns!(8, ns, bs, Self::write_i) }
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
    const is_big: bool = true;

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
    const is_lil: bool = true;

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

#[derive(Clone, Copy)]
pub struct End<A, E: Endian>(A, PhantomData<E>);

impl<A: PartialEq, E: Endian> PartialEq for End<A, E> {
    #[inline]
    fn eq(&self, other: &Self) -> bool { self.0 == other.0 }
}
impl<A: Eq, E: Endian> Eq for End<A, E> {}
impl<A: Hash, E: Endian> Hash for End<A, E> {
    #[inline]
    fn hash<H: Hasher>(&self, h: &mut H) { self.0.hash(h) }
}

macro_rules! impl_fmt {
    ($c:path) => {
        impl<A: Copy, E: Copy + Endian> $c for End<A, E> where A: From<End<A, E>> + $c {
            #[inline]
            fn fmt(&self, fmt: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                <A>::from(*self).fmt(fmt)
            }
        }
    };

    ($c:path, $($cs:path),*) => { impl_fmt!($c); $(impl_fmt!($cs);)* }
}

use core::fmt;
impl_fmt!(fmt::Debug, fmt::Display, fmt::Octal, fmt::LowerHex, fmt::UpperHex);

macro_rules! do_impls {
    ($t:ty) => {
        impl<E: Endian> From<$t> for End<$t, E> {
            #[inline]
            fn from(a: $t) -> Self {
                End(if cfg!(target_endian = "little") == E::is_lil { a } else { a.swap_bytes() },
                    PhantomData)
            }
        }

        impl<E: Endian> From<End<$t, E>> for $t {
            #[inline]
            fn from(End(a, _): End<$t, E>) -> Self {
                if cfg!(target_endian = "little") == E::is_lil { a } else { a.swap_bytes() }
            }
        }
    };

    ($t:ty, $($ts:ty),*) => { do_impls!($t); $(do_impls!($ts);)* }
}

do_impls!(usize, u8, u16, u32, u64, u128,
          isize, i8, i16, i32, i64, i128);

macro_rules! impl_op {
    ($op:ident, $f:ident) => {
        impl<A: $op<B>, B, E: Endian> $op<B> for End<A, E>
          where A: From<End<A, E>>, End<<A as $op<B>>::Output, E>: From<<A as $op<B>>::Output> {
            type Output = End<<A as $op<B>>::Output, E>;
            #[inline]
            fn $f(self, other: B) -> Self::Output { A::$f(self.into(), other).into() }
        }
    }
}

macro_rules! impl_op_assign {
    ($op:ident, $f:ident) => {
        impl<A: $op<B>, B, E: Endian> $op<B> for End<A, E> {
            #[inline]
            fn $f(&mut self, operand: B) { A::$f(&mut self.0, operand) }
        }
    }
}

macro_rules! impl_op_unary {
    ($op:ident, $f:ident) => {
        impl<A: $op, E: Endian> $op for End<A, E>
          where A: From<End<A, E>>, End<<A as $op>::Output, E>: From<<A as $op>::Output> {
            type Output = End<<A as $op>::Output, E>;
            #[inline]
            fn $f(self) -> Self::Output { A::$f(self.into()).into() }
        }
    }
}

use core::ops::*;

impl_op!(BitAnd, bitand);
impl_op!(BitOr,  bitor);
impl_op!(BitXor, bitxor);
impl_op!(Add, add);
impl_op!(Sub, sub);
impl_op!(Mul, mul);
impl_op!(Div, div);
impl_op!(Rem, rem);
impl_op!(Shl, shl);
impl_op!(Shr, shr);

impl_op_unary!(Neg, neg);
impl_op_unary!(Not, not);

impl_op_assign!(BitAndAssign, bitand_assign);
impl_op_assign!(BitOrAssign,  bitor_assign);
impl_op_assign!(BitXorAssign, bitxor_assign);

impl<A: Copy + PartialOrd, E: Copy + Endian> PartialOrd for End<A, E> where Self: Into<A> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        A::partial_cmp(&(*self).into(), &(*other).into())
    }
}

impl<A: Copy + Ord, E: Copy + Endian> Ord for End<A, E> where Self: Into<A> {
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
        A::cmp(&(*self).into(), &(*other).into())
    }
}
