#![allow(dead_code)]

pub use core::ops::Range;
use static_assertions::const_assert_eq;
use std::ops::{Deref, DerefMut};

pub const NULL_SPAN: Span = Span::new(0..0);

const_assert_eq!(core::mem::size_of::<Span>(), 8);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
    range: Range<u32>,
}

impl Span {
    #[inline]
    pub fn start(&self) -> usize {
        self.start as usize
    }

    #[inline]
    pub fn end(&self) -> usize {
        self.end as usize
    }

    #[inline]
    pub const fn new(range: Range<u32>) -> Self {
        Span { range }
    }

    #[inline]
    pub fn overlaps_with(&self, other: &Span) -> bool {
        self.contains(&other.start) || other.contains(&self.start)
    }

    #[inline]
    pub fn empty(&self) -> bool {
        self.range.is_empty()
    }

    #[inline]
    pub fn get_str<'a>(&self, src: &'a str) -> &'a str {
        &src[self.start()..self.end()]
    }

    #[inline]
    pub fn get_str_line<'a>(&self, src: &'a str) -> Option<(usize, &'a str)> {
        for (idx, line) in src.lines().enumerate() {
            let n_bytes = line.as_bytes().len();
            if (0..n_bytes)
                .find(|&offset| unsafe { line.as_ptr().add(offset) } == self.get_str(src).as_ptr())
                .is_some()
            {
                return Some((idx + 1, line));
            }
        }
        None
    }

    #[inline]
    pub fn to_end(&mut self, other: Span) {
        self.end = other.end
    }

    #[inline]
    pub fn to_start(&mut self, other: Span) {
        self.end = other.start
    }
}

impl Deref for Span {
    type Target = Range<u32>;
    fn deref(&self) -> &Self::Target {
        &self.range
    }
}
impl DerefMut for Span {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.range
    }
}

impl Default for Span {
    fn default() -> Self {
        NULL_SPAN
    }
}
