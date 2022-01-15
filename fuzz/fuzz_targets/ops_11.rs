#![no_main]
#[macro_use]
extern crate libfuzzer_sys;
extern crate arbitrary;
extern crate art;

use std::ops::{RangeBounds, Bound};

use arbitrary::Arbitrary;

#[derive(Debug, Clone, Copy)]
struct B {
    start: Bound<[u8; 11]>,
    end: Bound<[u8; 11]>,
}

impl RangeBounds<[u8; 11]> for B {
    fn start_bound(&self) -> Bound<&[u8; 11]> {
        ref_bound(&self.start)
    }

    fn end_bound(&self) -> Bound<&[u8; 11]> {
        ref_bound(&self.end)
    }
}

fn ref_bound<T>(bound: &Bound<T>) -> Bound<&T> {
    match bound {
        Bound::Unbounded => Bound::Unbounded,
        Bound::Included(x) => Bound::Included(&x),
        Bound::Excluded(x) => Bound::Excluded(&x),
    }
}

impl<'a> Arbitrary<'a> for B {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        let information: u8 = Arbitrary::arbitrary(u)?;

        let a: [u8; 11] = Arbitrary::arbitrary(u)?;
        let add: [u8; 11] = Arbitrary::arbitrary(u)?;
        let mut b: [u8; 11] = [0; 11];

        for i in 0..11 {
            b[i] = a[i].saturating_add(add[i]);
        }

        let (start, end) = match information % 8 {
            0 => (Bound::Included(a), Bound::Included(b)),
            1 => (Bound::Included(a), Bound::Excluded(b)),
            2 => (Bound::Included(a), Bound::Unbounded),
            3 => (Bound::Excluded(a), Bound::Included(b)),
            // Excluded..Excluded is skipped because it's not valid
            4 => (Bound::Excluded(a), Bound::Unbounded),
            5 => (Bound::Unbounded, Bound::Included(b)),
            6 => (Bound::Unbounded, Bound::Excluded(b)),
            7 => (Bound::Unbounded, Bound::Unbounded),
            _ => unreachable!(),
        };

        Ok(B {
            start,
            end,
        })
    }
}

#[derive(Debug, Arbitrary)]
enum Op {
    Insert([u8; 4], u8),
    Remove([u8; 4]),
    Get([u8; 4]),
    Range(B, bool),
}

fn expand(k: [u8; 4]) -> [u8; 11] {
    let mut ret = [0; 11];

    ret[0] = k[0];
    ret[5] = k[2];
    ret[10] = k[3];

    let mut b = k[1];
    // byte at index 0 is k[0]
    for i in 1..5 {
        if b.leading_zeros() == 0 {
            ret[i] = 255;
        }
        b = b.rotate_left(1);
    }
    // byte at index 5 is k[2]
    for i in 6..10 {
        if b.leading_zeros() == 0 {
            ret[i] = 255;
        }
        b = b.rotate_left(1);
    }
    // byte at index 10 is k[3]

    ret
}

fuzz_target!(|ops: Vec<Op>| {
    let mut art = art::Art::default();
    let mut model = std::collections::BTreeMap::new();

    /*
    println!();
    println!("~~~~~~~~~~~~~~~");
    println!();
    */
    for op in ops {
        // println!("op: {:?}", op);
        match op {
            Op::Insert(k, v) => {
                assert_eq!(art.insert(expand(k), v), model.insert(expand(k), v));
            }
            Op::Get(k) => {
                assert_eq!(art.get(&expand(k)), model.get(&expand(k)));
            }
            Op::Remove(k) => {
                assert_eq!(art.remove(&expand(k)), model.remove(&expand(k)));
            }
            Op::Range(range, forward) => {
                if forward {
                    let a = art.range(range).map(|(_, v)| v).collect::<Vec<_>>();
                    let m = model.range(range).map(|(_, v)| v).collect::<Vec<_>>();
                    assert_eq!(a, m);
                } else {
                    let a = art
                        .range(range)
                        .map(|(_, v)| v)
                        .rev()
                        .collect::<Vec<_>>();
                    let m = model.range(range).map(|(_, v)| v).rev().collect::<Vec<_>>();
                    assert_eq!(a, m);
                }
            }
        }
    }

    assert_eq!(art.len(), model.len());

    let a = art.iter().map(|(_, v)| v).collect::<Vec<_>>();
    let mut m = model.iter().map(|(_, v)| v).collect::<Vec<_>>();
    assert_eq!(a, m);

    let ab = art.iter().rev().map(|(_, v)| v).collect::<Vec<_>>();
    m.reverse();
    assert_eq!(ab, m);
});
