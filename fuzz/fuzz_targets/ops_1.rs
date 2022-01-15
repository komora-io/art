#![no_main]
#[macro_use]
extern crate libfuzzer_sys;
extern crate arbitrary;
extern crate art;

use std::ops::{RangeBounds, Bound};

use arbitrary::Arbitrary;

#[derive(Debug, Clone, Copy)]
struct B {
    start: Bound<[u8; 1]>,
    end: Bound<[u8; 1]>,
}

impl RangeBounds<[u8; 1]> for B {
    fn start_bound(&self) -> Bound<&[u8; 1]> {
        ref_bound(&self.start)
    }

    fn end_bound(&self) -> Bound<&[u8; 1]> {
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

        let a: [u8; 1] = Arbitrary::arbitrary(u)?;
        let add: u8 = Arbitrary::arbitrary(u)?;
        let b: [u8; 1] = [a[0].saturating_add(add)];

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
    Insert(u8, u8),
    Remove(u8),
    Get(u8),
    Range(B, bool),
    Len,
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
                assert_eq!(art.insert([k], v), model.insert([k], v));
            }
            Op::Remove(k) => {
                assert_eq!(art.remove(&[k]), model.remove(&[k]));
            }
            Op::Get(k) => {
                assert_eq!(art.get(&[k]), model.get(&[k]));
            }
            Op::Len => {
                assert_eq!(art.len(), model.len());
            }
            Op::Range(range, forward) => {
                if forward {
                    let a = art
                        .range(range)
                        .map(|(_, v)| v)
                        .collect::<Vec<_>>();
                    let m = model.range(range).map(|(_, v)| v).collect::<Vec<_>>();
                    assert_eq!(a, m);
                } else {
                    let a = art
                        .range(range)
                        .map(|(_, v)| v)
                        .rev()
                        .collect::<Vec<_>>();
                    let m = model
                        .range(range)
                        .map(|(_, v)| v)
                        .rev()
                        .collect::<Vec<_>>();
                    assert_eq!(a, m);
                }
            }
        };

        assert_eq!(art.len(), model.len());

        let a = art.iter().map(|(_, v)| v).collect::<Vec<_>>();
        let mut m = model.iter().map(|(_, v)| v).collect::<Vec<_>>();
        assert_eq!(a, m);

        let ab = art.iter().rev().map(|(_, v)| v).collect::<Vec<_>>();
        m.reverse();
        assert_eq!(ab, m);
    }
});
