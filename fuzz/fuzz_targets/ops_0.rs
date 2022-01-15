#![no_main]
#[macro_use]
extern crate libfuzzer_sys;
extern crate arbitrary;
extern crate art;

use std::ops::{RangeBounds, Bound};

use arbitrary::Arbitrary;

#[derive(Debug, Clone, Copy)]
struct B {
    start: Bound<[u8; 0]>,
    end: Bound<[u8; 0]>,
}

impl RangeBounds<[u8; 0]> for B {
    fn start_bound(&self) -> Bound<&[u8; 0]> {
        ref_bound(&self.start)
    }

    fn end_bound(&self) -> Bound<&[u8; 0]> {
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

        let (start, end) = match information % 8 {
            0 => (Bound::Included([]), Bound::Included([])),
            1 => (Bound::Included([]), Bound::Excluded([])),
            2 => (Bound::Included([]), Bound::Unbounded),
            3 => (Bound::Excluded([]), Bound::Included([])),
            // Excluded..Excluded is skipped because it's not valid
            4 => (Bound::Excluded([]), Bound::Unbounded),
            5 => (Bound::Unbounded, Bound::Included([])),
            6 => (Bound::Unbounded, Bound::Excluded([])),
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
    Insert(u8),
    Remove,
    Get,
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
            Op::Insert(v) => {
                assert_eq!(art.insert([], v), model.insert([], v));
            }
            Op::Remove => {
                assert_eq!(art.remove(&[]), model.remove(&[]));
            }
            Op::Get => {
                assert_eq!(art.get(&[]), model.get(&[]));
            }
            Op::Len => {
                assert_eq!(art.len(), model.len());
            }
            Op::Range(bound, forward) => {
                if forward {
                    let a = art
                        .range(bound)
                        .map(|(_, v)| v)
                        .collect::<Vec<_>>();
                    let m = model.range(bound).map(|(_, v)| v).collect::<Vec<_>>();
                    assert_eq!(a, m);
                } else {
                    let a = art
                        .range(bound)
                        .map(|(_, v)| v)
                        .rev()
                        .collect::<Vec<_>>();
                    let m = model
                        .range(bound)
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
