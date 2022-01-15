#![no_main]
#[macro_use]
extern crate libfuzzer_sys;
extern crate arbitrary;
extern crate art;

use std::ops::Range;

use arbitrary::Arbitrary;

#[derive(Debug, Clone)]
struct B(Range<[u8; 1]>);

impl<'a> Arbitrary<'a> for B {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        let first: u8 = Arbitrary::arbitrary(u)?;
        let addition: u8 = Arbitrary::arbitrary(u)?;

        Ok(B(Range {
            start: [first],
            end: [first.saturating_add(addition)],
        }))
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
                        .range(range.0.clone())
                        .map(|(_, v)| v)
                        .collect::<Vec<_>>();
                    let m = model.range(range.0).map(|(_, v)| v).collect::<Vec<_>>();
                    assert_eq!(a, m);
                } else {
                    let a = art
                        .range(range.0.clone())
                        .map(|(_, v)| v)
                        .rev()
                        .collect::<Vec<_>>();
                    let m = model
                        .range(range.0)
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
