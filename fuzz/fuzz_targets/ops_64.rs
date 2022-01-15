#![no_main]
#[macro_use]
extern crate libfuzzer_sys;
extern crate arbitrary;
extern crate art;

use std::ops::Range;

use arbitrary::Arbitrary;

#[derive(Debug, Arbitrary)]
enum Op {
    Insert([u8; 64], u8),
    Remove([u8; 64]),
    Get([u8; 64]),
    Range([u8; 64], [u8; 64], bool),
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
                assert_eq!(art.insert(k, v), model.insert(k, v));
            }
            Op::Get(k) => {
                assert_eq!(art.get(&k), model.get(&k));
            }
            Op::Remove(k) => {
                assert_eq!(art.remove(&k), model.remove(&k));
            }
            Op::Range(start, diff, forward) => {
                let mut end = [0; 64];
                for i in 0..64 {
                    end[i] = start[i].saturating_add(diff[i]);
                }
                let range = Range {
                    start: start,
                    end: end,
                };
                if forward {
                    let a = art.range(range.clone()).map(|(_, v)| v).collect::<Vec<_>>();
                    let m = model.range(range).map(|(_, v)| v).collect::<Vec<_>>();
                    assert_eq!(a, m);
                } else {
                    let a = art
                        .range(range.clone())
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
