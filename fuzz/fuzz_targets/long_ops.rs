#![no_main]
#[macro_use]
extern crate libfuzzer_sys;
extern crate arbitrary;
extern crate art;

use std::ops::Range;

use arbitrary::Arbitrary;

#[derive(Debug, Arbitrary)]
enum Op {
    Insert([u8; 4], u8),
    Remove([u8; 4]),
    Get([u8; 4]),
    Range([u8; 4], [u8; 4], bool),
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
            Op::Range(start, diff, forward) => {
                let mut end = [0; 4];
                for i in 0..4 {
                    end[i] = start[i].saturating_add(diff[i]);
                }
                 let range = Range {
                     start: expand(start),
                     end: expand(end),
                 };
                 if forward {
                    let a = art.range(range.clone()).map(|(_, v)| v).collect::<Vec<_>>();
                    let m = model.range(range).map(|(_, v)| v).collect::<Vec<_>>();
                    assert_eq!(a, m);
                } else {
                    let a = art.range(range.clone()).map(|(_, v)| v).rev().collect::<Vec<_>>();
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
