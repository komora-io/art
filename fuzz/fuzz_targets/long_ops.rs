#![no_main]
#[macro_use]
extern crate libfuzzer_sys;
extern crate arbitrary;
extern crate art;

use arbitrary::Arbitrary;

#[derive(Debug, Arbitrary)]
enum Op {
    Insert([u8; 3], u8),
    Remove([u8; 3]),
    Get([u8; 3]),
    Len,
}

fn expand(k: [u8; 3]) -> [u8; 10] {
    let mut ret = [0; 10];
    ret[0] = k[0];
    ret[9] = k[2];

    let mut b = k[1];
    for i in 1..9 {
        if b.leading_zeros() == 0 {
            ret[i] = 255;
        }
        b = b.rotate_left(1);
    }

    ret
}

fuzz_target!(|ops: Vec<Op>| {
    let mut art = art::Art::default();
    let mut model = std::collections::HashMap::new();

    println!();
    println!("~~~~~~~~~~~~~~~");
    println!();
    for op in ops {
        println!("op: {:?}", op);
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
            Op::Len => {
                assert_eq!(art.len(), model.len());
            }
        }
    }
});
