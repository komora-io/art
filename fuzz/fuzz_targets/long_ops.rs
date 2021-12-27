#![no_main]
#[macro_use]
extern crate libfuzzer_sys;
extern crate arbitrary;
extern crate art;

use arbitrary::Arbitrary;

#[derive(Debug, Arbitrary)]
enum Op {
    Insert([u8; 2], u8),
    Get([u8; 2]),
}

fn expand(k: [u8; 2]) -> [u8; 9] {
    let mut ret = [0; 9];
    ret[8] = k[1];

    let mut b = k[0];
    for i in 0..8 {
        if b.leading_zeros() == 0 {
            ret[i] = 1;
        }
        b = b.rotate_left(1);
    }

    ret
}

fuzz_target!(|ops: Vec<Op>| {
    let mut art = art::Art::default();
    let mut model = std::collections::HashMap::new();

    /*
    println!();
    println!("~~~~~~~~~~~~~~~");
    println!();
    */
    for op in ops {
        // println!("op: {:?}", op);
        match op {
            Op::Insert(k, v) => {
                assert_eq!(art.insert(expand(k), v), model.insert(k, v));
            }
            Op::Get(k) => {
                assert_eq!(art.get(&expand(k)), model.get(&k));
            }
        }
    }
});
