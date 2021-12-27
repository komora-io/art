#![no_main]
#[macro_use]
extern crate libfuzzer_sys;
extern crate arbitrary;
extern crate art;

use arbitrary::Arbitrary;

#[derive(Debug, Arbitrary)]
enum Op {
    Insert(u8, u8),
    Get(u8),
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
                assert_eq!(art.insert([k], v), model.insert([k], v));
            }
            Op::Get(k) => {
                assert_eq!(art.get(&[k]), model.get(&[k]));
            }
        }
    }
});
