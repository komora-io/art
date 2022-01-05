#![no_main]
#[macro_use]
extern crate libfuzzer_sys;
extern crate arbitrary;
extern crate art;

use arbitrary::Arbitrary;

#[derive(Debug, Arbitrary)]
enum Op {
    Insert(u8, u8),
    Remove(u8),
    Get(u8),
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
        };

        let a = art.iter().map(|(k, v)| v).collect::<Vec<_>>();
        let m = model.iter().map(|(k, v)| v).collect::<Vec<_>>();
        assert_eq!(a, m);
    };
});
