fn main() {

    const N: u64 = 100_000_000;

    let mut art = tiny_art::Art::new();

    let before_writes = std::time::Instant::now();
    for k in 0_u64..N {
        //println!("{}", k);
        assert!(art.insert(k.to_be_bytes(), [0_u8]).is_none());
        if (k + 1) % (N / 10) == 0 {
            println!(
                "{:.2} million wps",
                k as f64 / (before_writes.elapsed().as_micros() + 1) as f64,
            )
        }
    }
    dbg!(before_writes.elapsed());

    let before_reads = std::time::Instant::now();
    for k in 0_u64..N {
        assert_eq!(art.get(&k.to_be_bytes()), Some(&[0_u8]));
        if (k + 1) % (N / 10)== 0 {
            println!(
                "{:.2} million rps",
                k as f64 / (before_reads.elapsed().as_micros() + 1) as f64,
            )
        }
    }
    dbg!(before_reads.elapsed());

    drop(art);

    /*
    let mut btree = std::collections::BTreeMap::new();

    let before_writes = std::time::Instant::now();
    for k in 0_u64..N {
        //println!("{}", k);
        assert!(btree.insert(k.to_be_bytes(), [0_u8]).is_none());
        if (k + 1) % (N / 10)== 0 {
            println!(
                "{:.2} million wps",
                k as f64 / (before_writes.elapsed().as_micros() + 1) as f64,
            )
        }
    }
    dbg!(before_writes.elapsed());

    let before_reads = std::time::Instant::now();
    for k in 0_u64..N {
        assert_eq!(btree.get(&k.to_be_bytes()), Some(&[0_u8]));
        if (k + 1) % (N / 10)== 0 {
            println!(
                "{:.2} million rps",
                k as f64 / (before_reads.elapsed().as_micros() + 1) as f64,
            )
        }
    }
    dbg!(before_reads.elapsed());
    */
}
