use std::alloc::{Layout, System};
use std::sync::atomic::{AtomicUsize, Ordering};

#[global_allocator]
static ALLOCATOR: Alloc = Alloc;

static ALLOCATED: AtomicUsize = AtomicUsize::new(0);
static FREED: AtomicUsize = AtomicUsize::new(0);
static RESIDENT: AtomicUsize = AtomicUsize::new(0);

fn allocated() -> usize {
    ALLOCATED.swap(0, Ordering::Relaxed) / 1_000_000
}

fn freed() -> usize {
    FREED.swap(0, Ordering::Relaxed) / 1_000_000
}

fn resident() -> usize {
    RESIDENT.load(Ordering::Relaxed) / 1_000_000
}

#[derive(Default, Debug, Clone, Copy)]
struct Alloc;

unsafe impl std::alloc::GlobalAlloc for Alloc {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let ret = System.alloc(layout);
        assert_ne!(ret, std::ptr::null_mut());
        ALLOCATED.fetch_add(layout.size(), Ordering::Relaxed);
        RESIDENT.fetch_add(layout.size(), Ordering::Relaxed);
        std::ptr::write_bytes(ret, 0xa1, layout.size());
        ret
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        std::ptr::write_bytes(ptr, 0xde, layout.size());
        FREED.fetch_add(layout.size(), Ordering::Relaxed);
        RESIDENT.fetch_sub(layout.size(), Ordering::Relaxed);
        System.dealloc(ptr, layout)
    }
}

// extracted from the fnv crate for minor, mostly
// compile-time optimizations.
#[allow(missing_copy_implementations)]
pub struct Hasher(u64);

impl Default for Hasher {
    #[inline]
    fn default() -> Hasher {
        Hasher(0xcbf29ce484222325)
    }
}

impl std::hash::Hasher for Hasher {
    #[inline]
    fn finish(&self) -> u64 {
        self.0
    }

    #[inline]
    #[allow(clippy::cast_lossless)]
    fn write(&mut self, bytes: &[u8]) {
        let Hasher(mut hash) = *self;

        for byte in bytes.iter() {
            hash ^= *byte as u64;
            hash = hash.wrapping_mul(0x100000001b3);
        }

        *self = Hasher(hash);
    }
}

#[allow(unused)]
type FastMap8<K, V> = std::collections::HashMap<K, V, std::hash::BuildHasherDefault<Hasher>>;

fn main() {
    const N: u64 = 10_000_000;

    const VALUE: u8 = 0;

    let convert = u64::to_be_bytes;

    println!();
    println!("Art:");
    let mut art = art::Art::new();

    let before_writes = std::time::Instant::now();
    for k in 0_u64..N {
        //println!("{}", k);
        assert!(art.insert(convert(k), VALUE).is_none());
        if (k + 1) % (N / 10) == 0 {
            println!(
                "{:.2} million wps {} mb allocated {} mb freed {} mb resident",
                k as f64 / (before_writes.elapsed().as_micros().max(1)) as f64,
                allocated(),
                freed(),
                resident(),
            )
        }
    }
    dbg!(before_writes.elapsed());

    let before_reads = std::time::Instant::now();
    for k in 0_u64..N {
        assert_eq!(art.get(&convert(k)), Some(&VALUE));
        if (k + 1) % (N / 10) == 0 {
            println!(
                "{:.2} million rps {} mb allocated {} mb freed {} mb resident",
                k as f64 / (before_reads.elapsed().as_micros().max(1)) as f64,
                allocated(),
                freed(),
                resident(),
            )
        }
    }
    dbg!(before_reads.elapsed());

    let before_scan = std::time::Instant::now();
    assert_eq!(art.iter().count() as u64, N);
    println!("full scan took {:?}", before_scan.elapsed());

    freed();
    let before_free = std::time::Instant::now();
    drop(art);
    println!(
        "freeing Art cleared {} mb in {:?}",
        freed(),
        before_free.elapsed()
    );

    println!();
    println!("BTreeMap:");
    let mut btree = std::collections::BTreeMap::new();

    let before_writes = std::time::Instant::now();
    for k in 0_u64..N {
        //println!("{}", k);
        assert!(btree.insert(convert(k), VALUE).is_none());
        if (k + 1) % (N / 10) == 0 {
            println!(
                "{:.2} million wps {} mb allocated {} mb freed {} mb resident",
                k as f64 / (before_writes.elapsed().as_micros().max(1)) as f64,
                allocated(),
                freed(),
                resident(),
            )
        }
    }
    dbg!(before_writes.elapsed());

    let before_reads = std::time::Instant::now();
    for k in 0_u64..N {
        assert_eq!(btree.get(&convert(k)), Some(&VALUE));
        if (k + 1) % (N / 10) == 0 {
            println!(
                "{:.2} million rps {} mb allocated {} mb freed {} mb resident",
                k as f64 / (before_reads.elapsed().as_micros().max(1)) as f64,
                allocated(),
                freed(),
                resident(),
            )
        }
    }
    dbg!(before_reads.elapsed());

    let before_scan = std::time::Instant::now();
    assert_eq!(btree.iter().count() as u64, N);
    println!("full scan took {:?}", before_scan.elapsed());

    freed();
    let before_free = std::time::Instant::now();
    drop(btree);
    println!(
        "freeing BTreeMap cleared {} mb in {:?}",
        freed(),
        before_free.elapsed()
    );

    println!();
    println!("HashMap:");
    let mut hash = FastMap8::default();

    let before_writes = std::time::Instant::now();
    for k in 0_u64..N {
        //println!("{}", k);
        assert!(hash.insert(convert(k), VALUE).is_none());
        if (k + 1) % (N / 10) == 0 {
            println!(
                "{:.2} million wps {} mb allocated {} mb freed {} mb resident",
                k as f64 / (before_writes.elapsed().as_micros().max(1)) as f64,
                allocated(),
                freed(),
                resident(),
            )
        }
    }
    dbg!(before_writes.elapsed());

    let before_reads = std::time::Instant::now();
    for k in 0_u64..N {
        assert_eq!(hash.get(&convert(k)), Some(&VALUE));
        if (k + 1) % (N / 10) == 0 {
            println!(
                "{:.2} million rps {} mb allocated {} mb freed {} mb resident",
                k as f64 / (before_reads.elapsed().as_micros().max(1)) as f64,
                allocated(),
                freed(),
                resident(),
            )
        }
    }
    dbg!(before_reads.elapsed());

    let before_scan = std::time::Instant::now();
    assert_eq!(hash.iter().count() as u64, N);
    println!("full scan took {:?}", before_scan.elapsed());

    freed();
    let before_free = std::time::Instant::now();
    drop(hash);
    println!(
        "freeing HashMap cleared {} mb in {:?}",
        freed(),
        before_free.elapsed()
    );
}
