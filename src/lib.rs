use std::mem::MaybeUninit;

const MAX_PREFIX: usize = 13;

const NONE_HEADER: Header = Header {
    children: 0,
    path_len: 0,
    path: [0; MAX_PREFIX],
};

const VALUE_HEADER: Header = Header {
    children: 1,
    path_len: 0,
    path: [0; MAX_PREFIX],
};

#[derive(Debug, Default, Clone, Copy)]
struct Header {
    children: u16,
    path: [u8; MAX_PREFIX],
    path_len: u8,
}

#[derive(Debug)]
enum Node<V> {
    None,
    Node4(Box<Node4<V>>),
    Node16(Box<Node16<V>>),
    Node48(Box<Node48<V>>),
    Node256(Box<Node256<V>>),
    Value(Box<V>),
}

impl<V> Default for Node<V> {
    fn default() -> Node<V> {
        Node::None
    }
}

#[derive(Debug)]
struct Node4<V> {
    header: Header,
    keys: [u8; 4],
    slots: [Node<V>; 4],
}

impl<V> Default for Node4<V> {
    fn default() -> Node4<V> {
        Node4 {
            header: Default::default(),
            keys: [255; 4],
            slots: slots_array(),
        }
    }
}

impl<V> Node4<V> {
    fn slots(&self) -> &[Node<V>] {
        &self.slots
    }

    fn free_slot(&self) -> Option<usize> {
        self.slots.iter().position(Node::is_none)
    }

    fn child(&self, byte: u8) -> Option<&Node<V>> {
        for idx in 0..4 {
            if self.keys[idx] == byte && !self.slots[idx].is_none() {
                return Some(&self.slots[idx]);
            }
        }
        None
    }

    fn child_mut(&mut self, byte: u8) -> (&mut u16, &mut Node<V>) {
        let idx_opt = self.keys.iter().position(|i| *i == byte).and_then(|idx| {
            if !self.slots[idx].is_none() {
                Some(idx)
            } else {
                None
            }
        });
        if let Some(idx) = idx_opt {
            (&mut self.header.children, &mut self.slots[idx])
        } else {
            let free_slot = self.free_slot().unwrap();
            self.keys[free_slot] = byte;
            (&mut self.header.children, &mut self.slots[free_slot])
        }
    }

    fn upgrade(mut self) -> Node16<V> {
        let mut n16 = Node16::default();
        for (slot, byte) in self.keys.iter().enumerate() {
            std::mem::swap(&mut self.slots[slot], &mut n16.slots[slot]);
            n16.keys[slot] = *byte;
        }
        n16
    }
}

#[derive(Debug)]
struct Node16<V> {
    header: Header,
    keys: [u8; 16],
    slots: [Node<V>; 16],
}

impl<V> Default for Node16<V> {
    fn default() -> Node16<V> {
        Node16 {
            header: Default::default(),
            keys: [255; 16],
            slots: slots_array(),
        }
    }
}

impl<V> Node16<V> {
    fn slots(&self) -> &[Node<V>] {
        &self.slots
    }

    fn free_slot(&self) -> Option<usize> {
        self.slots.iter().position(Node::is_none)
    }

    fn child(&self, byte: u8) -> Option<&Node<V>> {
        for idx in 0..16 {
            if self.keys[idx] == byte && !self.slots[idx].is_none() {
                return Some(&self.slots[idx]);
            }
        }
        None
    }

    fn child_mut(&mut self, byte: u8) -> (&mut u16, &mut Node<V>) {
        let idx_opt = self.keys.iter().position(|i| *i == byte).and_then(|idx| {
            if !self.slots[idx].is_none() {
                Some(idx)
            } else {
                None
            }
        });
        if let Some(idx) = idx_opt {
            (&mut self.header.children, &mut self.slots[idx])
        } else {
            let free_slot = self.free_slot().unwrap();
            self.keys[free_slot] = byte;
            (&mut self.header.children, &mut self.slots[free_slot])
        }
    }

    fn upgrade(mut self) -> Node48<V> {
        let mut n48 = Node48::default();
        for (slot, byte) in self.keys.iter().enumerate() {
            if !self.slots[slot].is_none() {
                std::mem::swap(&mut self.slots[slot], &mut n48.slots[slot]);
                n48.child_index[*byte as usize] = u8::try_from(slot).unwrap();
            }
        }
        n48
    }
}

#[derive(Debug)]
struct Node48<V> {
    header: Header,
    child_index: [u8; 256],
    slots: [Node<V>; 48],
}

impl<V> Default for Node48<V> {
    fn default() -> Node48<V> {
        Node48 {
            header: Default::default(),
            child_index: [255; 256],
            slots: slots_array(),
        }
    }
}

impl<V> Node48<V> {
    fn slots(&self) -> &[Node<V>] {
        &self.slots
    }

    fn free_slot(&self) -> Option<usize> {
        self.slots.iter().position(Node::is_none)
    }

    fn child(&self, byte: u8) -> Option<&Node<V>> {
        let idx = self.child_index[byte as usize];
        if idx == 255 {
            None
        } else {
            if self.slots[idx as usize].is_none() {
                None
            } else {
                Some(&self.slots[idx as usize])
            }
        }
    }

    fn child_mut(&mut self, byte: u8) -> (&mut u16, &mut Node<V>) {
        let idx = self.child_index[byte as usize];

        if idx == 255 {
            let free_slot = self.free_slot().unwrap();
            self.child_index[byte as usize] = u8::try_from(free_slot).unwrap();
            (&mut self.header.children, &mut self.slots[free_slot])
        } else {
            (&mut self.header.children, &mut self.slots[idx as usize])
        }
    }

    fn upgrade(mut self) -> Node256<V> {
        let mut n256 = Node256::default();

        for (byte, idx) in self.child_index.iter().enumerate() {
            if *idx != 255 {
                std::mem::swap(&mut n256.slots[byte], &mut self.slots[*idx as usize]);
            }
        }

        n256
    }
}

fn slots_array<V, const N: usize>() -> [Node<V>; N] {
    let mut raw_slots: [MaybeUninit<Node<V>>; N] =
        unsafe { MaybeUninit::<[MaybeUninit<Node<V>>; N]>::uninit().assume_init() };
    for idx in 0..N {
        raw_slots[idx].write(Node::None);
    }
    unsafe { (&raw_slots as *const _ as *const [Node<V>; N]).read() }
}

#[derive(Debug)]
struct Node256<V> {
    header: Header,
    slots: [Node<V>; 256],
}

impl<V> Node256<V> {
    fn slots(&self) -> &[Node<V>] {
        &self.slots
    }

    fn child(&self, byte: u8) -> Option<&Node<V>> {
        if self.slots[byte as usize].is_none() {
            None
        } else {
            Some(&self.slots[byte as usize])
        }
    }

    fn child_mut(&mut self, byte: u8) -> (&mut u16, &mut Node<V>) {
        let slot = &mut self.slots[byte as usize];
        (&mut self.header.children, slot)
    }
}

impl<V> Default for Node256<V> {
    fn default() -> Node256<V> {
        Node256 {
            header: Default::default(),
            slots: slots_array(),
        }
    }
}

#[derive(Debug)]
pub struct Art<V, const K: usize> {
    len: usize,
    root: Node<V>,
}

impl<V, const K: usize> Default for Art<V, K> {
    fn default() -> Art<V, K> {
        Art { len: 0, root: Node::None }
    }
}

impl<V> Node<V> {
    fn truncate_prefix(&mut self, path: &[u8]) {
        // expand path at shared prefix
        //println!("chopping off a prefix at node {:?} since our path is {:?}", cursor.header(), path);
        let prefix = self.prefix();

        let shared_bytes = path
            .iter()
            .zip(prefix.iter())
            .take_while(|(a, b)| a == b)
            .count();

        let mut new_node4 = Node4::default();
        new_node4.header.path[..shared_bytes].copy_from_slice(&prefix[..shared_bytes]);
        new_node4.header.path_len = u8::try_from(shared_bytes).unwrap();

        let new_node = Node::Node4(Box::new(new_node4));

        assert!(prefix.starts_with(new_node.prefix()));

        let mut old_cursor = std::mem::replace(self, new_node);

        let old_cursor_header = old_cursor.header_mut();
        let old_cursor_new_child_byte = old_cursor_header.path[shared_bytes];
        old_cursor_header.path.rotate_left(shared_bytes + 1);
        old_cursor_header.path_len -= u8::try_from(shared_bytes + 1).unwrap();

        let (_, child) = self.child_mut(old_cursor_new_child_byte);
        *child = old_cursor;
    }

    #[inline]
    fn is_none(&self) -> bool {
        matches!(self, Node::None)
    }

    fn assert_size(&self) {
        debug_assert_eq!(
            self.slots().iter().filter(|s| !s.is_none()).count(),
            self.len(),
        )
    }

    fn slots(&self) -> &[Node<V>] {
        match self {
            Node::Node4(n4) => n4.slots(),
            Node::Node16(n16) => n16.slots(),
            Node::Node48(n48) => n48.slots(),
            Node::Node256(n256) => n256.slots(),
            _ => &[],
        }
    }

    fn is_full(&self) -> bool {
        match self {
            Node::Node4(_) => 4 == self.len(),
            Node::Node16(_) => 16 == self.len(),
            Node::Node48(_) => 48 == self.len(),
            Node::Node256(_) => 256 == self.len(),
            _ => unreachable!(),
        }
    }

    fn len(&self) -> usize {
        self.header().children as usize
    }

    fn header(&self) -> &Header {
        match self {
            Node::Node4(n4) => &n4.header,
            Node::Node16(n16) => &n16.header,
            Node::Node48(n48) => &n48.header,
            Node::Node256(n256) => &n256.header,
            Node::None => &NONE_HEADER,
            Node::Value(_) => &VALUE_HEADER,
        }
    }

    fn header_mut(&mut self) -> &mut Header {
        match self {
            Node::Node4(n4) => &mut n4.header,
            Node::Node16(n16) => &mut n16.header,
            Node::Node48(n48) => &mut n48.header,
            Node::Node256(n256) => &mut n256.header,
            _ => unreachable!(),
        }
    }

    fn prefix(&self) -> &[u8] {
        let header = self.header();
        &header.path[..header.path_len as usize]
    }

    fn child(&self, byte: u8) -> Option<&Node<V>> {
        match self {
            Node::Node4(n4) => n4.child(byte),
            Node::Node16(n16) => n16.child(byte),
            Node::Node48(n48) => n48.child(byte),
            Node::Node256(n256) => n256.child(byte),
            Node::None => None,
            Node::Value(_) => unreachable!(),
        }
    }

    fn child_mut(&mut self, byte: u8) -> (&mut u16, &mut Node<V>) {
        // TODO this is gross
        if self.child(byte).is_none() && self.is_full() {
            self.upgrade()
        }

        match self {
            Node::Node4(n4) => n4.child_mut(byte),
            Node::Node16(n16) => n16.child_mut(byte),
            Node::Node48(n48) => n48.child_mut(byte),
            Node::Node256(n256) => n256.child_mut(byte),
            Node::None => unreachable!(),
            Node::Value(_) => unreachable!(),
        }
    }

    fn upgrade(&mut self) {
        let old_header = *self.header();
        let swapped = std::mem::take(self);
        *self = match swapped {
            Node::Node4(n4) => Node::Node16(Box::new(n4.upgrade())),
            Node::Node16(n16) => Node::Node48(Box::new(n16.upgrade())),
            Node::Node48(n48) => Node::Node256(Box::new(n48.upgrade())),
            Node::Node256(_) => unreachable!(),
            Node::None => unreachable!(),
            Node::Value(_) => unreachable!(),
        };
        *self.header_mut() = old_header;
    }
}

impl<V: std::fmt::Debug, const K: usize> Art<V, K> {
    pub fn new() -> Art<V, K> {
        Art::default()
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn insert(&mut self, key: [u8; K], mut value: V) -> Option<V> {
        let (parent_opt, cursor) = self.slot_for_key(&key, true);
        match cursor {
            Node::Value(ref mut old) => {
                std::mem::swap(&mut **old, &mut value);
                Some(value)
            }
            Node::None => {
                *cursor = Node::Value(Box::new(value));
                if let Some(children) = parent_opt {
                    *children = children.checked_add(1).unwrap();
                }
                self.len += 1;
                None
            }
            _ => unreachable!(),
        }
    }

    pub fn remove(&mut self, key: &[u8; K]) -> Option<V> {
        let (parent_opt, cursor) = self.slot_for_key(key, false);
        match std::mem::take(cursor) {
            Node::Value(old) => {
                *cursor = Node::None;
                if let Some(children) = parent_opt {
                    *children = children.checked_sub(1).unwrap();
                }
                self.len -= 1;
                Some(*old)
            }
            Node::None => {
                None
            }
            _ => unreachable!(),
        }
    }

    // returns the optional parent node for child maintenance, and the value node
    fn slot_for_key(&mut self, key: &[u8; K], is_add: bool) -> (Option<&mut u16>, &mut Node<V>) {
        let mut parent = None;
        let mut path: &[u8] = &key[..];
        let mut cursor: &mut Node<V> = &mut self.root;
        println!("root is {:?}", cursor);

        while !path.is_empty() {
            //println!("path: {:?} cursor {:?}", path, cursor);
            if cursor.is_none() {
                if !is_add {
                    return (parent, cursor);
                }
                *cursor = Node::Node4(Box::new(Node4::default()));
                let prefix_len = (path.len() - 1).min(MAX_PREFIX);
                let prefix = &path[..prefix_len];
                cursor.header_mut().path[..prefix_len].copy_from_slice(prefix);
                cursor.header_mut().path_len = u8::try_from(prefix_len).unwrap();
                let (p, child) = cursor.child_mut(path[prefix_len]);
                parent = Some(p);
                cursor = child;
                path = &path[prefix_len + 1..];
                continue;
            }

            cursor.assert_size();

            let prefix = cursor.prefix();
            if !path.starts_with(prefix) {
                cursor.truncate_prefix(path);
                continue;
            }

            let next_byte = path[prefix.len()];
            path = &path[prefix.len() + 1..];

            //println!("cursor is now {:?}", cursor);
            let (p, next_cursor) = cursor.child_mut(next_byte);
            cursor = next_cursor;
            parent = Some(p);
        }

        (parent, cursor)
    }

    pub fn get(&self, key: &[u8; K]) -> Option<&V> {
        let mut k: &[u8] = &*key;
        let mut cursor: &Node<V> = &self.root;

        loop {
            let prefix = cursor.prefix();
            if !k.starts_with(prefix) || k.len() <= prefix.len() {
                return None;
            }

            let child = cursor.child(k[prefix.len()])?;
            k = &k[prefix.len() + 1..];

            match child {
                &Node::Value(ref v) => return Some(v),
                &Node::None => return None,
                _ => cursor = child,
            }
        }
    }
}

#[test]
fn test_inserts() {
    /*
    let mut art = Art::new();
    assert_eq!(art.insert([], "v1"), None);
    assert_eq!(art.insert([], "v2"), Some("v1"));


    let mut art = Art::new();
    assert_eq!(art.insert([0], "k 0 v 1"), None);
    assert_eq!(art.insert([10], "k 1 v 1"), None);
    assert_eq!(art.insert([0], "k 0 v 2"), Some("k 0 v 1"));
    assert_eq!(art.insert([10], "k 1 v 2"), Some("k 1 v 1"));
    assert_eq!(art.insert([0], "k 0 v 3"), Some("k 0 v 2"));
    assert_eq!(art.insert([10], "k 1 v 3"), Some("k 1 v 2"));
    */

    let mut art: Art<&str, 2> = Art::new();
    assert_eq!(art.get(&[255, 255]), None);
    /*
    assert_eq!(art.insert([20, 20], "k 0 v 1"), None);
    assert_eq!(art.insert([20, 192], "k 1 v 1"), None);
    assert_eq!(art.insert([20, 20], "k 0 v 2"), Some("k 0 v 1"));
    assert_eq!(art.insert([20, 192], "k 1 v 2"), Some("k 1 v 1"));
    assert_eq!(art.insert([20, 20], "k 0 v 3"), Some("k 0 v 2"));
    assert_eq!(art.insert([20, 192], "k 1 v 3"), Some("k 1 v 2"));
    */
}
