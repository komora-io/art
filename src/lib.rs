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

pub struct Iter<'a, V, const K: usize> {
    root: NodeIter<'a, V>,
    path: Vec<(u8, NodeIter<'a, V>)>,
}

impl<'a, V: std::fmt::Debug, const K: usize> IntoIterator for &'a Art<V, K> {
    type IntoIter = Iter<'a, V, K>;
    type Item = ([u8; K], &'a V);

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a, V, const K: usize> Iterator for Iter<'a, V, K> {
    type Item = ([u8; K], &'a V);

    fn next(&mut self) -> Option<Self::Item> {

        let (vc, v) = loop {
            if self.path.is_empty() {
                let (c, node) = self.root.children.next()?;
                self.path.push((c, node.node_iter()));
            }
            match self.path.last_mut().unwrap().1.children.next() {
                Some((c, node)) => {
                    match node {
                        Node::Value(v) => break (c, v),
                        Node::None => unreachable!(),
                        other => self.path.push((c, other.node_iter())),
                    }
                }
                None => {
                    self.path.pop();
                    continue;
                }
            }
        };

        let mut k = [0; K];
        let mut written = 0;
        let root_prefix = self.root.node.prefix();
        k[..root_prefix.len()].copy_from_slice(root_prefix);
        written += root_prefix.len();

        for (c, node_iter) in &self.path {
            k[written] = *c;
            written += 1;

            let node_prefix = node_iter.node.prefix();

            k[written..written + node_prefix.len()].copy_from_slice(node_prefix);
            written += node_prefix.len();
        }

        k[written] = vc;
        written += 1;

        assert_eq!(written, K);

        Some((k, v))
    }
}

struct NodeIter<'a, V> {
    node: &'a Node<V>,
    children: Box<dyn 'a + Iterator<Item = (u8, &'a Node<V>)>>,
}

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

    fn node_iter(&self) -> impl Iterator<Item = (u8, &Node<V>)> {
        let mut pairs: [(u8, &Node<V>); 4] = [
            (self.keys[0], &self.slots[0]),
            (self.keys[1], &self.slots[1]),
            (self.keys[2], &self.slots[2]),
            (self.keys[3], &self.slots[3]),
        ];

        pairs.sort_unstable_by_key(|(k, _)| *k);

        pairs.into_iter().filter(|(_, n)| !n.is_none())
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

    fn node_iter(&self) -> impl Iterator<Item = (u8, &Node<V>)> {
        let mut pairs: [(u8, &Node<V>); 16] = [
            (self.keys[0], &self.slots[0]),
            (self.keys[1], &self.slots[1]),
            (self.keys[2], &self.slots[2]),
            (self.keys[3], &self.slots[3]),
            (self.keys[4], &self.slots[4]),
            (self.keys[5], &self.slots[5]),
            (self.keys[6], &self.slots[6]),
            (self.keys[7], &self.slots[7]),
            (self.keys[8], &self.slots[8]),
            (self.keys[9], &self.slots[9]),
            (self.keys[10], &self.slots[10]),
            (self.keys[11], &self.slots[11]),
            (self.keys[12], &self.slots[12]),
            (self.keys[13], &self.slots[13]),
            (self.keys[14], &self.slots[14]),
            (self.keys[15], &self.slots[15]),
        ];

        pairs.sort_unstable_by_key(|(k, _)| *k);

        pairs.into_iter().filter(|(_, n)| !n.is_none())
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
                assert_eq!(n48.child_index[*byte as usize], 255);
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

    fn node_iter(&self) -> impl Iterator<Item = (u8, &Node<V>)> {
        self.child_index.iter()
            .filter(|i| **i != 255 && !self.slots[**i as usize].is_none())
            .map(|i| (*i, &self.slots[*i as usize]))
    }

    fn free_slot(&self) -> Option<usize> {
        self.slots.iter().position(Node::is_none)
    }

    fn child(&self, byte: u8) -> Option<&Node<V>> {
        let idx = self.child_index[byte as usize];
        if idx == 255 || self.slots[idx as usize].is_none() {
            None
        } else {
            Some(&self.slots[idx as usize])
        }
    }

    fn child_mut(&mut self, byte: u8, clear_child_index: bool) -> (&mut u16, &mut Node<V>) {
        let idx = self.child_index[byte as usize];

        if idx == 255 {
            let free_slot = self.free_slot().unwrap();
            if !clear_child_index {
                self.child_index[byte as usize] = u8::try_from(free_slot).unwrap();
            }
            (&mut self.header.children, &mut self.slots[free_slot])
        } else {
            if clear_child_index {
                self.child_index[byte as usize] = 255;
            }
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

    fn node_iter(&self) -> impl Iterator<Item = (u8, &Node<V>)> {
        self.slots.iter().enumerate().filter(|(_, slot)| !slot.is_none())
            .map(|(i, slot)| (u8::try_from(i).unwrap(), slot))
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
    fn truncate_prefix(&mut self, partial_path: &[u8]) {
        // println!("truncating prefix");
        // expand path at shared prefix
        //println!("chopping off a prefix at node {:?} since our partial path is {:?}", cursor.header(), partial_path);
        let prefix = self.prefix();

        let shared_bytes = partial_path
            .iter()
            .zip(prefix.iter())
            .take_while(|(a, b)| a == b)
            .count();

        println!("truncated node has path of len {} with a reduction of {}", shared_bytes, prefix.len() - shared_bytes);
        let mut new_node4 = Node4::default();
        new_node4.header.path[..shared_bytes].copy_from_slice(&prefix[..shared_bytes]);
        new_node4.header.path_len = u8::try_from(shared_bytes).unwrap();

        let new_node = Node::Node4(Box::new(new_node4));

        assert!(prefix.starts_with(new_node.prefix()));

        let mut old_cursor = std::mem::replace(self, new_node);

        let old_cursor_header = old_cursor.header_mut();
        let old_cursor_new_child_byte = old_cursor_header.path[shared_bytes];

        // we add +1 because we must account for the extra byte
        // reduced from the node's fan-out itself.
        old_cursor_header.path.rotate_left(shared_bytes + 1);
        old_cursor_header.path_len =
            old_cursor_header.path_len.checked_sub(
                u8::try_from(shared_bytes + 1).unwrap()
            ).unwrap();

        let (_, child) = self.child_mut(old_cursor_new_child_byte, false).unwrap();
        *child = old_cursor;
        child.assert_size();

        self.header_mut().children = 1;
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

    fn child_mut(&mut self, byte: u8, clear_child_index: bool) -> Option<(&mut u16, &mut Node<V>)> {
        // TODO this is gross
        if self.child(byte).is_none() && self.is_full() {
            if clear_child_index {
                return None;
            }
            self.upgrade()
        }

        Some(match self {
            Node::Node4(n4) => n4.child_mut(byte),
            Node::Node16(n16) => n16.child_mut(byte),
            Node::Node48(n48) => n48.child_mut(byte, clear_child_index),
            Node::Node256(n256) => n256.child_mut(byte),
            Node::None => unreachable!(),
            Node::Value(_) => unreachable!(),
        })
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

    fn node_iter<'a>(&'a self) -> NodeIter<'a, V> {
        let children: Box<dyn 'a + Iterator<Item = (u8, &Node<V>)>> = match self {
            Node::Node4(n4) => Box::new(n4.node_iter()),
            Node::Node16(n16) => Box::new(n16.node_iter()),
            Node::Node48(n48) => Box::new(n48.node_iter()),
            Node::Node256(n256) => Box::new(n256.node_iter()),

            // this is only an iterator over nodes, not leaf values
            Node::None => Box::new([].into_iter()),
            Node::Value(_) => Box::new([].into_iter()),
        };

        NodeIter {
            node: self,
            children
        }
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
        let (parent_opt, cursor) = self.slot_for_key(&key, true).unwrap();
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
        let (parent_opt, cursor) = self.slot_for_key(key, false)?;
        match std::mem::take(cursor) {
            Node::Value(old) => {
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
    fn slot_for_key(&mut self, key: &[u8; K], is_add: bool) -> Option<(Option<&mut u16>, &mut Node<V>)> {
        let mut parent: Option<&mut u16> = None;
        let mut path: &[u8] = &key[..];
        let mut cursor: &mut Node<V> = &mut self.root;
        println!("root is {:?}", cursor);

        while !path.is_empty() {
            println!("path: {:?} cursor {:?}", path, cursor);
            cursor.assert_size();
            if cursor.is_none() {
                if !is_add {
                    return None;
                }
                // we need to create intermediate nodes before
                // populating the value for this insert
                *cursor = Node::Node4(Box::new(Node4::default()));
                if let Some(children) = parent {
                    *children = children.checked_add(1).unwrap();
                }
                let prefix_len = 0; //(path.len() - 1).min(MAX_PREFIX);
                let prefix = &path[..prefix_len];
                cursor.header_mut().path[..prefix_len].copy_from_slice(prefix);
                cursor.header_mut().path_len = u8::try_from(prefix_len).unwrap();
                let (p, child) = cursor.child_mut(path[prefix_len], false).unwrap();
                parent = Some(p);
                cursor = child;
                path = &path[prefix_len + 1..];
                continue;
            }

            let prefix = cursor.prefix();
            let partial_path = &path[..path.len() - 1];
            if !partial_path.starts_with(prefix)  {
                // path compression needs to be reduced
                // to allow for this key, which does not
                // share the compressed path.
                println!("truncating cursor at {:?}", cursor);
                cursor.truncate_prefix(partial_path);
                println!("cursor is now after truncation {:?}", cursor);
                continue;
            }

            let next_byte = path[prefix.len()];
            path = &path[prefix.len() + 1..];

            //println!("cursor is now {:?}", cursor);
            let clear_child_index = !is_add && path.is_empty();
            let (p, next_cursor) = if let Some(opt) = cursor.child_mut(next_byte, clear_child_index) {
                opt
            } else {
                assert!(clear_child_index);
                return None;
            };
            cursor = next_cursor;
            parent = Some(p);
        }

        Some((parent, cursor))
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

    pub fn iter(&self) -> Iter<'_, V, K> {

        Iter {
            root: self.root.node_iter(),
            path: vec![],
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

#[test]
fn regression_00() {
    let mut art: Art<u8, 1> = Art::new();

    art.insert([37], 38);
    art.insert([0], 1);
    assert_eq!(art.len(), 2);

    art.insert([5], 5);
    art.insert([1], 9);
    art.insert([0], 0);
    art.insert([255], 255);
    art.insert([0], 0);
    art.insert([47], 0);
    art.insert([253], 37);
    assert_eq!(art.len(), 7);

    art.insert([10], 0,);
    art.insert([38], 28,);
    art.insert([24], 28,);
    assert_eq!(art.len(), 10);

    art.insert([28], 30,);
    art.insert([30], 30,);
    art.insert([28], 15,);
    art.insert([51], 48,);
    art.insert([53], 255,);
    art.insert([59], 58,);
    art.insert([58], 58,);
    assert_eq!(art.len(), 16);
    assert_eq!(art.remove(&[85]), None);
    assert_eq!(art.len(), 16);
    art.insert([30], 30,);
    art.insert([30], 0,);
    art.insert([30], 0,);
    assert_eq!(art.len(), 16);
    art.insert([143], 254,);
    assert_eq!(art.len(), 17);
    art.insert([30], 30,);
    assert_eq!(art.len(), 17);
    assert_eq!(art.len(), 17);
    println!("{:?}", art);
    assert_eq!(art.remove(&[85]), None);
    assert_eq!(art.len(), 17);
}

#[test]
fn regression_01() {
    let mut art: Art<u8, 3> = Art::new();

    assert_eq!(art.insert([0, 0, 0], 0), None);
    assert_eq!(art.insert([0, 11, 0], 1), None);
    assert_eq!(art.insert([0, 0, 0], 2), Some(0));

    assert_eq!(art.iter().collect::<Vec<_>>(), vec![
        ([0, 0, 0], &2),
        ([0, 11, 0], &1),
    ]);
}
