use std::ops::{Bound, RangeBounds};
use std::marker::PhantomData;
use std::mem::size_of;

#[derive(Debug, Clone)]
pub struct Art<V, const K: usize> {
    len: usize,
    root: Node<V>,
}

impl<V, const K: usize> Default for Art<V, K> {
    fn default() -> Art<V, K> {
        Art {
            len: 0,
            root: Node::none(),
        }
    }
}

impl<V: PartialEq, const K: usize> PartialEq for Art<V, K> {
    fn eq(&self, other: &Self) -> bool {
        if self.len() != other.len() {
            return false;
        }

        let mut other_iter = other.iter();

        for self_item in self.iter() {
            if let Some(other_item) = other_iter.next() {
                if self_item != other_item {
                    return false;
                }
            } else {
                return false;
            }
        }

        if other_iter.next().is_none() {
            true
        } else {
            false
        }
    }
}

impl<V: Eq, const K: usize> Eq for Art<V, K> {}

impl<V, const K: usize> Art<V, K> {
    pub const fn new() -> Art<V, K> {
        // TODO compiler error if K is above a range that
        // will cause stack overflow when the recursive prune
        // function is called
        Art {
            len: 0,
            root: Node::none(),
        }
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn insert(&mut self, key: [u8; K], mut value: V) -> Option<V> {
        let (parent_opt, cursor) = self.slot_for_key(&key, true).unwrap();
        match cursor.deref_mut() {
            NodeMut::Value(ref mut old) => {
                std::mem::swap(&mut **old, &mut value);
                Some(value)
            }
            NodeMut::None => {
                *cursor = Node::value(Box::new(value));
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

        match std::mem::take(cursor).take() {
            Some(old) => {
                if let Some(children) = parent_opt {
                    *children = children.checked_sub(1).unwrap();

                    if *children == 48 || *children == 16
                        || *children == 4 || *children == 0 {
                        self.prune(key);
                    }
                }
                self.len -= 1;
                Some(old)
            }
            None => None,
        }
    }

    // []
    //  don't do anything
    // [1]
    //  shrink without while loop
    // [1][2]
    //
    // [1:2]
    // [1:2][3]
    // [1][2:3]
    // [12:3][4]
    // [1:2][3:4]
    fn prune(&mut self, key: &[u8; K]) {
        self.root.prune(key);
    }

    // returns the optional parent node for child maintenance, and the value node
    fn slot_for_key(
        &mut self,
        key: &[u8; K],
        is_add: bool,
    ) -> Option<(Option<&mut u16>, &mut Node<V>)> {
        let mut parent: Option<&mut u16> = None;
        let mut path: &[u8] = &key[..];
        let mut cursor: &mut Node<V> = &mut self.root;
        // println!("root is {:?}", cursor);

        while !path.is_empty() {
            //println!("path: {:?} cursor {:?}", path, cursor);
            cursor.assert_size();
            if cursor.is_none() {
                if !is_add {
                    return None;
                }
                // we need to create intermediate nodes before
                // populating the value for this insert
                *cursor = Node::node4(Box::default());
                if let Some(children) = parent {
                    *children = children.checked_add(1).unwrap();
                }
                let prefix_len = (path.len() - 1).min(MAX_PATH_COMPRESSION_BYTES);
                let prefix = &path[..prefix_len];
                cursor.header_mut().path[..prefix_len].copy_from_slice(prefix);
                cursor.header_mut().path_len = u8::try_from(prefix_len).unwrap();
                let (p, child) = cursor.child_mut(path[prefix_len], is_add, false).unwrap();
                parent = Some(p);
                cursor = child;
                path = &path[prefix_len + 1..];
                continue;
            }

            let prefix = cursor.prefix();
            let partial_path = &path[..path.len() - 1];
            if !partial_path.starts_with(prefix) {
                if !is_add {
                    return None;
                }
                // path compression needs to be reduced
                // to allow for this key, which does not
                // share the compressed path.
                // println!("truncating cursor at {:?}", cursor);
                cursor.truncate_prefix(partial_path);
                // println!("cursor is now after truncation {:?}", cursor);
                continue;
            }

            let next_byte = path[prefix.len()];
            path = &path[prefix.len() + 1..];

            //println!("cursor is now {:?}", cursor);
            let clear_child_index = !is_add && path.is_empty();
            let (p, next_cursor) =
                if let Some(opt) = cursor.child_mut(next_byte, is_add, clear_child_index) {
                    opt
                } else {
                    assert!(!is_add);
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

        while !k.is_empty() {
            let prefix = cursor.prefix();

            if !k.starts_with(prefix) {
                return None;
            }

            cursor = cursor.child(k[prefix.len()])?;
            k = &k[prefix.len() + 1..];
        }

        match cursor.deref() {
            NodeRef::Value(ref v) => return Some(v),
            NodeRef::None => return None,
            _ => unreachable!(),
        }
    }

    pub fn iter(&self) -> Iter<'_, V, K> {
        self.range(..)
    }

    pub fn range<'a, R>(&'a self, range: R) -> Iter<'a, V, K>
    where
        R: RangeBounds<[u8; K]>,
    {
        Iter {
            root: self.root.node_iter(),
            path: vec![],
            rev_path: vec![],
            lower_bound: map_bound(range.start_bound(), |b| *b),
            upper_bound: map_bound(range.end_bound(), |b| *b),
            finished_0: false,
        }
    }
}

fn map_bound<T, U, F: FnOnce(T) -> U>(bound: Bound<T>, f: F) -> Bound<U> {
    match bound {
        Bound::Unbounded => Bound::Unbounded,
        Bound::Included(x) => Bound::Included(f(x)),
        Bound::Excluded(x) => Bound::Excluded(f(x)),
    }
}

#[cfg(target_pointer_width = "64")]
const fn _size_tests() {
    let _: [u8; 8] = [0; size_of::<Node<()>>()];
    let _: [u8; 16] = [0; size_of::<Header>()];
    // TODO see if we can get this to work, as
    // specified in the paper
    // let _: [u8; 52] = [0; size_of::<Node4<()>>()];
    let _: [u8; 56] = [0; size_of::<Node4<()>>()];
    let _: [u8; 160] = [0; size_of::<Node16<()>>()];
    let _: [u8; 656] = [0; size_of::<Node48<()>>()];
    let _: [u8; 2064] = [0; size_of::<Node256<()>>()];
}

const MAX_PATH_COMPRESSION_BYTES: usize = 13;

const NONE_HEADER: Header = Header {
    children: 0,
    path_len: 0,
    path: [0; MAX_PATH_COMPRESSION_BYTES],
};

const VALUE_HEADER: Header = Header {
    children: 1,
    path_len: 0,
    path: [0; MAX_PATH_COMPRESSION_BYTES],
};

#[must_use]
pub struct Iter<'a, V, const K: usize> {
    root: NodeIter<'a, V>,
    path: Vec<(u8, NodeIter<'a, V>)>,
    rev_path: Vec<(u8, NodeIter<'a, V>)>,
    lower_bound: Bound<[u8; K]>,
    upper_bound: Bound<[u8; K]>,
    finished_0: bool,
}

impl<'a, V, const K: usize> IntoIterator for &'a Art<V, K> {
    type IntoIter = Iter<'a, V, K>;
    type Item = ([u8; K], &'a V);

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a, V, const K: usize> Iter<'a, V, K> {
    fn char_bound(&self, is_forward: bool) -> (Bound<u8>, Bound<u8>) {
        let mut raw_path = [0_u8; K];
        let mut raw_len = 0_usize;

        let node_path = if is_forward {
            &self.path
        } else {
            &self.rev_path
        };

        let root_prefix = self.root.node.prefix();
        raw_path[0..root_prefix.len()].copy_from_slice(root_prefix);
        raw_len += root_prefix.len();

        for (byte, ancestor) in node_path {
            raw_path[raw_len] = *byte;
            raw_len += 1;
            let prefix = ancestor.node.prefix();
            raw_path[raw_len..raw_len + prefix.len()].copy_from_slice(prefix);
            raw_len += prefix.len();
        }

        let path = &raw_path[..raw_len];

        // included(12345)
        //   path
        //      122 => excluded(0)
        //      123 => included(4)
        //      124 => excluded(255)
        // excluded(12345)
        //   path
        //      122 => excluded(0)
        //      123 => excluded(4)
        //      124 => excluded(255)
        let lower = match self.lower_bound {
            Bound::Unbounded => Bound::Unbounded,
            Bound::Included(lower) => {
                if lower.starts_with(path) {
                    Bound::Included(lower[path.len()])
                } else if &lower[..path.len()] < path {
                    Bound::Unbounded
                } else {
                    Bound::Excluded(255)
                }
            }
            Bound::Excluded(lower) => {
                if lower.starts_with(path) {
                    if path.len() + 1 == K {
                        Bound::Excluded(lower[path.len()])
                    } else {
                        Bound::Included(lower[path.len()])
                    }
                } else if &lower[..path.len()] < path {
                    Bound::Unbounded
                } else {
                    Bound::Excluded(255)
                }
            }
        };

        let upper = match self.upper_bound {
            Bound::Unbounded => Bound::Unbounded,
            Bound::Included(upper) => {
                if upper.starts_with(path) {
                    Bound::Included(upper[path.len()])
                } else if &upper[..path.len()] > path {
                    Bound::Unbounded
                } else {
                    Bound::Excluded(0)
                }
            }
            Bound::Excluded(upper) => {
                if upper.starts_with(path) {
                    if path.len() + 1 == K {
                        Bound::Excluded(upper[path.len()])
                    } else {
                        Bound::Included(upper[path.len()])
                    }
                } else if &upper[..path.len()] > path {
                    Bound::Unbounded
                } else {
                    Bound::Excluded(0)
                }
            }
        };

        (lower, upper)
    }
}

impl<'a, V, const K: usize> Iterator for Iter<'a, V, K> {
    type Item = ([u8; K], &'a V);

    fn next(&mut self) -> Option<Self::Item> {
        if K == 0 {
            let k: [u8; K] = [0; K];
            let in_bounds = (self.lower_bound, self.upper_bound).contains(&k);
            let finished = self.finished_0;
            let can_return = in_bounds && !finished;
            self.finished_0 = true;
            match self.root.node.deref() {
                NodeRef::Value(ref v) if can_return => return Some(([0; K], v)),
                NodeRef::Value(_) | NodeRef::None => return None,
                _ => unreachable!(),
            }
        }

        // find next value, populating intermediate
        // iterators until we reach a leaf.
        let (vc, v) = loop {
            if let Some((_c, last)) = self.path.last_mut() {
                let child_opt = last.children.next();
                if child_opt.is_none() {
                    self.path.pop();
                    continue;
                }
                let (c, node) = child_opt.unwrap();
                let next_c_bound = self.char_bound(true);
                if !next_c_bound.contains(&c) {
                    continue;
                }
                match node.deref() {
                    NodeRef::Value(v) => break (c, v),
                    NodeRef::None => unreachable!(),
                    _other => {
                        let iter = node.node_iter();
                        self.path.push((c, iter))
                    }
                }
            } else {
                let (c, node) = self.root.children.next()?;
                let next_c_bound = self.char_bound(true);
                if !next_c_bound.contains(&c) {
                    continue;
                }

                match node.deref() {
                    NodeRef::Value(v) => break (c, v),
                    NodeRef::None => unreachable!(),
                    _other => {
                        let iter = node.node_iter();
                        self.path.push((c, iter))
                    }
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

impl<'a, V, const K: usize> DoubleEndedIterator for Iter<'a, V, K> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if K == 0 {
            let k: [u8; K] = [0; K];
            let in_bounds = (self.lower_bound, self.upper_bound).contains(&k);
            let finished = self.finished_0;
            let can_return = in_bounds && !finished;
            self.finished_0 = true;
            match self.root.node.deref() {
                NodeRef::Value(ref v) if can_return => return Some(([0; K], v)),
                NodeRef::Value(_) | NodeRef::None => return None,
                _ => unreachable!(),
            }
        }

        // find next value, populating intermediate
        // iterators until we reach a leaf.
        let (vc, v) = loop {
            if let Some((_c, last)) = self.rev_path.last_mut() {
                let child_opt = last.children.next_back();
                if child_opt.is_none() {
                    self.rev_path.pop();
                    continue;
                }
                let (c, node) = child_opt.unwrap();
                let next_c_bound = self.char_bound(false);
                if !next_c_bound.contains(&c) {
                    continue;
                }

                match node.deref() {
                    NodeRef::Value(v) => break (c, v),
                    NodeRef::None => unreachable!(),
                    _other => {
                        let iter = node.node_iter();
                        self.rev_path.push((c, iter))
                    }
                }
            } else {
                let (c, node) = self.root.children.next_back()?;
                let next_c_bound = self.char_bound(false);
                if !next_c_bound.contains(&c) {
                    continue;
                }

                match node.deref() {
                    NodeRef::Value(v) => break (c, v),
                    NodeRef::None => unreachable!(),
                    _other => {
                        let iter = node.node_iter();
                        self.rev_path.push((c, iter))
                    }
                }
            }
        };

        let mut k = [0; K];
        let mut written = 0;
        let root_prefix = self.root.node.prefix();
        k[..root_prefix.len()].copy_from_slice(root_prefix);
        written += root_prefix.len();

        for (c, node_iter) in &self.rev_path {
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
    children: Box<dyn 'a + DoubleEndedIterator<Item = (u8, &'a Node<V>)>>,
}

#[derive(Debug, Default, Clone, Copy)]
struct Header {
    children: u16,
    path: [u8; MAX_PATH_COMPRESSION_BYTES],
    path_len: u8,
}

// TODO correctly implement Debug and Clone
#[derive(Debug, Clone)]
struct Node<V>(usize, PhantomData<V>);

impl<V> Drop for Node<V> {
    fn drop(&mut self) {
        match self.0 & TAG_MASK {
            TAG_NONE => {},
            TAG_VALUE => {
                let ptr: *mut V = (self.0 & PTR_MASK) as *mut V;
                drop(unsafe {
                    Box::from_raw(ptr)
                });
            }
            TAG_4 => {
                let ptr: *mut Node4<V> = (self.0 & PTR_MASK) as *mut Node4<V>;
                drop(unsafe {
                    Box::from_raw(ptr)
                });
            }
            TAG_16 => {
                let ptr: *mut Node16<V> = (self.0 & PTR_MASK) as *mut Node16<V>;
                drop(unsafe {
                    Box::from_raw(ptr)
                });
            }
            TAG_48 => {
                let ptr: *mut Node48<V> = (self.0 & PTR_MASK) as *mut Node48<V>;
                drop(unsafe {
                    Box::from_raw(ptr)
                });
            }
            TAG_256 => {
                let ptr: *mut Node256<V> = (self.0 & PTR_MASK) as *mut Node256<V>;
                drop(unsafe {
                    Box::from_raw(ptr)
                });
            }
            _ => unreachable!(),
        }
    }
}

const TAG_NONE: usize = 0b000;
const TAG_VALUE: usize = 0b001;
const TAG_4: usize = 0b010;
const TAG_16: usize = 0b011;
const TAG_48: usize = 0b100;
const TAG_256: usize = 0b101;
const TAG_MASK: usize = 0b111;
const PTR_MASK: usize = usize::MAX - TAG_MASK;

enum NodeRef<'a, V> {
    None,
    Value(&'a V),
    Node4(&'a Node4<V>),
    Node16(&'a Node16<V>),
    Node48(&'a Node48<V>),
    Node256(&'a Node256<V>),
}

enum NodeMut<'a, V> {
    None,
    Value(&'a mut V),
    Node4(&'a mut Node4<V>),
    Node16(&'a mut Node16<V>),
    Node48(&'a mut Node48<V>),
    Node256(&'a mut Node256<V>),
}

impl<V> Default for Node<V> {
    fn default() -> Node<V> {
        Node::none()
    }
}

impl<V> Node<V> {
    const fn none() -> Node<V> {
        Node(TAG_NONE, PhantomData)
    }

    fn node4(n4: Box<Node4<V>>) -> Node<V> {
        let ptr: *mut Node4<V> = Box::into_raw(n4);
        let us = ptr as usize;
        assert_eq!(us & TAG_4, 0);
        Node(us | TAG_4, PhantomData)
    }

    fn node16(n16: Box<Node16<V>>) -> Node<V> {
        let ptr: *mut Node16<V> = Box::into_raw(n16);
        let us = ptr as usize;
        assert_eq!(us & TAG_16, 0);
        Node(us | TAG_16, PhantomData)
    }

    fn node48(n48: Box<Node48<V>>) -> Node<V> {
        let ptr: *mut Node48<V> = Box::into_raw(n48);
        let us = ptr as usize;
        assert_eq!(us & TAG_48, 0);
        Node(us | TAG_48, PhantomData)
    }

    fn node256(n256: Box<Node256<V>>) -> Node<V> {
        let ptr: *mut Node256<V> = Box::into_raw(n256);
        let us = ptr as usize;
        assert_eq!(us & TAG_256, 0);
        Node(us | TAG_256, PhantomData)
    }

    fn value(value: Box<V>) -> Node<V> {
        let ptr: *mut V = Box::into_raw(value);
        let us = ptr as usize;
        if size_of::<V>() > 0 {
            assert_eq!(us & TAG_VALUE, 0);
        } else {
            assert_eq!(ptr, std::ptr::NonNull::dangling().as_ptr());
        }
        Node(us | TAG_VALUE, PhantomData)
    }

    fn take(&mut self) -> Option<V> {
        let us = self.0;
        self.0 = 0;

        match us & TAG_MASK {
            TAG_NONE => None,
            TAG_VALUE => {
                let ptr: *mut V = if size_of::<V>() > 0 {
                    (us & PTR_MASK) as *mut V
                } else {
                    std::ptr::NonNull::dangling().as_ptr()
                };
                let boxed: Box<V> = unsafe {
                    Box::from_raw(ptr)
                };
                Some(*boxed)
            }
            _ => unreachable!(),
        }
    }

    fn deref(&self) -> NodeRef<'_, V> {
        match self.0 & TAG_MASK {
            TAG_NONE => NodeRef::None,
            TAG_VALUE => {
                let ptr: *const V = if size_of::<V>() > 0 {
                    (self.0 & PTR_MASK) as *const V
                } else {
                    std::ptr::NonNull::dangling().as_ptr()
                };
                let reference: &V = unsafe {
                    &*ptr
                };
                NodeRef::Value(reference)
            }
            TAG_4 => {
                let ptr: *const Node4<V> = (self.0 & PTR_MASK) as *const Node4<V>;
                let reference: &Node4<V> = unsafe {
                    &*ptr
                };
                NodeRef::Node4(reference)
            }
            TAG_16 => {
                let ptr: *const Node16<V> = (self.0 & PTR_MASK) as *const Node16<V>;
                let reference: &Node16<V> = unsafe {
                    &*ptr
                };
                NodeRef::Node16(reference)
            }
            TAG_48 => {
                let ptr: *const Node48<V> = (self.0 & PTR_MASK) as *const Node48<V>;
                let reference: &Node48<V> = unsafe {
                    &*ptr
                };
                NodeRef::Node48(reference)
            }
            TAG_256 => {
                let ptr: *const Node256<V> = (self.0 & PTR_MASK) as *const Node256<V>;
                let reference: &Node256<V> = unsafe {
                    &*ptr
                };
                NodeRef::Node256(reference)
            }
            _ => unreachable!(),
        }
    }

    fn deref_mut(&mut self) -> NodeMut<'_, V> {
        match self.0 & TAG_MASK {
            TAG_NONE => NodeMut::None,
            TAG_VALUE => {
                let ptr: *mut V = if size_of::<V>() > 0 {
                    (self.0 & PTR_MASK) as *mut V
                } else {
                    std::ptr::NonNull::dangling().as_ptr()
                };
                let reference: &mut V = unsafe {
                    &mut *ptr
                };
                NodeMut::Value(reference)
            }
            TAG_4 => {
                let ptr: *mut Node4<V> = (self.0 & PTR_MASK) as *mut Node4<V>;
                let reference: &mut Node4<V> = unsafe {
                    &mut *ptr
                };
                NodeMut::Node4(reference)
            }
            TAG_16 => {
                let ptr: *mut Node16<V> = (self.0 & PTR_MASK) as *mut Node16<V>;
                let reference: &mut Node16<V> = unsafe {
                    &mut *ptr
                };
                NodeMut::Node16(reference)
            }
            TAG_48 => {
                let ptr: *mut Node48<V> = (self.0 & PTR_MASK) as *mut Node48<V>;
                let reference: &mut Node48<V> = unsafe {
                    &mut *ptr
                };
                NodeMut::Node48(reference)
            }
            TAG_256 => {
                let ptr: *mut Node256<V> = (self.0 & PTR_MASK) as *mut Node256<V>;
                let reference: &mut Node256<V> = unsafe {
                    &mut *ptr
                };
                NodeMut::Node256(reference)
            }
            _ => unreachable!(),
        }
    }

    // returns true if this node went from Node4 to None
    fn prune(&mut self, partial_path: &[u8]) -> bool {
        let prefix = self.prefix();

        assert!(partial_path.starts_with(prefix));

        if partial_path.len() > prefix.len() + 1 {
            let byte = partial_path[prefix.len()];
            let subpath = &partial_path[prefix.len() + 1..];

            let (_, child) = self.child_mut(byte, false, false).expect(
                "prune may only be called with \
                freshly removed keys with a full \
                ancestor chain still in-place."
            );

            let child_shrunk = child.prune(subpath);
            if child_shrunk {
                let children: &mut u16 = &mut self.header_mut().children;
                *children = children.checked_sub(1).unwrap();

                if let NodeMut::Node48(n48) = self.deref_mut() {
                    n48.child_index[byte as usize] = 255;
                }
            }
        }

        self.shrink_to_fit()
    }

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

        // println!("truncated node has path of len {} with a reduction of {}", shared_bytes, prefix.len() - shared_bytes);
        let mut new_node4: Box<Node4<V>> = Box::default();
        new_node4.header.path[..shared_bytes].copy_from_slice(&prefix[..shared_bytes]);
        new_node4.header.path_len = u8::try_from(shared_bytes).unwrap();

        let new_node = Node::node4(new_node4);

        assert!(prefix.starts_with(new_node.prefix()));

        let mut old_cursor = std::mem::replace(self, new_node);

        let old_cursor_header = old_cursor.header_mut();
        let old_cursor_new_child_byte = old_cursor_header.path[shared_bytes];

        // we add +1 because we must account for the extra byte
        // reduced from the node's fan-out itself.
        old_cursor_header.path.rotate_left(shared_bytes + 1);
        old_cursor_header.path_len = old_cursor_header
            .path_len
            .checked_sub(u8::try_from(shared_bytes + 1).unwrap())
            .unwrap();

        let (_, child) = self
            .child_mut(old_cursor_new_child_byte, true, false)
            .unwrap();
        *child = old_cursor;
        child.assert_size();

        self.header_mut().children = 1;
    }

    #[inline]
    fn is_none(&self) -> bool {
        self.0 == TAG_NONE
    }

    fn assert_size(&self) {
        debug_assert_eq!(
            {
                let slots: &[Node<V>] = match self.deref() {
                    NodeRef::Node4(n4) => &n4.slots,
                    NodeRef::Node16(n16) => &n16.slots,
                    NodeRef::Node48(n48) => &n48.slots,
                    NodeRef::Node256(n256) => &n256.slots,
                    _ => &[],
                };
                slots.iter().filter(|s| !s.is_none()).count()
            },
            self.len(),
        )
    }

    fn is_full(&self) -> bool {
        match self.deref() {
            NodeRef::Node4(_) => 4 == self.len(),
            NodeRef::Node16(_) => 16 == self.len(),
            NodeRef::Node48(_) => 48 == self.len(),
            NodeRef::Node256(_) => 256 == self.len(),
            _ => unreachable!(),
        }
    }

    fn len(&self) -> usize {
        self.header().children as usize
    }

    fn header(&self) -> &Header {
        match self.deref() {
            NodeRef::Node4(n4) => &n4.header,
            NodeRef::Node16(n16) => &n16.header,
            NodeRef::Node48(n48) => &n48.header,
            NodeRef::Node256(n256) => &n256.header,
            NodeRef::None => &NONE_HEADER,
            NodeRef::Value(_) => &VALUE_HEADER,
        }
    }

    fn header_mut(&mut self) -> &mut Header {
        match self.deref_mut() {
            NodeMut::Node4(n4) => &mut n4.header,
            NodeMut::Node16(n16) => &mut n16.header,
            NodeMut::Node48(n48) => &mut n48.header,
            NodeMut::Node256(n256) => &mut n256.header,
            _ => unreachable!(),
        }
    }

    fn prefix(&self) -> &[u8] {
        let header = self.header();
        &header.path[..header.path_len as usize]
    }

    fn child(&self, byte: u8) -> Option<&Node<V>> {
        match self.deref() {
            NodeRef::Node4(n4) => n4.child(byte),
            NodeRef::Node16(n16) => n16.child(byte),
            NodeRef::Node48(n48) => n48.child(byte),
            NodeRef::Node256(n256) => n256.child(byte),
            NodeRef::None => None,
            NodeRef::Value(_) => unreachable!(),
        }
    }

    fn child_mut(
        &mut self,
        byte: u8,
        is_add: bool,
        clear_child_index: bool,
    ) -> Option<(&mut u16, &mut Node<V>)> {
        // TODO this is gross
        if self.child(byte).is_none() {
            if !is_add {
                return None;
            }
            if self.is_full() {
                self.upgrade()
            }
        }

        Some(match self.deref_mut() {
            NodeMut::Node4(n4) => n4.child_mut(byte),
            NodeMut::Node16(n16) => n16.child_mut(byte),
            NodeMut::Node48(n48) => n48.child_mut(byte, clear_child_index),
            NodeMut::Node256(n256) => n256.child_mut(byte),
            NodeMut::None => unreachable!(),
            NodeMut::Value(_) => unreachable!(),
        })
    }

    fn should_shrink(&self) -> bool {
        match (self.deref(), self.len()) {
            (NodeRef::Node4(_), 0) |
            (NodeRef::Node16(_), 4) |
            (NodeRef::Node48(_), 16) |
            (NodeRef::Node256(_), 48) => true,
            (_, _) => false,
        }
    }

    fn shrink_to_fit(&mut self) -> bool {
        if !self.should_shrink() {
            return false;
        }

        let old_header = *self.header();
        let children = old_header.children;

        let mut dropped = false;
        let mut swapped = std::mem::take(self);

        *self = match (swapped.deref_mut(), children) {
            (NodeMut::Node4(_), 0) => {
                dropped = true;
                Node::none()
            },
            (NodeMut::Node16(n16), 4) => Node::node4(n16.downgrade()),
            (NodeMut::Node48(n48), 16) => Node::node16(n48.downgrade()),
            (NodeMut::Node256(n256), 48) => Node::node48(n256.downgrade()),
            (_, _) => unreachable!(),
        };

        if !dropped {
            *self.header_mut() = old_header;
        }

        dropped
    }

    fn upgrade(&mut self) {
        let old_header = *self.header();
        let mut swapped = std::mem::take(self);
        *self = match swapped.deref_mut() {
            NodeMut::Node4(n4) => Node::node16(n4.upgrade()),
            NodeMut::Node16(n16) => Node::node48(n16.upgrade()),
            NodeMut::Node48(n48) => Node::node256(n48.upgrade()),
            NodeMut::Node256(_) => unreachable!(),
            NodeMut::None => unreachable!(),
            NodeMut::Value(_) => unreachable!(),
        };
        *self.header_mut() = old_header;
    }

    fn node_iter<'a>(&'a self) -> NodeIter<'a, V> {
        let children: Box<dyn 'a + DoubleEndedIterator<Item = (u8, &'a Node<V>)>> = match self.deref() {
            NodeRef::Node4(n4) => Box::new(n4.iter()),
            NodeRef::Node16(n16) => Box::new(n16.iter()),
            NodeRef::Node48(n48) => Box::new(n48.iter()),
            NodeRef::Node256(n256) => Box::new(n256.iter()),

            // this is only an iterator over nodes, not leaf values
            NodeRef::None => Box::new([].into_iter()),
            NodeRef::Value(_) => Box::new([].into_iter()),
        };

        NodeIter {
            node: self,
            children,
        }
    }
}

#[derive(Debug, Clone)]
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
            slots: [Node::none(), Node::none(), Node::none(), Node::none()],
        }
    }
}

impl<V> Node4<V> {
    fn iter<'a>(&'a self) -> impl DoubleEndedIterator<Item = (u8, &Node<V>)> {
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

    fn upgrade(&mut self) -> Box<Node16<V>> {
        let mut n16: Box<Node16<V>> = Box::default();
        for (slot, byte) in self.keys.iter().enumerate() {
            std::mem::swap(&mut self.slots[slot], &mut n16.slots[slot]);
            n16.keys[slot] = *byte;
        }
        n16
    }
}

#[derive(Debug, Clone)]
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
            slots: [
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
            ],
        }
    }
}

impl<V> Node16<V> {
    fn iter<'a>(&'a self) -> impl DoubleEndedIterator<Item = (u8, &Node<V>)> {
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

    fn upgrade(&mut self) -> Box<Node48<V>> {
        let mut n48: Box<Node48<V>> = Box::default();
        for (slot, byte) in self.keys.iter().enumerate() {
            if !self.slots[slot].is_none() {
                std::mem::swap(&mut self.slots[slot], &mut n48.slots[slot]);
                assert_eq!(n48.child_index[*byte as usize], 255);
                n48.child_index[*byte as usize] = u8::try_from(slot).unwrap();
            }
        }
        n48
    }

    fn downgrade(&mut self) -> Box<Node4<V>> {
        let mut n4: Box<Node4<V>> = Box::default();
        let mut dst_idx = 0;

        for (slot, byte) in self.keys.iter().enumerate() {
            if !self.slots[slot].is_none() {
                std::mem::swap(&mut self.slots[slot], &mut n4.slots[dst_idx]);
                n4.keys[dst_idx] = *byte;
                dst_idx += 1;
            }
        }

        assert_eq!(dst_idx, 4);

        n4
    }
}

#[derive(Debug, Clone)]
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
            slots: [
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
            ],
        }
    }
}

impl<V> Node48<V> {
    fn iter<'a>(&'a self) -> impl DoubleEndedIterator<Item = (u8, &Node<V>)> {
        self.child_index
            .iter()
            .enumerate()
            .filter(|(_, i)| **i != 255 && !self.slots[**i as usize].is_none())
            .map(|(c, i)| (u8::try_from(c).unwrap(), &self.slots[*i as usize]))
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

    fn upgrade(&mut self) -> Box<Node256<V>> {
        let mut n256: Box<Node256<V>> = Box::default();

        for (byte, idx) in self.child_index.iter().enumerate() {
            if *idx != 255 {
                assert!(!self.slots[*idx as usize].is_none());
                std::mem::swap(&mut n256.slots[byte], &mut self.slots[*idx as usize]);
            }
        }

        n256
    }

    fn downgrade(&mut self) -> Box<Node16<V>> {
        let mut n16: Box<Node16<V>> = Box::default();
        let mut dst_idx = 0;

        for (byte, idx) in self.child_index.iter().enumerate() {
            if *idx != 255 {
                assert!(!self.slots[*idx as usize].is_none());
                std::mem::swap(&mut self.slots[*idx as usize], &mut n16.slots[dst_idx]);
                n16.keys[dst_idx] = u8::try_from(byte).unwrap();
                dst_idx += 1;
            }
        }

        assert_eq!(dst_idx, 16);

        n16
    }
}

#[derive(Debug, Clone)]
struct Node256<V> {
    header: Header,
    slots: [Node<V>; 256],
}

impl<V> Node256<V> {
    fn iter<'a>(&'a self) -> impl DoubleEndedIterator<Item = (u8, &Node<V>)> {
        self.slots
            .iter()
            .enumerate()
            .filter(move |(_, slot)| !slot.is_none())
            .map(|(c, slot)| (u8::try_from(c).unwrap(), slot))
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

    fn downgrade(&mut self) -> Box<Node48<V>> {
        let mut n48: Box<Node48<V>> = Box::default();
        let mut dst_idx = 0;

        for (byte, slot) in self.slots.iter_mut().enumerate() {
            if !slot.is_none() {
                std::mem::swap(slot, &mut n48.slots[dst_idx]);
                n48.child_index[byte] = u8::try_from(dst_idx).unwrap();
                dst_idx += 1;
            }
        }

        assert_eq!(dst_idx, 48);

        n48
    }
}

impl<V> Default for Node256<V> {
    fn default() -> Node256<V> {
        Node256 {
            header: Default::default(),
            slots: [
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
                Node::none(), Node::none(), Node::none(), Node::none(),
            ],
        }
    }
}

#[test]
fn test_inserts() {
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

    let mut art: Art<&str, 2> = Art::new();
    assert_eq!(art.get(&[255, 255]), None);
    assert_eq!(art.insert([20, 20], "k 0 v 1"), None);
    assert_eq!(art.insert([20, 192], "k 1 v 1"), None);
    assert_eq!(art.insert([20, 20], "k 0 v 2"), Some("k 0 v 1"));
    assert_eq!(art.insert([20, 192], "k 1 v 2"), Some("k 1 v 1"));
    assert_eq!(art.insert([20, 20], "k 0 v 3"), Some("k 0 v 2"));
    assert_eq!(art.insert([20, 192], "k 1 v 3"), Some("k 1 v 2"));
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

    art.insert([10], 0);
    art.insert([38], 28);
    art.insert([24], 28);
    assert_eq!(art.len(), 10);

    art.insert([28], 30);
    art.insert([30], 30);
    art.insert([28], 15);
    art.insert([51], 48);
    art.insert([53], 255);
    art.insert([59], 58);
    art.insert([58], 58);
    assert_eq!(art.len(), 16);
    assert_eq!(art.remove(&[85]), None);
    assert_eq!(art.len(), 16);
    art.insert([30], 30);
    art.insert([30], 0);
    art.insert([30], 0);
    assert_eq!(art.len(), 16);
    art.insert([143], 254);
    assert_eq!(art.len(), 17);
    art.insert([30], 30);
    assert_eq!(art.len(), 17);
    assert_eq!(art.len(), 17);
    assert_eq!(art.remove(&[85]), None);
    assert_eq!(art.len(), 17);
}

#[test]
fn regression_01() {
    let mut art: Art<u8, 3> = Art::new();

    assert_eq!(art.insert([0, 0, 0], 0), None);
    assert_eq!(art.insert([0, 11, 0], 1), None);
    assert_eq!(art.insert([0, 0, 0], 2), Some(0));

    assert_eq!(
        art.iter().collect::<Vec<_>>(),
        vec![([0, 0, 0], &2), ([0, 11, 0], &1),]
    );
}

#[test]
fn regression_02() {
    let mut art = Art::new();
    art.insert([1, 1, 1], 1);
    art.remove(&[2, 2, 2]);
    art.insert([0, 0, 0], 5);
    assert_eq!(
        art.iter().collect::<Vec<_>>(),
        vec![([0, 0, 0], &5), ([1, 1, 1], &1),]
    );
}

#[test]
fn regression_03() {
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

    let mut art = Art::new();
    art.insert(expand([1, 173, 33, 255]), 255);
    art.insert(expand([255, 20, 255, 223]), 223);

    let start = expand([223, 223, 223, 223]);
    let end = expand([255, 255, 255, 255]);
    let v = art.range(start..end).count();
    assert_eq!(v, 1);
}

#[test]
fn regression_04() {
    let mut art = Art::new();

    art.insert([], 0);

    assert_eq!(art.get(&[]), Some(&0));
    assert_eq!(art.remove(&[]), Some(0));
    assert_eq!(art.get(&[]), None);

    art.insert([], 3);

    assert_eq!(art.iter().count(), 1);
}

#[test]
fn regression_05() {
    let mut art = Art::new();

    let k = [0; 2];
    art.insert(k, 0);
    art.remove(&k);

    assert!(art.root.is_none());
}

#[test]
fn regression_06() {
    let mut art = Art::new();

    let max = u16::MAX as u32 + 1;

    for i in 0..max {
        let k = i.to_be_bytes();
        art.insert(k, 0);
    }

    for i in 0..max {
        let k = i.to_be_bytes();
        art.remove(&k);
    }

    assert!(art.root.is_none());
}
