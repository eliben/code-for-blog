pub struct Tree {
    count: usize,
    root: *mut Node,
}

#[derive(Debug)]
struct Node {
    data: i32,

    // Null pointer means "None" here; right.is_null() ==> no right child, etc.
    left: *mut Node,
    right: *mut Node,
    parent: *mut Node,
}

impl Tree {
    pub fn new() -> Self {
        Self {
            count: 0,
            root: std::ptr::null_mut(),
        }
    }

    /// Returns the node count of the tree.
    pub fn node_count(&self) -> usize {
        assert!(self.count != 0 || self.root.is_null());
        self.count
    }

    /// Insert a new item into the tree; returns `true` if the insertion
    /// happened, and `false` if the given data was already present in the
    /// tree.
    pub fn insert(&mut self, data: i32) -> bool {
        if self.root.is_null() {
            self.root = Node::new(data);
        } else {
            if !insert_node(self.root, data) {
                return false;
            }
        }

        self.count += 1;
        true
    }

    /// Find the item in the tree; returns `true` iff the item is found.
    pub fn find(&self, data: i32) -> bool {
        !find_node(self.root, data).is_null()
    }

    /// Returns a string representation of the tree for debugging.
    pub fn display(&self) -> String {
        display_node(self.root, 0)
    }

    /// Returns all data of the tree, visited inorder.
    pub fn inorder(&self) -> Vec<i32> {
        let mut v = vec![];
        if !self.root.is_null() {
            let mut node = leftmost_child(self.root);
            loop {
                if node.is_null() {
                    break;
                }
                unsafe {
                    v.push((*node).data);
                }
                node = successor_of_node(node);
            }
        }
        v
    }

    /// Remove the given item from the tree; returns `true` if such a node was
    /// found and removed, `false` otherwise.
    pub fn remove(&mut self, data: i32) -> bool {
        let node = find_node(self.root, data);
        if node.is_null() {
            false
        } else {
            self.remove_node(node);
            self.count -= 1;
            true
        }
    }

    /// Find the successor of the given item in the tree; the successor is the
    /// next item in an inorder traversal.
    pub fn successor(&self, data: i32) -> Option<i32> {
        unsafe {
            let node = find_node(self.root, data);
            if !node.is_null() {
                let nodesucc = successor_of_node(node);
                if !nodesucc.is_null() {
                    return Some((*nodesucc).data);
                }
            }
            None
        }
    }

    // Remove the given node from the tree.
    fn remove_node(&mut self, node: *mut Node) {
        unsafe {
            let lchild = (*node).left;
            let rchild = (*node).right;
            if lchild.is_null() && rchild.is_null() {
                // Node has no children, so it's safe to dispose.
                self.replace_node(node, std::ptr::null_mut());
            } else if !lchild.is_null() && !rchild.is_null() {
                // Node has both children.
                // We find the successor of this node, replace our node's data
                // its data and then recursively remove the successor.
                let succ = successor_of_node(node);
                assert!(!succ.is_null());
                (*node).data = (*succ).data;
                self.remove_node(succ);
            } else if !lchild.is_null() {
                // Node has only left child, so replace it with its only child.
                self.replace_node(node, lchild);
            } else if !rchild.is_null() {
                // Node has only right child, so replace it with its only child.
                self.replace_node(node, rchild);
            } else {
                panic!("unreachable");
            }
        }
    }

    // Replaces `node` with `r` in the tree, by setting `node`'s parent's
    // left/right link to `node` with a link to `r`, and setting `r`'s parent
    // link to the `node`'s parent. `node` cannot be null.
    fn replace_node(&mut self, node: *mut Node, r: *mut Node) {
        unsafe {
            let parent = (*node).parent;
            if parent.is_null() {
                // Removing the root node.
                self.root = r;
                if !r.is_null() {
                    (*r).parent = std::ptr::null_mut();
                }
            } else {
                if !r.is_null() {
                    (*r).parent = parent;
                }
                if (*parent).left == node {
                    (*parent).left = r;
                } else if (*parent).right == node {
                    (*parent).right = r;
                }
            }
            // node is unused now, so we can deallocate it by assigning it to
            // an owning Box that will be automatically dropped.
            Box::from_raw(node);
        }
    }
}

impl Drop for Tree {
    fn drop(&mut self) {
        // Probably not the most efficient way to destroy the whole tree, but
        // it's simple and it works :)
        while !self.root.is_null() {
            self.remove_node(self.root);
        }
    }
}

impl Node {
    fn new(data: i32) -> *mut Self {
        Box::into_raw(Box::new(Self {
            data,
            left: std::ptr::null_mut(),
            right: std::ptr::null_mut(),
            parent: std::ptr::null_mut(),
        }))
    }

    fn new_with_parent(data: i32, parent: *mut Node) -> *mut Self {
        Box::into_raw(Box::new(Self {
            data,
            left: std::ptr::null_mut(),
            right: std::ptr::null_mut(),
            parent,
        }))
    }
}

// Inserts `data` into a new node at the `node` subtree.
fn insert_node(node: *mut Node, data: i32) -> bool {
    unsafe {
        if (*node).data == data {
            false
        } else if data < (*node).data {
            if (*node).left.is_null() {
                (*node).left = Node::new_with_parent(data, node);
                true
            } else {
                insert_node((*node).left, data)
            }
        } else {
            if (*node).right.is_null() {
                (*node).right = Node::new_with_parent(data, node);
                true
            } else {
                insert_node((*node).right, data)
            }
        }
    }
}

// Finds `data` in the `fromnode` subtree. Returns a null pointer if not found.
fn find_node(fromnode: *mut Node, data: i32) -> *mut Node {
    unsafe {
        if fromnode.is_null() || (*fromnode).data == data {
            fromnode
        } else if data < (*fromnode).data {
            find_node((*fromnode).left, data)
        } else {
            find_node((*fromnode).right, data)
        }
    }
}

// Returns a string representation of the `node` subtree, with an initial
// indentation level.
fn display_node(node: *const Node, indent: usize) -> String {
    let indent_str = " ".repeat(indent);
    if node.is_null() {
        indent_str + ".\n"
    } else {
        unsafe {
            let mut s = format!("{}{}\n", indent_str, (*node).data);
            s.push_str(&display_node((*node).left, indent + 2));
            s.push_str(&display_node((*node).right, indent + 2));
            s
        }
    }
}

// Find the leftmost child of `node`, or `node` itself in case it has no
// left child. `node` cannot be null.
fn leftmost_child(node: *mut Node) -> *mut Node {
    unsafe {
        if (*node).left.is_null() {
            node
        } else {
            leftmost_child((*node).left)
        }
    }
}

// Find the successor of `node` in the tree.
fn successor_of_node(node: *mut Node) -> *mut Node {
    unsafe {
        if !(*node).right.is_null() {
            // Case 1: node has a right child; then the successor is the
            // leftmost child of this right child (or the right child itself, if
            // it has no left children).
            leftmost_child((*node).right)
        } else {
            // Case 2: no right child; then climb the parent links to find a
            // node we're the left child of. Failing to find such a parent
            // before reaching the root means there's no successor.
            parent_with_left(node)
        }
    }
}

// Find a parent in `node`'s ancestor chain that is reached through its left
// child.
fn parent_with_left(node: *mut Node) -> *mut Node {
    unsafe {
        // If this node has a parent, and this parent has a left child, and
        // `node` is that left child, we found it!
        let parent = (*node).parent;
        if !parent.is_null() {
            if std::ptr::eq((*parent).left, node) {
                return parent;
            }
            return parent_with_left(parent);
        }

        // This node has no parent, so we've reached the root without
        // finding what we're after.
        std::ptr::null_mut()
    }
}
