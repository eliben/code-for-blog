//     !!!!!!!! NOTE: this is not a fully working version !!!!!!!!
//
// REALLY hard to make removal work here, wrangling all the mut references,
// and a bunch of the non-mut code has to be duplicated (finding a node,
// finding a successor in order to remove a node with two children, etc.)
//
// So removal doesn't work right now.

pub struct Tree {
    count: usize,
    root: Option<Box<Node>>,
}

#[derive(Debug)]
struct Node {
    data: i32,
    left: Option<Box<Node>>,
    right: Option<Box<Node>>,
    parent: *mut Node,
}

impl Tree {
    pub fn new() -> Self {
        Self {
            count: 0,
            root: None,
        }
    }

    /// Returns the node count of the tree.
    pub fn node_count(&self) -> usize {
        assert!(self.count != 0 || self.root.is_none());
        self.count
    }

    /// Insert a new item into the tree; returns `true` if the insertion
    /// happened, and `false` if the given data was already present in the
    /// tree.
    pub fn insert(&mut self, data: i32) -> bool {
        if let Some(root) = &mut self.root {
            if !root.insert(data) {
                return false;
            }
        } else {
            self.root = Some(Node::new(data));
        }
        self.count += 1;
        true
    }

    /// Find the item in the tree; returns `true` iff the item is found.
    pub fn find(&self, data: i32) -> bool {
        self.root
            .as_ref()
            .map_or(false, |root| self.find_node(root.as_ref(), data).is_some())
    }

    // Find a node with the given item starting from `fromnode`.
    // A lifetime specification here is required to tell the borrow checker
    // that the lifetimes of `fromnode` and `self` are similar.
    fn find_node<'a>(&'a self, fromnode: &'a Node, data: i32) -> Option<&'a Node> {
        if fromnode.data == data {
            Some(fromnode)
        } else if data < fromnode.data {
            fromnode
                .left
                .as_ref()
                .and_then(|lnode| self.find_node(lnode.as_ref(), data))
        } else {
            fromnode
                .right
                .as_ref()
                .and_then(|rnode| self.find_node(rnode.as_ref(), data))
        }
    }

    /// Find the successor of the given item in the tree; the successor is the
    /// next item in an inorder traversal.
    pub fn successor(&self, data: i32) -> Option<i32> {
        if let Some(root) = &self.root {
            if let Some(node) = self.find_node(root, data) {
                return self.successor_of_node(node).map(|succ| succ.data);
            }
        }
        None
    }

    // Find the successor of `node` in the tree.
    fn successor_of_node<'a>(&'a self, node: &'a Node) -> Option<&'a Node> {
        if let Some(rchild) = &node.right {
            // Case 1: node has a right child; then the successor is the leftmost
            // child of this right child (or the right child itself, if it has no
            // left children).
            Some(self.leftmost_child(rchild))
        } else {
            // Case 2: no right child; then climb the parent links to find a node
            // we're the left child of. Failing to find such a parent before
            // reaching the root means there's no successor.
            self.parent_with_left(node)
        }
    }

    // Find the leftmost child of `node`, or `node` itself in case it has no
    // left child.
    fn leftmost_child<'a>(&'a self, node: &'a Node) -> &'a Node {
        if let Some(lchild) = &node.left {
            self.leftmost_child(lchild)
        } else {
            node
        }
    }

    // Find a parent in `node`'s ancestor chain that is reached through its
    // left child. For example, in the tree:
    //
    //         9
    //        / \
    //       4   11
    //      /
    //     2
    //      \
    //       3
    //
    // if `node` is 3, this will find 4, because 2 is reached through its right
    // child in the chain. If node is 2, it will find 4 also. If node is 11, it
    // will return None.
    fn parent_with_left(&self, node: &Node) -> Option<&Node> {
        // If this node has a parent, and this parent has a left child, and
        // `node` is that left child, we found it!
        if !node.parent.is_null() {
            let parent = unsafe { &(*node.parent) };
            if let Some(left_child_of_parent) = &parent.left {
                if std::ptr::eq(left_child_of_parent.as_ref(), node) {
                    return Some(&parent);
                }
            }
            // The parent has no left child, or node is not its left child,
            // so recursively go up the parent link.
            return self.parent_with_left(parent);
        }

        // This node has no parent, so we've reached the root without finding
        // what we're after.
        None
    }

    /// Remove the given item from the tree; returns `true` if such a node was
    /// found and removed, `false` otherwise.
    pub fn remove(&mut self, _data: i32) -> bool {
        //if let Some(root) = &mut self.root {
        //if let Some(node) = find_node_mut(root, data) {
        //let nodeptr = node as *mut Node;
        ////self.remove_node(nodeptr);
        //self.count -= 1;
        //return true;
        //}
        //}
        false
    }

    /// Returns all data of the tree, visited inorder.
    pub fn inorder(&self) -> Vec<i32> {
        let mut v = vec![];
        if let Some(root) = &self.root {
            let mut node = self.leftmost_child(root);
            loop {
                v.push(node.data);
                match self.successor_of_node(node) {
                    None => break,
                    Some(succ) => node = succ,
                }
            }
        }
        v
    }

    /// Returns a string representation of the tree for debugging.
    pub fn display(&self) -> String {
        self.root
            .as_ref()
            .map_or(String::from(""), |node| self.display_node(node, 0))
    }

    fn display_node(&self, node: &Node, indent: usize) -> String {
        let mut s = format!("{}{}\n", " ".repeat(indent), node.data);
        s.push_str(&match &node.left {
            None => " ".repeat(indent + 2) + &String::from(".\n"),
            Some(lnode) => self.display_node(lnode, indent + 2),
        });
        s.push_str(&match &node.right {
            None => " ".repeat(indent + 2) + &String::from(".\n"),
            Some(rnode) => self.display_node(rnode, indent + 2),
        });
        s
    }
}

impl Node {
    fn new(data: i32) -> Box<Node> {
        Box::new(Self {
            data,
            left: None,
            right: None,
            parent: std::ptr::null_mut(),
        })
    }

    fn new_with_parent(data: i32, parent: *mut Node) -> Box<Node> {
        Box::new(Self {
            data,
            left: None,
            right: None,
            parent,
        })
    }

    // TODO: move this to standalone function?
    fn insert(&mut self, data: i32) -> bool {
        if data == self.data {
            false
        } else if data < self.data {
            match &mut self.left {
                None => {
                    self.left = Some(Node::new_with_parent(data, self as *mut Node));
                    true
                }
                Some(lnode) => lnode.insert(data),
            }
        } else {
            match &mut self.right {
                None => {
                    self.right = Some(Node::new_with_parent(data, self as *mut Node));
                    true
                }
                Some(rnode) => rnode.insert(data),
            }
        }
    }
}

// TODO: later try to replace find_node method by calls to this ?!
fn find_node_mut(fromnode: &mut Node, data: i32) -> Option<&mut Node> {
    if fromnode.data == data {
        Some(fromnode)
    } else if data < fromnode.data {
        fromnode
            .left
            .as_mut()
            .and_then(move |lnode| find_node_mut(lnode.as_mut(), data))
    } else {
        fromnode
            .right
            .as_mut()
            .and_then(move |rnode| find_node_mut(rnode.as_mut(), data))
    }
}
