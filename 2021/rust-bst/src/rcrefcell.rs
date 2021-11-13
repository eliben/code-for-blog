use std::cell::RefCell;
use std::rc::{Rc, Weak};

pub struct Tree {
    count: usize,
    root: Option<NodeLink>,
}

type NodeLink = Rc<RefCell<Node>>;

#[derive(Debug)]
struct Node {
    data: i32,
    left: Option<NodeLink>,
    right: Option<NodeLink>,
    parent: Option<Weak<RefCell<Node>>>,
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
        if let Some(root) = &self.root {
            if !self.insert_at(root, data) {
                return false;
            }
        } else {
            self.root = Some(Node::new(data));
        }
        self.count += 1;
        true
    }

    // Insert a new item into the subtree rooted at `atnode`.
    fn insert_at(&self, atnode: &NodeLink, data: i32) -> bool {
        let mut node = atnode.borrow_mut();
        if data == node.data {
            false
        } else if data < node.data {
            match &node.left {
                None => {
                    let new_node = Node::new_with_parent(data, atnode);
                    node.left = Some(new_node);
                    true
                }
                Some(lnode) => self.insert_at(lnode, data),
            }
        } else {
            match &node.right {
                None => {
                    let new_node = Node::new_with_parent(data, atnode);
                    node.right = Some(new_node);
                    true
                }
                Some(rnode) => self.insert_at(rnode, data),
            }
        }
    }

    /// Find the item in the tree; returns `true` iff the item is found.
    pub fn find(&self, data: i32) -> bool {
        self.root
            .as_ref()
            .map_or(false, |root| self.find_node(root, data).is_some())
    }

    // Find a node with the given item starting from `fromnode`.
    fn find_node(&self, fromnode: &NodeLink, data: i32) -> Option<NodeLink> {
        let node = fromnode.borrow();
        if node.data == data {
            Some(fromnode.clone())
        } else if data < node.data {
            node.left
                .as_ref()
                .and_then(|lnode| self.find_node(lnode, data))
        } else {
            node.right
                .as_ref()
                .and_then(|rnode| self.find_node(rnode, data))
        }
    }

    /// Find the successor of the given item in the tree; the successor is the
    /// next item in an inorder traversal.
    pub fn successor(&self, data: i32) -> Option<i32> {
        if let Some(root) = &self.root {
            if let Some(node) = self.find_node(root, data) {
                return self.successor_of_node(&node).map(|succ| succ.borrow().data);
            }
        }
        None
    }

    // Find the successor of `node` in the tree.
    fn successor_of_node(&self, node: &NodeLink) -> Option<NodeLink> {
        // Case 1: node has a right child; then the successor is the leftmost
        // child of this right child (or the right child itself, if it has no
        // left children).
        if let Some(rchild) = &node.borrow().right {
            return Some(self.leftmost_child(rchild));
        }
        // Case 2: no right child; then climb the parent links to find a node
        // we're the left child of. Failing to find such a parent before
        // reaching the root means there's no successor.
        self.parent_with_left(node)
    }

    // Find the leftmost child of `node`, or `node` itself in case it has no
    // left child.
    fn leftmost_child(&self, node: &NodeLink) -> NodeLink {
        if let Some(lchild) = &node.borrow().left {
            self.leftmost_child(lchild)
        } else {
            node.clone()
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
    fn parent_with_left(&self, node: &NodeLink) -> Option<NodeLink> {
        // If this node has a parent, and this parent has a left child, and
        // `node` is that left child, we found it!
        if let Some(parent) = &node.borrow().parent {
            let parent_rc = Weak::upgrade(parent)?;
            if let Some(left_child_of_parent) = &parent_rc.borrow().left {
                if Rc::ptr_eq(left_child_of_parent, node) {
                    return Some(parent_rc.clone());
                }
            }
            // The parent has no left child, or node is not its left child,
            // so recursively go up the parent link.
            return self.parent_with_left(&parent_rc);
        }
        // This node has no parent, so we've reached the root without finding
        // what we're after.
        None
    }

    /// Remove the given item from the tree; returns `true` if such a node was
    /// found and removed, `false` otherwise.
    pub fn remove(&mut self, data: i32) -> bool {
        if let Some(root) = &self.root {
            if let Some(mut node) = self.find_node(root, data) {
                self.remove_node(&mut node);
                self.count -= 1;
                return true;
            }
        }
        false
    }

    // Remove the given node from the tree.
    fn remove_node(&mut self, node: &mut NodeLink) {
        if node.borrow().left.is_none() && node.borrow().right.is_none() {
            // Node has no children, so it's safe to dispose.
            self.replace_node(node, None);
        } else if node.borrow().left.is_some() && node.borrow().right.is_some() {
            // Node has both children.
            // We find the successor of this node, replace our node's data with
            // its data and then recursively remove the successor.
            if let Some(mut succ) = self.successor_of_node(node) {
                node.borrow_mut().data = succ.borrow().data;
                self.remove_node(&mut succ);
            }
        } else if let Some(lchild) = &node.borrow().left {
            // Node has only left child, so replace it with its only child.
            self.replace_node(node, Some(lchild.clone()));
        } else if let Some(rchild) = &node.borrow().right {
            // Node has only right child, so replace it with its only child.
            self.replace_node(node, Some(rchild.clone()));
        } else {
            panic!("unreachable");
        }
    }

    // Replaces `node` with `r` in the tree, by setting `node`'s parent's
    // left/right link to `node` with a link to `r`, and setting `r`'s parent
    // link to `node`'s parent. This drops the only strong reference to `node`.
    fn replace_node(&mut self, node: &NodeLink, r: Option<NodeLink>) {
        if let Some(parent) = &node.borrow().parent {
            let parent_rc = Weak::upgrade(parent).unwrap();
            let mut parent_node = parent_rc.borrow_mut();
            // Set r's parent link to node's parent instead of node.
            if let Some(rnode) = r.as_ref() {
                rnode.borrow_mut().parent = Some(parent.clone());
            }
            // Figure out which child node is of parent - left/right - and
            // update links accordingly.
            if matches!(&parent_node.left, Some(left_child_of_parent) if
                Rc::ptr_eq(left_child_of_parent, node))
            {
                parent_node.left = r;
            } else if matches!(&parent_node.right, Some(right_child_of_parent) if
                Rc::ptr_eq(right_child_of_parent, node))
            {
                parent_node.right = r;
            }
        } else {
            if let Some(rnode) = r.as_ref() {
                rnode.borrow_mut().parent = None;
            }
            self.root = r;
        }
    }

    /// Returns all data of the tree, visited inorder.
    pub fn inorder(&self) -> Vec<i32> {
        let mut v = vec![];
        if let Some(root) = &self.root {
            let mut node = self.leftmost_child(root);
            loop {
                v.push(node.borrow().data);
                match self.successor_of_node(&node) {
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
            .map_or(String::from(""), |node| node.borrow().display(0))
    }
}

impl Node {
    fn new(data: i32) -> NodeLink {
        Rc::new(RefCell::new(Self {
            data,
            left: None,
            right: None,
            parent: None,
        }))
    }

    fn new_with_parent(data: i32, parent: &NodeLink) -> NodeLink {
        Rc::new(RefCell::new(Self {
            data,
            left: None,
            right: None,
            parent: Some(Rc::<RefCell<Node>>::downgrade(parent)),
        }))
    }

    fn display(&self, indent: usize) -> String {
        let mut s = format!("{}{}\n", " ".repeat(indent), self.data);
        s.push_str(&match &self.left {
            None => " ".repeat(indent + 2) + &String::from(".\n"),
            Some(lnode) => lnode.borrow().display(indent + 2),
        });
        s.push_str(&match &self.right {
            None => " ".repeat(indent + 2) + &String::from(".\n"),
            Some(rnode) => rnode.borrow().display(indent + 2),
        });

        s
    }
}
