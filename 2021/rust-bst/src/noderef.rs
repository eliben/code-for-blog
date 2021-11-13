// The goal for this implementation is just showing how we can't have naked
// references for the parent node...
//
// If the `insert` method is uncommented, this code does not compile.
struct Tree<'a> {
    root: Option<Node<'a>>,
}

struct Node<'a> {
    data: i32,
    left: Option<Box<Node<'a>>>,
    right: Option<Box<Node<'a>>>,
    parent: Option<&'a Node<'a>>,
}

impl<'a> Node<'a> {
    fn new(data: i32) -> Self {
        Self {
            data: data,
            left: None,
            right: None,
            parent: None,
        }
    }

    // This demonstrates the compilation error. The error is less clear before
    // we add 'a to the self reference here and in tree's insert.
    // Uncomment and reformat the file ot see it.

    //fn insert(&'a mut self, n: i32) -> bool {
    //if n == self.data {
    //false
    //} else if n < self.data {
    //match &mut self.left {
    //None => {
    //let mut new_node = Node::new(n);
    //new_node.parent = Some(self);
    //self.left = Some(Box::new(new_node));
    //true
    //}
    //Some(lnode) => lnode.insert(n),
    //}
    //} else {
    //match &mut self.right {
    //None => {
    //let mut new_node = Node::new(n);
    //new_node.parent = Some(self);
    //self.right = Some(Box::new(new_node));
    //true
    //}
    //Some(rnode) => rnode.insert(n),
    //}
    //}
    //}

    fn display(&self, indent: usize) -> String {
        let mut s = format!("{}{}\n", " ".repeat(indent), self.data);
        s.push_str(&match &self.left {
            None => " ".repeat(indent + 2) + &String::from(".\n"),
            Some(lnode) => lnode.display(indent + 2),
        });
        s.push_str(&match &self.right {
            None => " ".repeat(indent + 2) + &String::from(".\n"),
            Some(rnode) => rnode.display(indent + 2),
        });

        s
    }
}

impl<'a> Tree<'a> {
    pub fn new() -> Self {
        Self { root: None }
    }

    pub fn insert(&'a mut self, n: i32) -> bool {
        match &mut self.root {
            None => {
                self.root = Some(Node::new(n));
                true
            }
            Some(node) => node.insert(n),
        }
    }

    pub fn display(&self) -> String {
        match &self.root {
            None => String::from(""),
            Some(node) => node.display(0),
        }
    }
}
