#![allow(dead_code)]
#![allow(clippy::comparison_chain)]
#![allow(clippy::collapsible_else_if)]
mod nodehandle;
mod rcrefcell;
mod unsafeall;

fn main() {
    let mut t = unsafeall::Tree::new();
    t.insert(10);
    t.insert(12);
    t.insert(5);
    t.insert(4);
    t.insert(20);
    t.insert(8);
    t.insert(7);
    t.insert(15);
    t.insert(13);

    println!("{}", t.display());

    println!("{}", t.find(20));
    println!("{}", t.find(21));
    println!("{}", t.find(10));
}

#[cfg(test)]
mod tests {
    macro_rules! bst_tests {
        ($($name:ident: $type:ty,)*) => {
        $(
            mod $name {
                #[test]
                fn test_empty_tree() {
                    let t = <$type>::new();
                    assert_eq!(t.node_count(), 0);
                    assert!(!t.find(1));
                    assert!(!t.find(2));
                    assert_eq!(t.inorder(), vec![]);
                }

                #[test]
                fn test_simple_tree() {
                    let mut t = <$type>::new();
                    t.insert(10);
                    t.insert(5);
                    t.insert(12);
                    assert_eq!(t.node_count(), 3);
                    assert_eq!(t.inorder(), vec![5, 10, 12]);

                    assert!(t.find(5));
                    assert!(t.find(10));
                    assert!(t.find(12));

                    assert!(!t.find(1));
                    assert!(!t.find(2));

                    assert_eq!(
                        t.display(),
                        r"10
  5
    .
    .
  12
    .
    .
"
                    );
                }

                #[test]
                fn test_skewed_tree() {
                    // Insertion order here creates a "string" tree.
                    let mut t = <$type>::new();
                    t.insert(20);
                    t.insert(21);
                    t.insert(22);
                    t.insert(23);
                    t.insert(24);
                    t.insert(25);
                    assert_eq!(t.inorder(), vec![20, 21, 22, 23, 24, 25]);

                    for i in 20..26 {
                        assert!(t.find(i));
                    }
                    for i in 1..20 {
                        assert!(!t.find(i));
                    }
                }

                #[test]
                fn test_repeat_insert() {
                    let mut t = <$type>::new();
                    t.insert(20);

                    assert!(t.insert(21));
                    assert!(!t.insert(21));
                    assert!(!t.insert(20));

                    assert!(t.insert(72));
                    assert!(t.insert(2));
                    assert!(t.insert(12));

                    assert!(!t.insert(12));
                    assert!(!t.insert(72));
                    assert!(!t.insert(2));
                    assert_eq!(t.inorder(), vec![2, 12, 20, 21, 72]);
                }

                #[test]
                fn test_successor() {
                    let mut t = <$type>::new();
                    t.insert(10);
                    t.insert(12);
                    t.insert(5);
                    t.insert(4);
                    t.insert(20);
                    t.insert(8);
                    t.insert(7);
                    t.insert(15);
                    t.insert(13);

                    assert_eq!(t.successor(4), Some(5));
                    assert_eq!(t.successor(5), Some(7));
                    assert_eq!(t.successor(7), Some(8));
                    assert_eq!(t.successor(8), Some(10));
                    assert_eq!(t.successor(10), Some(12));
                    assert_eq!(t.successor(12), Some(13));
                    assert_eq!(t.successor(13), Some(15));
                    assert_eq!(t.successor(15), Some(20));
                    assert_eq!(t.successor(20), None);

                    assert_eq!(t.successor(1), None);
                    assert_eq!(t.successor(90), None);
                }

                #[test]
                fn test_successor_larger_tree() {
                    // These are just values 1..20 shuffled.
                    let vals = vec![
                        2, 3, 19, 8, 1, 17, 13, 20, 11, 4, 9, 7, 6, 12, 15, 16, 5, 10, 14, 18,
                    ];
                    let mut t = <$type>::new();
                    for v in vals {
                        t.insert(v);
                    }
                    assert_eq!(t.node_count(), 20);

                    for i in 1..20 {
                        assert_eq!(t.successor(i), Some(i + 1));
                    }
                    assert_eq!(t.successor(20), None);
                }

                #[test]
                fn test_remove_node_without_children() {
                    let mut t = <$type>::new();
                    t.insert(10);
                    t.insert(5);
                    t.insert(12);
                    assert_eq!(t.node_count(), 3);
                    assert_eq!(t.inorder(), vec![5, 10, 12]);

                    assert!(t.find(12));

                    // Can't remove values that are not in the tree
                    assert!(!t.remove(6));
                    assert!(!t.remove(11));

                    // Remove a node with no children.
                    assert!(t.remove(12));
                    assert!(!t.find(12));
                    assert_eq!(t.node_count(), 2);
                    assert_eq!(t.inorder(), vec![5, 10]);

                    // Remove another node with no children.
                    assert!(t.remove(5));
                    assert!(!t.find(5));
                    assert_eq!(t.node_count(), 1);
                    assert_eq!(t.inorder(), vec![10]);

                    // Remove final node with no children -- the root.
                    assert!(t.remove(10));
                    assert!(!t.find(10));
                    assert_eq!(t.node_count(), 0);
                }

                #[test]
                fn test_remove_node_with_left_child() {
                    // Build a left-leaning linear tree to test this.
                    let mut t = <$type>::new();
                    t.insert(10);
                    t.insert(5);
                    t.insert(2);
                    assert_eq!(t.node_count(), 3);
                    assert_eq!(t.inorder(), vec![2, 5, 10]);

                    assert!(t.remove(5));
                    assert_eq!(t.node_count(), 2);
                    assert_eq!(t.successor(2), Some(10));
                    assert_eq!(t.inorder(), vec![2, 10]);

                    assert!(t.remove(10));
                    assert_eq!(t.node_count(), 1);
                    assert!(t.find(2));
                    assert_eq!(t.inorder(), vec![2]);
                }

                #[test]
                fn test_remove_node_with_right_child() {
                    // Build a right-leaning linear tree to test this.
                    let mut t = <$type>::new();
                    t.insert(10);
                    t.insert(15);
                    t.insert(22);
                    assert_eq!(t.node_count(), 3);

                    assert!(t.remove(15));
                    assert_eq!(t.node_count(), 2);
                    assert_eq!(t.successor(10), Some(22));
                    assert_eq!(t.inorder(), vec![10, 22]);

                    assert!(t.remove(10));
                    assert_eq!(t.node_count(), 1);
                    assert!(t.find(22));
                }

                #[test]
                fn test_remove_node_with_two_children() {
                    let mut t = <$type>::new();
                    t.insert(10);
                    t.insert(12);
                    t.insert(5);
                    t.insert(4);
                    t.insert(20);
                    t.insert(8);
                    t.insert(7);
                    t.insert(15);
                    t.insert(13);
                    assert_eq!(t.node_count(), 9);

                    // 5 has two children. Remove it and sanity check.
                    assert!(t.remove(5));
                    assert_eq!(t.node_count(), 8);
                    assert_eq!(t.successor(4), Some(7));
                    assert!(t.find(4));
                    assert!(t.find(7));
                    assert!(t.find(8));

                    // Now 7 has two children. Remove it as well.
                    assert!(t.remove(7));
                    assert_eq!(t.node_count(), 7);
                    assert_eq!(t.successor(4), Some(8));

                    // The root 10 also has two children.
                    assert!(t.remove(10));
                    assert_eq!(t.node_count(), 6);
                    assert_eq!(t.successor(8), Some(12));
                }

                #[test]
                fn test_remove_larger_tree() {
                    // These are just values 1..20 shuffled.
                    let vals = vec![
                        2, 3, 19, 8, 1, 17, 13, 20, 11, 4, 9, 7, 6, 12, 15, 16, 5, 10, 14, 18,
                    ];
                    let mut t = <$type>::new();
                    for v in vals {
                        t.insert(v);
                    }
                    assert_eq!(t.node_count(), 20);

                    for i in 1..19 {
                        assert!(t.remove(i));
                    }
                    assert_eq!(t.inorder(), vec![19, 20]);
                }

                #[test]
                fn test_churn_insert_remove() {
                    let mut t = <$type>::new();
                    t.insert(12);
                    t.insert(17);
                    t.insert(11);
                    t.insert(15);

                    t.remove(17);
                    t.remove(11);

                    t.insert(18);
                    t.insert(13);

                    t.remove(12);
                    assert!(!t.remove(11));

                    t.remove(13);
                    t.insert(11);
                    t.insert(13);
                    t.insert(12);

                    t.remove(17);
                    t.remove(12);

                    let vals = vec![10, 11, 12, 13, 14, 15, 16, 17, 18, 19];
                    for v in &vals {
                        t.insert(*v);
                    }
                    assert_eq!(t.node_count(), 10);

                    assert!(t.find(11));
                    assert!(t.find(12));

                    for v in &vals {
                        t.remove(*v);
                    }

                    assert_eq!(t.node_count(), 0);
                }

                quickcheck::quickcheck! {
                    fn quickcheck_add_remove_all(xs: Vec<i32>) -> bool {
                        // Inserts all `xs` into a tree, then makes sure they
                        // can all be found, then removes all of them and
                        // ensures that the tree becomes empty.
                        // Note that quickcheck will generate some `xs` with
                        // duplicate values.
                        let mut t = <$type>::new();
                        for i in &xs {
                            t.insert(*i);
                        }

                        for i in &xs {
                            assert!(t.find(*i));
                        }

                        for i in &xs {
                            t.remove(*i);
                        }

                        assert_eq!(t.inorder(), vec![]);
                        assert_eq!(t.node_count(), 0);
                        true
                    }
                }

                quickcheck::quickcheck! {
                    fn quickcheck_successor(xs: Vec<i32>) -> bool {
                        // Inserts all `xs` into the tree, then checks that
                        // successor calls work correctly for all values.
                        if xs.len() < 1 {
                            return true;
                        }
                        let mut t = <$type>::new();
                        for i in &xs {
                            t.insert(*i);
                        }

                        let mut sorted = xs.clone();
                        sorted.sort();
                        sorted.dedup();
                        assert_eq!(t.node_count(), sorted.len());

                        for i in 0..sorted.len()-1 {
                            assert_eq!(t.successor(sorted[i]), Some(sorted[i+1]));
                        }
                        assert_eq!(t.successor(sorted[sorted.len()-1]), None);

                        for i in &sorted {
                            t.remove(*i);
                        }

                        assert_eq!(t.inorder(), vec![]);
                        assert_eq!(t.node_count(), 0);
                        true
                    }
                }
            }
        )*
        }
    }

    bst_tests! {
        rcrefcell: crate::rcrefcell::Tree,
        nodehandle: crate::nodehandle::Tree,
        unsafeall: crate::unsafeall::Tree,
    }
}
