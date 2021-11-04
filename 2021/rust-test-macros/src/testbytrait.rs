// if I have multiple test functions, each test_$Type has to invoke each
// one of them, and they all cannot run in parallel, be seen in reports,
// invoked by name w/ "cargo test" etc.

#[cfg(test)]
mod tests {
    use crate::calculator::{Bar, Calculator, Foo};

    fn trait_tester<C: Calculator>() {
        let c = C::new();
        assert_eq!(c.add(2, 3), 5);
        assert_eq!(c.add(10, 43), 53);
    }

    #[test]
    fn test_foo() {
        trait_tester::<Foo>();
    }

    #[test]
    fn test_bar() {
        trait_tester::<Bar>();
    }
}
