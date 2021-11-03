// TODO: issues -- need to pass object, not type (how to test various
// ctors?
// if I have multiple test functions, each test_$Type has to invoke each
// one of them.

#[cfg(test)]
mod tests {
    use crate::calculator::{Bar, Calculator, Foo};

    fn trait_tester<C: Calculator>(c: &C) {
        assert_eq!(c.add(2, 3), 5);
        assert_eq!(c.add(10, -43), -33);
    }

    #[test]
    fn test_foo() {
        trait_tester(&Foo::new());
    }

    #[test]
    fn test_bar() {
        trait_tester(&Bar::new());
    }
}
