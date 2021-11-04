// Example of writing tests using a generic trait-based function.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
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
