// Writing tests with a macro that generates a function per type.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
macro_rules! calculator_tests {
    ($($name:ident: $type:ty,)*) => {
    $(
        #[test]
        fn $name() {
            let c = <$type>::new();
            assert_eq!(c.add(2, 3), 5);
            assert_eq!(c.add(10, 43), 53);
        }
    )*
    }
}

#[cfg(test)]
mod tests {
    use crate::calculator::{Bar, Calculator, Foo};

    calculator_tests! {
        foo: Foo,
        bar: Bar,
    }
}
