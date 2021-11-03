// Keeping name different from type because the test func name needs to be an
// identifier, and types can be qualified like crate::module::Type.

macro_rules! calculator_tests {
    ($($name:ident: $type:ty,)*) => {
    $(
        mod $name {
            use super::*;

            #[test]
            fn mytest() {
                let c = <$type>::new();
                assert_eq!(c.add(2, 3), 5);
                assert_eq!(c.add(10, -43), -33);
            }
        }
    )*
    }
}

// The different module is to give different names to tests.
// Could also place all of them into the same module...

#[cfg(test)]
mod tests {
    use crate::calculator::{Bar, Calculator, Foo};

    calculator_tests! {
        foo: Foo,
        bar: Bar,
    }
}
