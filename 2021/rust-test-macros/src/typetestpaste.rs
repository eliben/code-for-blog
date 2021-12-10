// Writing tests using the paste! macro.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.

macro_rules! calculator_tests {
    ($($name:ident: $type:ty,)*) => {
    $(
        paste! {
        #[test]
        fn [<test_ $name>]() {
            let c = <$type>::new();
            assert_eq!(c.add(2, 3), 5);
            assert_eq!(c.add(10, 43), 53);
        }

        #[test]
        fn [<test_ $name _feature1>]() {
            let c = <$type>::new();
            assert_eq!(c.add(6, 9), 15);
        }
        } // end paste
    )*
    }
}

#[cfg(test)]
mod tests {
    use crate::calculator::{Bar, Calculator, Foo};
    use paste::paste;

    calculator_tests! {
        foo: Foo,
        bar: Bar,
    }
}
