// This works for one test fn, but not for multiple; how do we crate different
// names? No simple way to do this, due to macro hygiene.
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
