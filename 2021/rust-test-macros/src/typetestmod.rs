macro_rules! calculator_tests {
    ($($name:ident: $type:ty,)*) => {
    $(
        mod $name {
            use super::*;

            #[test]
            fn test() {
                let c = <$type>::new();
                assert_eq!(c.add(2, 3), 5);
                assert_eq!(c.add(10, 43), 53);
            }

            #[test]
            fn test_feature1() {
                let c = <$type>::new();
                assert_eq!(c.add(6, 9), 15);
            }
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
