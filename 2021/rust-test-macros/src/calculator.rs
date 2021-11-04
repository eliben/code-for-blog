// Calculator trait and a couple of basic implementations.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
pub trait Calculator {
    fn new() -> Self;
    fn add(&self, a: u32, b: u32) -> u32;
}

pub struct Foo {}

impl Calculator for Foo {
    fn new() -> Self {
        Self {}
    }

    fn add(&self, a: u32, b: u32) -> u32 {
        a + b
    }
}

pub struct Bar {}

impl Calculator for Bar {
    fn new() -> Self {
        Self {}
    }

    fn add(&self, a: u32, b: u32) -> u32 {
        if b == 0 {
            a
        } else {
            self.add(a, b - 1) + 1
        }
    }
}
