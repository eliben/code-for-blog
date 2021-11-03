pub trait Calculator {
    fn add(&self, a: u32, b: u32) -> u32;
}

pub struct Foo {}

impl Foo {
    pub fn new() -> Self {
        Self {}
    }
}

impl Calculator for Foo {
    fn add(&self, a: u32, b: u32) -> u32 {
        a + b
    }
}

pub struct Bar {}

impl Bar {
    pub fn new() -> Self {
        Self {}
    }
}

impl Calculator for Bar {
    fn add(&self, a: u32, b: u32) -> u32 {
        if b == 0 {
            a
        } else {
            self.add(a, b - 1) + 1
        }
    }
}
