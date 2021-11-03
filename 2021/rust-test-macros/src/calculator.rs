pub trait Calculator {
    fn add(&self, a: i32, b: i32) -> i32;
}

pub struct Foo {}

impl Foo {
    pub fn new() -> Self {
        Self {}
    }
}

impl Calculator for Foo {
    fn add(&self, a: i32, b: i32) -> i32 {
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
    fn add(&self, a: i32, b: i32) -> i32 {
        a + b
    }
}
