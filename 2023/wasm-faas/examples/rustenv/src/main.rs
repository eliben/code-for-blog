// Basic Rust example compiled to WASM.
use std::env;

fn main() {
    println!("rustenv environment:");

    for (key, value) in env::vars() {
        println!("  {key}: {value}");
    }
}
