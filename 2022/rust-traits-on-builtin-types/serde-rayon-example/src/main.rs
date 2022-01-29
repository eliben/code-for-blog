// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
use serde::Serialize;

use byteorder::{LittleEndian, WriteBytesExt};

use rayon::prelude::*;

fn main() {
    let mut serializer = serde_json::Serializer::new(std::io::stdout());

    // Note: it would be cleaner to user serde_json::to_string, but I wanted
    // specifically to demonstrate the method `serialize` added onto integers
    // when the Serialize trait is imported.
    print!("Serializing an integer: ");
    185.serialize(&mut serializer).unwrap();
    println!();

    print!("Serializing a vector of integers: ");
    vec![2, 3, 4].serialize(&mut serializer).unwrap();
    println!();

    let mut wv = vec![];
    wv.write_u16::<LittleEndian>(259).unwrap();
    wv.write_u16::<LittleEndian>(517).unwrap();
    println!("{:?}", wv);

    let exps = vec![2, 4, 6, 12, 24];
    let powers_of_two: Vec<_> = exps.par_iter().map(|n| 2_u64.pow(*n)).collect();
    println!("{:?}", powers_of_two);
}
