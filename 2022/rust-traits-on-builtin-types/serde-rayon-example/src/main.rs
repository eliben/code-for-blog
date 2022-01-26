use serde::Serialize;
use serde_json;

fn main() {
    let mut serializer = serde_json::Serializer::new(std::io::stdout());

    // Note: it would be cleaner to user serde_json::to_string, but I wanted
    // specifically to demonstrate the method `serialize` added onto integers
    // when the Serialize trait is imported.
    print!("Serializing an integer: ");
    185.serialize(&mut serializer).unwrap();
    println!("");

    print!("Serializing a vector of integers: ");
    vec![2, 3, 4].serialize(&mut serializer).unwrap();
    println!("");
}
