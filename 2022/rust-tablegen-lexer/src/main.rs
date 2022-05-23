// Binary for profiling purposes.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
use lexer::slice;

fn main() {
    let data = slice::read_testfile();
    let toks = slice::tokenize_all_push_prealloc(&data);

    println!("#toks = {}", toks.len());
}
