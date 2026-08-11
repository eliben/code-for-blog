// Threaded socket server - accepting multiple clients concurrently, by
// creating a new thread for each connecting client.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
use async_socket_server::serve_connection;
use std::{net::TcpListener, thread};

fn main() -> std::io::Result<()> {
    let port = match std::env::args().nth(1) {
        Some(s) => s,
        None => "9090".to_string(),
    };
    let addr = format!("127.0.0.1:{port}");

    let listener = TcpListener::bind(addr)?;
    println!("Serving on port {port}");

    loop {
        let (stream, addr) = listener.accept()?;
        println!("connection received from {}", addr);

        let res = thread::Builder::new().spawn(move || {
            if let Err(e) = serve_connection(stream) {
                eprintln!("error serving connection: {}", e);
            } else {
                println!("peer done {addr}");
            }
        });

        if let Err(e) = res {
            eprintln!("error spawning thread: {}", e);
        }
    }
}
