// Threaded socket server - accepting multiple clients concurrently, by
// dispatching them into a thread pool.
use async_socket_server::serve_connection;
use rayon;
use std::{net::TcpListener};

fn main() -> std::io::Result<()> {
    let port = match std::env::args().nth(1) {
        Some(s) => s,
        None => "9090".to_string(),
    };
    let addr = format!("127.0.0.1:{port}");

    let listener = TcpListener::bind(addr)?;
    println!("Serving on port {port}");

    let pool = rayon::ThreadPoolBuilder::new()
        .num_threads(256)
        .build()
        .unwrap();

    loop {
        let (stream, addr) = listener.accept()?;
        println!("connection received from {}", addr);

        pool.spawn(move || {
            if let Err(e) = serve_connection(stream) {
                println!("error serving connection: {}", e);
            } else {
                println!("peer done {addr}");
            }
        });
    }
}
