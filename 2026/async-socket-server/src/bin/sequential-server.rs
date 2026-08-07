// Sequential version of the server: each client is served in turn, one
// at a time.
use async_socket_server::serve_connection;
use std::net::TcpListener;

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
        if let Err(e) = serve_connection(stream) {
            println!("error serving connection: {}", e);
        } else {
            println!("peer done {addr}");
        }
    }
}
