use std::net::TcpListener;
use std::io::Write;

fn main() -> std::io::Result<()> {
    let mut port = "9090";
    let args: Vec<String> = std::env::args().collect();
    eprintln!("{:?}", args);
    if args.len() >= 2 {
        port = &args[1];
    }
    let addr = format!("127.0.0.1:{port}");

    let listener = TcpListener::bind(addr)?;
    println!("Serving on port {port}");

    loop {
        let (mut stream, addr) = listener.accept()?;
        println!("connection received from {}", addr);
        serve_connection(stream)?;
        println!("peer done {addr}");
    }
}

fn serve_connection(mut stream: std::net::TcpStream) -> std::io::Result<()> {
    // Send a welcome asterisk to the client.
    stream.write_all(b"*")?;
    Ok(())
}
