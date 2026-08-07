use std::net::TcpListener;
use std::io::{Read, Write};

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
        let (stream, addr) = listener.accept()?;
        println!("connection received from {}", addr);
        serve_connection(stream)?;
        println!("peer done {addr}");
    }
}

enum ProcessingState {
    WaitForMsg,
    InMsg,
}

fn serve_connection(mut stream: std::net::TcpStream) -> std::io::Result<()> {
    stream.write_all(b"*")?;

    let mut state = ProcessingState::WaitForMsg;
    let mut buf = [0u8; 1024];
    loop {
        let n = stream.read(&mut buf)?;
        if n == 0 {
            // Connection closed by the client.
            break;
        }

        for byte in &buf[..n] {
            match state {
                ProcessingState::WaitForMsg => {
                    if *byte == b'^' {
                        state = ProcessingState::InMsg;
                    }
                }
                ProcessingState::InMsg => {
                    if *byte == b'$' {
                        state = ProcessingState::WaitForMsg;
                    } else {
                        let newbyte = *byte + 1;
                        stream.write_all(&[newbyte])?;
                    }
                }
            }
        }
    }

    Ok(())
}
