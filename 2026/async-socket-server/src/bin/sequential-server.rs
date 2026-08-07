// Sequential version of the server: each client is served in turn, one
// at a time.
use std::io::{Read, Write};
use std::net::{TcpListener, TcpStream};

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

enum ProcessingState {
    WaitForMsg,
    InMsg,
}

// Implements the serial protocol.
fn serve_connection(mut stream: TcpStream) -> std::io::Result<()> {
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
                        let newbyte = byte.wrapping_add(1);
                        stream.write_all(&[newbyte])?;
                    }
                }
            }
        }
    }

    Ok(())
}
