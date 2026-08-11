use async_socket_server::ProcessingState;
use tokio::io::{self, AsyncReadExt, AsyncWriteExt};
use tokio::net::TcpListener;

#[tokio::main]
async fn main() -> io::Result<()> {
    let port = match std::env::args().nth(1) {
        Some(s) => s,
        None => "9090".to_string(),
    };
    let addr = format!("127.0.0.1:{port}");

    let listener = TcpListener::bind(addr).await?;
    println!("Serving on port {port}");

    loop {
        let (socket, addr) = listener.accept().await?;
        println!("connection received from {addr:?}");

        tokio::spawn(async move {
            if let Err(e) = serve_connection_async(socket).await {
                eprintln!("error serving connection from {addr:?}: {e}");
            } else {
                println!("peer done {addr:?}");
            }
        });
    }
}

// Implements the serial protocol as an async function.
async fn serve_connection_async(mut stream: tokio::net::TcpStream) -> std::io::Result<()> {
    stream.write_all(b"*").await?;

    let mut state = ProcessingState::WaitForMsg;
    let mut buf = [0u8; 1024];
    loop {
        let n = stream.read(&mut buf).await?;
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
                        stream.write_all(&[newbyte]).await?;
                    }
                }
            }
        }
    }

    Ok(())
}
