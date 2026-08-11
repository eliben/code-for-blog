use tokio::io::{self, AsyncReadExt, AsyncWriteExt};
use tokio::net::TcpListener;

#[tokio::main]
async fn main() -> io::Result<()> {
    let port = match std::env::args().nth(1) {
        Some(s) => s,
        None => "8070".to_string(),
    };
    let addr = format!("127.0.0.1:{port}");

    let listener = TcpListener::bind(addr).await?;
    println!("Serving on port {port}");

    loop {
        let (socket, addr) = listener.accept().await?;
        println!("connection received from {addr:?}");

        tokio::spawn(async move {
            if let Err(e) = serve_client(socket).await {
                eprintln!("error serving connection from {addr:?}: {e}");
            } else {
                println!("peer done {addr:?}");
            }
        });
    }
}

async fn serve_client(mut stream: tokio::net::TcpStream) -> std::io::Result<()> {
    let mut buf = [0u8; 1024];

    loop {
        let n = stream.read(&mut buf).await?;
        if n == 0 {
            // Connection closed by the client.
            return Ok(());
        }

        // Parse read buf into u64.
        let s = std::str::from_utf8(&buf[..n])
            .map_err(|error| io::Error::new(io::ErrorKind::InvalidData, error))?
            .trim();
        let n = s
            .parse::<u64>()
            .map_err(|error| io::Error::new(io::ErrorKind::InvalidData, error))?;

        let answer = if isprime(n) { "prime" } else { "composite" };
        stream
            .write_all((answer.to_string() + "\n").as_bytes())
            .await?;
    }
}

fn isprime(n: u64) -> bool {
    if n % 2 == 0 {
        return n == 2;
    }

    let mut r = 3;
    while r * r <= n {
        if n % r == 0 {
            return false;
        }
        r += 2;
    }
    true
}
