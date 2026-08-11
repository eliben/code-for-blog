use redis::AsyncTypedCommands;
use redis::aio::MultiplexedConnection;
use tokio::io::{self, AsyncReadExt, AsyncWriteExt};
use tokio::net::TcpListener;

#[derive(Clone)]
struct AppState {
    redis_connection: MultiplexedConnection,
}

#[tokio::main]
async fn main() -> io::Result<()> {
    let port = match std::env::args().nth(1) {
        Some(s) => s,
        None => "8070".to_string(),
    };
    let addr = format!("127.0.0.1:{port}");

    let app_state = AppState {
        redis_connection: {
            let client =
                redis::Client::open("redis://127.0.0.1").expect("Failed to create Redis client");
            client
                .get_multiplexed_async_connection()
                .await
                .expect("Failed to connect to Redis")
        },
    };

    let listener = TcpListener::bind(addr).await?;
    println!("Serving on port {port}");

    loop {
        let (socket, addr) = listener.accept().await?;
        println!("connection received from {addr:?}");
        let app_state = app_state.clone();

        tokio::spawn(async move {
            if let Err(e) = serve_client(socket, app_state).await {
                eprintln!("error serving connection from {addr:?}: {e}");
            } else {
                println!("peer done {addr:?}");
            }
        });
    }
}

async fn serve_client(
    mut stream: tokio::net::TcpStream,
    mut app_state: AppState,
) -> std::io::Result<()> {
    let mut buf = [0u8; 1024];

    loop {
        let n = stream.read(&mut buf).await?;
        if n == 0 {
            // Connection closed by the client.
            return Ok(());
        }

        // Parse read buf into u64.
        let num = std::str::from_utf8(&buf[..n])
            .map_err(|error| io::Error::new(io::ErrorKind::InvalidData, error))?
            .trim()
            .parse::<u64>()
            .map_err(|error| io::Error::new(io::ErrorKind::InvalidData, error))?;

        // Try the redis cache first. If found in cache, send the cached
        // answer.
        let cachekey = format!("primecache:{num}");
        if let Ok(result) = app_state.redis_connection.get(cachekey.clone()).await
            && let Some(cached) = result
        {
            stream.write_all((cached.clone() + "\n").as_bytes()).await?;
            println!("cached num {num} is {cached}");
            continue;
        }

        let answer = if isprime(num, true) {
            "prime"
        } else {
            "composite"
        };

        // Save answer in cache and send it to the client.
        app_state
            .redis_connection
            .set(cachekey, answer)
            .await
            .map_err(|error| io::Error::new(io::ErrorKind::Other, error))?;
        stream
            .write_all((answer.to_string() + "\n").as_bytes())
            .await?;
    }
}

// Check if n is prime, returning a boolean. The delay parameter is optional;
// if true, the function will block for n milliseconds before computing the
// answer. This is useful for simulating a long-running computation.
fn isprime(n: u64, delay: bool) -> bool {
    if delay {
        std::thread::sleep(std::time::Duration::from_millis(n));
    }

    if n < 2 {
        return false;
    }
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
