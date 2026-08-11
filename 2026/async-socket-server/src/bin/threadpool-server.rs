// Threaded socket server - accepting multiple clients concurrently, by
// dispatching them into a thread pool.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
use async_socket_server::serve_connection;
use crossbeam_channel::{Receiver, bounded};
use std::{
    io,
    net::{SocketAddr, TcpListener, TcpStream},
    thread,
};

const NUM_WORKERS: usize = 256;
const JOB_QUEUE_CAPACITY: usize = NUM_WORKERS;

struct Job {
    stream: TcpStream,
    addr: SocketAddr,
}

fn worker(receiver: Receiver<Job>) {
    while let Ok(job) = receiver.recv() {
        if let Err(e) = serve_connection(job.stream) {
            eprintln!("error serving connection from {}: {}", job.addr, e);
        } else {
            println!("peer done {}", job.addr);
        }
    }
}

fn main() -> io::Result<()> {
    let port = match std::env::args().nth(1) {
        Some(s) => s,
        None => "9090".to_string(),
    };
    let addr = format!("127.0.0.1:{port}");

    let listener = TcpListener::bind(addr)?;
    println!("Serving on port {port}");

    let (tx, rx) = bounded::<Job>(JOB_QUEUE_CAPACITY);
    for _ in 0..NUM_WORKERS {
        let receiver = rx.clone();
        thread::Builder::new().spawn(move || worker(receiver))?;
    }
    drop(rx);

    loop {
        let (stream, addr) = listener.accept()?;
        println!("connection received from {addr}");

        // A full queue blocks this loop, preventing it from accepting more
        // connections until a worker becomes available.
        if tx.send(Job { stream, addr }).is_err() {
            return Err(io::Error::other("all connection workers stopped"));
        }
    }
}
