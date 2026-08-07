use std::io::{Read, Write};
use std::net::TcpStream;
enum ProcessingState {
    WaitForMsg,
    InMsg,
}

// Implements the serial protocol.
pub fn serve_connection(mut stream: TcpStream) -> std::io::Result<()> {
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
