import argparse
import os
import sys
import socket
import struct
from tensorflow.keras import models


def recv_msg(sock):
    """Receive a length-prefixed message from a socket.

    Returns a tuple (type, body) where type is a single-byte number.
    """
    raw_msglen = recvall(sock, 4)
    if not raw_msglen:
        return None, None
    msglen = struct.unpack(">I", raw_msglen)[0]
    payload = recvall(sock, msglen)
    return (int(payload[0]), payload[1:])


def recvall(sock, n):
    """Receive exactly n bytes or return None if EOF is hit earlier."""
    data = bytearray()
    while len(data) < n:
        packet = sock.recv(n - len(data))
        if not packet:
            raise ValueError("Incomplete message")
        data.extend(packet)
    return data


def server_main():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--socketfile",
        type=str,
        default="/tmp/imageserver.sock",
        help="The unix domain socket to listen on.",
    )
    parser.add_argument("--model", type=str, help="The model file (*.keras) to load.")
    args = parser.parse_args()

    print(args.socketfile)

    model = models.load_model(args.model)

    # Ensure the socket does not already exist

    if os.path.exists(args.socketfile):
        os.remove(args.socketfile)

    # Listn on a Unix domain socket
    sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    sock.bind(args.socketfile)
    sock.listen(1)

    print(f"Server ready on {args.socketfile}")

    try:
        while True:
            # Wait for a connection
            connection, client_address = sock.accept()
            try:
                print("Connection established:", connection, client_address)

                msgtype, msgbody = recv_msg(connection)
                print(f"Received message type={msgtype}, body={msgbody.decode()}")

            finally:
                connection.close()
    except KeyboardInterrupt:
        print("Server shutting down")
    finally:
        sock.close()
        os.remove(args.socketfile)


if __name__ == "__main__":
    server_main()
