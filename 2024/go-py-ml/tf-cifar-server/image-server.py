import argparse
import os
import sys
import socket
import struct
import numpy as np
from tensorflow.keras import models
import tensorflow as tf


# TODO: move this comment to the README?
# Uses a simple length-prefix protocol over a Unix domain socket.
# Message structure:
# - 4 bytes: message length (not including these 4 bytes), in network byte order
# - 1 byte: message type (one of MSGTYPE_*)
# - N bytes: message body

MSGTYPE_ECHO = 0
MSGTYPE_CLASSIFY = 1


def send_msg(sock, msgtype, msgbody):
    """Send a message over a socket, using our protocol."""
    msglen = len(msgbody) + 1
    msg = struct.pack(">I", msglen) + struct.pack("B", msgtype) + msgbody
    sock.sendall(msg)


def recv_msg(sock):
    """Receive a length-prefixed message from a socket.

    Returns a tuple (type, body) where type is one of the MSGTYPE_* constants.
    In case of errors (including closed connection), returns (None, None).
    """
    raw_msglen = recvall(sock, 4)
    if raw_msglen is None:
        return None, None
    msglen = struct.unpack(">I", raw_msglen)[0]
    payload = recvall(sock, msglen)
    if payload is None:
        return None, None
    return (int(payload[0]), payload[1:])


def recvall(sock, n):
    """Receive and return exactly n bytes; return None if EOF is hit earlier."""
    data = bytearray()
    while len(data) < n:
        packet = sock.recv(n - len(data))
        if not packet:
            return None
        data.extend(packet)
    return data


def img_to_numpy(imgdata):
    """Translates an image (as received from the client) into a numpy array.

    The received image is an array of 3072 bytes (32x32x3), where each byte
    represents the intensity of a single color channel at a single pixel.
    The array is in row-major order, with the red channel first, then green,
    then blue.

    The resulting Numpy array has shape (32, 32, 3) and dtype float64, in
    a format expected by the model.
    """
    red = np.frombuffer(imgdata[:1024], dtype=np.uint8).reshape((32, 32))
    green = np.frombuffer(imgdata[1024:2048], dtype=np.uint8).reshape((32, 32))
    blue = np.frombuffer(imgdata[2048:], dtype=np.uint8).reshape((32, 32))
    uints = np.stack([red, green, blue], axis=-1)
    return uints.astype(np.float64) / 255.0


label_classes = [
    "airplane",
    "automobile",
    "bird",
    "cat",
    "deer",
    "dog",
    "frog",
    "horse",
    "ship",
    "truck",
]


def run_prediction(model, imgdata):
    npdata = img_to_numpy(imgdata)
    # Create a batch of 1 image to suit the model's API
    batch = np.expand_dims(npdata, axis=0)
    prediction = model(batch)
    probs = tf.nn.softmax(prediction)
    predindex = tf.argmax(probs, axis=1).numpy()[0]
    return label_classes[predindex]


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

    print(f"Loading model from {args.model}")
    model = models.load_model(args.model)

    # Ensure the socket does not already exist
    if os.path.exists(args.socketfile):
        os.remove(args.socketfile)

    # Listen on a Unix domain socket
    print(f"Listening on {args.socketfile}")
    sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    sock.bind(args.socketfile)
    sock.listen(1)

    try:
        while True:
            # Wait for a connection
            conn = sock.accept()[0]
            print("Connection established:", conn)

            while True:
                # Receive messages until None is returned (connection closed)
                msgtype, msgbody = recv_msg(conn)
                if msgtype is None:
                    break
                elif msgtype == MSGTYPE_ECHO:
                    # Echo: send the message back to the client
                    send_msg(conn, MSGTYPE_ECHO, msgbody)
                elif msgtype == MSGTYPE_CLASSIFY:
                    result = run_prediction(model, msgbody)
                    send_msg(conn, MSGTYPE_CLASSIFY, result.encode("utf-8"))

            conn.close()
    except KeyboardInterrupt:
        print("Server shutting down")
    finally:
        sock.close()
        os.remove(args.socketfile)


if __name__ == "__main__":
    server_main()
