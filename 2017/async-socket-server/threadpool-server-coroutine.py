# Threaded socket server - accepting multiple clients concurrently, by
# dispatching them into a thread pool.
#
# The client protocol in this sample is implemented with coroutines rather than
# with an explicit state machine, using the technique discussed in:
# https://eli.thegreenplace.net/2009/08/29/co-routines-as-an-alternative-to-state-machines
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.
import argparse
from concurrent.futures import ThreadPoolExecutor
import socket
import sys


def coroutine(func):
    def start(*args,**kwargs):
        cr = func(*args,**kwargs)
        next(cr)
        return cr
    return start


@coroutine
def client_protocol(target=None):
    while True:
        # Each iteration of this outer loop processes a whole "frame" (bytes
        # delimited by ^....$).
        b = (yield)
        if b == ord(b'^'):
            # Frame starts. Loop until end is encountered and send replies to
            # target.
            while True:
                b = (yield)
                if b == ord(b'$'):
                    break
                target.send(bytes([b + 1]))


@coroutine
def reply_processor(sockobj):
    while True:
        reply = (yield)
        sockobj.send(reply)


def serve_connection(sockobj, client_address):
    print('{0} connected'.format(client_address))
    sockobj.sendall(b'*')
    protocol = client_protocol(target=reply_processor(sockobj))

    while True:
        try:
            buf = sockobj.recv(1024)
            if not buf:
                break
        except IOError as e:
            break
        for b in buf:
            protocol.send(b)

    print('{0} done'.format(client_address))
    sys.stdout.flush()
    sockobj.close()


if __name__ == '__main__':
    argparser = argparse.ArgumentParser('Threadpool server')
    argparser.add_argument('--port', type=int, default=9090, help='Server port')
    argparser.add_argument('-n', type=int,
                           default=64, help='Number of threads in pool')
    args = argparser.parse_args()

    pool = ThreadPoolExecutor(args.n)
    sockobj = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sockobj.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    sockobj.bind(('localhost', args.port))
    sockobj.listen(15)

    try:
        while True:
            client_socket, client_address = sockobj.accept()
            pool.submit(serve_connection, client_socket, client_address)
    except KeyboardInterrupt as e:
        print(e)
        sockobj.close()
