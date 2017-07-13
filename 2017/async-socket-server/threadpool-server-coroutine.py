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
        # This loops over whole "frames" delimited in ^...$
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
    portnum = 9090
    if len(sys.argv) >= 2:
        portnum = int(sys.argv[1])

    # TODO: we can set # client to just below or above this number to see
    # blocking in effect.
    pool = ThreadPoolExecutor(64)
    sockobj = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sockobj.bind(('localhost', portnum))
    sockobj.listen(15)

    try:
        while True:
            client_socket, client_address = sockobj.accept()
            pool.submit(serve_connection, client_socket, client_address)
    except KeyboardInterrupt as e:
        print(e)
        sockobj.close()
