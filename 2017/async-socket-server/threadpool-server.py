from concurrent.futures import ThreadPoolExecutor
from enum import Enum
import socket
import sys

ProcessingState = Enum('ProcessingState', 'WAIT_FOR_MSG IN_MSG')


def serve_connection(sockobj, client_address):
    print('{0} connected'.format(client_address))
    sockobj.sendall(b'*')
    state = ProcessingState.WAIT_FOR_MSG

    while True:
        try:
            buf = sockobj.recv(1024)
            if not buf:
                break
        except IOError as e:
            break
        for b in buf:
            if state == ProcessingState.WAIT_FOR_MSG:
                if b == ord(b'^'):
                    state = ProcessingState.IN_MSG
            elif state == ProcessingState.IN_MSG:
                if b == ord(b'$'):
                    state = ProcessingState.WAIT_FOR_MSG
                else:
                    nb = b + 1
                    sockobj.send(bytes([nb]))
            else:
                assert False

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
