# A Python version of the redis-caching server.
#
# Issues a thread per connection, each thread blocks on every operation.
#
# Tested with Python 3.6 (requires the 'redis' package to be installed).
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.
import math
import redis
import socket
import sys

from concurrent.futures import ThreadPoolExecutor


def is_prime(num):
    if num == 2:
        return True
    elif num < 2 or num % 2 == 0:
        return False
    else:
        upto = int(math.sqrt(num)) + 1
        for i in range(3, upto, 2):
            if num % i == 0:
                return False
    return True


def serve_connection(sockobj, client_address):
    print('peer {0} connected'.format(client_address))

    while True:
        try:
            buf = sockobj.recv(1024)
            if not buf:
                break
        except IOError as e:
            break
        # TODO: redis caching here, etc.
        num = int(buf)
        if is_prime(num):
            sockobj.send(b'prime\n')
        else:
            sockobj.send(b'composite\n')

    print('connection from {0} closed'.format(client_address))
    sys.stdout.flush()
    sockobj.close()


if __name__ == '__main__':
    portnum = 8070 if len(sys.argv) < 2 else int(sys.argv[1])

    pool = ThreadPoolExecutor()
    sockobj = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sockobj.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    sockobj.bind(('localhost', portnum))
    sockobj.listen(15)

    try:
        while True:
            client_socket, client_address = sockobj.accept()
            pool.submit(serve_connection, client_socket, client_address)
    except KeyboardInterrupt as e:
        print(e)
        sockobj.close()
