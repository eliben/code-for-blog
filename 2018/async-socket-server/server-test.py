# Simple unit testing for prime servers. Run with -h for details.
#
# Tested with Python 3.6
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.
import argparse
import itertools
import logging
import queue
import socket
import subprocess
import sys
import threading
import time

# The port number the server will listen on.
PORTNUM = 8099


def factorize_naive(n):
    """ A naive factorization method. Take integer 'n', return list of
        factors.
    """
    if n < 2:
        return []
    factors = []
    p = 2

    while True:
        if n == 1:
            return factors

        r = n % p
        if r == 0:
            factors.append(p)
            n = n // p
        elif p * p >= n:
            factors.append(n)
            return factors
        elif p > 2:
            # Advance in steps of 2 over odd numbers
            p += 2
        else:
            # If p == 2, get to 3
            p += 1
    assert False, "unreachable"


def server_runner(path, args, stop_event):
    """Runs the server as a subprocess until stop is requested.

    Run this function in a separate thread!

    path is the path to the server to run, with the given args. If 'path' ends
    with .js, node is prepended. The args have to be a (possibly empty)
    iterable.
    stop_event is a threading.Event object; when it's set, the subprocess is
    killed and this function returns.
    """
    runcmd = ['node', path] if path.endswith('.js') else [path]
    runcmd.extend(args)
    logging.info('server_runner: executing subprocess "{0}"'.format(runcmd))
    proc = subprocess.Popen(runcmd)
    logging.info('server_runner waiting for stop event')
    stop_event.wait()
    logging.info('server_runner sending kill to subprocess')
    proc.terminate()
    try:
        proc.wait(timeout=0.2)
    except subprocess.TimeoutExpired:
        logging.info('server_runner: subprocess did not die within timeout')


def client_thread_runner(port, nums=[], timeout=1.0):
    """Client.
    """
    tid = threading.current_thread().ident
    sockobj = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sockobj.settimeout(timeout)
    sockobj.connect(('localhost', port))
    logging.info('Client {0} connected to server'.format(tid))

    for num in nums:
        sockobj.send(bytes(str(num), encoding='ascii'))
        logging.info('Client {0} sent "{1}"'.format(tid, num))
        reply = sockobj.recv(20)
        logging.info('Client {0} received "{1}"'.format(tid, reply))

        isprime = len(factorize_naive(num)) <= 1
        if isprime:
            assert b'prime' in reply
        else:
            assert b'composite' in reply

    sockobj.shutdown(socket.SHUT_RDWR)
    sockobj.close()


def test_main():
    argparser = argparse.ArgumentParser('Server test')
    argparser.add_argument('server_path', help='path to the server executable')
    argparser.add_argument('-n', '--num-clients', default=4, type=int,
                           help='number of clients to launch simultaneously; ')
    args = argparser.parse_args()
    assert args.num_clients >= 1

    logging.basicConfig(
        level=logging.DEBUG,
        format='%(levelname)s:%(asctime)s:%(message)s')

    # Launch the server in a thread, listening on the port.
    stop_event = threading.Event()
    server_thread = threading.Thread(
            target=server_runner,
            args=(args.server_path, [str(PORTNUM)], stop_event))
    server_thread.start()
    time.sleep(0.2)

    threads = []
    for i in range(args.num_clients):
        tester_thread = threading.Thread(
            target=client_thread_runner,
            args=(PORTNUM, [900, 431], 5.0))
        tester_thread.start()
        threads.append(tester_thread)

    for thread in threads:
        thread.join()

    stop_event.set()


if __name__ == '__main__':
    test_main()
