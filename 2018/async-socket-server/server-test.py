# Simple unit testing for prime servers. Run with -h for details.
#
# Tested with Python 3.6
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.
import argparse
import itertools
import logging
import math
import queue
import random
import socket
import subprocess
import sys
import threading
import time

# The port number the server will listen on.
PORTNUM = 8070


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


def server_runner(path, args, stop_event):
    """Runs the server as a subprocess until stop is requested.

    Run this function in a separate thread!

    path is the path to the server to run, with the given args. If 'path' ends
    with .js, node is prepended. The args have to be a (possibly empty)
    iterable.
    stop_event is a threading.Event object; when it's set, the subprocess is
    killed and this function returns.
    """
    if path.endswith('.js'):
        runcmd = ['node', path]
    elif path.endswith('.py'):
        runcmd = ['python', path]
    else:
        runcmd = path
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


def client_thread_runner(port, nums=[]):
    """Client.
    """
    tid = threading.current_thread().ident
    sockobj = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sockobj.settimeout(1.0 * len(nums))
    sockobj.connect(('localhost', port))
    logging.info('Client {0} connected to server'.format(tid))

    for num in nums:
        sockobj.send(bytes(str(num), encoding='ascii'))
        logging.info('Client {0} sent "{1}"'.format(tid, num))
        reply = sockobj.recv(20)
        logging.info('Client {0} received "{1}"'.format(tid, reply))

        if is_prime(num):
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

    # Generate some pseudo-random numbers, with guaranteed repetition to hit
    # the caches.
    nums = [random.randint(20, 1990) // 2 for i in range(8)]
    nums.extend(nums[0:2])

    for i in range(args.num_clients):
        tester_thread = threading.Thread(
            target=client_thread_runner,
            args=(PORTNUM, nums))
        tester_thread.start()
        threads.append(tester_thread)

    for thread in threads:
        thread.join()

    stop_event.set()


if __name__ == '__main__':
    test_main()
