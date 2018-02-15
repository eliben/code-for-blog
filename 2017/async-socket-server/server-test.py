# Tests a concurrent server, by connecting multiple clients sending pre-set
# messages, and comparing the echoes with expected values.
#
# Run with -h for full usage.
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


def server_runner(path, args, stop_event):
    """Runs the server as a subprocess until stop is requested.

    Run this function in a separate thread!

    path is the path to the server to run, with the given args. If 'path' ends
    with .py, a python interpreter is prepended. The args have to be a (possibly
    empty) iterable.
    stop_event is a threading.Event object; when it's set, the subprocess is
    killed and this function returns.
    """
    runcmd = ['python3.6', '-u', path] if path.endswith('.py') else [path]
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


def socket_reader(sockobj, outq, exit_event):
    """Reads from sockobj, 1 byte at a time; places results in outq.

    This function runs in a loop until the sockobj connection is closed or until
    exit_event is set.
    """
    while not exit_event.is_set():
        try:
            buf = sockobj.recv(1)
            if len(buf) < 1:
                break
            outq.put(buf)
        except socket.timeout:
            continue
        except OSError:
            break


def assert_queue_contains(q, val, timeout=0.1):
    try:
        v = q.get(timeout=timeout)
        assert v == val
    except queue.Empty:
        assert False, f'queue was empty with timeout={timeout}'


def assert_queue_empty(q, wait=0.1):
    time.sleep(wait)
    assert q.empty(), 'queue had {0} with wait={1}'.format(q.get(), wait)


def client_thread_runner(client_body_func, port, initial_timeout=0.1):
    """Abstracts the function running within a client thread.

    Connects to the port with a socket, launches a reading thread and makes sure
    to shut down properly. client_body_func is the actual interaction with a
    socket, once connected.
    """
    sockobj = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sockobj.settimeout(initial_timeout)
    sockobj.connect(('localhost', port))
    logging.info('{0} connected to server'.format(client_body_func.__name__))

    readq = queue.Queue()
    exit_event = threading.Event()
    tread = threading.Thread(
            target=socket_reader,
            args=(sockobj, readq, exit_event))
    tread.start()

    try:
        client_body_func(sockobj, readq, initial_timeout)
    finally:
        # Closing the socket before killing the server helps the bound socket be
        # fully released on the server side; otherwise it may be kept alive by
        # the kernel for a while after the server process exits.
        sockobj.shutdown(socket.SHUT_RDWR)
        sockobj.close()
        exit_event.set()
        tread.join()


def client0(sock, readq, initial_timeout):
    assert_queue_contains(readq, b'*', timeout=initial_timeout)
    assert_queue_empty(readq)


def client1(sock, readq, initial_timeout):
    assert_queue_contains(readq, b'*', timeout=initial_timeout)

    sock.send(b'abcdef')
    assert_queue_empty(readq)

    sock.send(b'^')
    assert_queue_empty(readq)

    sock.send(b'f')
    assert_queue_contains(readq, b'g')

    sock.send(b'1234')
    assert_queue_contains(readq, b'2')
    assert_queue_contains(readq, b'3')
    assert_queue_contains(readq, b'4')
    assert_queue_contains(readq, b'5')

    sock.send(b'$')
    assert_queue_empty(readq)
    sock.send(b'1234')
    assert_queue_empty(readq)

    sock.send(b'^')
    sock.send(b'xy')
    assert_queue_contains(readq, b'y')
    assert_queue_contains(readq, b'z')


def client2(sock, readq, initial_timeout):
    assert_queue_contains(readq, b'*', timeout=initial_timeout)
    sock.send(b'^ab$^kl$^80$50')
    for b in [b'b', b'c', b'l', b'm', b'9', b'1']:
        assert_queue_contains(readq, b)
    assert_queue_empty(readq)


def client3(sock, readq, initial_timeout):
    assert_queue_contains(readq, b'*', timeout=initial_timeout)
    sock.send(b'^$^$^$^$^$^$$^$$$$foobarjoemoedoe^$$')
    assert_queue_empty(readq)


def test_main():
    argparser = argparse.ArgumentParser('Server test')
    argparser.add_argument('server_path', help='path to the server executable')
    argparser.add_argument('-p', '--server-port', default=9090, type=int,
                           help='the server listens on this port')
    argparser.add_argument('--timeout-bump', default=0.0, type=float,
                           help='amount of time (in sec) by which to bump the '
                                'timeout between consecutive clients')
    argparser.add_argument('-n', '--num-clients', default=2, type=int,
                           help='number of clients to launch simultaneously; ')
    argparser.add_argument('--loop', default=1, type=int,
                           help='launch test in a loop')
    args = argparser.parse_args()
    assert args.num_clients >= 1

    logging.basicConfig(
        level=logging.DEBUG,
        format='%(levelname)s:%(asctime)s:%(message)s')

    # Launch the server in a thread, listening on the port.
    stop_event = threading.Event()
    server_thread = threading.Thread(
            target=server_runner,
            args=(args.server_path, [str(args.server_port)], stop_event))
    server_thread.start()
    time.sleep(0.3)

    TIMEOUT = 0.5 + (args.num_clients - 1) * args.timeout_bump

    for i in range(args.loop):
        logging.info('** Test iteration {}'.format(i))
        client_iter = itertools.cycle([client0, client1, client2, client3])
        threads = []
        for i in range(args.num_clients):
            tester_thread = threading.Thread(
                    target=client_thread_runner,
                    args=(next(client_iter), args.server_port, TIMEOUT))
            tester_thread.start()
            threads.append(tester_thread)

        time.sleep(TIMEOUT)
        for thread in threads:
            thread.join()

    stop_event.set()


if __name__ == '__main__':
    test_main()
