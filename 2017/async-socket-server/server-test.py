import argparse
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
    proc = subprocess.Popen(runcmd, stdout=subprocess.PIPE,
                                    stderr=subprocess.PIPE)
    logging.info('server_runner waiting for stop event')
    stop_event.wait()
    logging.info('server_runner sending kill to subprocess')
    proc.terminate()
    try:
        outs, errs = proc.communicate(timeout=0.2)
        print('outs=', outs.decode())
        print('errs=', errs.decode())
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
    assert q.empty(), 'queue had {0}'.format(q.get())


def client_tester1(port, initial_timeout=0.1):
    sockobj = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sockobj.settimeout(0.1)
    sockobj.connect(('localhost', port))

    readq = queue.Queue()
    exit_event = threading.Event()
    tread = threading.Thread(
            target=socket_reader,
            args=(sockobj, readq, exit_event))
    tread.start()

    try:
        assert_queue_contains(readq, b'*', timeout=initial_timeout)

        sockobj.send(b'abcdef')
        assert_queue_empty(readq)

        sockobj.send(b'^')
        assert_queue_empty(readq)

        sockobj.send(b'f')
        assert_queue_contains(readq, b'g')

        sockobj.send(b'1234')
        assert_queue_contains(readq, b'2')
        assert_queue_contains(readq, b'3')
        assert_queue_contains(readq, b'4')
        assert_queue_contains(readq, b'5')

        sockobj.send(b'$')
        assert_queue_empty(readq)
        sockobj.send(b'1234')
        assert_queue_empty(readq)

        sockobj.send(b'^')
        sockobj.send(b'xy')
        assert_queue_contains(readq, b'y')
        assert_queue_contains(readq, b'z')
    finally:
        # Closing the socket before killing the server helps the bound socket be
        # fully released on the server side; otherwise it may be kept alive by
        # the kernel for a while after the server process exits.
        sockobj.close()
        exit_event.set()
        tread.join()


def client_tester2(port, initial_timeout=0.1):
    sockobj = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sockobj.settimeout(0.1)
    sockobj.connect(('localhost', port))

    exit_event = threading.Event()
    readq = queue.Queue()
    tread = threading.Thread(
            target=socket_reader,
            args=(sockobj, readq, exit_event))
    tread.start()

    try:
        assert_queue_contains(readq, b'*', timeout=initial_timeout)
        sockobj.send(b'^ab$^kl$^80$50')
        for b in [b'b', b'c', b'l', b'm', b'9', b'1']:
            assert_queue_contains(readq, b)
        assert_queue_empty(readq)
    finally:
        sockobj.close()
        exit_event.set()
        tread.join()


def test_main():
    argparser = argparse.ArgumentParser('Server test')
    argparser.add_argument('server_path', help='path to the server executable')
    argparser.add_argument('-p', '--server_port', default=9090, type=int,
                           help='the server listens on this port')
    args = argparser.parse_args()

    logging.basicConfig(
        level=logging.DEBUG,
        format='%(levelname)s:%(asctime)s:%(message)s')

    stop_event = threading.Event()
    server_thread = threading.Thread(
            target=server_runner,
            args=(args.server_path, [str(args.server_port)], stop_event))
    server_thread.start()
    time.sleep(0.3)

    tester1_thread = threading.Thread(
            target=client_tester1,
            args=(args.server_port, 0.5))
    tester1_thread.start()

    tester2_thread = threading.Thread(
            target=client_tester2,
            args=(args.server_port, 2.5))
    tester2_thread.start()

    time.sleep(2.0)

    stop_event.set()
    server_thread.join()
    tester1_thread.join()


if __name__ == '__main__':
    test_main()
