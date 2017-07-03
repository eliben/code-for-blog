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


def socket_reader(sockobj, outq):
    """Reads from sockobj, 1 byte at a time; places results in outq.

    This function runs in a loop until the sockobj connection is closed.
    """
    while True:
        buf = sockobj.recv(1)
        if len(buf) < 1:
            break
        outq.put(buf)


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
    t = threading.Thread(
            target=server_runner,
            args=(args.server_path, [str(args.server_port)], stop_event))
    t.start()
    time.sleep(0.2)

    sockobj = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sockobj.connect(('localhost', args.server_port))

    readq = queue.Queue()
    tread = threading.Thread(
            target=socket_reader,
            args=(sockobj, readq))
    tread.start()

    try:
        time.sleep(0.2)

        print(readq.get())
        #if sockobj.recv(1) != b'*':
            #logging.error('Something is wrong! Did not receive *')
        #sockobj.send(b'abcde')

        time.sleep(1)
    finally:
        # Closing the socket before killing the server helps the bound socket be
        # fully released on the server side; otherwise it may be kept alive by
        # the kernel for a while after the server process exits.
        sockobj.close()
        stop_event.set()
        t.join()
        tread.join()


if __name__ == '__main__':
    test_main()
