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


def test_main():
    argparser = argparse.ArgumentParser('Server test')
    argparser.add_argument('server_path', help='path to the server executable')
    argparser.add_argument('-n', '--num-clients', default=2, type=int,
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
    time.sleep(0.3)

    #TIMEOUT = 0.5 + (args.num_clients - 1) * args.timeout_bump

    #for i in range(args.loop):
        #logging.info('** Test iteration {}'.format(i))
        #client_iter = itertools.cycle([client0, client1, client2, client3])
        #threads = []
        #for i in range(args.num_clients):
            #tester_thread = threading.Thread(
                    #target=client_thread_runner,
                    #args=(next(client_iter), args.server_port, TIMEOUT))
            #tester_thread.start()
            #threads.append(tester_thread)

        #time.sleep(TIMEOUT)
        #for thread in threads:
            #thread.join()

    #stop_event.set()


if __name__ == '__main__':
    test_main()
