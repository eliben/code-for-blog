import argparse
import logging
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
    runcmd = ['python3.6', path] if path.endswith('.py') else [path]
    runcmd.extend(args)
    logging.info('server_runner: executing subprocess "{0}"'.format(runcmd))
    proc = subprocess.Popen(runcmd, stdout=subprocess.PIPE,
                                    stderr=subprocess.PIPE)
    logging.info('server_runner waiting for stop event')
    stop_event.wait()
    logging.info('server_runner sending kill to subprocess')
    proc.terminate()
    try:
        # TODO: the child process has to flush otherwise we won't see stdout
        outs, errs = proc.communicate(timeout=0.2)
        print('outs=', outs)
        print('errs=', errs)
    except subprocess.TimeoutExpired:
        logging.info('server_runner: subprocess did not die within timeout')


def test_main():
    argparser = argparse.ArgumentParser('Server test')
    argparser.add_argument('server_path', help='path to the server executable')
    args = argparser.parse_args()

    logging.basicConfig(
        level=logging.DEBUG,
        format='%(levelname)s:%(asctime)s:%(message)s')

    stop_event = threading.Event()
    t = threading.Thread(
            target=server_runner,
            args=(args.server_path, ['9090'], stop_event))
    t.start()
    time.sleep(2)
    stop_event.set()
    t.join()


if __name__ == '__main__':
    test_main()
