# Interaction with an HTTP server as a child process, using a thread to read the
# child's stdout and push it into a queue.
#
# Tested with Python 3.6
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.
import queue
import subprocess
import time
import threading
import urllib.request


def output_reader(proc, outq):
    for line in iter(proc.stdout.readline, b''):
        outq.put(line.decode('utf-8'))


def main():
    # Note the -u here: essential for not buffering the stdout of the subprocess
    proc = subprocess.Popen(['python3', '-u', '-m', 'http.server', '8070'],
                            stdout=subprocess.PIPE,
                            stderr=subprocess.STDOUT)

    outq = queue.Queue()
    t = threading.Thread(target=output_reader, args=(proc, outq))
    t.start()

    try:
        time.sleep(0.2)

        for i in range(4):
            resp = urllib.request.urlopen('http://localhost:8070')
            assert b'Directory listing' in resp.read()

            try:
                line = outq.get(block=False)
                print('got line from outq: {0}'.format(line), end='')
            except queue.Empty:
                print('could not get line from queue')

            time.sleep(0.1)
    finally:
        # This is in 'finally' so that we can terminate the child if something
        # goes wrong
        proc.terminate()
        try:
            proc.wait(timeout=0.2)
            print('== subprocess exited with rc =', proc.returncode)
        except subprocess.TimeoutExpired:
            print('subprocess did not terminate in time')

    t.join()


if __name__ == '__main__':
    main()
