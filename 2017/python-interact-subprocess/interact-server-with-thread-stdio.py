# Interaction with an HTTP server as a child process, using a thread to read the
# child's stdout and print it out.
#
# Tested with Python 3.6
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.
import subprocess
import time
import threading
import urllib.request


def output_reader(proc):
    for line in iter(proc.stdout.readline, b''):
        print('got line: {0}'.format(line.decode('utf-8')), end='')


def main():
    # note the -u here: essential for not buffering the stdout of the subprocess
    proc = subprocess.Popen(['python3', '-u', '-m', 'http.server', '8070'],
                            stdout=subprocess.PIPE,
                            stderr=subprocess.STDOUT)

    t = threading.Thread(target=output_reader, args=(proc,))
    t.start()

    try:
        time.sleep(0.2)

        for i in range(4):
            resp = urllib.request.urlopen('http://localhost:8070')
            assert b'Directory listing' in resp.read()
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
