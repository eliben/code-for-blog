# Simple interaction with an HTTP server as a child process.
#
# Tested with Python 3.6
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.
import subprocess
import time
import urllib.request


def main():
    # Note the -u here: essential for not buffering the stdout of the subprocess
    proc = subprocess.Popen(['python3', '-u', '-m', 'http.server', '8070'],
                            stdout=subprocess.PIPE,
                            stderr=subprocess.STDOUT)

    try:
        time.sleep(0.2)
        resp = urllib.request.urlopen('http://localhost:8070')
        assert b'Directory listing' in resp.read()
    finally:
        # This is in 'finally' so that we can terminate the child if something
        # goes wrong
        proc.terminate()
        try:
            outs, _ = proc.communicate(timeout=0.2)
            # We'll see it exiting with -15 which means killed by SIGTERM
            print('== subprocess exited with rc =', proc.returncode)
            print(outs.decode('utf-8'))
        except subprocess.TimeoutExpired:
            print('subprocess did not terminate in time')


if __name__ == '__main__':
    main()
