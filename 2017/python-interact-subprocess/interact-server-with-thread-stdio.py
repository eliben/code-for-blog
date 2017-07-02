import subprocess
import time
import threading
import urllib.request


# TODO: shove this into a queue instead of printing out
# TODO: test to see what happens when the child process doesn't output a \n before dying
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
        resp = urllib.request.urlopen('http://localhost:8070')
        assert b'Directory listing' in resp.read()

        resp = urllib.request.urlopen('http://localhost:8070/')
        assert b'Directory listing' in resp.read()
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
