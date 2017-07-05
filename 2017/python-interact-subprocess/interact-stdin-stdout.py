import subprocess
import time


def main():
    proc = subprocess.Popen(['python3', '-i'],
                            stdin=subprocess.PIPE,
                            stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE)

    # communicate() will close stdin in the end, so the python subprocess will
    # exit.

    # To avoid deadlocks: careful to: add \n to output, flush output, use
    # readline() rather than read()
    proc.stdin.write(b'2+2\n')
    proc.stdin.flush() # without this it hangs...
    print(proc.stdout.readline())

    proc.stdin.write(b'len("foobar")\n')
    proc.stdin.flush()
    print(proc.stdout.readline())

    proc.terminate()
    proc.wait(timeout=0.2)


if __name__ == '__main__':
    main()
