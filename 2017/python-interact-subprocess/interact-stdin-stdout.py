import subprocess
import time


def main():
    proc = subprocess.Popen(['python3', '-i'],
                            stdin=subprocess.PIPE,
                            stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE)

    # communicate will close stdin in the end...
    proc.stdin.write(b'2+2\n')
    proc.stdin.flush()
    time.sleep(0.1)
    print(proc.stdout.readline())

    proc.stdin.write(b'2+3\n')
    proc.stdin.flush()
    time.sleep(0.1)
    print(proc.stdout.readline())
    #out, _ = proc.communicate(b'2+2\n')
    #print(out)
    #print(proc.returncode)

    #out, _ = proc.communicate(b'3+2\n')
    #print(out)


if __name__ == '__main__':
    main()
