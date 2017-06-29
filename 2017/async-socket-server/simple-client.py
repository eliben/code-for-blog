import logging
import socket
import sys
import threading
import time


class ReadThread(threading.Thread):
    def __init__(self, sockobj):
        super().__init__()
        self.sockobj = sockobj
        self.bufsize = 8 * 1024

    def run(self):
        while True:
            buf = self.sockobj.recv(self.bufsize)
            logging.info('Received: {0}'.format(buf))
            if b'1111' in buf:
                break


def make_new_connection(name, host, port):
    sockobj = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sockobj.connect((host, port))

    rthread = ReadThread(sockobj)
    rthread.start()
    logging.info('Starting send')
    sockobj.send(b'foo^1234$jo')
    time.sleep(1.0)
    sockobj.send(b'sdfsdfsdfsdf^a')
    time.sleep(1.0)
    sockobj.send(b'fkfkf0000$dfk^$sdf^a$^kk$')
    time.sleep(1.0)

    sockobj.close()
    rthread.join()


def main():
    logging.basicConfig(
        level=logging.DEBUG,
        format='%(levelname)s:%(asctime)s:%(message)s')

    if len(sys.argv) <= 2:
        print("Error, expecting <host> <port>")
        sys.exit(1)

    host = sys.argv[1]
    port = int(sys.argv[2])

    make_new_connection("foo", host, port)


if __name__ == '__main__':
    main()
