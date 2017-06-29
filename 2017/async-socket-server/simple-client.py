import argparse
import logging
import socket
import sys
import threading
import time


class ReadThread(threading.Thread):
    def __init__(self, name, sockobj):
        super().__init__()
        self.sockobj = sockobj
        self.name = name
        self.bufsize = 8 * 1024

    def run(self):
        while True:
            buf = self.sockobj.recv(self.bufsize)
            logging.info('{0} Received: {1}'.format(self.name, buf))
            if b'1111' in buf:
                break


def make_new_connection(name, host, port):
    sockobj = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sockobj.connect((host, port))
    if sockobj.recv(1) != b'*':
        logging.error('Something is wrong! Did not receive *')
    logging.info('{0} connected...'.format(name))

    rthread = ReadThread(name, sockobj)
    rthread.start()
    logging.info('{0} sending'.format(name))
    sockobj.send(b'foo^1234$jo')
    time.sleep(1.0)
    logging.info('{0} sending'.format(name))
    sockobj.send(b'sdfsdfsdfsdf^a')
    time.sleep(1.0)
    logging.info('{0} sending'.format(name))
    sockobj.send(b'fkfkf0000$dfk^$sdf^a$^kk$')
    time.sleep(0.1)

    sockobj.close()
    rthread.join()


def main():
    argparser = argparse.ArgumentParser('Simple TCP client')
    argparser.add_argument('host', help='Server host name')
    argparser.add_argument('port', type=int, help='Server port')
    argparser.add_argument('-n', '--num_concurrent', type=int,
                           default=1,
                           help='Number of concurrent connections')
    args = argparser.parse_args()

    logging.basicConfig(
        level=logging.DEBUG,
        format='%(levelname)s:%(asctime)s:%(message)s')

    t1 = time.time()
    connections = []
    for i in range(args.num_concurrent):
        name = 'conn{0}'.format(i)
        tconn = threading.Thread(target=make_new_connection,
                                 args=(name, args.host, args.port))
        tconn.start()
        connections.append(tconn)

    for conn in connections:
        conn.join()

    print('Elapsed:', time.time() - t1)


if __name__ == '__main__':
    main()
