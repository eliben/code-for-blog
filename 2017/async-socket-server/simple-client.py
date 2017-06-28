import sys, time
import socket


def make_new_connection(name, host, port):
    sockobj = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sockobj.connect((host, port))

    sockobj.send(b'foo^1234$jo')
    sockobj.send(b'sdfsdfsdfsdf^a')
    sockobj.send(b'fkfkf0000$dfk^$sdf^a$^kk$')

    buf = b''
    while True:
        buf += sockobj.recv(1024)
        print(buf)

    sockobj.close()


def main():
    if len(sys.argv) <= 2:
        print("Error, expecting <host> <port>")
        sys.exit(1)

    host = sys.argv[1]
    port = int(sys.argv[2])

    make_new_connection("foo", host, port)


if __name__ == '__main__':
    main()
