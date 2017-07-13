# Helper for testing.
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.
import socketserver


class MyTCPHandler(socketserver.BaseRequestHandler):

    def handle(self):
        self.data = self.request.recv(1024).strip()
        print("{} wrote:".format(self.client_address[0]))
        print(self.data)
        self.request.sendall(self.data.upper())


if __name__ == "__main__":
    with socketserver.TCPServer(('localhost', 29999), MyTCPHandler) as server:
        # Activate the server; this will keep running until you
        # interrupt the program with Ctrl-C
        server.serve_forever()
