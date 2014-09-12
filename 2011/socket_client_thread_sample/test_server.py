""" 
Test server for the client thread sample

Eli Bendersky (eliben@gmail.com)
This code is in the public domain
"""
from socket import *
import time
myHost = ''
myPort = 50007

sockobj = socket(AF_INET, SOCK_STREAM)
sockobj.bind((myHost, myPort))
sockobj.listen(2)

connection, address = sockobj.accept()
print 'Server connected by', address

d = connection.recv(1024)
print(d.encode('hex'))

time.sleep(1.5)
connection.send('\x04\x00\x00\x00tuxy')
