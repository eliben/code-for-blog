""" 
Simple socket client using Twisted.

Eli Bendersky (eliben@gmail.com)
This code is in the public domain
"""
import struct

from twisted.internet.protocol import Protocol, ClientFactory
from twisted.protocols.basic import IntNStringReceiver


class SocketClientProtocol(IntNStringReceiver):
    """ The protocol is based on twisted.protocols.basic
        IntNStringReceiver, with little-endian 32-bit 
        length prefix.
    """
    structFormat = "<L"
    prefixLength = struct.calcsize(structFormat)

    def stringReceived(self, s):
        self.factory.got_msg(s)

    def connectionMade(self):
        self.factory.clientReady(self)


class SocketClientFactory(ClientFactory):
    """ Created with callbacks for connection and receiving.
        send_msg can be used to send messages when connected.
    """
    protocol = SocketClientProtocol

    def __init__(   
            self, 
            connect_success_callback,
            connect_fail_callback,
            recv_callback):
        self.connect_success_callback = connect_success_callback
        self.connect_fail_callback = connect_fail_callback
        self.recv_callback = recv_callback
        self.client = None
    
    def clientConnectionFailed(self, connector, reason):
        self.connect_fail_callback(reason)
    
    def clientReady(self, client):
        self.client = client
        self.connect_success_callback()
    
    def got_msg(self, msg):
        self.recv_callback(msg)

    def send_msg(self, msg):
        if self.client:
            self.client.sendString(msg)


