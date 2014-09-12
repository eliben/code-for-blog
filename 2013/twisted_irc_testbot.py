#-------------------------------------------------------------------------------
# twisted_irc_testbot.py
#
# A sample IRC bot based on the example in Twisted's docs.
#
# Eli Bendersky (eliben@gmail.com)
# Last updated: 2013.01.27
# This code is in the public domain
#-------------------------------------------------------------------------------
import argparse
import sys

from twisted.internet import reactor, protocol
from twisted.python import log
from twisted.words.protocols import irc


class TestBot(irc.IRCClient):
    def __init__(self, channel, nickname, password):
        self.channel = channel
        self.nickname = nickname
        self.password = password

    def connectionMade(self):
        irc.IRCClient.connectionMade(self)
        log.msg("[connected]")

    def connectionLost(self, reason):
        irc.IRCClient.connectionLost(self, reason)
        log.msg("[disconnected]")

    def signedOn(self):
        """Called after sucessfully signing on to the server."""
        self.join(self.channel)

    def joined(self, channel):
        """Called when I finish joining a channel.

        channel has the starting character intact.
        """
        log.msg("[I have joined %s]" % channel)
        self.msg(channel, "user1: bonbon")

    def privmsg(self, user, channel, msg):
        """Called when I have a message from a user to me or a channel."""
        user = user.split('!', 1)[0]
        log.msg("<%s> %s" % (user, msg))

        # Check to see if they're sending me a private message
        if channel == self.nickname:
            self.msg(user, 'Thanks for the private message')
        else:
            # Otherwise check to see if it is a message directed at me
            if msg.startswith(self.nickname + ":"):
                msg = "%s: I am a bot" % user
                self.msg(channel, msg)
                log.msg("<%s> %s" % (self.nickname, msg))

    def lineReceived(self, line):
        """Low level LineReceiver callback, used for debugging..."""
        log.msg('>> %s' % line)
        # Twisted's classes are old-style, so no super(), oh my...
        irc.IRCClient.lineReceived(self, line)


class TestBotFactory(protocol.ClientFactory):
    def __init__(self, channel, nickname, password):
        self.channel = channel
        self.nickname = nickname
        self.password = password

    def buildProtocol(self, addr):
        return TestBot(self.channel, self.nickname, self.password)

    def clientConnectionLost(self, connector, reason):
        reactor.stop()
        connector.connect()

    def clientConnectionFailed(self, connector, reason):
        reactor.stop()


if __name__ == '__main__':
    argparser = argparse.ArgumentParser()
    argparser.add_argument('--server', help='the server to connect to',
                           default='localhost')
    argparser.add_argument('--port', help='TCP port',
                           default=6667)
    argparser.add_argument('--channel', help='channel/room name to join',
                           default='room')
    argparser.add_argument('--nickname', default='user1')
    argparser.add_argument('--password', default='pass1')
    args = argparser.parse_args()

    log.startLogging(sys.stdout)

    # Create client, connect to server and run
    f = TestBotFactory(args.channel, args.nickname, args.password)
    reactor.connectTCP(args.server, args.port, f)
    reactor.run()

