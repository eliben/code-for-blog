#-------------------------------------------------------------------------------
# twisted_irc_server.py
#
# A simple IRC server that flattens out what's happening behind the scenes of
# 'twistd words'. Useful mainly to test clients on localhost.
#
# Eli Bendersky (eliben@gmail.com)
# Last updated: 2013.01.27
# This code is in the public domain
#-------------------------------------------------------------------------------
import sys

from twisted.cred import checkers, portal
from twisted.internet import reactor
from twisted.internet.endpoints import TCP4ServerEndpoint
from twisted.python import log
from twisted.words import service


ROOM = 'room'
USERS = dict(
    user1='pass1',
    user2='pass2',
    user3='pass3',
    user4='pass4')


if __name__ == '__main__':
    log.startLogging(sys.stdout)

    # Initialize the Cred authentication system used by the IRC server.
    realm = service.InMemoryWordsRealm('testrealm')
    realm.addGroup(service.Group(ROOM))
    user_db = checkers.InMemoryUsernamePasswordDatabaseDontUse(**USERS)
    portal = portal.Portal(realm, [user_db])

    # IRC server factory.
    ircfactory = service.IRCFactory(realm, portal)

    # Connect a server to the TCP port 6667 endpoint and start listening.
    endpoint = TCP4ServerEndpoint(reactor, 6667)
    endpoint.listen(ircfactory)
    reactor.run()

