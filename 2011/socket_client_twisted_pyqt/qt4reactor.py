# Copyright (c) 2001-2008 Twisted Matrix Laboratories.
# See LICENSE for details.


"""
This module provides support for Twisted to be driven by the Qt mainloop.

In order to use this support, simply do the following::
    |  app = QApplication(sys.argv) # your code to init Qt
    |  import qt4reactor
    |  qt4reactor.install()
    
alternatively:

    |  from twisted.application import reactors
    |  reactors.installReactor('qt4')

Then use twisted.internet APIs as usual.  The other methods here are not
intended to be called directly.

If you don't instantiate a QApplication or QCoreApplication prior to
installing the reactor, a QCoreApplication will be constructed
by the reactor.  QCoreApplication does not require a GUI so trial testing
can occur normally.

Twisted can be initialized after QApplication.exec_() with a call to
reactor.runReturn().  calling reactor.stop() will unhook twisted but
leave your Qt application running

API Stability: stable

Maintainer: U{Glenn H Tarbox, PhD<mailto:glenn@tarbox.org>}

Previous maintainer: U{Itamar Shtull-Trauring<mailto:twisted@itamarst.org>}
Original port to QT4: U{Gabe Rudy<mailto:rudy@goldenhelix.com>}
Subsequent port by therve
"""

__all__ = ['install']


import sys, time

from zope.interface import implements

from PyQt4.QtCore import QSocketNotifier, QObject, SIGNAL, QTimer, QCoreApplication
from PyQt4.QtCore import QEventLoop

from twisted.internet.interfaces import IReactorFDSet
from twisted.python import log
from twisted.internet.posixbase import PosixReactorBase

class TwistedSocketNotifier(QSocketNotifier):
    """
    Connection between an fd event and reader/writer callbacks.
    """

    def __init__(self, reactor, watcher, type):
        QSocketNotifier.__init__(self, watcher.fileno(), type)
        self.reactor = reactor
        self.watcher = watcher
        self.fn = None
        if type == QSocketNotifier.Read:
            self.fn = self.read
        elif type == QSocketNotifier.Write:
            self.fn = self.write
        QObject.connect(self, SIGNAL("activated(int)"), self.fn)


    def shutdown(self):
        QObject.disconnect(self, SIGNAL("activated(int)"), self.fn)
        self.setEnabled(False)
        self.fn = self.watcher = None
        self.deleteLater()


    def read(self, sock):
        w = self.watcher
        #self.setEnabled(False)    # ??? do I need this?            
        def _read():
            why = None
            try:
                why = w.doRead()
            except:
                log.err()
                why = sys.exc_info()[1]
            if why:
                self.reactor._disconnectSelectable(w, why, True)
            elif self.watcher:
                pass
                #self.setEnabled(True)
        log.callWithLogger(w, _read)
        self.reactor.reactorInvocation()

    def write(self, sock):
        w = self.watcher
        self.setEnabled(False)
        def _write():
            why = None
            try:
                why = w.doWrite()
            except:
                log.err()
                why = sys.exc_info()[1]
            if why:
                self.reactor._disconnectSelectable(w, why, False)
            elif self.watcher:
                self.setEnabled(True)
        log.callWithLogger(w, _write)
        self.reactor.reactorInvocation()

class fakeApplication(QEventLoop):
    def __init__(self):
        QEventLoop.__init__(self)
        
    def exec_(self):
        QEventLoop.exec_(self)
        
class QTReactor(PosixReactorBase):
    """
    Qt based reactor.
    """
    implements(IReactorFDSet)

    _timer = None

    def __init__(self):
        self._reads = {}
        self._writes = {}
        self._timer=QTimer()
        self._timer.setSingleShot(True)
        if QCoreApplication.startingUp():
            self.qApp=QCoreApplication([])
            self._ownApp=True
        else:
            self.qApp = QCoreApplication.instance()
            self._ownApp=False
        self._blockApp = None
        self._readWriteQ=[]
        
        """ some debugging instrumentation """
        self._doSomethingCount=0
        
        PosixReactorBase.__init__(self)

    def addReader(self, reader):
        if not reader in self._reads:
            self._reads[reader] = TwistedSocketNotifier(self, reader,
                                                       QSocketNotifier.Read)


    def addWriter(self, writer):
        if not writer in self._writes:
            self._writes[writer] = TwistedSocketNotifier(self, writer,
                                                        QSocketNotifier.Write)


    def removeReader(self, reader):
        if reader in self._reads:
            #self._reads[reader].shutdown()
            #del self._reads[reader]
            self._reads.pop(reader).shutdown()

    def removeWriter(self, writer):
        if writer in self._writes:
            self._writes[writer].shutdown()
            #del self._writes[writer]
            self._writes.pop(writer)


    def removeAll(self):
        return self._removeAll(self._reads, self._writes)


    def getReaders(self):
        return self._reads.keys()


    def getWriters(self):
        return self._writes.keys()
    
    def callLater(self,howlong, *args, **kargs):
        rval = super(QTReactor,self).callLater(howlong, *args, **kargs)
        self.reactorInvocation()
        return rval
    
    def crash(self):
        super(QTReactor,self).crash()
        
    def iterate(self,delay=0.0):
        t=self.running # not sure I entirely get the state of running
        self.running=True
        self._timer.stop() # in case its not (rare?)
        try:
            if delay == 0.0:
                self.reactorInvokePrivate()
                self._timer.stop() # supports multiple invocations
            else:
                endTime = delay + time.time()
                self.reactorInvokePrivate()
                while True:
                    t = endTime - time.time()
                    if t <= 0.0: return
                    self.qApp.processEvents(QEventLoop.AllEvents | 
                                      QEventLoop.WaitForMoreEvents,t*1010)
        finally:
            self.running=t
            
    def addReadWrite(self,t):
        self._readWriteQ.append(t)
        
    def runReturn(self, installSignalHandlers=True):
        QObject.connect(self._timer, SIGNAL("timeout()"), 
                        self.reactorInvokePrivate)
        self.startRunning(installSignalHandlers=installSignalHandlers)
        self._timer.start(0)
        
    def run(self, installSignalHandlers=True):
        try:
            if self._ownApp:
                self._blockApp=self.qApp
            else:
                self._blockApp = fakeApplication()
            self.runReturn(installSignalHandlers)
            self._blockApp.exec_()
        finally:
            self._timer.stop() # should already be stopped

    def reactorInvocation(self):
        self._timer.setInterval(0)
        
    def reactorInvokePrivate(self):
        if not self.running:
            self._blockApp.quit()
        self._doSomethingCount += 1
        self.runUntilCurrent()
        t = self.timeout()
        if t is None: t=0.1
        else: t = min(t,0.1)
        self._timer.setInterval(t*1010)
        self.qApp.processEvents() # could change interval
        self._timer.start()
                
    def doIteration(self):
        assert False, "doiteration is invalid call"
            
def install():
    """
    Configure the twisted mainloop to be run inside the qt mainloop.
    """
    from twisted.internet import main
    reactor = QTReactor()
    main.installReactor(reactor)
