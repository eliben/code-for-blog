"""
Sample GUI using SocketClientThread for socket communication, while doing other
stuff in parallel.

Eli Bendersky (eliben@gmail.com)
This code is in the public domain
"""
import os, sys, time
import Queue
from PyQt4.QtCore import *
from PyQt4.QtGui import *

from socketclientthread import SocketClientThread, ClientCommand, ClientReply

SERVER_ADDR = 'localhost', 50007


class CircleWidget(QWidget):
    def __init__(self, parent=None):
        super(CircleWidget, self).__init__(parent)
        self.nframe = 0
        self.setBackgroundRole(QPalette.Base)
        self.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Expanding)

    def minimumSizeHint(self):
        return QSize(50, 50)

    def sizeHint(self):
        return QSize(180, 180)

    def next(self):
        self.nframe += 1
        self.update()

    def paintEvent(self, event):
        painter = QPainter(self)
        painter.setRenderHint(QPainter.Antialiasing, True)
        painter.translate(self.width() / 2, self.height() / 2)

        for diameter in range(0, 64, 9):
            delta = abs((self.nframe % 64) - diameter / 2)
            alpha = 255 - (delta * delta) / 4 - diameter
            if alpha > 0:
                painter.setPen(QPen(QColor(0, diameter / 2, 127, alpha), 3))
                painter.drawEllipse(QRectF(
                    -diameter / 2.0,
                    -diameter / 2.0,
                    diameter,
                    diameter))


class LogWidget(QTextBrowser):
    def __init__(self, parent=None):
        super(LogWidget, self).__init__(parent)
        palette = QPalette()
        palette.setColor(QPalette.Base, QColor("#ddddfd"))
        self.setPalette(palette)


class SampleGUIClientWindow(QMainWindow):
    def __init__(self, parent=None):
        super(SampleGUIClientWindow, self).__init__(parent)

        self.create_main_frame()
        self.create_client()
        self.create_timers()

    def create_main_frame(self):
        self.circle_widget = CircleWidget()
        self.doit_button = QPushButton('Do it!')
        self.doit_button.clicked.connect(self.on_doit)
        self.log_widget = LogWidget()

        hbox = QHBoxLayout()
        hbox.addWidget(self.circle_widget)
        hbox.addWidget(self.doit_button)
        hbox.addWidget(self.log_widget)

        main_frame = QWidget()
        main_frame.setLayout(hbox)

        self.setCentralWidget(main_frame)

    def create_client(self):
        self.client = SocketClientThread()
        self.client.start()

    def create_timers(self):
        self.circle_timer = QTimer(self)
        self.circle_timer.timeout.connect(self.circle_widget.next)
        self.circle_timer.start(25)

        self.client_reply_timer = QTimer(self)
        self.client_reply_timer.timeout.connect(self.on_client_reply_timer)
        self.client_reply_timer.start(100)

    def on_doit(self):
        self.client.cmd_q.put(ClientCommand(ClientCommand.CONNECT, SERVER_ADDR))
        self.client.cmd_q.put(ClientCommand(ClientCommand.SEND, 'hello'))
        self.client.cmd_q.put(ClientCommand(ClientCommand.RECEIVE))
        self.client.cmd_q.put(ClientCommand(ClientCommand.CLOSE))

    def on_client_reply_timer(self):
        try:
            reply = self.client.reply_q.get(block=False)
            status = "SUCCESS" if reply.type == ClientReply.SUCCESS else "ERROR"
            self.log('Client reply %s: %s' % (status, reply.data))
        except Queue.Empty:
            pass

    def log(self, msg):
        timestamp = '[%010.3f]' % time.clock()
        self.log_widget.append(timestamp + ' ' + str(msg))



#-------------------------------------------------------------------------------
if __name__ == "__main__":
    app = QApplication(sys.argv)
    mainwindow = SampleGUIClientWindow()
    mainwindow.show()
    app.exec_()

