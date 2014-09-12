""" A convenience startup file for the game. 
"""
import sys
from PyQt4.QtCore import *
from PyQt4.QtGui import *

from lib.tetrisgame import TetrisMainWindow


app = QApplication(sys.argv)
form = TetrisMainWindow()
form.show()
app.exec_()


