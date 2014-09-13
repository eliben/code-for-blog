import sys
import wx

sys.path.insert(0, 'lib.zip')
from lib.TetrisGame import TetrisGame


if __name__ == '__main__':
    app = wx.PySimpleApp()
    frame = TetrisGame(None)
    frame.Show(True)
    app.MainLoop()







