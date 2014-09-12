from PyQt4.QtCore import *
from PyQt4.QtGui import *


class HighscoresDialog(QDialog):
    def __init__(self, scorelist, parent=None):
        super(HighscoresDialog, self).__init__(parent)
        self.setWindowTitle('High Scores')
        
        frame = QFrame(self)
        frame.setFrameStyle(QFrame.Panel | QFrame.Sunken)
        
        grid = QGridLayout()
        
        for i in range(10):
            name, score = ('', '') if i >= len(scorelist) else scorelist[i]
            
            place_label = QLabel('%3s.' % (i + 1))
            name_label = QLabel('%-50s' % name)
            score_label = QLabel('%7s' % score)
            score_label.setAlignment(Qt.AlignRight)
            
            grid.addWidget(place_label, i, 0)
            grid.addWidget(name_label, i, 1)
            grid.addWidget(score_label, i, 2)
        
        frame.setLayout(grid)
        
        #
        # Dialog layout
        #
        okbutton = QPushButton('&OK')
        self.connect(okbutton, SIGNAL('clicked()'), self, SLOT('accept()'))
        
        bbox = QHBoxLayout()
        bbox.addStretch()
        bbox.addWidget(okbutton)
        bbox.addStretch()
        
        layout = QVBoxLayout()
        layout.addWidget(frame)
        layout.addLayout(bbox)
        self.setLayout(layout)
        

if __name__ == "__main__":
    import sys
    import highscores
    
    FILENAME = 'pyqtris_highscores'
    ds = highscores.HighScores(10) 
    
    ds.load_from_file(FILENAME)

    app = QApplication(sys.argv)
    dialog = HighscoresDialog(ds.get_list())
    dialog.exec_()

