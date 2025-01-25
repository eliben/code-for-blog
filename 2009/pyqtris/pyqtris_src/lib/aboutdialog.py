from PyQt4.QtCore import *
from PyQt4.QtGui import *

from images import get_logo_pixmap


about_text = """\
PyQtris is a simple, free Tetris clone, developed by
Eli Bendersky (https://eli.thegreenplace.net) in Python
using PyQt as the GUI toolkit.

It was tested on Windows and Linux with Python 2.6
and PyQt 4.5

Copyright (C) <2009> Eli Bendersky
License: LGPL (http://www.gnu.org/copyleft/lgpl.html)
"""

scoring_text = """\
The score in PyQtris is computed as follows:

1) A point is earned for every line the active figure
is dropped with the 'space' key. For example, if you
pressed 'space' and the figure dropped 10 lines before
reaching the bottom, you get 10 points.

2) Points are awarded for completed lines, as follows:
30 points for a single line, 120 for two lines, 270
for three lines and 480 for four lines.

3) The bonuses explained in (1) and (2) are further
increased with higher levels. On level 2, the bonus
is multiplied by 1.1, on level 3 by 1.2, on level 4
by 1.3 and so on.

The game level increases with each 10 completed lines."""


keys_desc = [
    ('Left arrow', 'Move figure left'),
    ('Right arrow', 'Move figure right'),
    ('Down arrow', 'Move figure down faster'),
    ('Up arrow', 'Rotate figure clockwise'),
    ('Space', 'Drop figure'),
    ('Ctrl-H', 'Show high scores'),
    ('Ctrl-N', 'New game'),
    ('Ctrl-P', 'Pause / Resume game'),
    ('Ctrl-Q', 'Quit'),
    ('F1', 'About PyQtris'),
    ]


class AboutDialog(QDialog):
    def __init__(self, parent=None):
        super(AboutDialog, self).__init__(parent)
        self.setWindowTitle('About PyQtris')

        #
        # About
        #
        about_page = QWidget(self)
        logo = QLabel()
        logo.setPixmap(get_logo_pixmap())

        about_label = QLabel(about_text)

        about_layout = QVBoxLayout()
        about_layout.addWidget(logo, 0, Qt.AlignCenter)
        about_layout.addWidget(about_label, 0, Qt.AlignCenter)
        about_page.setLayout(about_layout)

        #
        # Keys
        #
        keys_page = QWidget(self)
        keys_layout = QGridLayout()

        i = 0
        for key, desc in keys_desc:
            keys_layout.addWidget(QLabel(key), i, 0)
            keys_layout.addWidget(QLabel(desc), i, 1)
            i += 1

        keys_page.setLayout(keys_layout)

        #
        # Scoring
        #
        score_page = QWidget(self)
        score_label = QLabel(scoring_text)
        score_layout = QVBoxLayout()
        score_layout.addWidget(score_label)
        score_page.setLayout(score_layout)

        tabs = QTabWidget(self)
        tabs.addTab(about_page, 'About')
        tabs.addTab(keys_page, 'Keys')
        tabs.addTab(score_page, 'Scoring')

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
        layout.addWidget(tabs)
        layout.addLayout(bbox)
        self.setLayout(layout)


if __name__ == "__main__":
    import sys
    app = QApplication(sys.argv)
    dialog = AboutDialog()
    dialog.exec_()

