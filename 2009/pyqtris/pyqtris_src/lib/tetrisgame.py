import os, sys
from collections import namedtuple

from PyQt4.QtCore import *
from PyQt4.QtGui import *

from aboutdialog import AboutDialog
from highscoresdialog import HighscoresDialog
from highscores import HighScores
from images import get_icon_pixmap
from enum import Enum
from tetrismodel import TetrisBoard, Figure, FigureBank
from highscores import HighScores
from version import pyqtris_version


class DrawabaleTetrisBoard(QWidget):
    """ A base class for tetris board widgets that can draw
        themselves.
    """
    def __init__(self, parent, nrows, ncols, blocksize):
        super(DrawabaleTetrisBoard, self).__init__(parent)
        
        self.nrows = nrows
        self.ncols = ncols
        self.blocksize = blocksize

        self.nrows = nrows
        self.ncols = ncols
        self.blocksize = blocksize
        self.showgrid = True
        self.gridwidth = 2
        self.gridcolor = QColor(204, 204, 204)
        
        # the "sink" is the white border around the actual 
        # tetris grid
        #
        self.sinkwidth = 4
        self.sinkcolor = 'white'
        self.block_border_color = 'black'
        self.bgcolor = QColor(234, 234, 244)
        
        self.setSizePolicy(QSizePolicy(
                            QSizePolicy.Fixed, QSizePolicy.Fixed))
        
        self.width = self.sinkwidth * 2 + ncols * blocksize + (ncols + 1) * self.gridwidth
        self.height = self.sinkwidth * 2 + nrows * blocksize + (nrows + 1) * self.gridwidth
        
        self.board = TetrisBoard(nrows, ncols)

    def minimumSizeHint(self):
        return QSize(self.width, self.height)
    
    def sizeHint(self):
        return self.minimumSizeHint()
    
    def paintEvent(self, event=None):
        painter = QPainter(self)
        painter.setRenderHint(QPainter.Antialiasing)
        
        painter.fillRect(0, 0, self.width, self.height, QBrush(self.bgcolor, Qt.SolidPattern))
        
        if self.showgrid: self._draw_grid(painter)
        self._draw_sink(painter)
        
        self._draw_all_blocks(painter)
    
    def _draw_sink(self, painter):
        sink_pen = QPen(QColor(self.sinkcolor))
        sink_pen.setWidth(self.sinkwidth)
        painter.setPen(sink_pen)
        
        halfsink = self.sinkwidth / 2
        painter.drawLine(halfsink, 0, halfsink, self.height)
        painter.drawLine(   
            self.width - halfsink, 0,
            self.width - halfsink, self.height)
        painter.drawLine(0, halfsink, self.width, halfsink)
        painter.drawLine(
            0, self.height - halfsink,
            self.width, self.height - halfsink)
    
    def _draw_grid(self, painter):
        grid_pen = QPen(QColor(self.gridcolor))
        grid_pen.setWidth(self.gridwidth)
        painter.setPen(grid_pen)
        
        # combined size of a block with a single grid line
        blockgrid_size = self.blocksize + self.gridwidth
        
        # horizontal
        for row in range(self.nrows + 1):
            painter.drawLine(
                self.sinkwidth,
                self.sinkwidth + row * blockgrid_size + 1,
                self.width - 1 - self.sinkwidth,
                self.sinkwidth + row * blockgrid_size + 1)
                    
        # vertical
        for col in range(self.ncols + 1):
            painter.drawLine(
                self.sinkwidth + col * blockgrid_size + 1,
                self.sinkwidth,
                self.sinkwidth + col * blockgrid_size + 1,
                self.height - 1 - self.sinkwidth)
    
    def _draw_all_blocks(self, painter):
        board = self.board.board_with_active_figure()
        
        for row in range(self.nrows):
            for col in range(self.ncols):
                color = board[row][col]
                
                if color != 0:
                    self._draw_block(painter, row, col, color)
    
    def _draw_block(self, painter, row, col, color):
        block_pen = QPen(QColor(self.block_border_color))
        block_pen.setWidth(self.gridwidth)
        painter.setPen(block_pen)
        
        blockgrid_size = self.blocksize + self.gridwidth
        block_rect = QRect(
            self.sinkwidth + col * blockgrid_size + 1,
            self.sinkwidth + row * blockgrid_size + 1,
            blockgrid_size,
            blockgrid_size)
        
        painter.fillRect(block_rect, QBrush(color, Qt.SolidPattern))
        painter.drawRect(block_rect)
    

class MainTetrisWidget(DrawabaleTetrisBoard):
    """ The main tetris window type, with an active figure that can be moved
        around, dropped, etc. and a bunch of inactive blocks.
        Supports all the expected tetris operations, like removing completed
        rows and generating new figures.
    """
    def __init__(self, parent, nrows, ncols, blocksize, startfigure):
        super(MainTetrisWidget, self).__init__(parent, nrows, ncols, blocksize)
        
        self.board.spawn_figure(startfigure)
        
        # Keeps track of the amount of rows the active figure
        # fell in the last "drop" command. This is used to 
        # update the score.
        #
        self.last_drop_height = 0
        
    def restart(self, startfigure):
        self.board = TetrisBoard(self.nrows, self.ncols)
        self.board.spawn_figure(startfigure)
        self.update()
        
    def keyPressEvent(self, event):
        if event.key() == Qt.Key_Up:
            self.board.rotate_figure()
        elif event.key() == Qt.Key_Down:
            self.board.move_figure_down()
        elif event.key() == Qt.Key_Left:
            self.board.move_figure_left()
        elif event.key() == Qt.Key_Right:
            self.board.move_figure_right()
        elif event.key() == Qt.Key_Space:
            for i in range(self.nrows):
                if not self.board.move_figure_down():
                    self.last_drop_height = i
                    break
        else:
            return
        
        self.update()

    Result = namedtuple('Result', 'state completed_rows drop_height')

    def timer_tick(self, nextfigure):
        """ One timer tick for the tetris game.
            
            Advances the game by one step and returns a result as 
            a namedtuple: 
            
                result.state:   
                    The game state.
                        running -   The current figure was moved 
                                    down by one cell successfully.
                        newfigure - The current figure could no 
                                    longer be moved down, so a new 
                                    figure was created.
                        gameover -  The current figure could no 
                                    longer be moved down, and a 
                                    new figure could not be
                                    created.
                                    
                result.completed_rows:
                    A list of row numbers that were completed with 
                    the current figure reaching bottom. It is 
                    applicable in the "newfigure" state
                
                result.drop_height: 
                    The amount of lines the figure was dropped in 
                    the last drop.
        """
        state = 'running'
        completed_rows = []
        drop_height = 0
        
        if self.board.move_figure_down():
            state = 'running'
        else:
            completed_rows = self.board.finish_fall()
            
            if self.board.spawn_figure(nextfigure):
                state = 'newfigure'
            else:
                state = 'gameover'
            
        drop_height = self.last_drop_height
        self.last_drop_height = 0
        
        self.update()
        return self.Result(state, completed_rows, drop_height)


class TetrisPreviewWidget(DrawabaleTetrisBoard):
    """ A preview window for figures. Only shows a single figure, trying to
        center it vertically.
        Note: the success of this method depends on the actual figures that
        participate in the game. It works well for the default 7 Tetris
        figures.
    """
    def __init__(self, parent, nrows, ncols, blocksize):
        super(TetrisPreviewWidget, self).__init__(parent, nrows, ncols, blocksize)
        
        self.showgrid = False
        self.figure = None
    
    def set_figure(self, figure):
        self.figure = figure
        self.board = TetrisBoard(self.nrows, self.ncols)
        self.board.spawn_figure(figure)
        self.board.move_figure_down()
        self.board.move_figure_down()

        self.update()


class SizedButton(QPushButton):
    def __init__(self, text, size, parent=None):
        super(SizedButton, self).__init__(text, parent)
        self.size = size
        
    def sizeHint(self):
        return self.size


class StatsLabel(QLabel):
    def __init__(self, text, align_right=False, parent=None):
        super(StatsLabel, self).__init__('', parent)
        
        self.format_text = '<font size=16>%s</font>'
        self.setText(text)
        
        if align_right:
            self.setAlignment(Qt.AlignRight)
        else:
            self.setAlignment(Qt.AlignLeft)
    
    def setText(self, text):
        super(StatsLabel, self).setText(self.format_text % text)
        

GameState = Enum('running', 'paused', 'gameover')


class TetrisMainWindow(QMainWindow):
    """ The main tetris game window.
    """
    def __init__(self, parent=None):
        super(TetrisMainWindow, self).__init__(parent)
        self.setWindowTitle('PyQtris v%s' % pyqtris_version)

        self.setWindowIcon(QIcon(get_icon_pixmap()))

        self.figurebank = self.make_figure_bank()

        self.create_main_frame()
        self.create_menu()
        self.create_status_bar()
        
        self.timer = QTimer()
        self.timer_interval = 1000
        self.connect(self.timer, SIGNAL('timeout()'), self.on_timer)
        self.timer.start(self.timer_interval)
        
        self.init_highscores()
        self.restart_game()

    def on_restart(self):
        if self.state == GameState.gameover:
            self.restart_game()
        else:
            saved_state = self.state
            self.pause_game()
            
            reply = QMessageBox.question(self,
                'Restart confirmation', 
                'Are you sure you want to restart the game?',
                QMessageBox.Yes | QMessageBox.No)
            
            if reply == QMessageBox.Yes:
                self.restart_game()
            else:
                self.set_game_state(saved_state)
        
    def on_pause(self):
        if self.state == GameState.paused:
            self.resume_game()
        elif self.state == GameState.running:
            self.pause_game()

    def on_about(self):
        saved_state = self.state
        self.pause_game()
        
        dialog = AboutDialog(self)
        dialog.exec_()
        
        self.set_game_state(saved_state)
        
    def on_highscores(self):
        saved_state = self.state
        self.pause_game()
        self.show_highscores()        
        self.set_game_state(saved_state)
    
    def set_game_state(self, state):
        if state == GameState.paused:
            self.pause_game()
        elif state == GameState.running:
            self.resume_game()
        elif state == GameState.gameover:
            self.state = GameState.gameover
            self.status_text.setText('Game over!')
            self.timer.stop()
        else:
            assert False
    
    def pause_game(self):
        self.state = GameState.paused
        self.status_text.setText('Paused')
        self.pause_button.setText('Resume')
        self.timer.stop()
        
    def resume_game(self):
        self.state = GameState.running
        self.status_text.setText('Playing')
        self.pause_button.setText('Pause')
        self.timer.start(self.timer_interval)
    
    def game_over(self):
        self.state = GameState.gameover
        
        self.status_text.setText('Game over!')
        self.timer.stop()
        
        if self.game_score > self.highscores.lowest_score():
            text, ok = QInputDialog.getText(self, 
                "High score!",
                'Your name:')
            
            if ok and not text.isEmpty():
                self.highscores.add_score(text, self.game_score)
                self.save_highscores()
                self.show_highscores()
    
    def restart_game(self):
        self.game_level = 1
        self.game_lines = 0
        self.game_score = 0
        self.timer_interval = 1000
        
        self.resume_game()
        self.update_stats()
        
        self.board_widget.restart(self.figurebank.get_random())
        self.preview_figure = self.figurebank.get_random()
        self.preview_widget.set_figure(self.preview_figure)
        self.timer.start(self.timer_interval)
        
    def on_timer(self):
        if self.state == GameState.running:
            result = self.board_widget.timer_tick(self.preview_figure)
            num_rows = len(result.completed_rows)
            
            if result.state == 'gameover':
                self.game_over()
            elif result.state == 'newfigure':
                old_line_count = self.game_lines
                self.game_lines += num_rows
                score_bonus = result.drop_height + num_rows ** 2 * 30
                score_bonus = int(score_bonus * (1 + 0.1 * (self.game_level - 1)))
                self.game_score += score_bonus
                
                self.preview_figure = self.figurebank.get_random()
                self.preview_widget.set_figure(self.preview_figure)
                
                if num_rows > 0 and old_line_count % 10 + num_rows >= 10:
                    self.game_level += 1
                    self.timer_interval = 1000 - self.game_level * 100
                    if self.timer_interval < 100:
                        self.timer_interval = 100
                    self.timer.stop()
                    self.timer.start(self.timer_interval)
            
            self.update_stats()

    def update_stats(self):
        self.level_text.setText(str(self.game_level))
        self.lines_text.setText(str(self.game_lines))
        self.score_text.setText(str(self.game_score))

    def create_main_frame(self):
        self.board_widget = MainTetrisWidget(self, 
            nrows=20, 
            ncols=10, 
            blocksize=25,
            startfigure=self.figurebank.get_random())
        self.board_widget.setFocus()

        self.control_panel = QFrame(self)
        self.control_panel.setFrameStyle(QFrame.Panel | QFrame.Sunken)
        self.control_panel.setFocusPolicy(Qt.NoFocus)
        
        self.populate_control_panel()

        main_layout = QHBoxLayout()
        main_layout.addWidget(self.board_widget)
        main_layout.addWidget(self.control_panel)
        main_layout.setSizeConstraint(QLayout.SetFixedSize)
        
        main_frame = QWidget()
        main_frame.setLayout(main_layout)
        self.setCentralWidget(main_frame)
    
    def populate_control_panel(self):
        self.preview_widget = TetrisPreviewWidget(
            self.control_panel,
            nrows=7,
            ncols=7,
            blocksize=10)
        
        # preview_group is the group-box for titling the preview
        # widget. preview_box is its internal layout.
        # finally, preview_layout is used to add stretchers around
        # the group box (to keep it slim)
        #
        preview_group = QGroupBox('Preview')
        preview_box = QHBoxLayout()
        preview_box.addWidget(self.preview_widget)        
        preview_group.setLayout(preview_box)
        preview_layout = QHBoxLayout()
        preview_layout.addStretch()
        preview_layout.addWidget(preview_group)
        preview_layout.addStretch()        
        
        text_layout = QGridLayout()
        
        level_label = StatsLabel('Level')
        self.level_text = StatsLabel('1', True)
        lines_label = StatsLabel('Lines')
        self.lines_text = StatsLabel('0', True)
        score_label = StatsLabel('Score')
        self.score_text = StatsLabel('0', True)
        
        text_layout.addWidget(level_label, 0, 0)
        text_layout.addWidget(self.level_text, 0, 1)
        text_layout.addWidget(lines_label, 1, 0)
        text_layout.addWidget(self.lines_text, 1, 1)
        text_layout.addWidget(score_label, 2, 0)
        text_layout.addWidget(self.score_text, 2, 1)
        
        self.pause_button = SizedButton('&Pause', QSize(90, 40))
        self.pause_button.setFocusPolicy(Qt.NoFocus)
        self.connect(self.pause_button, SIGNAL('clicked()'), self.on_pause)
        
        self.restart_button = SizedButton('&New game', QSize(90, 40))
        self.restart_button.setFocusPolicy(Qt.NoFocus)
        self.connect(self.restart_button, SIGNAL('clicked()'), self.on_restart)
        
        button_layout = QHBoxLayout()
        button_layout.addWidget(self.pause_button)
        button_layout.addSpacing(40)
        button_layout.addWidget(self.restart_button)
        
        vbox = QVBoxLayout()
        vbox.addSpacing(15)
        vbox.addLayout(preview_layout)
        vbox.addSpacing(40)
        vbox.addLayout(text_layout)
        vbox.addSpacing(40)
        vbox.addLayout(button_layout)
        vbox.addStretch()
        
        hbox = QHBoxLayout()
        hbox.addStretch()
        hbox.addLayout(vbox)
        hbox.addStretch()
        
        self.control_panel.setLayout(hbox)
    
    def create_status_bar(self):
        self.status_text = QLabel('Playing')
        self.statusBar().addWidget(self.status_text, 1)
        
    def create_menu(self):
        self.game_menu = self.menuBar().addMenu("&Game")
        self.help_menu = self.menuBar().addMenu("&Help")
        
        new_game_action = self.create_action("&New",
            shortcut='Ctrl+N', slot=self.on_restart,
            tip='New game')
        pause_game_action = self.create_action("&Pause",
            shortcut='Ctrl+P', slot=self.on_pause,
            tip='Pause game')
        highscores_action = self.create_action("&High Scores",
            shortcut='Ctrl+H', slot=self.on_highscores,
            tip='Pause game')
        quit_action = self.create_action("&Quit",
            shortcut='Ctrl+Q', slot=self.close,
            tip='Quit')
        
        about_action = self.create_action("&About",
            shortcut='F1', slot=self.on_about,
            tip='About PyQtris')
        
        self.add_actions(self.game_menu,
            (   new_game_action, pause_game_action, None, 
                highscores_action, None, quit_action))
        
        self.add_actions(self.help_menu, (about_action,))
        
    def add_actions(self, target, actions):
        for action in actions:
            if action is None:
                target.addSeparator()
            else:
                target.addAction(action)

    def create_action(  self, text, slot=None, shortcut=None, 
                        icon=None, tip=None, checkable=False, 
                        signal="triggered()"):
        action = QAction(text, self)
        if icon is not None:
            action.setIcon(QIcon(":/%s.png" % icon))
        if shortcut is not None:
            action.setShortcut(shortcut)
        if tip is not None:
            action.setToolTip(tip)
            action.setStatusTip(tip)
        if slot is not None:
            self.connect(action, SIGNAL(signal), slot)
        if checkable:
            action.setCheckable(True)
        return action

    def make_figure_bank(self):
        # The standard 7 Tetris figures, in the orientation
        # in which they appear from the top of the screen
        #
        
        #  oo
        # oO
        S = Figure([(0,0), (-1,0), (0,-1), (-1,1)], color=QColor(122, 197, 205))
        
        # oo
        #  Oo
        Z = Figure([(0,0), (0,1), (-1,-1), (-1,0)], color=QColor(0, 205, 0))
        
        # oOo
        #  o
        T = Figure([(0,0), (0,1), (0,-1), (1,0)], color=QColor(238, 238, 0))
        
        # oOoo
        #
        I = Figure([(0,0), (0,-1), (0,1), (0,2)], color=QColor(238, 118, 33))
        
        # oo
        # Oo
        O = Figure([(0,0), (-1,0), (-1,1), (0,1)], rotatable=False, color=QColor(238, 44, 44))
        
        # oOo
        # o
        L = Figure([(0,0), (0,-1), (0,1), (1,-1)], color=QColor(0, 0, 225))
        
        # oOo
        #   o
        J = Figure([(0,0), (0,-1), (0,1), (1,1)], color=QColor(148, 0, 211))
        
        return FigureBank([S, Z, T, I, O, L, J])

    def keyPressEvent(self, event):
        if self.state == GameState.running:
            self.board_widget.keyPressEvent(event)
            
    HS_FILENAME = 'pyqtris_highscores'
    
    def init_highscores(self):
        self.highscores = HighScores(10)
        
        if os.path.exists(self.HS_FILENAME):
            self.highscores.load_from_file(self.HS_FILENAME)

    def save_highscores(self):
        self.highscores.save_to_file(self.HS_FILENAME)

    def show_highscores(self):
        dialog = HighscoresDialog(self.highscores.get_list(), self)
        dialog.exec_()


if __name__ == "__main__":
    app = QApplication(sys.argv)
    form = TetrisMainWindow()
    form.show()
    app.exec_()

