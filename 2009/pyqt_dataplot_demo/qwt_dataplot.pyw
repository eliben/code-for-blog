"""
Series of data are loaded from a .csv file, and their names are
displayed in a checkable list view. The user can select the series
it wants from the list and plot them on a Qwt plot.

Use the sample .csv file that comes with the script for an example
of data series.

Eli Bendersky (eliben@gmail.com)
License: this code is in the public domain
Last modified: 23.05.2009
"""
import sys, os, csv
from PyQt4.QtCore import *
from PyQt4.QtGui import *

import PyQt4.Qwt5 as Qwt
import random


COLORS = ['brown', 'cyan', 'blue', 'red', 'limegreen', 'navy']


class Spy(QObject):    
    def __init__(self, parent):
        QObject.__init__(self, parent)
        parent.setMouseTracking(True)
        parent.installEventFilter(self)

    def eventFilter(self, _, event):
        if event.type() == QEvent.MouseMove:
            self.emit(SIGNAL("MouseMove"), event.pos())
        return False


class Form(QMainWindow):
    def __init__(self, parent=None):
        super(Form, self).__init__(parent)
        self.setWindowTitle('PyQt & PyQwt demo: Data plotting')
        self.resize(600, 400)
        
        self.data = DataHolder()
        self.series_list_model = QStandardItemModel()

        self.create_menu()
        self.create_main_frame()
        self.create_status_bar()
        
        self.update_ui()
        self.on_show()

    def load_file(self, filename=None):
        filename = QFileDialog.getOpenFileName(self,
            'Open a data file', '.', 'CSV files (*.csv);;All Files (*.*)')
        
        if filename:
            self.data.load_from_file(filename)
            self.fill_series_list(self.data.series_names())
            self.update_ui()
    
    def update_ui(self):
        if self.data.series_count() > 0 and self.data.series_len() > 0:
            self.from_spin.setValue(0)
            self.to_spin.setValue(self.data.series_len() - 1)
            
            for w in [self.from_spin, self.to_spin]:
                w.setRange(0, self.data.series_len() - 1)
                w.setEnabled(True)
        else:
            for w in [self.from_spin, self.to_spin]:
                w.setEnabled(False)
    
    def on_show(self):
        self.plot.clear()
        self.plot.setAxisTitle(Qwt.QwtPlot.xBottom, 'x -->')
        self.plot.setAxisTitle(Qwt.QwtPlot.yLeft, 'y -->')
        
        grid = Qwt.QwtPlotGrid()
        pen = QPen(Qt.DotLine)
        pen.setColor(Qt.black)
        pen.setWidth(0)
        grid.setPen(pen)
        grid.attach(self.plot)
        
        has_series = False
        
        coloriter = iter(COLORS)
        
        for row in range(self.series_list_model.rowCount()):
            model_index = self.series_list_model.index(row, 0)
            checked = self.series_list_model.data(model_index,
                Qt.CheckStateRole) == QVariant(Qt.Checked)
            name = str(self.series_list_model.data(model_index).toString())
            
            if checked:
                has_series = True
                
                x_from = self.from_spin.value()
                x_to = self.to_spin.value()
                series = self.data.get_series_data(name)[x_from:x_to + 1]
                
                color = QColor(coloriter.next())
                
                curve = Qwt.QwtPlotCurve(name)
                pen = QPen(color)
                pen.setWidth(2)
                curve.setPen(pen)
                curve.setData(range(len(series)), series)
                
                curve.setSymbol(Qwt.QwtSymbol(Qwt.QwtSymbol.Ellipse,
                    QBrush(color),
                    QPen(color),
                    QSize(5, 5)))
                
                curve.attach(self.plot)
        
        if has_series and self.legend_cb.isChecked():
            self.plot.insertLegend(Qwt.QwtLegend(), Qwt.QwtPlot.RightLegend)
        else:
            self.plot.insertLegend(None)
            
        self.plot.replot()

    def on_about(self):
        msg = __doc__
        QMessageBox.about(self, "About the demo", msg.strip())

    def fill_series_list(self, names):
        self.series_list_model.clear()
        
        for name in names:
            item = QStandardItem(name)
            item.setCheckState(Qt.Unchecked)
            item.setCheckable(True)
            self.series_list_model.appendRow(item)
    
    def create_main_frame(self):
        self.main_frame = QWidget()
        
        plot_frame = QWidget()
        
        self.plot = Qwt.QwtPlot(self)
        self.plot.setCanvasBackground(Qt.white)
        
        log_label = QLabel("Data series:")
        self.series_list_view = QListView()
        self.series_list_view.setModel(self.series_list_model)
        
        spin_label1 = QLabel('X from')
        self.from_spin = QSpinBox()
        spin_label2 = QLabel('to')
        self.to_spin = QSpinBox()
        
        spins_hbox = QHBoxLayout()
        spins_hbox.addWidget(spin_label1)
        spins_hbox.addWidget(self.from_spin)
        spins_hbox.addWidget(spin_label2)
        spins_hbox.addWidget(self.to_spin)
        spins_hbox.addStretch(1)
        
        self.legend_cb = QCheckBox("Show L&egend")
        self.legend_cb.setChecked(False)
        
        self.show_button = QPushButton("&Show")
        self.connect(self.show_button, SIGNAL('clicked()'), self.on_show)

        left_vbox = QVBoxLayout()
        left_vbox.addWidget(self.plot)

        right_vbox = QVBoxLayout()
        right_vbox.addWidget(log_label)
        right_vbox.addWidget(self.series_list_view)
        right_vbox.addLayout(spins_hbox)
        right_vbox.addWidget(self.legend_cb)
        right_vbox.addWidget(self.show_button)
        right_vbox.addStretch(1)
        
        hbox = QHBoxLayout()
        hbox.addLayout(left_vbox, 3)
        hbox.addLayout(right_vbox, 1)
        self.main_frame.setLayout(hbox)

        self.setCentralWidget(self.main_frame)
        
    def showCoordinates(self, position):
        self.statusBar().showMessage(
            'x = %6.2f, y = %6.2f'
            % (self.plot.invTransform(Qwt.QwtPlot.xBottom, position.x()),
               self.plot.invTransform(Qwt.QwtPlot.yLeft, position.y())))

    def create_status_bar(self):
        self.connect(Spy(self.plot.canvas()),
                     SIGNAL("MouseMove"),
                     self.showCoordinates) 

        self.statusBar().showMessage(
            'Mouse movements in the plot canvas are shown in the status bar')

    def create_menu(self):        
        self.file_menu = self.menuBar().addMenu("&File")
        
        load_action = self.create_action("&Load file",
            shortcut="Ctrl+L", slot=self.load_file, tip="Load a file")
        quit_action = self.create_action("&Quit", slot=self.close, 
            shortcut="Ctrl+Q", tip="Close the application")
        
        self.add_actions(self.file_menu, 
            (load_action, None, quit_action))
            
        self.help_menu = self.menuBar().addMenu("&Help")
        about_action = self.create_action("&About", 
            shortcut='F1', slot=self.on_about, 
            tip='About the demo')
        
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


class DataHolder(object):
    """ Just a thin wrapper over a dictionary that holds integer 
        data series. Each series has a name and a list of numbers 
        as its data. The length of all series is assumed to be
        the same.
        
        The series can be read from a CSV file, where each line
        is a separate series. In each series, the first item in 
        the line is the name, and the rest are data numbers.
    """
    def __init__(self, filename=None):
        self.load_from_file(filename)
    
    def load_from_file(self, filename=None):
        self.data = {}
        self.names = []
        
        if filename:
            for line in csv.reader(open(filename, 'rb')):
                self.names.append(line[0])
                self.data[line[0]] = map(int, line[1:])
                self.datalen = len(line[1:])
    
    def series_names(self):
        """ Names of the data series
        """
        return self.names
    
    def series_len(self):
        """ Length of a data series
        """
        return self.datalen
    
    def series_count(self):
        return len(self.data)

    def get_series_data(self, name):
        return self.data[name]


def main():
    app = QApplication(sys.argv)
    form = Form()
    form.show()
    app.exec_()


if __name__ == "__main__":
    main()
    
    
    
    
    
     