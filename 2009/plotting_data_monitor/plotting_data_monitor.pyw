""" 
A simple demonstration of a serial port monitor that plots live
data using PyQwt.

The monitor expects to receive single-byte data packets on the 
serial port. Each received byte is understood as a temperature
reading and is shown on a live chart.

When the monitor is active, you can turn the 'Update speed' knob
to control the frequency of screen updates.

Eli Bendersky (eliben@gmail.com)
License: this code is in the public domain
Last modified: 07.08.2009
"""
import random, sys
from PyQt4.QtCore import *
from PyQt4.QtGui import *
import PyQt4.Qwt5 as Qwt
import Queue

from com_monitor import ComMonitorThread
from eblib.serialutils import full_port_name, enumerate_serial_ports
from eblib.utils import get_all_from_queue, get_item_from_queue
from livedatafeed import LiveDataFeed


class PlottingDataMonitor(QMainWindow):
    def __init__(self, parent=None):
        super(PlottingDataMonitor, self).__init__(parent)
        
        self.monitor_active = False
        self.com_monitor = None
        self.com_data_q = None
        self.com_error_q = None
        self.livefeed = LiveDataFeed()
        self.temperature_samples = []
        self.timer = QTimer()
        
        self.create_menu()
        self.create_main_frame()
        self.create_status_bar()
        
    def make_data_box(self, name):
        label = QLabel(name)
        qle = QLineEdit()
        qle.setEnabled(False)
        qle.setFrame(False)
        return (label, qle)
        
    def create_plot(self):
        plot = Qwt.QwtPlot(self)
        plot.setCanvasBackground(Qt.black)
        plot.setAxisTitle(Qwt.QwtPlot.xBottom, 'Time')
        plot.setAxisScale(Qwt.QwtPlot.xBottom, 0, 10, 1)
        plot.setAxisTitle(Qwt.QwtPlot.yLeft, 'Temperature')
        plot.setAxisScale(Qwt.QwtPlot.yLeft, 0, 250, 40)
        plot.replot()
        
        curve = Qwt.QwtPlotCurve('')
        curve.setRenderHint(Qwt.QwtPlotItem.RenderAntialiased)
        pen = QPen(QColor('limegreen'))
        pen.setWidth(2)
        curve.setPen(pen)
        curve.attach(plot)
        
        return plot, curve

    def create_thermo(self):
        thermo = Qwt.QwtThermo(self)
        thermo.setPipeWidth(6)
        thermo.setRange(0, 120)
        thermo.setAlarmLevel(80)
        thermo.setAlarmEnabled(True)
        thermo.setFillColor(Qt.green)
        thermo.setAlarmColor(Qt.red)
        thermo.setOrientation(Qt.Horizontal, Qwt.QwtThermo.BottomScale)
        
        return thermo

    def create_knob(self):
        knob = Qwt.QwtKnob(self)
        knob.setRange(0, 20, 0, 1)
        knob.setScaleMaxMajor(10)
        knob.setKnobWidth(50)
        knob.setValue(10)
        return knob

    def create_status_bar(self):
        self.status_text = QLabel('Monitor idle')
        self.statusBar().addWidget(self.status_text, 1)

    def create_main_frame(self):
        # Port name
        #
        portname_l, self.portname = self.make_data_box('COM Port:')
        
        portname_layout = QHBoxLayout()
        portname_layout.addWidget(portname_l)
        portname_layout.addWidget(self.portname, 0)
        portname_layout.addStretch(1)
        portname_groupbox = QGroupBox('COM Port')
        portname_groupbox.setLayout(portname_layout)
        
        # Plot and thermo
        #
        self.plot, self.curve = self.create_plot()
        self.thermo = self.create_thermo()
        
        thermo_l = QLabel('Average')
        thermo_layout = QHBoxLayout()
        thermo_layout.addWidget(thermo_l)
        thermo_layout.addWidget(self.thermo)
        thermo_layout.setSpacing(5)
        
        self.updatespeed_knob = self.create_knob()
        self.connect(self.updatespeed_knob, SIGNAL('valueChanged(double)'),
            self.on_knob_change)
        self.knob_l = QLabel('Update speed = %s (Hz)' % self.updatespeed_knob.value())
        self.knob_l.setAlignment(Qt.AlignTop | Qt.AlignHCenter)
        knob_layout = QVBoxLayout()
        knob_layout.addWidget(self.updatespeed_knob)
        knob_layout.addWidget(self.knob_l)
        
        plot_layout = QVBoxLayout()
        plot_layout.addWidget(self.plot)
        plot_layout.addLayout(thermo_layout)
        plot_layout.addLayout(knob_layout)
        
        plot_groupbox = QGroupBox('Temperature')
        plot_groupbox.setLayout(plot_layout)
        
        # Main frame and layout
        #
        self.main_frame = QWidget()
        main_layout = QVBoxLayout()
        main_layout.addWidget(portname_groupbox)
        main_layout.addWidget(plot_groupbox)
        main_layout.addStretch(1)
        self.main_frame.setLayout(main_layout)
        
        self.setCentralWidget(self.main_frame)
        self.set_actions_enable_state()

    def create_menu(self):
        self.file_menu = self.menuBar().addMenu("&File")
        
        selectport_action = self.create_action("Select COM &Port...",
            shortcut="Ctrl+P", slot=self.on_select_port, tip="Select a COM port")
        self.start_action = self.create_action("&Start monitor",
            shortcut="Ctrl+M", slot=self.on_start, tip="Start the data monitor")
        self.stop_action = self.create_action("&Stop monitor",
            shortcut="Ctrl+T", slot=self.on_stop, tip="Stop the data monitor")
        exit_action = self.create_action("E&xit", slot=self.close, 
            shortcut="Ctrl+X", tip="Exit the application")
        
        self.start_action.setEnabled(False)
        self.stop_action.setEnabled(False)
        
        self.add_actions(self.file_menu, 
            (   selectport_action, self.start_action, self.stop_action,
                None, exit_action))
            
        self.help_menu = self.menuBar().addMenu("&Help")
        about_action = self.create_action("&About", 
            shortcut='F1', slot=self.on_about, 
            tip='About the monitor')
        
        self.add_actions(self.help_menu, (about_action,))

    def set_actions_enable_state(self):
        if self.portname.text() == '':
            start_enable = stop_enable = False
        else:
            start_enable = not self.monitor_active
            stop_enable = self.monitor_active
        
        self.start_action.setEnabled(start_enable)
        self.stop_action.setEnabled(stop_enable)

    def on_about(self):
        msg = __doc__
        QMessageBox.about(self, "About the demo", msg.strip())
    
    def on_select_port(self):
        ports = list(enumerate_serial_ports())
        if len(ports) == 0:
            QMessageBox.critical(self, 'No ports',
                'No serial ports found')
            return
        
        item, ok = QInputDialog.getItem(self, 'Select a port',
                    'Serial port:', ports, 0, False)
        
        if ok and not item.isEmpty():
            self.portname.setText(item)            
            self.set_actions_enable_state()

    def on_stop(self):
        """ Stop the monitor
        """
        if self.com_monitor is not None:
            self.com_monitor.join(0.01)
            self.com_monitor = None

        self.monitor_active = False
        self.timer.stop()
        self.set_actions_enable_state()
        
        self.status_text.setText('Monitor idle')
    
    def on_start(self):
        """ Start the monitor: com_monitor thread and the update
            timer
        """
        if self.com_monitor is not None or self.portname.text() == '':
            return
        
        self.data_q = Queue.Queue()
        self.error_q = Queue.Queue()
        self.com_monitor = ComMonitorThread(
            self.data_q,
            self.error_q,
            full_port_name(str(self.portname.text())),
            38400)
        self.com_monitor.start()
        
        com_error = get_item_from_queue(self.error_q)
        if com_error is not None:
            QMessageBox.critical(self, 'ComMonitorThread error',
                com_error)
            self.com_monitor = None

        self.monitor_active = True
        self.set_actions_enable_state()
        
        self.timer = QTimer()
        self.connect(self.timer, SIGNAL('timeout()'), self.on_timer)
        
        update_freq = self.updatespeed_knob.value()
        if update_freq > 0:
            self.timer.start(1000.0 / update_freq)
        
        self.status_text.setText('Monitor running')
    
    def on_timer(self):
        """ Executed periodically when the monitor update timer
            is fired.
        """
        self.read_serial_data()
        self.update_monitor()

    def on_knob_change(self):
        """ When the knob is rotated, it sets the update interval
            of the timer.
        """
        update_freq = self.updatespeed_knob.value()
        self.knob_l.setText('Update speed = %s (Hz)' % self.updatespeed_knob.value())

        if self.timer.isActive():
            update_freq = max(0.01, update_freq)
            self.timer.setInterval(1000.0 / update_freq)

    def update_monitor(self):
        """ Updates the state of the monitor window with new 
            data. The livefeed is used to find out whether new
            data was received since the last update. If not, 
            nothing is updated.
        """
        if self.livefeed.has_new_data:
            data = self.livefeed.read_data()
            
            self.temperature_samples.append(
                (data['timestamp'], data['temperature']))
            if len(self.temperature_samples) > 100:
                self.temperature_samples.pop(0)
            
            xdata = [s[0] for s in self.temperature_samples]
            ydata = [s[1] for s in self.temperature_samples]
            
            avg = sum(ydata) / float(len(ydata))
                
            self.plot.setAxisScale(Qwt.QwtPlot.xBottom, xdata[0], max(20, xdata[-1]))
            self.curve.setData(xdata, ydata)
            self.plot.replot()
            
            self.thermo.setValue(avg)
            
    def read_serial_data(self):
        """ Called periodically by the update timer to read data
            from the serial port.
        """
        qdata = list(get_all_from_queue(self.data_q))
        if len(qdata) > 0:
            data = dict(timestamp=qdata[-1][1], 
                        temperature=ord(qdata[-1][0]))
            self.livefeed.add_data(data)
    
    # The following two methods are utilities for simpler creation
    # and assignment of actions
    #
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


def main():
    app = QApplication(sys.argv)
    form = PlottingDataMonitor()
    form.show()
    app.exec_()


if __name__ == "__main__":
    main()
    
    

