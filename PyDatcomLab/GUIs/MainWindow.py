# -*- coding: utf-8 -*-

"""
Module implementing DatcomMainWindow.
"""

from PyQt5.QtCore import pyqtSlot
from PyQt5.QtWidgets import QMainWindow, QLabel

from .Ui_MainWindow import Ui_MainWindow


class DatcomMainWindow(QMainWindow, Ui_MainWindow):
    """
    Class documentation goes here.
    """
    def __init__(self, parent=None):
        """
        Constructor
        
        @param parent reference to the parent widget
        @type QWidget
        """
        super(DatcomMainWindow, self).__init__(parent)
        self.setupUi(self)
        #添加日志系同
        self.logger = None
    
    @pyqtSlot()
    def on_actionAbout_triggered(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        print('Test!')
        #raise NotImplementedError
    
    @pyqtSlot()
    def on_actionBrowse_triggered(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        raise NotImplementedError
    
    @pyqtSlot()
    def on_actionCopyModel_triggered(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        raise NotImplementedError
    
    @pyqtSlot()
    def on_actionCopyCASE_triggered(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        raise NotImplementedError
    
    @pyqtSlot()
    def on_actionImportModel_triggered(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        raise NotImplementedError
    
    @pyqtSlot()
    def on_actionDocs_triggered(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        raise NotImplementedError
    
    @pyqtSlot(str)
    def on_statusBar_windowTitleChanged(self, title):
        """
        Slot documentation goes here.
        
        @param title DESCRIPTION
        @type str
        """
        # TODO: not implemented yet
        raise NotImplementedError
    
    @pyqtSlot()
    def on_actionNewProject_triggered(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        label = QLabel( 'Action : 创建项目!')
        self.statusBar.addWidget(label)
        #raise NotImplementedError
