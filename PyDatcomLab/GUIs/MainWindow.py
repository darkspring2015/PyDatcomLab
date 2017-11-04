# -*- coding: utf-8 -*-

"""
Module implementing DatcomMainWindow.
"""

from PyQt5.QtCore import pyqtSlot
from PyQt5.QtWidgets import QMainWindow, QLabel

from PyQt5 import  QtCore,  QtWidgets

from .Ui_MainWindow import Ui_MainWindow
from PyDatcomLab.GUIs  import  Logger4GUI, logForm



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
        
        #配置MainWindow的Dock系统信息
        self.setDockNestingEnabled(True)
        #初始化存储Dock配置信息的位置
        self.docksConfig={}  #key 是dock的name，内容是{}，包括
        self.docksConfig['DefaulLeft'] = self.dockWidget_Left
        self.docksConfig['DefaulRight'] = self.dockWidget_Right
        self.docksConfig['DefaulBottom'] = self.dockWidget_Bottom
        
        #添加日志系统
        self.logForm = logForm.logForm()
        self.logger = self.logForm.getLogger()
        #添加日志窗口到下方dack中
        #self.singal
        self.on_actionLogWindow_triggered()

    
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
    
    @pyqtSlot()
    def on_actionLogWindow_triggered(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        if '日志窗口'  not in self.docksConfig.keys():
            #创建日志窗口并添加
            self.dock_log = QtWidgets.QDockWidget('日志窗口', self)  
            self.dock_log.setAllowedAreas(QtCore.Qt.LeftDockWidgetArea|QtCore.Qt.RightDockWidgetArea|QtCore.Qt.BottomDockWidgetArea)          
            self.dock_log.setWidget(self.logForm)  
            self.addDockWidget(QtCore.Qt.BottomDockWidgetArea, self.dock_log)  
            self.docksConfig['日志窗口'] = self.dock_log  
        else:
            if self.docksConfig['日志窗口'].isHidden() :
                self.docksConfig['日志窗口'].show()
            else:
                self.docksConfig['日志窗口'].hide()
            
            
            

            
