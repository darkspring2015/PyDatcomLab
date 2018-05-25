# -*- coding: utf-8 -*-

"""
Module implementing logForm.
"""

from PyQt5.QtCore import pyqtSlot, QSize
from PyQt5 import QtWidgets

from .Ui_logForm import Ui_Form
from PyDatcomLab.GUIs import  Logger4GUI

import logging

class logForm(QtWidgets.QWidget, Ui_Form):
    """
    Class documentation goes here.
    """
    def __init__(self, parent=None):
        """
        Constructor
        
        @param parent reference to the parent widget
        @type QWidget
        """
        super(logForm, self).__init__(parent)
        self.setupUi(self)
        
        #设置按钮信息
        self.dockWidget.setTitleBarWidget(QtWidgets.QWidget())  #关闭
        self.dockWidget.resize(QSize(174, 25))
        self.toolButton_Clear.setDefaultAction(self.actionClearLog)
        self.toolButton_Error.setDefaultAction(self.actionErrorLevel)
        self.toolButton_Debug.setDefaultAction(self.actionDebugLevel)
        self.toolButton_Info.setDefaultAction(self.actionInfoLevel)
        
        #self.initDock()
        #消除Dock的Title
        #导入logger系统
        self.logger, self.logHandler = Logger4GUI.getLogger(self.showLog)
    
    def showLog(self, val):
        """
        响应信号 LoggerSignal
        """        
        self.plainTextEdit_log.appendPlainText(val)    
        
    def getLogger(self):
        return self.logger
    
    @pyqtSlot()
    def on_actionClearLog_triggered(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        self.plainTextEdit_log.setPlainText(r'')
        self.logger.info(r'清理日志窗口')
    
    @pyqtSlot()
    def on_actionDebugLevel_triggered(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        self.logHandler.setLevel(logging.DEBUG)
        self.logger.info(r'设置日志级别:DEBUG')
        self.logger.debug(r'设置日志级别:DEBUG')
    
    @pyqtSlot()
    def on_actionInfoLevel_triggered(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        self.logHandler.setLevel(logging.INFO)
        self.logger.info(r'设置日志级别:INFO')

    
    @pyqtSlot()
    def on_actionErrorLevel_triggered(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        self.logHandler.setLevel(logging.ERROR)
        self.logger.info(r'设置日志级别:ERROR')
        self.logger.error(r'设置日志级别:ERROR')

