# -*- coding: utf-8 -*-

"""
Module implementing DlgBrowseModels.
"""

from PyQt5.QtCore import pyqtSlot
from PyQt5.QtWidgets import QDialog, QFileDialog

from .Ui_BrowseModels import Ui_Dialog
import logging

class DlgBrowseModels(QDialog, Ui_Dialog):
    """
    Class documentation goes here.
    """
    def __init__(self, parent=None):
        """
        Constructor
        
        @param parent reference to the parent widget
        @type QWidget
        """
        super(DlgBrowseModels, self).__init__(parent)
        self.setupUi(self)
        
        self.logger = logging.getLogger(r'Datcomlogger')
    
    @pyqtSlot()
    def on_listWidget_Models_itemSelectionChanged(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        self.logger.info(r"点击了模型")
    
    @pyqtSlot(bool)
    def on_pushButton_ChoiseDir_toggled(self, checked):
        """
        Slot documentation goes here.
        
        @param checked DESCRIPTION
        @type bool
        """
        # TODO: not implemented yet
        modelDir = QFileDialog.getExistingDirectory("打开模型目录", '.')
        self.logger.info("Try 打开模型目录")
        if modelDir is None:
            self.logger.error("无效的目录")
        self.textEdit_Dir.setText(modelDir)
        #raise NotImplementedError
