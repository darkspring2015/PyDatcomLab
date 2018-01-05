# -*- coding: utf-8 -*-

"""
Module implementing PlaneConfiguration.
"""

from PyQt5.QtCore import pyqtSlot
from PyQt5.QtWidgets import QDialog

from .Ui_PlaneConfiguration import Ui_Dialog

from PyDatcomLab.GUIs.PlaneConfiguration import BODY, HTPLNF,  SYNTHS, VTPLNF,WGPLNF

import logging

class PlaneConfiguration(QDialog, Ui_Dialog):
    """
    Class documentation goes here.
    """
    def __init__(self, parent=None):
        """
        Constructor
        
        @param parent reference to the parent widget
        @type QWidget
        """
        super(PlaneConfiguration, self).__init__(parent)
        self.setupUi(self)
        
        self.logger = logging.getLogger(r'Datcomlogger')
        self.Initialize()
       
        
    def Initialize(self):
        """
        初始化所有的page页
        """
        self.tabWidget_Configuration.clear()
        self.tabWidget_Configuration.addTab( BODY.BODY(), r"机体")
        self.tabWidget_Configuration.addTab( SYNTHS.SYNTHS(), r"SYNTHS")
        self.tabWidget_Configuration.addTab( VTPLNF.VTPLNF(), r"VTPLNE")
        self.tabWidget_Configuration.addTab( HTPLNF.HTPLNF(), r"HTPLNF")
        self.tabWidget_Configuration.addTab( WGPLNF.WGPLNF(), r"WGPLNF")
        
