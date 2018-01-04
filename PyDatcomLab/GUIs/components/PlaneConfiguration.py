# -*- coding: utf-8 -*-

"""
Module implementing PlaneConfiguration.
"""

from PyQt5.QtCore import pyqtSlot
from PyQt5.QtWidgets import QDialog

from .Ui_PlaneConfiguration import Ui_Dialog




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
        
        
    def Initialize(self):
        """
        初始化所有的page页
        """
        
