# -*- coding: utf-8 -*-

"""
Module implementing ImageTips.
"""

from PyQt5.QtCore import pyqtSlot
from PyQt5.QtWidgets import QWidget

from .Ui_ImageTips import Ui_Form
import logging

class ImageTips(QWidget, Ui_Form):
    """
    Class documentation goes here.
    """
    def __init__(self, parent=None):
        """
        Constructor
        
        @param parent reference to the parent widget
        @type QWidget
        """
        super(ImageTips, self).__init__(parent)
        self.setupUi(self)
        
        self.logger = logging.getLogger(r'Datcomlogger')
    
    @pyqtSlot(str)
    def on_imageTips_windowTitleChanged(self, title):
        """
        Slot documentation goes here.
        
        @param title DESCRIPTION
        @type str
        """
        # TODO: not implemented yet
        raise NotImplementedError
