# -*- coding: utf-8 -*-

"""
Module implementing WGPLNF.
"""

from PyQt5.QtCore import pyqtSlot
from PyQt5.QtWidgets import QWidget

from .Ui_WGPLNF import Ui_Form


class WGPLNF(QWidget, Ui_Form):
    """
    Class documentation goes here.
    """
    def __init__(self, parent=None):
        """
        Constructor
        
        @param parent reference to the parent widget
        @type QWidget
        """
        super(WGPLNF, self).__init__(parent)
        self.setupUi(self)
        self.initXMLModel()
        
    def initXMLModel(self):
        """
        初始化本节点的xml描述文档
        """
        self.docxml = None
        
    def getXML(self):
        """
        返回本节点的xml描述文件
        """
        return self.docxml
    
    def setXML(self, tXML):
        """
        将界面的内容设置成tXML对应的内容
        """
