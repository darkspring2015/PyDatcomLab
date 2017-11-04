# -*- coding: utf-8 -*-

"""
Module implementing ProjectsManager.
"""

from PyQt5.QtCore import pyqtSlot
from PyQt5.QtWidgets import QMainWindow

from .Ui_ProjectsManager import Ui_ProjectsMainWindow

import logging


class ProjectsManager(QMainWindow, Ui_ProjectsMainWindow):
    """
    Class documentation goes here.
    """
    def __init__(self, parent=None):
        """
        Constructor
        
        @param parent reference to the parent widget
        @type QWidget
        """
        super(ProjectsManager, self).__init__(parent)
        self.setupUi(self)
        
        self.logger = logging.getLogger(r'Datcomlogger')
        
        
    def  BindingAction(self, tActions = []):
        """
        将父窗口的Action添加到控件
        """
        for act in tActions:
            self.toolBar.addAction(act)
            self.logger.info('添加Action%s'%act.text())
            
    def BindingModel(self, tModel = None):
        """
        重新绑定项目数据
        """
        #断言
        if tModel is None :
            self.logger.error(r'没有Model')
            return None
        
        #开始绑定
        self.treeView_Projects.setModel(tModel)
        self.logger.info('绑定数据成功')

