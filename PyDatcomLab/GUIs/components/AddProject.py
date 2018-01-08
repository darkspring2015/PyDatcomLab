# -*- coding: utf-8 -*-

"""
Module implementing AddProject.
"""

from PyQt5.QtCore import pyqtSlot
from PyQt5.QtWidgets import QDialog, QFileDialog, QMessageBox

import os
import logging

from .Ui_AddProject import Ui_Dialog


class AddProject(QDialog, Ui_Dialog):
    """
    Class documentation goes here.
    """
    def __init__(self, parent=None):
        """
        Constructor
        
        @param parent reference to the parent widget
        @type QWidget
        """
        super(AddProject, self).__init__(parent)
        self.setupUi(self)
        #初始化日志系统
        self.logger = logging.getLogger(r'Datcomlogger')
    
    @pyqtSlot()
    def on_pushButton_ChoiseDirectory_clicked(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        prjDir = QFileDialog.getExistingDirectory(self,"选择项目目录", '.')
        if prjDir is "":
            self.logger.error("没有选择有效的目录")
            return
        else:
            self.Path.setText(prjDir)
        
    
    @pyqtSlot()
    def on_pushButton_Create_clicked(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        if self.projectName.text() == "":
            QMessageBox.information( None,"请完成配置", '项目名称不能为空')
            return
        if not os.path.exists(self.Path.text()):
            QMessageBox.information( None,"请完成配置", '请选择正确的项目目录')
            return
            
        #开始创建项目结构的逻辑
#        baseDir = os.path.join(os.path.abspath(self.Path.text()), self.projectName.text())
#        try :
#            os.mkdir(baseDir)
#            os.makedirs(os.path.join(baseDir, r'3DModels')) #存放3D模型所需信息的目录
#            os.makedirs(os.path.join(baseDir, r'Reports'))  #存放结果文件的目录
#            os.makedirs(os.path.join(baseDir, r'Problems')) #存放算例的目录
#            os.makedirs(os.path.join(baseDir, r'Problems', r'defaultGroup')) #第一个算例组
#            
#        except IOError:
#            self.logger.error(u'无法创建项目目录！dir:%s ,name:%s'%(self.Path.text(), self.projectName.text()))
       
        self.close()
        
        
    def getData(self):
        """
        """
        return {'ProjectName':self.projectName.text(), 
        'Path':self.Path.text(), 
        'Describe':self.prjDescribe.toPlainText()
        }
