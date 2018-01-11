# -*- coding: utf-8 -*-

"""
Module implementing NewModelDlg.
"""

from PyQt5.QtCore import pyqtSlot
from PyQt5.QtWidgets import QDialog, QFileDialog, QMessageBox

from .Ui_NewModel import Ui_Dialog
import logging

from PyDatcomLab.Core.dcModel import dcModel 

#from xml.etree import ElementTree  as ET
import os

class NewModelDlg(QDialog, Ui_Dialog):
    """
    Class documentation goes here.
    """
    def __init__(self, parent=None):
        """
        Constructor
        
        @param parent reference to the parent widget
        @type QWidget
        """
        super(NewModelDlg, self).__init__(parent)
        self.setupUi(self)
        
        #日志系统        
        self.logger = logging.getLogger(r'Datcomlogger')
        self.ext = '.dcxml'
        self.ModelName = "test"
        self.ModelDir = '.'
        self.Modelpath = os.path.join(self.ModelDir , self.ModelName +  self.ext)
    

    
    @pyqtSlot()
    def on_pushButton_New_clicked(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        if self.textEdit_ModelName.toPlainText() == "":
            QMessageBox.information(self, '请指定模型名称', '模型名称不能为空')
            return
        self.ModelName = self.textEdit_ModelName.toPlainText()
        if self.textEdit_DirPath.toPlainText() == '':
            QMessageBox.information(self, '警告', '请模型名称不能为空')
            return
        self.ModelDir = self.textEdit_DirPath.toPlainText()
        if not os.path.exists(self.ModelDir):
            os.mkdirs(self.ModelDir)
        self.Modelpath = os.path.join(self.ModelDir, self.ModelName+self.ext )
        tModel = dcModel(self.ModelName, '常规布局')
        tModel.writeToXML(self.Modelpath)
        
        #加载到模型管理器
        self.close()
    
    @pyqtSlot()
    def on_pushButton_ChoiseDir_clicked(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        
        fN = QFileDialog.getExistingDirectory(self, "创建模型文件", "", QFileDialog.DontUseNativeDialog)      

        if not os.path.exists (fN):
            self.logger.error("目录：%s 不存在！"%fN)
            return
        #
        self.ModelDir = fN
        self.textEdit_DirPath.setText(fN)
                

    def getModelPath(self):
        """
        返回模型的路径
        """
        return self.Modelpath        

