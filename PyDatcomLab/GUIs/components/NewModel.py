# -*- coding: utf-8 -*-

"""
Module implementing NewModelDlg.
"""

from PyQt5.QtCore import pyqtSlot, Qt
from PyQt5.QtWidgets import QDialog, QFileDialog, QMessageBox, QCheckBox #, QComboBox

from .Ui_NewModel import Ui_Dialog
import logging

#from PyDatcomLab.Core.dcModel import dcModel 
from PyDatcomLab.Core.datcomModel import dcModel as dcModel 
from PyDatcomLab.Core.datcomDefine import modelTemplate
#from PyDatcomLab.Core.datcomDefine import reserved_NAMELISTS as allCard

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
        #定义模式
        self.comboBox_template.clear()
        for tM in modelTemplate.keys():
            tName = modelTemplate[tM]['TName']
            self.comboBox_template.addItem('%s:%s'%(tM ,  tName))


    
    @pyqtSlot()
    def on_pushButton_New_clicked(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        if self.lineEdit_ModelName.text() == "":
            QMessageBox.information(self, '请指定模型名称', '模型名称不能为空')
            return
        self.ModelName = self.lineEdit_ModelName.text()
        if self.lineEdit_DirPath.text() == '':
            QMessageBox.information(self, '警告', '请模型名称不能为空')
            return
        self.ModelDir = self.lineEdit_DirPath.text()
        if not os.path.exists(self.ModelDir):
            os.mkdirs(self.ModelDir)
        self.Modelpath = os.path.join(self.ModelDir, self.ModelName+self.ext )           
        tModel = dcModel(self.ModelName, self.comboBox_template.currentText())
        #获得配置
        tCARDList = []
        for iC in allCard:
            tWidget = self.findChild(QCheckBox, 'checkBox_%s'%iC)
            if tWidget and tWidget.checkState() == Qt.Checked:
                tCARDList.append(iC)
        tModel.setCARDList(tCARDList)
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
        self.lineEdit_DirPath.setText(fN)
                

    def getModelPath(self):
        """
        返回模型的路径
        """
        return self.Modelpath        
    
    @pyqtSlot(int)
    def on_comboBox_template_currentIndexChanged(self, index):
        """
        Slot documentation goes here.
        
        @param index DESCRIPTION
        @type int
        """

        tKey = '%d.0'%(index+1)
        tList = []
        if tKey in modelTemplate.keys():
            tList = modelTemplate[tKey]['CARDList']
        else:
            self.logger.error('尝试的Index：%s并不存在对应的基础定义'%tKey)
            return
        for iC in allCard:
            tWidget = self.findChild(QCheckBox, 'checkBox_%s'%iC)
            if not tWidget: 
                self.logger.error('并不存在对应的CARD复选框：%s'%iC)
                continue
            #执行包括
            if iC in tList:
                tWidget.setCheckState(Qt.Checked)
            else:
                tWidget.setCheckState(Qt.Unchecked)
    
                
            
            
        
