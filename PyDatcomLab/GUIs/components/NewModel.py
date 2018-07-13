# -*- coding: utf-8 -*-

"""
Module implementing NewModelDlg.
"""
import logging
import os

from PyQt5.QtCore import pyqtSlot, Qt
from PyQt5.QtWidgets import QDialog, QFileDialog, QMessageBox, QCheckBox #, QComboBox
from .Ui_NewModel import Ui_Dialog
from PyDatcomLab.Core.datcomModel import dcModel as dcModel 
from PyDatcomLab.Core.DictionaryLoader import  defaultDatcomDefinition as DDefine
from PyDatcomLab.Core.PyDatcomConfigLoader import  defaultConfig 


class NewModelDlg(QDialog, Ui_Dialog):
    """
    Class documentation goes here.
    """
    def __init__(self, parent=None, iDefine = DDefine, iConfig = defaultConfig):
        """
        Constructor
        
        @param parent reference to the parent widget
        @type QWidget
        """
        super(NewModelDlg, self).__init__(parent)
        self.setupUi(self)
        
        #日志系统        
        self.logger = logging.getLogger(r'Datcomlogger')
        self.dtDefine = iDefine
        self.dtConfig = iConfig
        self.libraryName = 'ConfigurationType'
        self.ext = '.dcxml'
        self.extFilter = "Datcom Model Files (*.dcxml);;XML Files (*.xml)"
        self.ModelName = "test"
        self.ModelDir = os.path.join(os.path.expanduser('~'), '.PyDatcomLab', 'extras', 'PyDatcomProjects')
        self.Modelpath = os.path.join(self.ModelDir , self.ModelName +  self.ext)
        #定义模式
        self.comboBox_template.clear()
        self.configurationList = self.dtConfig.getConfigurationList()
        for tM in self.configurationList:            
            self.comboBox_template.addItem(tM['DisplayName'])
  
    @pyqtSlot()
    def on_pushButton_New_clicked(self):
        """
        点击新建模型按钮.
        """
        # TODO: not implemented yet
        if self.lineEdit_ModelName.text() == "":
            QMessageBox.information(self, '请指定模型名称', '模型名称不能为空')
            return
        self.ModelName = self.lineEdit_ModelName.text()
        if self.lineEdit_DirPath.text() == '':
            QMessageBox.information(self, '警告', '请模型名称不能为空')
            return
        tObjPath = self.lineEdit_DirPath.text()
        self.ModelDir =os.path.dirname(tObjPath)
        if not os.path.exists(self.ModelDir):
            os.mkdirs(self.ModelDir)
        self.Modelpath = os.path.join(self.ModelDir, self.ModelName+self.ext )      
        #开始创建对应的DatcomModel
        tModel = dcModel( iDefine = self.dtDefine)
        tModel.Properties.update({'CName': self.ModelName, 
                                            'Configuration':self.comboBox_template.currentText(), 
                                            'Describe':self.textEdit_Describe.toPlainText(), 
                                            'AerocraftName':self.ModelName, 
        })
        #获得配置
        for iC in self.dtDefine.getNamelistCollection():
            tWidget = self.findChild(QCheckBox, 'checkBox_%s'%iC)
            if tWidget and tWidget.checkState() == Qt.Checked:
                tModel.addNamelist(iC)
            else:
                tModel.deleteNamelist(iC)               
        #保存文件到
        tModel.save(self.Modelpath)       
        #加载到模型管理器
        #self.finished.emit(QDialog.Accepted)
        #self.setResult(QDialog.Accepted)
        self.accept()
    
    @pyqtSlot()
    def on_pushButton_GetFileName_clicked(self):
        """
        打开文件对话框，选在一个文件保存模型
        """
        # 选择文件对话框        
        #fN = QFileDialog.getSaveFileName(self, "创建模型文件", "",self.extFilter,  QFileDialog.DontUseNativeDialog)     
        fN , tExt= QFileDialog.getSaveFileName(self, "创建模型文件", self.ModelDir ,self.extFilter, "Datcom Model Files (*.dcxml)",  options=QFileDialog.DontUseNativeDialog)   
        tFilePath = fN +".dcxml" 
        if fN !='':
            self.lineEdit_DirPath.setText(tFilePath)
            self.Modelpath = tFilePath
                

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
        if index < 0 : return 

        tKey = '%d.0'%(index+1)
        tList = []
        tSet    = self.dtConfig.getLibrary(self.libraryName)
        allCard = self.dtDefine.getNamelistCollection()
        if 'Namelist' in tSet[index].keys():
            tList = tSet[index]['Namelist']
        else:
            self.logger.error('尝试的Index：%s并不存在对应的基础定义'%tKey)
            return
        for iC in allCard:
            tWidget = self.findChild(QCheckBox, 'checkBox_%s'%iC)
            if  tWidget is None: 
                self.logger.error('并不存在对应的CARD复选框：%s'%iC)
                continue
            #执行包括
            if iC in tList:
                tWidget.setCheckState(Qt.Checked)
            else:
                tWidget.setCheckState(Qt.Unchecked)
    
                
            
            
        
