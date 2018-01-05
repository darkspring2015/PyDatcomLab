# -*- coding: utf-8 -*-

"""
Module implementing NewModelDlg.
"""

from PyQt5.QtCore import pyqtSlot
from PyQt5.QtWidgets import QDialog, QFileDialog

from .Ui_NewModel import Ui_Dialog
import logging

from xml.etree import ElementTree  as ET
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
        
        self.logger = logging.getLogger(r'Datcomlogger')
    

    
    @pyqtSlot()
    def on_pushButton_New_clicked(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        raise NotImplementedError
    
    @pyqtSlot()
    def on_pushButton_ChoiseDir_clicked(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        fN = QFileDialog.getSaveFileName(self, "创建模型文件", ".", "飞机模型 (*.xml)")
        
        if fN[0] != None:            
            dirName, fileName =os.path.split(fN[0])
            self.textEdit_DirPath.setText(dirName)
            self.textEdit_ModelName.setText(fileName)
            if not os.path.exists (fN[0]):
                self.createModel(fN[0])
                

            
    def createModel(self, filePath):
        """
        写入一个空的文件
        """
        self.ModelTemplate = """\
<?xml version="1.0" encoding="utf-8"?>
<datcomProjectManager>
    <managerInfo>
        <managerName>DefaultManager</managerName>    
    </managerInfo>
    <project>
        <projectName>某个项目 </projectName>
        <projectDescribe>项目样例</projectDescribe>
        <projectUUID> </projectUUID>
        <projectPath>.</projectPath>
        <canUse>False</canUse>
        <createTime/>
        <modifyTime/>
    </project>
</datcomProjectManager>
        """
        self.doc = ET.fromstring(self.ModelTemplate)
        tXML = ET.ElementTree(self.doc)
        #ET.register_namespace('datcom', "https://github.com/darkspring2015/PyDatcomLab/")
        tXML.write(filePath ,encoding = 'utf-8',xml_declaration=True)  
