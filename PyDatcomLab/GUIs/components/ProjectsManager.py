# -*- coding: utf-8 -*-

"""
Module implementing ProjectsManager.
"""

import logging
import os 
from PyQt5.QtCore import pyqtSlot, QPoint, QModelIndex
from PyQt5.QtWidgets import QMainWindow, QFileDialog, QMessageBox , QHeaderView, QApplication

from PyDatcomLab.Core.projectManager import dcProject 
from PyDatcomLab.GUIs.tools.XMLEditer.XMLModel import XMLModel 
from PyDatcomLab.GUIs.tools.XMLEditer.XMLTreeEditer import XMLTreeModel as XM
#from xml.etree import ElementTree  as ET
from lxml import etree as ET

from .Ui_ProjectsManager import Ui_ProjectsMainWindow


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
        #日志系统
        self.logger = logging.getLogger(r'Datcomlogger')
        #初始化一个项目管理器        
        self.nowProject = dcProject(None)
        #self.model = XM('')
        self.model = XMLModel('')
        #内部属性
        self.index = QModelIndex() #存储触发右键菜单时候的信息
        self.treeView_xml.header().setSectionResizeMode(QHeaderView.ResizeToContents )
        self.treeView_xml.header().setStretchLastSection( True)
        self.treeView_xml.uniformRowHeights() 


    def hiddenToolBar(self, isHide = True):
        """
        隐藏工具栏：True  
        显示工具栏：False
        """
        self.toolBar.setHidden(isHide)
        
        
    def BindingAction(self, tActions = []):
        """
        将父窗口的Action添加到控件
        """
        for act in tActions:
            self.toolBar.addAction(act)
            self.logger.info('添加Action%s'%act.text())
            
    def BindingModel(self, tModel):
        """
        重新绑定项目数据
        """
        #断言
        if tModel is None :return 
        #分类加载
        if type(tModel) == str:
            if os.path.isfile(tModel):
                self.model.setXMLData(tModel)
                self.treeView_xml.setModel(self.model)
            else:
                self.logger.error("输入的文件无效！%s"%tModel)
                return 
        elif type(tModel) == XM:
            self.treeView_xml.setModel(self.model)
        elif type(tModel) == dcProject:
            if os.path.isfile(tModel.prjPath):
                self.treeView_xml.setModel(tModel.prjPath)
        else:
            self.logger.error(r'BindingModel()无效的Model输入')
            return 
            
    def addProject(self, tPath):
        """
        向控件添加一个datcom项目，tPath指向项目文件
        """
        if tPath is None:
            return 
        if not os.path.isfile(tPath):
            return 
        #添加
        try:
            root = ET.parse(tPath)
            if root is None:
                self.logger.error("读取XML失败:%s"%tPath)
                return
        except Exception as e:
            self.logger.error("%s触发异常%s：%s"%('加载文件', repr(e), str(e)))
            
        if root.getroot().tag != 'datcomProject':
            self.logger.error("读取XML失败:%s"%tPath)
            return
        #添加到项目
        if self.comboBox_project.currentText() != tPath:            
            if self.comboBox_project.findText(tPath) == -1:
                self.comboBox_project.addItem(tPath)
            self.comboBox_project.setCurrentIndex(self.comboBox_project.findText(tPath))            
   
    
    @pyqtSlot()
    def on_pushButton_projectChoise_clicked(self):
        """
        Slot documentation goes here.
        """
        tPath , fType = QFileDialog.getOpenFileName(self, "项目文件路径",'',
                            "Datcom Projects Files (*.dcprj *.xml )" )
        if not os.path.exists (tPath):
            self.logger.error("项目文件：%s 不存在！"%tPath)
            return
        if not self.nowProject.loadProject(tPath):
            QMessageBox.information(self, '提示','加载项目不成功' )
        #添加到项目
        if self.comboBox_project.findText(tPath) == -1:
            self.comboBox_project.addItem(tPath)
        self.comboBox_project.setCurrentIndex(self.comboBox_project.findText(tPath))

    @pyqtSlot(int)
    def on_comboBox_project_currentIndexChanged(self, index):
        """
        Slot documentation goes here.
        
        @param index DESCRIPTION
        @type int
        """
        tPath = self.comboBox_project.itemText(index)
        if not os.path.isfile(tPath):
            self.logger.error("文件：%s 不存在！"%tPath)
            QMessageBox.information(self, '提示',"文件：%s 不存在！"%tPath)
        #加载
        #进行了模型绑定
        #self.model = XM(tPath)
        self.model = XMLModel(tPath)
        #self.model.setXMLData(tPath)
        self.treeView_xml.setModel(self.model)
 #       self.treeView_xml.expandAll()
        #self.treeView_xml.e

    @pyqtSlot()
    def on_actionNewCase_triggered(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        raise NotImplementedError
    
    @pyqtSlot()
    def on_actionCopyCase_triggered(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        raise NotImplementedError
    
    @pyqtSlot()
    def on_actionAddCase_triggered(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        raise NotImplementedError
    
    @pyqtSlot()
    def on_actionRemoveCase_triggered(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        raise NotImplementedError
    
    @pyqtSlot()
    def on_actionNewGroup_triggered(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        raise NotImplementedError
    
    @pyqtSlot()
    def on_actionAddGroup_triggered(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        raise NotImplementedError
    
    @pyqtSlot()
    def on_actionCopyGroup_triggered(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        raise NotImplementedError
    
    @pyqtSlot()
    def on_actionRemoveGroup_triggered(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        raise NotImplementedError
    
    @pyqtSlot(QPoint)
    def on_treeView_xml_customContextMenuRequested(self, pos):
        """
        Slot documentation goes here.
        
        @param pos DESCRIPTION
        @type QPoint
        """
        # TODO: not implemented yet
        raise NotImplementedError
        
        
if __name__ == "__main__":
    import sys
    app = QApplication(sys.argv)
    tWidget = ProjectsManager()
    tWidget.addProject(r'E:\Projects\PyDatcomLab\extras\PyDatcomProjects\tests\fghgh\fghgh.dcprj')
    tWidget.show()
    sys.exit(app.exec_())
