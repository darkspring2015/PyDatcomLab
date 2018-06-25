
# -*- coding: utf-8 -*-

"""
Module implementing DatcomCASEEditer.

"""
import os 
import logging
#Qt  
from PyQt5.QtCore import pyqtSlot
from PyQt5.QtWidgets import QDialog, QWidget
from PyQt5 import QtWidgets

#PyDatcomLab
from PyDatcomLab.Core import datcomModel as dcModel
from PyDatcomLab.Core.DictionaryLoader import  defaultDatcomDefinition as DDefine  , DTdictionary as dtDefinition
#from PyDatcomLab.Core.datcomCaseConstraint import  datcomCaseConstraint as dcConstrait
from PyDatcomLab.GUIs.InputCard.DatcomCASEEditerUi import DatcomCASEEditerUi
from PyDatcomLab.GUIs.InputCard.DatcomWidgetBase import DatcomWidgetBase


class DatcomCASEEditer(QDialog, DatcomCASEEditerUi):
    """
    Class documentation goes here.
    """
    def __init__(self, parent=None, modelpath = None, iDefine = DDefine):
        """
        Constructor
        
        @param parent reference to the parent widget
        @type QWidget
        """
        super(DatcomCASEEditer, self).__init__(parent)

        #初始化日志系统
        self.logger = logging.getLogger(r'Datcomlogger')
        #创建内部数据结构        
        if iDefine is None or type(iDefine) != dtDefinition:
            self.dtDefine = DDefine
        else:
            self.dtDefine = iDefine
            
        self.dcModelPath =  modelpath
        if os.path.isfile(modelpath):
            self.dcModel = dcModel.dcModel(modelpath, self.dtDefine)             
        else:
            self.dcModel = dcModel.dcModel(dtDefine = self.dtDefine) 
        
        #内部数据
        self.lastIndex  = -1     
        #初始化界面
        self.setupUi(self)        
        #添加页码
        self.Initialize()        
        #连接各个页面之间的信号
       
        
    def Initialize(self):
        """
        初始化所有的page页
        """
        self.tabWidget_Configuration.clear()  
        for mdName in self.dcModel.getNamelistCollection():
            dF               =   self.dtDefine.getNamelistDefineByName(mdName)
            dFNmlstAtrrib =  self.dtDefine.getNamelistAttributeByName(mdName)
            if dF == {} :
                self.logger.error("不存在%s对应的选项卡定义"%mdName)
                continue    
            if 'DisplayName' not in dFNmlstAtrrib.keys():
                dFNmlstAtrrib["DisplayName"] = mdName
            aW = DatcomWidgetBase(iNamelist = mdName , iModel =  self.dcModel, parent = self)     
            aW.setObjectName(mdName)
            self.tabWidget_Configuration.addTab( aW, dFNmlstAtrrib['DisplayName'])    
    
    @pyqtSlot(int)
    def on_tabWidget_Configuration_currentChanged(self, index):
        """
        Slot documentation goes here.
        
        @param index DESCRIPTION
        @type int
        """
        # TODO: not implemented yet
        #raise NotImplementedError
        if self.lastIndex != -1:
            tW =self.tabWidget_Configuration.widget(self.lastIndex)
            if not tW is None:
                self.dcModel = tW.getDoc()
        #刷新
        self.lastIndex = index
        if not self.tabWidget_Configuration.widget(index) is None:
            if not type(self.tabWidget_Configuration.widget(index)) is QWidget:
                self.tabWidget_Configuration.widget(index).setModel(self.dcModel)
            
            
        
        
    def getDoc(self):
        """
        刷新Doc并返回
        """
        tabCount = self.tabWidget_Configuration.count()
        if tabCount > 0:
            for num in range(0,tabCount):
                self.tabWidget_Configuration.widget(num).getDoc()

                
        return self.dcModel
    
    def writeToXML(self, tFile = None):
        """
        将结果写入到XML文件中
        """
        if tFile is None: tFile = self.dcModelPath
        
        #判断dcModelPath
        if not os.path.isFile(tFile):
            self.logger.error("输入的XML文件不存在%s"%tFile)
            #raise  Exception("输入的XML文件不存在%s"%tFile)
            return
        
        #写入到XML
        self.dcModel.writeToXML(tFile)
        
        

if __name__ == "__main__":
    import sys
    bPath = os.path.join(os.path.expanduser('~'), '.PyDatcomLab', 'extras')
    sPath  = os.path.join(bPath,    '\PyDatcomProjects\1\case2.xml')
    obPath  = os.path.join(bPath,  '\PyDatcomProjects\1\case3.xml')
    dtPath  = os.path.join(bPath,   '\PyDatcomProjects\1\case3.inp')
    #sPath  = r'E:\Projects\PyDatcomLab\extras\PyDatcomProjects\1\case2.xml'
    #obPath = r'E:\Projects\PyDatcomLab\extras\PyDatcomProjects\1\case3.xml'
    #dtPath = r'E:\Projects\PyDatcomLab\extras\PyDatcomProjects\1\case3.inp'
    app = QtWidgets.QApplication(sys.argv)
    Dialog = DatcomCASEEditer(modelpath = sPath)
    Dialog.show()
    sys.exit(app.exec_())
    
