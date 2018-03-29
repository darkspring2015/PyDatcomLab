# -*- coding: utf-8 -*-

"""
Module implementing PlaneConfiguration.
"""
import os 
from PyQt5.QtCore import pyqtSlot
from PyQt5.QtWidgets import QDialog, QWidget

from .Ui_PlaneConfiguration import Ui_Dialog

from PyDatcomLab.Core import  dcModel, datcomDefine as dF
#from PyDatcomLab.Core import  datcomDefine as dF
from PyDatcomLab.GUIs.PlaneConfiguration import *
#from PyDatcomLab.Core import datcomModel as dcModel

import logging

class PlaneConfiguration(QDialog, Ui_Dialog):
    """
    Class documentation goes here.
    """
    def __init__(self, parent=None, modelpath = None):
        """
        Constructor
        
        @param parent reference to the parent widget
        @type QWidget
        """
        super(PlaneConfiguration, self).__init__(parent)
        self.setupUi(self)
        #初始化日志系统
        self.logger = logging.getLogger(r'Datcomlogger')
        #创建内部数据结构
        self.dcModel = dcModel.dcModel() 
        if os.path.exists(modelpath):
            self.dcModel.loadXML(modelpath)
            self.dcModelPath =  modelpath
        else:
            self.dcModelPath =  None
        
        self.lastIndex = -1
        
        self.widgetNameList = {
            FLTCON.FLTCON:['FLTCON','飞行条件'], 
            SYNTHS.SYNTHS:['SYNTHS','综合参数'], 
            BODY.BODY:[    'BODY'  ,'机体参数'], 
            WGPLNF.WGPLNF:['WGPLNF','机翼几何参数'], 
            WGSCHR.WGSCHR:['WGSCHR','机翼气动参数'], 
            VTPLNF.VTPLNF:['VTPLNF','垂尾几何参数'], 
            VTSCHR.VTSCHR:['VTSCHR','垂尾气动参数'],
            HTPLNF.HTPLNF:['HTPLNF','平尾几何参数'], 
            HTSCHR.HTSCHR:['HTSCHR','平尾气动参数'], 
            VFPLNF.VFPLNF:['VFPLNF','腹鳍几何参数'],
            VFSCHR.VFSCHR:['VFSCHR','腹鳍气动参数'],
            EXPR.EXPR:[    'EXPR'  ,'试验数据'],             
        }
        
        #self.cardList = dF.namelistDefine.keys() #默认使用所有的CARD
        
        #建立内存结构
        if os.path.isfile(modelpath):
            self.dcModel.loadXML(modelpath)
        else: 
            self.logger.error("输入路径无效！%s"%modelpath)
            
        #添加页码
        self.Initialize()
        
        #连接各个页面之间的信号
       
        
    def Initialize(self):
        """
        初始化所有的page页
        """
        self.tabWidget_Configuration.clear()  
        for mdName in self.dcModel.getCARDList():
            mdObj = __import__(mdName)
            cardMd = getattr(mdObj,mdName)
            aW = cardMd(self, tModel = self.dcModel)
            aW.setObjectName(mdName)
            self.tabWidget_Configuration.addTab( aW, dF.namelistDefine[mdName]['ShowName'])
            
#        for mdName in self.widgetNameList.keys():
#            aW = mdName(tModel = self.dcModel)
#            aW.setObjectName(self.widgetNameList[mdName][0] )
#            self.tabWidget_Configuration.addTab( aW, self.widgetNameList[mdName][1])
            
    
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
        
        
                
    
