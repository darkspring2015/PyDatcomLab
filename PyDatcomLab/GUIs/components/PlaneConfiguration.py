# -*- coding: utf-8 -*-

"""
Module implementing PlaneConfiguration.
"""
import os 
from PyQt5.QtCore import pyqtSlot
from PyQt5.QtWidgets import QDialog

from .Ui_PlaneConfiguration import Ui_Dialog

from PyDatcomLab.Core import  dcModel

from PyDatcomLab.GUIs.PlaneConfiguration import BODY, HTPLNF,  SYNTHS, VTPLNF,WGPLNF, WGSCHR
from PyDatcomLab.GUIs.PlaneConfiguration import FLTCON

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
        self.dcModel = dcModel.dcModel('AircraftName', '常规构型') 
        self.dcModelPath =  modelpath
        
        #建立内存结构
        if os.path.isfile(modelpath):
            self.dcModel.loadXML(modelpath)
        else: 
            self.logger.error("输入路径无效！%s"%modelpath)


        #test#
        #self.dcModel.setNamelist('SYNTHS','XCG',10)
        
        #添加页码
        self.Initialize()
        #连接各个页面之间的信号
        
        
        
       
        
    def Initialize(self):
        """
        初始化所有的page页
        """
        self.tabWidget_Configuration.clear()
        #FLTCON
        aW = FLTCON.FLTCON(tModel = self.dcModel)
        aW.setObjectName('FLTCON')
        self.tabWidget_Configuration.addTab( aW, r'飞行条件')
        #SYNTHS
        aW = SYNTHS.SYNTHS(tModel = self.dcModel)
        aW.setObjectName('SYNTHS')
        self.tabWidget_Configuration.addTab( aW, r"综合参数")
        #BODY
        aW = BODY.BODY(tModel = self.dcModel)
        aW.setObjectName('BODY') 
        self.tabWidget_Configuration.addTab( aW, r"机体参数") 
        #WGPLNF
        aW = WGPLNF.WGPLNF(tModel = self.dcModel)
        aW.setObjectName('WGPLNF')         
        self.tabWidget_Configuration.addTab( aW, r"机翼几何参数")
        #WGSCHR
        theModel = {'model':self.dcModel}
        aW = WGSCHR.WGSCHR(config  = theModel)
        aW.setObjectName('WGSCHR')         
        self.tabWidget_Configuration.addTab( aW, r"机翼翼型参数")        
        #VTPLNE
        aW = VTPLNF.VTPLNF()
        aW.setObjectName('VTPLNE')         
        self.tabWidget_Configuration.addTab( aW, r"VTPLNE")
        #HTPLNF
        aW = HTPLNF.HTPLNF()
        aW.setObjectName('HTPLNF')   
        self.tabWidget_Configuration.addTab( aW, r"HTPLNF")


    
    @pyqtSlot(int)
    def on_tabWidget_Configuration_currentChanged(self, index):
        """
        Slot documentation goes here.
        
        @param index DESCRIPTION
        @type int
        """
        # TODO: not implemented yet
        nTab = self.tabWidget_Configuration.currentWidget()
        if not nTab is None:
            self.logger.info('编辑当前写%s'%nTab.windowTitle())
        #raise NotImplementedError
        
        #刷新
        
        
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
        
        
                
    
