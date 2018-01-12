# -*- coding: utf-8 -*-

"""
Module implementing VFPLNF.
"""
from PyQt5.QtCore import pyqtSlot, Qt, QPoint
from PyQt5.QtWidgets import QWidget, QMenu, QAction
from PyQt5.QtGui import  QIcon, QPixmap


from PyDatcomLab.Core import dcModel
from PyDatcomLab.GUIs.PlaneConfiguration import DatcomCARD as DC

import logging

from .Ui_VFPLNF import Ui_VFPLNF


class VFPLNF(QWidget, Ui_VFPLNF):
    """
    Class documentation goes here.
    """
    def __init__(self, parent=None, tModel =None):
        """
        Constructor
        
        @param parent reference to the parent widget
        @type QWidget
        """
        super(VFPLNF, self).__init__(parent)
        self.setupUi(self)
        #创建日志
        self.logger = logging.getLogger(r'Datcomlogger')
        #开始核心数据的定义
        self.NameList = 'VFPLNF'
        self.VariableList = {
                #
                'CHRDTP':{  'TYPE':'REAL'},
                'CHRDBP':{'TYPE':'REAL'},
                'CHRDR':{'TYPE':'REAL'},
                'SSPNOP':{'TYPE':'REAL'},
                'SSPNE':{ 'TYPE':'REAL'},
                'SSPN':{'TYPE':'REAL'}, 
                'SAVSI':{'TYPE':'REAL'}, 
                'SAVSO':{'TYPE':'REAL'},
                'CHSTAT':{ 'TYPE':'REAL'},
                #
                'TYPE':{ 'TYPE':'List', 'Range':['1.0' , '2.0', '3.0'], 'Default':'1.0'},             
                #垂尾受机翼平尾影响参数定义
                'SVWB':{ 'TYPE':'Array' , 'Limit':[0, 20]  , 'Group':'VTArea'}  , 
                'SVB':{  'TYPE':'Array' , 'Limit':[0, 20]  , 'Group':'VTArea'}  , 
                'SVHB':{ 'TYPE':'Array' , 'Limit':[0, 20]  , 'Group':'VTArea'}  , 
 
        }  
        self.NMACHLinkTable = ['VTArea' ]
        
        #设置表格功能
        self.tableWidget_VTArea.setContextMenuPolicy(Qt.CustomContextMenu)


        #调用其他初始化过程
        #修改后台的数据
        if tModel is None:
            tModel = dcModel.dcModel('J6', '常规布局')  
        #定义数据
        self.DatcomCARD = DC.DatcomCARD(self)
        self.DatcomCARD.InitUi()
        self.DatcomCARD.setModel(tModel)   #设置模型 
        
        #界面参数
        self.curPos = QPoint(0, 0)
        self.curWidget = None
        self.curN = None
        self.popMenu = None
        #刷新界面
        self.UILogic()  
        
 

        
    def setModel(self, tModel):
        """
        初始化本节点的xml描述文档
        """
        
        #self.Model = tModel        
        #执行参数配置过程    
        self.DatcomCARD.setModel(tModel)      
        self.UILogic()
        
    def getDoc(self):
        """
        将界面的内容刷新到变量model
        """
        
        #执行界面刷新
        return self.DatcomCARD.getModel()
        
        
        
    def UILogic(self):
        """
        在此刷新UI，需要根据不同的情况执行判断
        """       
        self.DatcomCARD.UILogic()
        
        """
        该函数必须在SetupUi和给self.NameList,self.VariableList 赋值之后才能使用
        """

        if self.comboBox_TYPE.currentIndex() == 0: #不需要部分输入
            self.SSPNOP.setEnabled(False)
            self.CHRDBP.setEnabled(False)
            self.SAVSO.setEnabled(False)
            self.SSPNDD.setEnabled(False)
        else:
            self.SSPNOP.setEnabled(True)
            self.CHRDBP.setEnabled(True)
            self.SAVSO.setEnabled(True)
            self.SSPNDD.setEnabled(True)                  



        
    def getColumnIndex(self, tTable, tHeader):
        """"""
        tIndex =-1
        for itC in range(0, tTable.columnCount()):
            if tTable.horizontalHeaderItem(itC).text() == tHeader:
                tIndex = itC 
                break
        return tIndex            
    
    
    @pyqtSlot(int)
    def on_comboBox_TYPE_currentIndexChanged(self, index):
        """
        Slot documentation goes here.
        
        @param index DESCRIPTION
        @type int
        """
        # TODO: not implemented yet
        self.UILogic()
    
    @pyqtSlot(QPoint)
    def on_tableWidget_VTArea_customContextMenuRequested(self, pos):
        """
        Slot documentation goes here.
        
        @param pos DESCRIPTION
        @type QPoint
        """
        # TODO: not implemented yet
        self.curPos = pos
        self.curWidget = self.tableWidget_VTArea        
        posG = self.curWidget.mapToGlobal(pos)
        self.popMenu = QMenu(self.curWidget)
        #self.popMenu.addAction(self.actionAddRow)
        #self.popMenu.addAction(self.actionDeleteRow)        
        tAct = QAction(self)
        icon = QIcon()
        icon.addPixmap(QPixmap(":/cardIco/rc_card/icos/AddedIcon.ico"), QIcon.Normal, QIcon.Off)
        tAct.setIcon(icon)
        tAct.setObjectName("tipsAction")
        tAct.setText('表格行数须等于FLTCON中NMACH或VINF，请修改算例')
        self.popMenu.addAction(tAct)
        self.curWidget.setContextMenuPolicy(Qt.CustomContextMenu)
        self.curN = None
        
        self.popMenu.exec(posG)
