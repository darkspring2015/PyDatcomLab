# -*- coding: utf-8 -*-

"""
Module implementing VTSCHR.
"""
from PyQt5.QtCore import pyqtSlot, Qt, QPoint
from PyQt5.QtWidgets import QWidget, QMenu, QAction, QMessageBox
from PyQt5.QtGui import QIcon, QPixmap

from PyDatcomLab.GUIs.PlaneConfiguration import DatcomCARD as DC
from PyDatcomLab.Core import dcModel 

import logging

from Ui_VFSCHR import Ui_VFSCHR


class VFSCHR(QWidget, Ui_VFSCHR):
    """
    Class documentation goes here.
    """
    def __init__(self, parent=None, tModel = None):
        """
        Constructor
        
        @param parent reference to the parent widget
        @type QWidget
        """
        super(VFSCHR, self).__init__(parent)
        self.setupUi(self)
        #创建日志
        self.logger = logging.getLogger(r'Datcomlogger') 
        
        #定义核心数据
        self.NameList = 'VFSCHR'
        self.VariableList = {
                'TOVC':{'TYPE':'REAL'  ,'Range':[0, float('inf') ] },  
                #'DELTAY':{'TYPE':'REAL'}, 
                'XOVC':{'TYPE':'REAL'}, 
                #'CLI':{'TYPE':'REAL'},
                #'ALPHAI':{'TYPE':'REAL'},
                #'CMO':{'TYPE':'REAL'},    
                'LERI':{'TYPE':'REAL'}, 
                'LERO':{'TYPE':'REAL'},
                'TOVCO':{'TYPE':'REAL'}, 
                'XOVCO':{'TYPE':'REAL'}, 
                #'CMOT':{'TYPE':'REAL'}, 
                #'CLMAXL':{'TYPE':'REAL'},  
                #'CLAMO':{'TYPE':'REAL'}, 
                'TCEFF':{'TYPE':'REAL'}, 
                'KSHARP':{'TYPE':'REAL'}, 
                'ARCL':{'TYPE':'REAL'}, 
                #'YCM':{'TYPE':'REAL'}, 
                #'CLD':{'TYPE':'REAL', 'Default':0.0}, 
                'NPTS':{'TYPE':'INT'  , 'Range':[0, 50]}, 
                'CAMBER':{'TYPE':'List','Range':['.TRUE.', '.FALSE.']   , 'Default':'.TRUE.'}, 
                #'DWASH':{'TYPE':'List', 'Range':['1.0'   , '2.0', '3.0'], 'Default':'1.0'}, 
                'TYPEIN':{'TYPE':'List','Range':['1.0'   , '2.0']       , 'Default':'1.0'}, 
                #'SLOPE':{'TYPE':'Array' , 'Limit':[6, 6]  , 'Group':'SLOPE'}  , 
                'CLALPA':{'TYPE':'Array', 'Limit':[0, 20] , 'Group':'Lift'}, 
                #'CLMAX':{'TYPE':'Array',  'Limit':[0, 20] , 'Group':'Lift'}, 
                #'XAC':{'TYPE':'Array',    'Limit':[0, 20] , 'Group':'Lift'}, 
                'XCORD':{'TYPE':'Array',  'Limit':[0, 50] , 'Group':'AirfoilSection'},
                'YUPPER':{'TYPE':'Array', 'Limit':[0, 50] , 'Group':'AirfoilSection'},
                'YLOWER':{'TYPE':'Array', 'Limit':[0, 50] , 'Group':'AirfoilSection'},
                'MEAN':{'TYPE':'Array',   'Limit':[0, 50] , 'Group':'AirfoilSection'},
                'THICK':{'TYPE':'Array',  'Limit':[0, 50] , 'Group':'AirfoilSection'},   
        }
        
        self.NMACHLinkTable = ['Lift' ]
        
        #修改后台的数据
        if tModel is None:
            tModel = dcModel.dcModel('J6', '常规布局')  
        #定义数据
        self.DatcomCARD = DC.DatcomCARD(self)
        self.DatcomCARD.InitUi()
        self.DatcomCARD.setModel(tModel)   #设置模型
        #
        #设置表格功能
        self.tableWidget_AirfoilSection.setContextMenuPolicy(Qt.CustomContextMenu)
        self.tableWidget_Lift.setContextMenuPolicy(Qt.CustomContextMenu)
        #self.tableWidget_SLOPE.setContextMenuPolicy(Qt.CustomContextMenu)

        
        #界面参数-表格逻辑
        self.curPos = QPoint(0, 0)
        self.curWidget = None
        self.curN = None
        self.popMenu = None
        
        #初始化数据和内容  
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
        执行UI界面刷新操作
        """        
        self.DatcomCARD.UILogic()
        
    
    @pyqtSlot(int)
    def on_comboBox_CAMBER_currentIndexChanged(self, index):
        """
        Slot documentation goes here.
        
        @param index DESCRIPTION
        @type int
        """
        # TODO: not implemented yet
        self.UILogic()  
    
    @pyqtSlot(int)
    def on_comboBox_TYPEIN_currentIndexChanged(self, index):
        """
        Slot documentation goes here.
        
        @param index DESCRIPTION
        @type int
        """
        # TODO: not implemented yet
        self.UILogic()  
    
    
    @pyqtSlot(QPoint)
    def on_tableWidget_AirfoilSection_customContextMenuRequested(self, pos):
        """
        Slot documentation goes here.
        
        @param pos DESCRIPTION
        @type QPoint
        """
        # TODO: not implemented yet
        self.curPos = pos
        self.curWidget = self.tableWidget_AirfoilSection        
        posG = self.curWidget.mapToGlobal(pos)
        self.popMenu = QMenu(self.curWidget)
        self.popMenu.addAction(self.actionAddRow)
        self.popMenu.addAction(self.actionDeleteRow)
        self.curWidget.setContextMenuPolicy(Qt.CustomContextMenu)
        self.curN = self.NPTS
        
        self.popMenu.exec(posG)
    
    @pyqtSlot(QPoint)
    def on_tableWidget_Lift_customContextMenuRequested(self, pos):
        """
        Slot documentation goes here.
        
        @param pos DESCRIPTION
        @type QPoint
        """
        # TODO: not implemented yet
        # TODO: not implemented yet
        self.curPos = pos
        self.curWidget = self.tableWidget_Lift        
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
    
    @pyqtSlot()
    def on_actionDeleteRow_triggered(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        aItem = self.curWidget.indexAt(self.curPos)
        if  aItem.row() >= 0 :            
            self.curWidget.removeRow(aItem.row())
        else:
            self.logger.info("没有命中任何行")
            
        if not self.curN is None:
            self.curN.setText(str(self.curWidget.rowCount()))

        self.UILogic() 
    
    @pyqtSlot()
    def on_actionAddRow_triggered(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        #添加行
        aItem = self.curWidget.indexAt(self.curPos) #认为是表格 ，否则会异常
        rowIndex = 0
        if aItem.row() == -1 :
            #没有命中
            rowIndex = self.curWidget.rowCount()
        else:
            rowIndex = aItem.row()
        
        if self.curWidget.objectName() == 'tableWidget_SLOPE' :
            tLimit = 6    
        if self.curWidget.objectName() == 'tableWidget_AirfoilSection':
            tLimit = 50
        if self.curWidget.objectName() == 'tableWidget_Lift':
            tLimit = 20
        

        if self.curWidget.rowCount() < tLimit:
            self.curWidget.insertRow(rowIndex)
        else:
            self.logger.info("%s已经达到最大行数不能添加"%self.curWidget.objectName())
        if not self.curN is None: 
            self.curN.setText(str(self.curWidget.rowCount()))
        
        self.UILogic()  
    
    @pyqtSlot()
    def on_NPTS_editingFinished(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        
        nowRows = self.tableWidget_AirfoilSection.rowCount()
        tNx = int(self.NPTS.text())
        if nowRows < tNx: 
            #if 当前行数少，考虑增加
            for itR in range(nowRows, tNx):
                self.tableWidget_AirfoilSection.insertRow(itR)
        if nowRows == tNx:
            pass
        if nowRows > tNx:
            self.NPTS.setText(str(nowRows))
            strInfo = "尝试的NPTS小于现有数据行数，请手动从表格中删除对应行"
            QMessageBox.information(self, "提示" , strInfo)  
            self.logger.info(strInfo)             
        
        self.UILogic()  
