# -*- coding: utf-8 -*-

"""
Module implementing HYPEFF.
"""
from PyQt5.QtCore import pyqtSlot, Qt, QPoint #, pyqtSignal
from PyQt5.QtWidgets import QWidget, QMenu  #, QLineEdit, QComboBox, QTableWidget
from PyDatcomLab.GUIs.PlaneConfiguration import DatcomCARD as DC


from PyDatcomLab.Core import dcModel 

import logging

from Ui_HYPEFF import Ui_HYPEFF


class HYPEFF(QWidget, Ui_HYPEFF):
    """
    Class documentation goes here.
    """
    def __init__(self, parent=None, tModel = None):
        """
        Constructor
        
        @param parent reference to the parent widget
        @type QWidget
        """
        super(HYPEFF, self).__init__(parent)
        self.setupUi(self)
        
        #
        #创建日志
        self.logger = logging.getLogger(r'Datcomlogger')
        #开始核心数据的定义
        self.NameList = 'HYPEFF'
        self.VariableList = {
                'ALITD':{  'TYPE':'REAL'  },    
                'XHL':{    'TYPE':'REAL'  },
                'TWOTI':{  'TYPE':'REAL'  },               
                'CF':{     'TYPE':'REAL'  },    
                'HNDLTA':{ 'TYPE':'INT'  },   
                'HDELTA':{ 'TYPE':'Array', 'Limit':[0, 10] , 'Group':'controlAngle'}, 
                'LAMNR':{  'TYPE':'List'  ,'Range':['.TRUE.', '.FALSE.'], 'Default':'.TRUE.'}, 
        }  
        #self.NMACHLinkTable = []
        #tableWidget_controlAngle
        self.RuleNumToCount = [{'Num':'HNDLTA' , 'Group':'controlAngle'}, 
                               ]
        #self.RuleIndexToCombo = [   ]        
        #self.RuleVariableStatus = [             ]

        #修改后台的数据
        if tModel is None:
            tModel = dcModel.dcModel('J6', '常规布局')  
        #定义数据
        self.DatcomCARD = DC.DatcomCARD(self)
        #self.InitComboVar(tModel)
        self.DatcomCARD.InitUi()
        self.DatcomCARD.setModel(tModel)   #设置模型
        
        #界面参数-表格逻辑
        self.curPos = QPoint(0, 0)
        self.curWidget = None
        self.curN = None
        self.popMenu = None        
        self.tableWidget_controlAngle.setContextMenuPolicy(Qt.CustomContextMenu)
    
        #初始化数据和内容  
        self.UILogic()   
        
    def setModel(self, tModel):
        """
        初始化本节点的xml描述文档
        """
      
        #执行参数配置过程    
        #self.InitComboVar(tModel)
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
        #其他表格的逻辑
        
    @pyqtSlot()
    def on_HNDLTA_editingFinished(self):
        """
        Slot documentation goes here.
        """
        self.UILogic()
        
    
    
    @pyqtSlot(int)
    def on_comboBox_LAMNR_currentIndexChanged(self, index):
        """
        Slot documentation goes here.
        
        @param index DESCRIPTION
        @type int
        """
        self.UILogic()
    
    
    @pyqtSlot(QPoint)
    def on_tableWidget_controlAngle_customContextMenuRequested(self, pos):
        """
        Slot documentation goes here.
        
        @param pos DESCRIPTION
        @type QPoint
        """
        self.curPos = pos
        self.curWidget = self.tableWidget_controlAngle        
        posG = self.curWidget.mapToGlobal(pos)
        self.popMenu = QMenu(self.curWidget)
        self.popMenu.addAction(self.actionAddRow)
        self.popMenu.addAction(self.actionDeleteRow)
        self.curWidget.setContextMenuPolicy(Qt.CustomContextMenu)
        self.curN = self.HNDLTA
        
        self.popMenu.exec(posG)
    
    @pyqtSlot()
    def on_actionDeleteRow_triggered(self):
        """
        Slot documentation goes here.
        """
        
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
        #添加行
        aItem = self.curWidget.indexAt(self.curPos) #认为是表格 ，否则会异常
        rowIndex = 0
        if aItem.row() == -1 :
            #没有命中
            rowIndex = self.curWidget.rowCount()
        else:
            rowIndex = aItem.row()
            
        tLimit = 10    
        if self.curWidget.rowCount() < tLimit:
            self.curWidget.insertRow(rowIndex)
        else:
            self.logger.info("%s已经达到最大行数不能添加"%self.curWidget.objectName())
        if not self.curN is None: 
            self.curN.setText(str(self.curWidget.rowCount()))
        
        self.UILogic() 
