# -*- coding: utf-8 -*-

"""
Module implementing TRNJET.
"""

from PyQt5.QtCore import pyqtSlot , Qt, QPoint #, pyqtSignal
from PyQt5.QtWidgets import QWidget , QMenu  #, QLineEdit, QComboBox, QTableWidget
from PyDatcomLab.GUIs.PlaneConfiguration import DatcomCARD as DC


from PyDatcomLab.Core import dcModel 

import logging

from Ui_TRNJET import Ui_TRNJET


class TRNJET(QWidget, Ui_TRNJET):
    """
    Class documentation goes here.
    """
    def __init__(self, parent=None, tModel = None):
        """
        Constructor
        
        @param parent reference to the parent widget
        @type QWidget
        """
        super(TRNJET, self).__init__(parent)
        self.setupUi(self)
        #创建日志
        self.logger = logging.getLogger(r'Datcomlogger')
        #开始核心数据的定义
        self.NameList = 'TRNJET'
        self.VariableList = {                         
                'NT':{     'TYPE':'INT'  , 'Range':[0, 10 ] }, 
                'SPAN':{   'TYPE':'REAL'  },    
                'PHE':{    'TYPE':'REAL'  },
                'ME':{     'TYPE':'REAL'  },               
                'ISP':{    'TYPE':'REAL'  },    
                'CC':{     'TYPE':'REAL'  },  
                'GP':{     'TYPE':'REAL'  },   
                'LFP':{    'TYPE':'REAL'  }, 
                'TIME':{   'TYPE':'Array', 'Limit':[0, 10] , 'Group':'timeVar'}, 
                'FC':{     'TYPE':'Array', 'Limit':[0, 10] , 'Group':'timeVar'},
                'ALPHA':{  'TYPE':'Array', 'Limit':[0, 10] , 'Group':'timeVar'}, 
                'LAMNRJ':{ 'TYPE':'Array', 'Limit':[0, 10] , 'Group':'timeVar'},
        }  
        #self.NMACHLinkTable = []
        self.RuleNumToCount = [{'Num':'NT' , 'Group':'timeVar'}, ]
#        self.RuleIndexToCombo = [
#           {'Index':'STYPE', 
#            'HowTo':{'1.0':['DELTAS', 'XSOC',   'HSOC'], 
#                     '2.0':['DELTAS', 'XSOC',   'HSOC'],  
#                     '3.0':['DELTAD', 'DELTAS', 'HSOC'], 
#                     '4.0':['DELTAL', 'DELTAR' ], 
#                     '5.0':['DELTAL', 'DELTAR' ], 
#                     }, 
#            'Group':'input'} ,           
#        ]        
#        self.RuleVariableStatus = []

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
        self.tableWidget_timeVar.setContextMenuPolicy(Qt.CustomContextMenu)
    
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
    def on_NT_editingFinished(self):
        """
        Slot documentation goes here.
        """
        self.UILogic()
    
    @pyqtSlot(QPoint)
    def on_tableWidget_timeVar_customContextMenuRequested(self, pos):
        """
        Slot documentation goes here.
        
        @param pos DESCRIPTION
        @type QPoint
        """
        self.curPos = pos
        self.curWidget = self.tableWidget_timeVar        
        posG = self.curWidget.mapToGlobal(pos)
        self.popMenu = QMenu(self.curWidget)
        self.popMenu.addAction(self.actionAddRow)
        self.popMenu.addAction(self.actionDeleteRow)
        self.curWidget.setContextMenuPolicy(Qt.CustomContextMenu)
        self.curN = self.NT
        
        self.popMenu.exec(posG)
    
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
