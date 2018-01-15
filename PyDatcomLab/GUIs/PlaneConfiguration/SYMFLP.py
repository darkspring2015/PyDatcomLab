# -*- coding: utf-8 -*-

"""
Module implementing SYMFLP.
"""

from PyQt5.QtCore import pyqtSlot, Qt, QPoint #, pyqtSignal
from PyQt5.QtWidgets import QWidget, QMenu 
from PyDatcomLab.GUIs.PlaneConfiguration import DatcomCARD as DC


from PyDatcomLab.Core import dcModel 

import logging

from .Ui_SYMFLP import Ui_SYMFLP


class SYMFLP(QWidget, Ui_SYMFLP):
    """
    Class documentation goes here.
    """
    def __init__(self, parent=None, tModel = None):
        """
        Constructor
        
        @param parent reference to the parent widget
        @type QWidget
        """
        super(SYMFLP, self).__init__(parent)
        self.setupUi(self)
        #创建日志
        self.logger = logging.getLogger(r'Datcomlogger')
        #开始核心数据的定义
        self.NameList = 'TVTPAN'
        self.VariableList = {
                'LOOP':{  'TYPE':'List'  ,'Range':['1.0', '2.0', '3.0']   , 'Default':'1.0'},         
                'NMACH':{ 'TYPE':'INT'   ,'Range':[0, 20 ] }, 
                'NALT':{  'TYPE':'INT'   ,'Range':[0, 20 ] },
                'NALPHA':{'TYPE':'INT'   ,'Range':[0, 20 ] },
                'WT':{    'TYPE':'REAL'  ,'Range':[0, float('inf') ] },
                'GAMMA':{ 'TYPE':'REAL'  },
                'STMACH':{'TYPE':'REAL'  ,'Range':[0.6, 0.99 ]},    
                'TSMACH':{'TYPE':'REAL'  ,'Range':[1.01, 1.4 ]},  
                'HYPERS':{'TYPE':'List'  ,'Range':['.TRUE.', '.FALSE.']  , 'Default':'.TRUE.'}, 
                'TR':{    'TYPE':'List'  ,'Range':['0.0', '1.0']  , 'Default':'0.0'}, 
                'DELTA':{ 'TYPE':'Array', 'Limit':[0, 9] , 'Group':'flap'}, 
                'CPRMEI':{'TYPE':'Array', 'Limit':[0, 9] , 'Group':'flap'}, 
                'CPRMEO':{'TYPE':'Array', 'Limit':[0, 9] , 'Group':'flap'}, 
                'CAPINB':{'TYPE':'Array', 'Limit':[0, 9] , 'Group':'flap'},
                'CAPOUT':{'TYPE':'Array', 'Limit':[0, 9] , 'Group':'flap'},                
                'DOBDEF':{'TYPE':'Array', 'Limit':[0, 9] , 'Group':'flap'},  
                'SCLD':{  'TYPE':'Array', 'Limit':[0, 9] , 'Group':'delta'},
                'SCMD':{  'TYPE':'Array', 'Limit':[0, 9] , 'Group':'delta'},

        }  
        self.NMACHLinkTable = []
        self.RuleNumToCount = [{'Num':'NMACH' , 'Group':'delta'}, 
                              {'Num':'NALPHA', 'Group':'flap'}]
        self.RuleIndexToCombo = []
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
        self.tableWidget_delta.setContextMenuPolicy(Qt.CustomContextMenu)
        self.tableWidget_flap.setContextMenuPolicy(Qt.CustomContextMenu)
        
        
        #初始化数据和内容  
        self.UILogic()   
        
    def setModel(self, tModel):
        """
        初始化本节点的xml描述文档
        """
        
        #self.Model = tModel        
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
        
    @pyqtSlot(int)
    def on_comboBox_FTYPE_currentIndexChanged(self, index):
        """
        Slot documentation goes here.
        
        @param index DESCRIPTION
        @type int
        """
        self.UILogic() 
    
    @pyqtSlot(int)
    def on_comboBox_NTYPE_currentIndexChanged(self, index):
        """
        Slot documentation goes here.
        
        @param index DESCRIPTION
        @type int
        """
        self.UILogic() 
    
    @pyqtSlot(QPoint)
    def on_tableWidget_flap_customContextMenuRequested(self, pos):
        """
        Slot documentation goes here.
        
        @param pos DESCRIPTION
        @type QPoint
        """

        self.curPos = pos
        self.curWidget = self.tableWidget_flap        
        posG = self.curWidget.mapToGlobal(pos)
        self.popMenu = QMenu(self.curWidget)
        self.popMenu.addAction(self.actionAddRow)
        self.popMenu.addAction(self.actionDeleteRow)
        self.curWidget.setContextMenuPolicy(Qt.CustomContextMenu)
        self.curN = self.NDELTA
        
        self.popMenu.exec(posG)
    
    @pyqtSlot(QPoint)
    def on_tableWidget_delta_customContextMenuRequested(self, pos):
        """
        Slot documentation goes here.
        
        @param pos DESCRIPTION
        @type QPoint
        """

        self.curPos = pos
        self.curWidget = self.tableWidget_delta        
        posG = self.curWidget.mapToGlobal(pos)
        self.popMenu = QMenu(self.curWidget)
        self.popMenu.addAction(self.actionAddRow)
        self.popMenu.addAction(self.actionDeleteRow)
        self.curWidget.setContextMenuPolicy(Qt.CustomContextMenu)
        self.curN = self.NDELTA
        
        self.popMenu.exec(posG)
