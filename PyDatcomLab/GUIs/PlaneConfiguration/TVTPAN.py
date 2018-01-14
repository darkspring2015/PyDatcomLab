# -*- coding: utf-8 -*-

"""
Module implementing TVTPAN.
"""

#from PyQt5.QtCore import pyqtSlot
from PyQt5.QtWidgets import QWidget

from PyDatcomLab.GUIs.PlaneConfiguration import DatcomCARD as DC
from PyDatcomLab.Core import dcModel 

import logging

from Ui_TVTPAN import Ui_TVTPAN


class TVTPAN(QWidget, Ui_TVTPAN):
    """
    Class documentation goes here.
    """
    def __init__(self, parent=None, tModel = None):
        """
        Constructor
        
        @param parent reference to the parent widget
        @type QWidget
        """
        super(TVTPAN, self).__init__(parent)
        self.setupUi(self)
        #创建日志
        self.logger = logging.getLogger(r'Datcomlogger')
        #开始核心数据的定义
        self.NameList = 'TVTPAN'
        self.VariableList = {
                'BVP':{     'TYPE':'REAL'},
                'BV':{      'TYPE':'REAL'},
                'BDV':{     'TYPE':'REAL'},
                'BH':{      'TYPE':'REAL'},
                'SV':{      'TYPE':'REAL'},
                'VPHITE':{  'TYPE':'REAL'},
                'VLP':{     'TYPE':'REAL'},
                'ZP':{      'TYPE':'REAL'},
        }  
        self.NMACHLinkTable = []
        self.RuleNumToCount = []
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
#        self.curPos = QPoint(0, 0)
#        self.curWidget = None
#        self.curN = None
#        self.popMenu = None        
#        self.tableWidget_GHeight.setContextMenuPolicy(Qt.CustomContextMenu)
        
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
    
    
