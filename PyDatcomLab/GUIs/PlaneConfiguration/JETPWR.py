# -*- coding: utf-8 -*-

"""
Module implementing JETPWR.
"""

from PyQt5.QtCore import pyqtSlot
from PyQt5.QtWidgets import QWidget

from PyDatcomLab.GUIs.PlaneConfiguration import DatcomCARD as DC
from PyDatcomLab.Core import dcModel 


import logging

from Ui_JETPWR import Ui_JETPWR


class JETPWR(QWidget, Ui_JETPWR):
    """
    Class documentation goes here.
    """
    def __init__(self, parent=None, tModel = None):
        """
        Constructor
        
        @param parent reference to the parent widget
        @type QWidget
        """
        super(JETPWR, self).__init__(parent)
        self.setupUi(self)
        #创建日志
        self.logger = logging.getLogger(r'Datcomlogger')
        #开始核心数据的定义
        self.NameList = 'JETPWR'
        self.VariableList = {
                'AIETLJ':{  'TYPE':'REAL'},
                'NENGSJ':{  'TYPE':'INT'},
                'THSTCJ':{  'TYPE':'REAL'},
                'JIALOC':{  'TYPE':'REAL'},
                'JEVLOC':{  'TYPE':'REAL'},
                'JINLTA':{  'TYPE':'REAL'},
                'JEANGL':{  'TYPE':'REAL'},
                'JEVEJO':{  'TYPE':'REAL'},
                'AMBTMP':{  'TYPE':'REAL'},
                'JESTMP':{  'TYPE':'REAL'},
                'JELLOC':{  'TYPE':'INT'},
                'JETOTP':{  'TYPE':'REAL'},                                
                'AMBSTP':{  'TYPE':'REAL'},                
                'JERAD':{   'TYPE':'REAL'},  

        }  
        self.NMACHLinkTable = []
        self.RuleNumToCount =[]
        self.RuleIndexToCombo = []
     
  
        #修改后台的数据
        if tModel is None:
            tModel = dcModel.dcModel('J6', '常规布局')  
        #定义数据
        self.DatcomCARD = DC.DatcomCARD(self)
        #self.InitComboVar(tModel)
        self.DatcomCARD.InitUi()
        self.DatcomCARD.setModel(tModel)   #设置模型
        
        
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
