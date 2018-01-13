# -*- coding: utf-8 -*-

"""
Module implementing PROPWR.
"""

from PyQt5.QtCore import pyqtSlot
from PyQt5.QtWidgets import QWidget

from PyDatcomLab.GUIs.PlaneConfiguration import DatcomCARD as DC
from PyDatcomLab.Core import dcModel 


import logging

from Ui_PROPWR import Ui_PROPWR


class PROPWR(QWidget, Ui_PROPWR):
    """
    Class documentation goes here.
    """
    def __init__(self, parent=None, tModel = None):
        """
        Constructor
        
        @param parent reference to the parent widget
        @type QWidget
        """
        super(PROPWR, self).__init__(parent)
        self.setupUi(self)
        #创建日志
        self.logger = logging.getLogger(r'Datcomlogger')
        #开始核心数据的定义
        self.NameList = 'PROPWR'
        self.VariableList = {

                'AIETLP':{  'TYPE':'REAL'},
                'NENGSP':{  'TYPE':'INT'},
                'THSTCP':{  'TYPE':'REAL'},
                'PHALOC':{  'TYPE':'REAL'},
                'PHVLOC':{  'TYPE':'REAL'},
                'PRPRAD':{  'TYPE':'REAL'},
                'ENGFCT':{  'TYPE':'REAL'},
                'BWAPR3':{  'TYPE':'REAL'},
                'BWAPR6':{  'TYPE':'REAL'},
                'BWAPR9':{  'TYPE':'REAL'},
                'NOPBPE':{  'TYPE':'INT'},
                'BAPR75':{  'TYPE':'REAL'},                                
                'YP':{      'TYPE':'REAL'},                
                'CROT':{  'TYPE':'List','Range':['.TRUE.', '.FLASE.']   , 'Default':'.TRUE.'},

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
        self.InitComboVar(tModel)
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
    
    @pyqtSlot(bool)
    def on_groupBox_KN_toggled(self, p0):
        """
        Slot documentation goes here.
        
        @param p0 DESCRIPTION
        @type bool
        """
        # TODO: not implemented yet
        if p0 :
            self.groupBox_propeller.setChecked(False)
        else:
            self.groupBox_propeller.setChecked(True)
    
    @pyqtSlot(bool)
    def on_groupBox_propeller_toggled(self, p0):
        """
        Slot documentation goes here.
        
        @param p0 DESCRIPTION
        @type bool
        """
        # TODO: not implemented yet
        if p0 :
            self.groupBox_KN.setChecked(False)
        else:
            self.groupBox_KN.setChecked(True)
