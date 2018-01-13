# -*- coding: utf-8 -*-

"""
Module implementing WGPLNF.
"""

from PyQt5.QtCore import pyqtSlot,  QPoint
from PyQt5.QtWidgets import QWidget
#from PyQt5.QtGui import  QIcon, QPixmap


from PyDatcomLab.Core import dcModel
from PyDatcomLab.GUIs.PlaneConfiguration import DatcomCARD as DC

import logging
from .Ui_WGPLNF import Ui_Form


class WGPLNF(QWidget, Ui_Form):
    """
    Class documentation goes here.
    """
    def __init__(self, parent=None, tModel = None):
        """
        Constructor
        
        @param parent reference to the parent widget
        @type QWidget
        """
        super(WGPLNF, self).__init__(parent)
        self.setupUi(self)
        #创建日志
        self.logger = logging.getLogger(r'Datcomlogger')
        
        #开始核心数据的定义
        self.NameList = 'WGPLNF'
        self.VariableList = {
                #
                'CHRDTP':{  'TYPE':'REAL'},
                'CHRDBP':{  'TYPE':'REAL'},
                'CHRDR':{   'TYPE':'REAL'},
                'SSPNOP':{  'TYPE':'REAL'},
                'SSPNE':{   'TYPE':'REAL'},
                'SSPN':{    'TYPE':'REAL'}, 
                'SAVSI':{   'TYPE':'REAL'}, 
                'SAVSO':{   'TYPE':'REAL'},
                'CHSTAT':{ 'TYPE':'REAL'},
                'TWISTA':{ 'TYPE':'REAL'},
                'SSPNDD':{ 'TYPE':'REAL'},
                'DHDADI':{ 'TYPE':'REAL'},
                'DHDADO':{ 'TYPE':'REAL'},                
                #
                'TYPE':{ 'TYPE':'List', 'Range':['1.0' , '2.0', '3.0'], 'Default':'1.0'},             
                #垂尾受机翼平尾影响参数定义
                #'SVWB':{  'TYPE':'Array' , 'Limit':[0, 20]  , 'Group':'HTArea'}  , 
                #'SVB':{   'TYPE':'Array' , 'Limit':[0, 20]  , 'Group':'HTArea'}  , 
                #'SVHB':{  'TYPE':'Array' , 'Limit':[0, 20]  , 'Group':'HTArea'}  , 
 
        } 
        self.NMACHLinkTable = [ ]
        self.RuleNumToCount =[]
        self.RuleIndexToCombo = []
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
        #调用公用的刷新逻辑      
        self.DatcomCARD.UILogic()  

        #特有的刷新逻辑
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
    
    
    @pyqtSlot()
    def on_lineEdit_CHRDTP_editingFinished(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        self.UILogic()  
    
    @pyqtSlot()
    def on_lineEdit_CHRDBP_editingFinished(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        self.UILogic()  
    
    @pyqtSlot()
    def on_lineEdit_CHRDR_editingFinished(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        self.UILogic()  
    
    @pyqtSlot()
    def on_lineEdit_SSPNOP_editingFinished(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        self.UILogic()  
    
    @pyqtSlot()
    def on_lineEdit_SSPNE_editingFinished(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        self.UILogic()  
    
    @pyqtSlot()
    def on_lineEdit_SSPN_editingFinished(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        self.UILogic()  
    
    @pyqtSlot()
    def on_lineEdit_SAVSI_editingFinished(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        self.UILogic()  
    
    @pyqtSlot()
    def on_lineEdit_SAVSO_editingFinished(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        self.UILogic()  
    
    @pyqtSlot()
    def on_lineEdit_TWISTA_editingFinished(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        self.UILogic()  
    
    @pyqtSlot()
    def on_lineEdit_CHSTAT_editingFinished(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        self.UILogic()  
    
    @pyqtSlot()
    def on_lineEdit_SSPNDD_editingFinished(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        self.UILogic()  
    
    @pyqtSlot()
    def on_lineEdit_DHDADI_editingFinished(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        self.UILogic()  
    
    @pyqtSlot()
    def on_lineEdit_DHDADO_editingFinished(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        self.UILogic()  
    
    @pyqtSlot(int)
    def on_comboBox_TYPE_currentIndexChanged(self, index):
        """
        Slot documentation goes here.
        
        @param index DESCRIPTION
        @type int
        """
        # TODO: not implemented yet
        self.UILogic()  
