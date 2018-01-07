# -*- coding: utf-8 -*-

"""
Module implementing SYNTHS.
"""

from PyQt5.QtCore import pyqtSlot, Qt
from PyQt5.QtWidgets import QWidget
from PyQt5.QtGui import QDoubleValidator

from PyDatcomLab.Core import dcModel

from .Ui_SYNTHS import Ui_Form


class SYNTHS(QWidget, Ui_Form):
    """
    Class documentation goes here.
    SCLAE ：缩放因子，用来适应风洞数据
    """
    def __init__(self, parent=None, model = None):
        """
        Constructor
        
        @param parent reference to the parent widget
        @type QWidget
        """
        super(SYNTHS, self).__init__(parent)
        self.setupUi(self)
        
        #初始化数据
        self.InitDoc(model)
 
        #初始化界面
        self.initUI()
        
        
    def InitDoc(self, tModel):
        """
        分析并初始化后台数据
        """
        if tModel is None:
            tModel = dcModel.dcModel()
            self.logger.info('SYNTHS内部创建了一个doc类')
        
        self.model = tModel        
        #如果没有数据，使用默认配置
        
        #赋初值   
        #XCG
        tVar = self.model.getNamelistVar('SYNTHS','XCG')
        tVar = '0.0' if tVar is None else str(tVar)
        self.XCG.setText(tVar)
        #ZCG
        tVar = self.model.getNamelistVar('SYNTHS','ZCG')
        tVar = '0.0' if tVar is None else str(tVar)
        self.ZCG.setText(tVar)
        #XW
        tVar = self.model.getNamelistVar('SYNTHS','XW')
        tVar = '0.0' if tVar is None else str(tVar)
        self.XW.setText(tVar)
        #ZW
        tVar = self.model.getNamelistVar('SYNTHS','ZW')
        tVar = '0.0' if tVar is None else str(tVar)
        self.ZW.setText(tVar)
        #ALIW
        tVar = self.model.getNamelistVar('SYNTHS','ALIW')
        tVar = '0.0' if tVar is None else str(tVar)
        self.ALIW.setText(tVar)
        #XH
        tVar = self.model.getNamelistVar('SYNTHS','XH')
        tVar = '0.0' if tVar is None else str(tVar)
        self.XH.setText(tVar)        
        #ZH
        tVar = self.model.getNamelistVar('SYNTHS','ZH')
        tVar = '0.0' if tVar is None else str(tVar)
        self.ZH.setText(tVar)
        #ALIH
        tVar = self.model.getNamelistVar('SYNTHS','ALIH')
        tVar = '0.0' if tVar is None else str(tVar)
        self.ALIH.setText(tVar)
        #HINAX
        tVar = self.model.getNamelistVar('SYNTHS','HINAX')
        tVar = '0.0' if tVar is None else str(tVar)
        self.HINAX.setText(tVar)
        #XV
        tVar = self.model.getNamelistVar('SYNTHS','XV')
        tVar = '0.0' if tVar is None else str(tVar)
        self.XV.setText(tVar)
        #ZV
        tVar = self.model.getNamelistVar('SYNTHS','ZV')
        tVar = '0.0' if tVar is None else str(tVar)
        self.ZV.setText(tVar)
        #XVF
        tVar = self.model.getNamelistVar('SYNTHS','XVF')
        tVar = '0.0' if tVar is None else str(tVar)
        self.XVF.setText(tVar)
        #ZVF
        tVar = self.model.getNamelistVar('SYNTHS','ZVF')
        tVar = '0.0' if tVar is None else str(tVar)
        self.ZVF.setText(tVar)
        #SCALE
        tVar = self.model.getNamelistVar('SYNTHS','SCALE')
        tVar = '1.0' if tVar is None else str(tVar)
        self.SCALE.setText(tVar)

  
    def initUI(self):
        """
        执行初始化UI的工作
        给文本框赋初值和验证器
        """
        self.checkBox_HINAX.setCheckState(Qt.Unchecked)
        self.HINAX.setEnabled(False)
        #增加验证器
        postionMax = 1000000
        self.XCG.setValidator(QDoubleValidator(self))
        self.ZCG.setValidator(QDoubleValidator(self))
        self.XW.setValidator(QDoubleValidator(self))
        self.ZW.setValidator(QDoubleValidator(self))
        self.ALIW.setValidator(QDoubleValidator(self))
        self.XH.setValidator(QDoubleValidator(self))
        self.ZH.setValidator(QDoubleValidator(self))
        self.ALIH.setValidator(QDoubleValidator(self))
        self.HINAX.setValidator(QDoubleValidator(self))
        self.XV.setValidator(QDoubleValidator(self))
        self.ZV.setValidator(QDoubleValidator(self))
        self.XVF.setValidator(QDoubleValidator(self))
        self.ZVF.setValidator(QDoubleValidator(self))
        aVator = QDoubleValidator(self)
        aVator.setRange(0, postionMax)
        self.SCALE.setValidator(aVator)       

        
        #刷新界面管理
        self.UILogin()
        
    def UILogin(self):
        """
        整体负责界面的逻辑控制
        """
        if self.checkBox_HINAX.checkState() == Qt.Unchecked:
            self.HINAX.setEnabled(False)
        elif self.checkBox_HINAX.checkState() == Qt.Checked:
            self.HINAX.setEnabled(True)

    def getDoc(self):
        """
        将界面的内容刷新到变量model
        """
        
        #执行界面刷新
        
        #获取界面输入值
        self.model.setNamelist('SYNTHS','XCG',float(self.XCG.text()))
        self.model.setNamelist('SYNTHS','ZCG',float(self.ZCG.text()))
        self.model.setNamelist('SYNTHS','XW',float(self.XW.text()))
        self.model.setNamelist('SYNTHS','ZW',float(self.ZW.text()))
        self.model.setNamelist('SYNTHS','ALIW',float(self.ALIW.text()))
        self.model.setNamelist('SYNTHS','XH',float(self.XH.text()))
        self.model.setNamelist('SYNTHS','ZH',float(self.ZH.text()))
        self.model.setNamelist('SYNTHS','ALIH',float(self.ALIH.text()))
        self.model.setNamelist('SYNTHS','HINAX',float(self.HINAX.text()))
        self.model.setNamelist('SYNTHS','XV',float(self.XV.text()))
        self.model.setNamelist('SYNTHS','ZV',float(self.ZV.text()))
        self.model.setNamelist('SYNTHS','XVF',float(self.XVF.text()))
        self.model.setNamelist('SYNTHS','ZVF',float(self.ZVF.text()))
        self.model.setNamelist('SYNTHS','SCALE',float(self.SCALE.text()))
        
        if self.VERTUP.currentIndex == 0:
            self.model.setNamelist('SYNTHS','VERTUP','.TRUE.')
        elif self.VERTUP.currentIndex == 1:
            self.model.setNamelist('SYNTHS','VERTUP','.FALSE.')
        
        return self.model
        

    
    @pyqtSlot(int)
    def on_checkBox_HINAX_stateChanged(self, p0):
        """
        Slot documentation goes here.
        
        @param p0 DESCRIPTION
        @type int
        """
        # TODO: not implemented yet
        self.UILogin()

        
    
    @pyqtSlot(str)
    def on_VERTUP_currentIndexChanged(self, p0):
        """
        Slot documentation goes here.
        
        @param p0 DESCRIPTION
        @type str
        """
        # TODO: not implemented yet
        #raise NotImplementedError

