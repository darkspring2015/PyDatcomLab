# -*- coding: utf-8 -*-

"""
Module implementing WGPLNF.
"""

from PyQt5.QtCore import pyqtSlot
from PyQt5.QtWidgets import QWidget
from PyQt5.QtGui import QDoubleValidator
from PyDatcomLab.Core import dcModel
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
        
        #修改后台的数据
        if tModel is None:
            tModel = dcModel.dcModel('J6', '常规布局')         
        self.model = tModel  
        #配置界面
        self.lineEdit_CHRDTP.setValidator(QDoubleValidator(self))
        self.lineEdit_CHRDBP.setValidator(QDoubleValidator(self))
        self.lineEdit_CHRDR.setValidator(QDoubleValidator(self))
        self.lineEdit_SSPNOP.setValidator(QDoubleValidator(self))
        self.lineEdit_SSPNE.setValidator(QDoubleValidator(self))
        self.lineEdit_SSPN.setValidator(QDoubleValidator(self))        
        self.lineEdit_SAVSI.setValidator(QDoubleValidator(self))
        self.lineEdit_SAVSO.setValidator(QDoubleValidator(self))
        self.lineEdit_TWISTA.setValidator(QDoubleValidator(self))
        self.lineEdit_CHSTAT.setValidator(QDoubleValidator(self))
        self.lineEdit_SSPNDD.setValidator(QDoubleValidator(self))
        self.lineEdit_DHDADI.setValidator(QDoubleValidator(self))
        self.lineEdit_DHDADO.setValidator(QDoubleValidator(self))
        
        
        #self.NX.setEnabled(False)
        #初始化数据和内容
        self.InitDoc()   
        self.UILogic()     
        
    def InitDoc(self):
        """
        分析并初始化后台数据
        """
        
#        self.lineEdit_CHRDTP.setValidator(QDoubleValidator(self))
        tVar = self.model.getNamelistVar('WGPLNF','CHRDTP')
        tVar = '0.0' if tVar is None else str(tVar)
        self.lineEdit_CHRDTP.setText(tVar)
#        self.lineEdit_CHRDBP.setValidator(QDoubleValidator(self))
        tVar = self.model.getNamelistVar('WGPLNF','CHRDBP')
        tVar = '0.0' if tVar is None else str(tVar)
        self.lineEdit_CHRDBP.setText(tVar)
#        self.lineEdit_CHRDR.setValidator(QDoubleValidator(self))
        tVar = self.model.getNamelistVar('WGPLNF','CHRDR')
        tVar = '0.0' if tVar is None else str(tVar)
        self.lineEdit_CHRDR.setText(tVar)
#        self.lineEdit_SSPNOP.setValidator(QDoubleValidator(self))
        tVar = self.model.getNamelistVar('WGPLNF','SSPNOP')
        tVar = '0.0' if tVar is None else str(tVar)
        self.lineEdit_SSPNOP.setText(tVar)
#        self.lineEdit_SSPNE.setValidator(QDoubleValidator(self))
        tVar = self.model.getNamelistVar('WGPLNF','SSPNE')
        tVar = '0.0' if tVar is None else str(tVar)
        self.lineEdit_SSPNE.setText(tVar)
#        self.lineEdit_SSPN.setValidator(QDoubleValidator(self)) 
        tVar = self.model.getNamelistVar('WGPLNF','SSPN')
        tVar = '0.0' if tVar is None else str(tVar)
        self.lineEdit_SSPN.setText(tVar)
#        self.lineEdit_SAVSI.setValidator(QDoubleValidator(self))
        tVar = self.model.getNamelistVar('WGPLNF','SAVSI')
        tVar = '0.0' if tVar is None else str(tVar)
        self.lineEdit_SAVSI.setText(tVar)
#        self.lineEdit_SAVSO.setValidator(QDoubleValidator(self))
        tVar = self.model.getNamelistVar('WGPLNF','SAVSO')
        tVar = '0.0' if tVar is None else str(tVar)
        self.lineEdit_SAVSO.setText(tVar)
#        self.lineEdit_TWISTA.setValidator(QDoubleValidator(self))
        tVar = self.model.getNamelistVar('WGPLNF','TWISTA')
        tVar = '0.0' if tVar is None else str(tVar)
        self.lineEdit_TWISTA.setText(tVar)
#        self.lineEdit_CHSTAT.setValidator(QDoubleValidator(self))
        tVar = self.model.getNamelistVar('WGPLNF','CHSTAT')
        tVar = '0.0' if tVar is None else str(tVar)
        self.lineEdit_CHSTAT.setText(tVar)
#        self.lineEdit_SSPNDD.setValidator(QDoubleValidator(self))
        tVar = self.model.getNamelistVar('WGPLNF','SSPNDD')
        tVar = '0.0' if tVar is None else str(tVar)
        self.lineEdit_SSPNDD.setText(tVar)
#        self.lineEdit_DHDADI.setValidator(QDoubleValidator(self))
        tVar = self.model.getNamelistVar('WGPLNF','DHDADI')
        tVar = '0.0' if tVar is None else str(tVar)
        self.lineEdit_DHDADI.setText(tVar)
#        self.lineEdit_DHDADO.setValidator(QDoubleValidator(self))
        tVar = self.model.getNamelistVar('WGPLNF','DHDADO')
        tVar = '0.0' if tVar is None else str(tVar)
        self.lineEdit_DHDADO.setText(tVar)
        
        #TYPE
        tVar = self.model.getNamelistVar('WGPLNF','TYPE')
        tVar = 0 if tVar is None else int(float(tVar)) -1
        self.comboBox_TYPE.setCurrentIndex(tVar)
        



    def setModel(self, tModel):
        """
        初始化本节点的xml描述文档
        """
        
        self.Model = tModel        
        #执行参数配置过程        
        self.InitDoc()
        
        self.UILogic()


    def getDoc(self):
        """
        将界面的内容刷新到变量model
        """
        
        #执行界面刷新
        #TYPE
        tTYPE = self.comboBox_TYPE.currentIndex() + 1
        self.model.setNamelist( 'WGPLNF' , 'TYPE', 
                                  '%d.'%(tTYPE) )

#        self.lineEdit_CHRDTP
        self.model.setNamelist( 'WGPLNF' , 'CHRDTP',float(self.lineEdit_CHRDTP.text()) )
#        self.lineEdit_CHRDBP
        if tTYPE == 1:        
            self.model.setNamelist( 'WGPLNF' , 'CHRDBP',None )
        else:
            self.model.setNamelist( 'WGPLNF' , 'CHRDBP',float(self.lineEdit_CHRDBP.text()) )
#        self.lineEdit_CHRDR
        self.model.setNamelist( 'WGPLNF' , 'CHRDR',float(self.lineEdit_CHRDR.text()) )
#        self.lineEdit_SSPNOP
        if tTYPE == 1:
            self.model.setNamelist( 'WGPLNF' , 'SSPNOP',None )
        else:
            self.model.setNamelist( 'WGPLNF' , 'SSPNOP',float(self.lineEdit_SSPNOP.text()) )
        
#        self.lineEdit_SSPNE
        self.model.setNamelist( 'WGPLNF' , 'SSPNE',float(self.lineEdit_SSPNE.text()) )
#        self.lineEdit_SSPN      
        self.model.setNamelist( 'WGPLNF' , 'SSPN',float(self.lineEdit_SSPN.text()) )
#        self.lineEdit_SAVSI
        self.model.setNamelist( 'WGPLNF' , 'SAVSI',float(self.lineEdit_SAVSI.text()) )
#        self.lineEdit_SAVSO
        if tTYPE == 1:
            self.model.setNamelist( 'WGPLNF' , 'SAVSO',None )
        else:
            self.model.setNamelist( 'WGPLNF' , 'SAVSO',float(self.lineEdit_SAVSO.text()) )

#        self.lineEdit_TWISTA
        self.model.setNamelist( 'WGPLNF' , 'TWISTA',float(self.lineEdit_TWISTA.text()) )
#        self.lineEdit_CHSTAT
        self.model.setNamelist( 'WGPLNF' , 'CHSTAT',float(self.lineEdit_CHSTAT.text()) )
#        self.lineEdit_SSPNDD
        if tTYPE == 1:
            self.model.setNamelist( 'WGPLNF' , 'SSPNDD',None )
        else:
            self.model.setNamelist( 'WGPLNF' , 'SSPNDD',float(self.lineEdit_SSPNDD.text()) )

#        self.lineEdit_DHDADI
        self.model.setNamelist( 'WGPLNF' , 'DHDADI',float(self.lineEdit_DHDADI.text()) )
#        self.lineEdit_DHDADO
        self.model.setNamelist( 'WGPLNF' , 'DHDADO',float(self.lineEdit_DHDADO.text()) )

        
        #获取界面输入值
        return self.model
        
    def UILogic(self):
        """
        在此刷新UI，需要根据不同的情况执行判断
        """
        if self.comboBox_TYPE.currentIndex() == 0: #不需要部分输入
            self.lineEdit_SSPNOP.setEnabled(False)
            self.lineEdit_CHRDBP.setEnabled(False)
            self.lineEdit_SAVSO.setEnabled(False)
            self.lineEdit_SSPNDD.setEnabled(False)
        else:
            self.lineEdit_SSPNOP.setEnabled(True)
            self.lineEdit_CHRDBP.setEnabled(True)
            self.lineEdit_SAVSO.setEnabled(True)
            self.lineEdit_SSPNDD.setEnabled(True)
            
            
            
        
        
    
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
