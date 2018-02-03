# -*- coding: utf-8 -*-

"""
Module implementing OPTIONS.
"""

from PyQt5.QtCore import pyqtSlot, Qt
from PyQt5.QtWidgets import QWidget
from PyQt5.QtGui import QDoubleValidator

from PyDatcomLab.Core import dcModel 

from Ui_OPTINS import Ui_OPTINS
import logging

class OPTINS(QWidget, Ui_OPTINS):
    """
    Class documentation goes here.
    """
    def __init__(self, parent=None, tModel = None):
        """
        Constructor
        
        @param parent reference to the parent widget
        @type QWidget
        """
        super(OPTINS, self).__init__(parent)
        self.setupUi(self)
        
        #创建日志
        self.logger = logging.getLogger(r'Datcomlogger')
        
        #创建输入验证器
        self.ROUGFC.setValidator(QDoubleValidator(self))
        self.SREF.setValidator(QDoubleValidator(self))
        self.CBARR.setValidator(QDoubleValidator(self))        
        self.BLREF.setValidator(QDoubleValidator(self))
        
        #修改后台的数据
        if tModel is None:
            tModel = dcModel.dcModel('J6', '常规布局')
            #tModel.setNamelist('OPTIONS', 'ROUGFC', 0)            
        self.model = tModel   
        #开始核心数据的定义
        self.NameList = 'OPTINS'
        self.VariableList = {
                'ROUGFC':{    'TYPE':'REAL' , 'DisplayName':'ROUGFC 表面粗糙度' },    
                'SREF':{      'TYPE':'REAL' , 'DisplayName':'SREF 参考面积' },
                'CBARR':{     'TYPE':'REAL' , 'DisplayName':'CBARR 纵向参考长度值' },               
                'BLREF':{     'TYPE':'REAL' , 'DisplayName':'BLREF 横向参考长度值'  },
        }  

        #初始化数据和内容
        self.InitDoc()
        
        
        
        
        
    def InitDoc(self):
        """
        分析并初始化后台数据
        """
        #checkBox_ROUGFC
        tVar = self.model.getNamelistVar('OPTINS','ROUGFC')
        if tVar is None:
            self.ROUGFC.setEnabled(False)
            self.checkBox_ROUGFC.setCheckState(Qt.Unchecked)
            self.ROUGFC.setText( '0.0')  
        else:
            self.ROUGFC.setEnabled(True)
            self.checkBox_ROUGFC.setCheckState(Qt.Checked)
            self.ROUGFC.setText( str(tVar))            

        #checkBox_SREF
        tVar = self.model.getNamelistVar('OPTINS','SREF')
        if tVar is None:
            self.SREF.setEnabled(False)
            self.checkBox_SREF.setCheckState(Qt.Unchecked)
            self.SREF.setText( '0.0')  
        else:
            self.SREF.setEnabled(True)
            self.checkBox_SREF.setCheckState(Qt.Checked)
            self.SREF.setText( str(tVar))  
            
        #checkBox_CBARR
        tVar = self.model.getNamelistVar('OPTINS','CBARR')
        if tVar is None:
            self.CBARR.setEnabled(False)
            self.checkBox_CBARR.setCheckState(Qt.Unchecked)
            self.CBARR.setText( '0.0')  
        else:
            self.CBARR.setEnabled(True)
            self.checkBox_CBARR.setCheckState(Qt.Checked)
            self.CBARR.setText( str(tVar))              

        #checkBox_BLREF
        
        tVar = self.model.getNamelistVar('OPTINS','BLREF')
        if tVar is None:
            self.BLREF.setEnabled(False)
            self.checkBox_BLREF.setCheckState(Qt.Unchecked)
            self.BLREF.setText( '0.0') 
        else:
            self.BLREF.setEnabled(True)
            self.checkBox_BLREF.setCheckState(Qt.Checked)
            self.BLREF.setText( str(tVar))   
            

        
        
    def setModel(self, tModel):
        """
        初始化本节点的xml描述文档
        """
        
        self.Model = tModel        
        #执行参数配置过程        
        self.InitDoc()
        
    def getDoc(self):
        """
        将界面的内容刷新到变量model
        """        
        #执行界面刷新
        #checkBox_ROUGFC
        if self.checkBox_ROUGFC.checkState() == Qt.Checked:
            self.model.setNamelist( 'OPTINS' , 'ROUGFC', 
                 float(self.ROUGFC.text()) )
        else :
            self.model.setNamelist( 'OPTINS' , 'ROUGFC', None ) #删除对应的数据

        #checkBox_SREF
        if self.checkBox_SREF.checkState() == Qt.Checked:
            self.model.setNamelist( 'OPTINS' , 'SREF', 
                 float(self.SREF.text()) )
        else :
            self.model.setNamelist( 'OPTINS' , 'SREF', None ) #删除对应的数据
            
        #checkBox_CBARR
        
        if self.checkBox_CBARR.checkState() == Qt.Checked:
            self.model.setNamelist( 'OPTINS' , 'CBARR', 
                 float(self.CBARR.text()) )
        else :
            self.model.setNamelist( 'OPTINS' , 'CBARR', None ) #删除对应的数据

        #checkBox_BLREF
        if self.checkBox_BLREF.checkState() == Qt.Checked:
            self.model.setNamelist( 'OPTINS' , 'BLREF', 
                 float(self.BLREF.text()) )
        else :
            self.model.setNamelist( 'OPTINS' , 'BLREF', None ) #删除对应的数据
        
        #返回当前结果
        return self.model
        
        
        
    def UILogic(self):
        """
        在此刷新UI，需要根据不同的情况执行判断
        """
        
        #checkBox_ROUGFC
        if self.checkBox_ROUGFC.checkState() == Qt.Checked:
            self.ROUGFC.setEnabled(True)
        else:
            self.ROUGFC.setEnabled(False)
        #checkBox_SREF
        if self.checkBox_SREF.checkState() == Qt.Checked:
            self.SREF.setEnabled(True)
        else:
            self.SREF.setEnabled(False)
        #checkBox_CBARR
        if self.checkBox_CBARR.checkState() == Qt.Checked:
            self.CBARR.setEnabled(True)
        else:
            self.CBARR.setEnabled(False)
        #checkBox_BLREF
        if self.checkBox_BLREF.checkState() == Qt.Checked:
            self.BLREF.setEnabled(True)
        else:
            self.BLREF.setEnabled(False)
            
        
    
    @pyqtSlot(int)
    def on_checkBox_SREF_stateChanged(self, p0):
        """
        Slot documentation goes here.
        
        @param p0 DESCRIPTION
        @type int
        """
        # TODO: not implemented yet
        self.UILogic()  
    
    @pyqtSlot(int)
    def on_checkBox_ROUGFC_stateChanged(self, p0):
        """
        Slot documentation goes here.
        
        @param p0 DESCRIPTION
        @type int
        """
        # TODO: not implemented yet
        self.UILogic()  
    
    @pyqtSlot(int)
    def on_checkBox_CBARR_stateChanged(self, p0):
        """
        Slot documentation goes here.
        
        @param p0 DESCRIPTION
        @type int
        """
        # TODO: not implemented yet
        self.UILogic()  
    
    @pyqtSlot(int)
    def on_checkBox_BLREF_stateChanged(self, p0):
        """
        Slot documentation goes here.
        
        @param p0 DESCRIPTION
        @type int
        """
        # TODO: not implemented yet
        self.UILogic()  
