# -*- coding: utf-8 -*-

"""
Module implementing BODY.
"""

from PyQt5.QtCore import pyqtSlot
from PyQt5.QtWidgets import QWidget, QTableWidgetItem
from PyQt5.QtGui import QStandardItemModel

from .Ui_BODY import Ui_Form
from xml.etree import ElementTree  as ET

class BODY(QWidget, Ui_Form):
    """
    Class documentation goes here.
    """
    def __init__(self, parent=None):
        """
        Constructor
        
        @param parent reference to the parent widget
        @type QWidget
        """
        super(BODY, self).__init__(parent)
        self.setupUi(self)
        
        
        #修改
        self.model = QStandardItemModel()
        self.initiate()
        self.initXMLModel()
        
        
    def initiate(self): 
        """
        """  
        #icon2 = QImage(r":/") 
 
        #self.Ui.image_BODY.setPixmap(icon2)
        self.label_BNOSE.setEnabled(False)
        self.label_BTAIL.setEnabled(False)
        self.label_BLN.setEnabled(False)
        self.label_BLA.setEnabled(False)
        self.checkBox_DS.setEnabled(False)
        self.comboBox_BNOSE.setEnabled(False)
        self.comboBox_BTAIL.setEnabled(False)
        self.lineEdit_BLN.setEnabled(False)
        self.lineEdit_BLA.setEnabled(False)
        self.lineEdit_DS.setEnabled(False)
        self.comboBox_ITYPE.setEnabled(False)
        # initiate the table
        self.Tab_ComboVariables.setRowCount(1)
        self.Tab_ComboVariables.setColumnCount(1)
        self.Tab_ComboVariables.setHorizontalHeaderItem(0, QTableWidgetItem('截面半径'))
        self.Tab_ComboVariables.setItem(0, 0, QTableWidgetItem())
        self.lineEdit_BODY_Num.setText('')
        self.lineEdit_BLN.setText('')
        self.label_BTAIL.setText('')
        self.lineEdit_DS.setText('')
    
    def initXMLModel(self):
        """
        初始化本节点的xml描述文档
        """
        baseXML = """
    <NAMELIST name ='BODY',alias ='机身'>
        <VARIABLE name ='NX',alias = '截面数',varType = 'INTEGER'>10.0</VARIABLE>
        <VARIABLE name ='X',alias = '截面坐标X',varType = 'REAL',startId ='1'>
        0.0, .175,.322,.530,.850,1.46,2.5,3.43,3.97,4.57,
        </VARIABLE>
        <VARIABLE name ='S',alias = '截面面积S',varType = 'REAL',startId ='1'>
        0.0,.00547,.022,.0491,.0872,.136,.136,.136,.0993,.0598,
        </VARIABLE>
    </NAMELIST>"""
        self.docxml = ET.fromstring(baseXML)
        
    def getXML(self):
        """
        返回本节点的xml描述文件
        """
        return self.docxml
    
    def setXML(self, tXML):
        """
        将界面的内容设置成tXML对应的内容
        """
        
    
    @pyqtSlot(int)
    def on_comboBox_ITYPE_currentIndexChanged(self, index):
        """
        Slot documentation goes here.
        
        @param index DESCRIPTION
        @type int
        """
        # TODO: not implemented yet
        raise NotImplementedError
    
    @pyqtSlot(int)
    def on_checkBox_ITYPE_stateChanged(self, p0):
        """
        Slot documentation goes here.
        
        @param p0 DESCRIPTION
        @type int
        """
        # TODO: not implemented yet
        raise NotImplementedError
    
    @pyqtSlot(int)
    def on_comboBox_BNOSE_currentIndexChanged(self, index):
        """
        Slot documentation goes here.
        
        @param index DESCRIPTION
        @type int
        """
        # TODO: not implemented yet
        raise NotImplementedError
    
    @pyqtSlot(int)
    def on_comboBox_BTAIL_currentIndexChanged(self, index):
        """
        Slot documentation goes here.
        
        @param index DESCRIPTION
        @type int
        """
        # TODO: not implemented yet
        raise NotImplementedError
    
    @pyqtSlot(str)
    def on_lineEdit_BLN_textChanged(self, p0):
        """
        Slot documentation goes here.
        
        @param p0 DESCRIPTION
        @type str
        """
        # TODO: not implemented yet
        raise NotImplementedError
    
    @pyqtSlot(str)
    def on_lineEdit_BLA_textChanged(self, p0):
        """
        Slot documentation goes here.
        
        @param p0 DESCRIPTION
        @type str
        """
        # TODO: not implemented yet
        raise NotImplementedError
    
    @pyqtSlot(str)
    def on_lineEdit_DS_textChanged(self, p0):
        """
        Slot documentation goes here.
        
        @param p0 DESCRIPTION
        @type str
        """
        # TODO: not implemented yet
        raise NotImplementedError
    
    @pyqtSlot(int)
    def on_comboBox_Conf_currentIndexChanged(self, index):
        """
        Slot documentation goes here.
        
        @param index DESCRIPTION
        @type int
        """
        # TODO: not implemented yet
        raise NotImplementedError
    
    @pyqtSlot(str)
    def on_lineEdit_BODY_Num_textChanged(self, p0):
        """
        Slot documentation goes here.
        
        @param p0 DESCRIPTION
        @type str
        """
        # TODO: not implemented yet
        raise NotImplementedError
    
    @pyqtSlot(int)
    def on_comboBox_ComboVariables_currentIndexChanged(self, index):
        """
        Slot documentation goes here.
        
        @param index DESCRIPTION
        @type int
        """
        # TODO: not implemented yet
        raise NotImplementedError
    
    @pyqtSlot()
    def on_pushButton_ZU_clicked(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        raise NotImplementedError
    
    @pyqtSlot()
    def on_pushButton_ZL_clicked(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        raise NotImplementedError
    
    @pyqtSlot(int)
    def on_comboBox_METHED_currentIndexChanged(self, index):
        """
        Slot documentation goes here.
        
        @param index DESCRIPTION
        @type int
        """
        # TODO: not implemented yet
        raise NotImplementedError
