# -*- coding: utf-8 -*-

"""
Module implementing bodyFrom.
"""

from PyQt5.QtCore import pyqtSlot
from PyQt5.QtWidgets import QWidget
from PyQt5.QtGui import QStandardItemModel

from .Ui_BODY import Ui_Form


class bodyFrom(QWidget, Ui_Form):
    """
    Class documentation goes here.
    """
    def __init__(self, parent=None):
        """
        Constructor
        
        @param parent reference to the parent widget
        @type QWidget
        """
        super(bodyFrom, self).__init__(parent)
        self.setupUi(self)
        
        
        #修改
        self.model = QStandardItemModel()
        self.initiate()
        
    def initiate(self):
        path = os.path.abspath(os.path.dirname(sys.argv[0]))
        path = path.rstrip("\\BODYz")  # remove the BODY in the path
        path2 = "\\rc\\Body_SubSonic.png"
        filePath = path + path2
        icon = QtGui.QPixmap(filePath)
        icon2 = icon.scaled(500, 400, 0, 0)
        self.Ui.image_BODY.setPixmap(icon2)
        self.Ui.label_BNOSE.setEnabled(False)
        self.Ui.label_BTAIL.setEnabled(False)
        self.Ui.label_BLN.setEnabled(False)
        self.Ui.label_BLA.setEnabled(False)
        self.Ui.checkBox_DS.setEnabled(False)
        self.Ui.comboBox_BNOSE.setEnabled(False)
        self.Ui.comboBox_BTAIL.setEnabled(False)
        self.Ui.lineEdit_BLN.setEnabled(False)
        self.Ui.lineEdit_BLA.setEnabled(False)
        self.Ui.lineEdit_DS.setEnabled(False)
        self.Ui.comboBox_ITYPE.setEnabled(False)
        # initiate the table
        self.Ui.Tab_ComboVariables.setRowCount(1)
        self.Ui.Tab_ComboVariables.setColumnCount(1)
        self.Ui.Tab_ComboVariables.setHorizontalHeaderItem(0, QtWidgets.QTableWidgetItem('截面半径'))
        self.Ui.Tab_ComboVariables.setItem(0, 0, QtWidgets.QTableWidgetItem())
        self.Ui.lineEdit_BODY_Num.setText('')
        self.Ui.lineEdit_BLN.setText('')
        self.Ui.label_BTAIL.setText('')
        self.Ui.lineEdit_DS.setText('')
    
    def initXMLModel(self):
        """
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
