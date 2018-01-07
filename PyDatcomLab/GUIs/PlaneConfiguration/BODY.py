# -*- coding: utf-8 -*-

"""
Module implementing BODY.
"""

from PyQt5.QtCore import pyqtSlot, Qt
from PyQt5.QtWidgets import QWidget, QTableWidgetItem
from PyQt5.QtGui import QStandardItemModel

from PyDatcomLab.Core import dcModel, datcomDefine as DD


from .Ui_BODY import Ui_Form
#from xml.etree import ElementTree  as ET

class BODY(QWidget, Ui_Form):
    """
    Class documentation goes here.
    """
    def __init__(self, parent=None, tModel = None):
        """
        Constructor
        
        @param parent reference to the parent widget
        @type QWidget
        """
        super(BODY, self).__init__(parent)
        self.setupUi(self)  
  
        #修改后台的数据
        self.InitDoc(tModel)
        self.currentTable = 'R-ZU-ZL'
        
        #修改
        self.model = QStandardItemModel()
        self.initiate()
        
        
    def InitDoc(self, tModel):
        """
        分析并初始化后台数据
        """
        if tModel is None:
            tModel = dcModel.dcModel('J6', '常规布局')
        
        self.model = tModel
        
        #根据doc内容执行初始化操作
        bodyDict = self.model.getNamelist('BODY')
        if bodyDict is None: return 
        
        #判断是不是亚音速布局
        ssList = ['BNOSE', 'BTAIL', 'BLN', 'BLA', 'DS']
        aa = [i for i in ssList if i in bodyDict.keys()]
        if len(aa)> 0: #没有高超声速的相关定义参数
            return 
        #加载亚音速布局下的参数
        
        
            
        
    def getDoc(self):
        """
        将界面的内容刷新到变量model
        """        
        #执行界面刷新
        
        if self.comboBox_SpeedRegime.currentIndex <= 1: #亚音速和跨音速
            #执行数据 NX X R S P ELLIP ZU ZL
            self.Model.setNamelist( 'BODY' , 'NX', float(self.NX.text()) )
            #ITYPE
            if not self.comboBox_ITYPE.currentIndex == 1:
                self.Model.setNamelist( 'BODY' , 'ITYPE', 
                                  '%d.'%(self.comboBox_ITYPE.currentIndex() + 1) )
            #Method
            if not self.comboBox_METHED.currentIndex == 0:
                self.Model.setNamelist( 'BODY' , 'METHED', 
                                  '%d.'%(self.comboBox_METHED.currentIndex() + 1) )
            #ELLIP
            if self.checkBox_ELLIP.checkState() == Qt.Checked:
                self.Model.setNamelist( 'BODY' , 'ELLIP', 
                                  self.ELLIP.text()  )
            #X R S P ZU ZL
            for col in range(0, self.Tab_ComboVariables.columnCount()):
                varName = self.Tab_ComboVariables.horizontalHeaderItem(col).text()
                varLst =[]
                for rowN in range(0, self.Tab_ComboVariables.rowCount()):
                    #获得表格对应项的值
                    varLst.append(float(self.Tab_ComboVariables.item(rowN, col).text()))
                #写入到Model中
                self.Model.setNamelist( 'BODY' , varName, varLst, 1)
        if self.comboBox_SpeedRegime.currentIndex > 1: #超音速和高跨音速
            #执行数据 BNOSE BTAIL BLN BLA DS
            pass
        
        
        
        
    def initiate(self): 
        """
        初始化界面
        """  
        self.comboBox_ITYPE.setCurrentIndex(1) #ITPYE 默认是2

        
    
    
    def setModel(self, tModel):
        """
        初始化本节点的xml描述文档
        """
        self.Model = tModel
        
        #执行初始化过程        
        self.initiate()
        
        #获取配置
        
    def UILogic(self):
        """
        在此刷新UI，需要根据不同的情况执行判断
        """
        
        #判断ITYPE是否使能
        if self.checkBox_ITYPE.checkState() == Qt.Checked:
            self.comboBox_ITYPE.setEnabled(True)
        else : self.comboBox_ITYPE.setEnabled(False)
        
        #判断速度范围
        if self.comboBox_SpeedRegime.currentIndex() == 0: #亚音速
            #屏蔽高超声速输入
            self.comboBox_BNOSE.setEnabled(False)
            self.comboBox_BTAIL.setEnabled(False)
            self.lineEdit_BLN.setEnabled(False)
            self.lineEdit_BLA.setEnabled(False)
            self.checkBox_DS.setEnabled(False)
            self.lineEdit_DS.setEnabled(False)
            #使能RSPZUZL
            if not self.currentTable == self.comboBox_VarCombo.currentText():
                #如果变量组合模式发生了变化则要重新创建表格
                self.createTableWidget( self.comboBox_VarCombo.currentText())
        if self.comboBox_SpeedRegime.currentIndex() == 1: #跨音速
            #屏蔽高超声速输入
            self.comboBox_BNOSE.setEnabled(False)
            self.comboBox_BTAIL.setEnabled(False)
            self.lineEdit_BLN.setEnabled(False)
            self.lineEdit_BLA.setEnabled(False)
            self.checkBox_DS.setEnabled(False)
            self.lineEdit_DS.setEnabled(False)
            #使能RSPZUZL
            if not self.currentTable == self.comboBox_VarCombo.currentText():
                #如果变量组合模式发生了变化则要重新创建表格
                self.createTableWidget( self.comboBox_VarCombo.currentText())
                
        if self.comboBox_SpeedRegime.currentIndex() == 2: #超音速
            #激活其他的表格项
            self.comboBox_BNOSE.setEnabled(True)
            self.comboBox_BTAIL.setEnabled(True)
            self.lineEdit_BLN.setEnabled(True)
            self.lineEdit_BLA.setEnabled(True)
            self.checkBox_DS.setEnabled(True)
            self.lineEdit_DS.setEnabled(True)
            
        if self.comboBox_SpeedRegime.currentIndex() == 3: #高超音速
            #激活其他的表格项
            self.comboBox_BNOSE.setEnabled(True)
            self.comboBox_BTAIL.setEnabled(True)
            self.lineEdit_BLN.setEnabled(True)
            self.lineEdit_BLA.setEnabled(True)
            self.checkBox_DS.setEnabled(True)
            self.lineEdit_DS.setEnabled(True)
        
        
        #判断机身参数变量组合
        
    def createTableWidget(self, vars):
        """
        根据vars来创建表头
        """   
        varList = ['X']+ vars.split('-')
        self.Tab_ComboVariables.setColumnCount(len(varList))
        self.Tab_ComboVariables.setRowCount(int(self.NX.text()))
        self.Tab_ComboVariables.setHorizontalHeaderLabels(varList)
        
        
        
    
    @pyqtSlot(int)
    def on_comboBox_ITYPE_currentIndexChanged(self, index):
        """
        ITYPE 代表.
        
        @param index DESCRIPTION
        @type int
        """
        # TODO: not implemented yet
        
        # "1. 直翼，不计算面积效应(Area Rule)"
        # "2. 掠翼，不计算面积效应(系统默认)"
        # "3. 掠翼，计算面积效应"
        itype = "%d."%(self.comboBox_ITYPE.currentIndex() + 1)
        self.Model.setNamelist( 'BODY' , 'ITYPE', itype )
        self.logger.info("选择了ITYPE：%d"%itype )
        #刷新界面
        #self.UiLogin()
            
    
    @pyqtSlot(int)
    def on_comboBox_BNOSE_currentIndexChanged(self, index):
        """
        Slot documentation goes here.
        
        @param index DESCRIPTION
        @type int
        """
        # TODO: not implemented yet
        #1.0 圆锥型机头
        #2.0 尖型机头
        bnose = '%d.0'%(self.comboBox_BNOSE.currentIndex() +1)
        self.Model.setNamelist( 'BODY' , 'BNOSE', bnose )
        self.logger.info("选择了BNOSE：%d"%bnose )
        #刷新界面
        self.UILogic()    
    
    @pyqtSlot(int)
    def on_comboBox_BTAIL_currentIndexChanged(self, index):
        """
        Slot documentation goes here.
        
        @param index DESCRIPTION
        @type int
        """
        # TODO: not implemented yet
        #1.0 圆锥型机尾
        #2.0 尖型机尾
        aVar = '%d.0'%(self.comboBox_BTAIL.currentIndex() +1)
        self.Model.setNamelist( 'BODY' , 'BTAIL', aVar )
        self.logger.info("选择了BTAIL：%d"%aVar )
        
        self.UILogic()    
        

 
    @pyqtSlot(int)
    def on_comboBox_METHED_currentIndexChanged(self, index):
        """
        Slot documentation goes here.
        
        @param index DESCRIPTION
        @type int
        """
        # TODO: not implemented yet
        #1. 使用现有方法（系统默认）
        #2. 使用Jorgensen方法
        
        aVar = '%d.0'%(self.comboBox_METHED.currentIndex() +1)
        self.Model.setNamelist( 'BODY' , 'METHED', aVar )
        self.logger.info("选择了METHED：%d"%aVar )
        
        self.UILogic()    
        
        
    @pyqtSlot(int)
    def on_checkBox_ITYPE_stateChanged(self, p0):
        """
        Slot documentation goes here.
        
        @param p0 DESCRIPTION
        @type int
        """
        # TODO: not implemented yet        
        self.UILogic()    
    
    @pyqtSlot(int)
    def on_comboBox_VarCombo_currentIndexChanged(self, index):
        """
        Slot documentation goes here.
        
        @param index DESCRIPTION
        @type int
        """
        # TODO: not implemented yet        
        self.UILogic()        
        
    
        
    
    @pyqtSlot(str)
    def on_comboBox_SpeedRegime_currentIndexChanged(self, p0):
        """
        选择不同的速度关系时的逻辑关系.
        
        @param p0 DESCRIPTION
        @type str
        """
        # TODO: not implemented yet        
        self.UILogic()
        
        
        
        
