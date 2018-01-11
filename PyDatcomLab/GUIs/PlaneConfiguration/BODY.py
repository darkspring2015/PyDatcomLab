# -*- coding: utf-8 -*-

"""
Module implementing BODY.
"""

from PyQt5.QtCore import pyqtSlot, Qt, QPoint
from PyQt5.QtWidgets import QWidget , QTableWidgetItem , QMessageBox, QMenu
from PyQt5.QtGui import QIntValidator ,QDoubleValidator#, QStandardItemModel#, 

from PyDatcomLab.Core import dcModel #, datcomDefine as DD#
import logging

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
        #创建日志
        self.logger = logging.getLogger(r'Datcomlogger')
        #基础数据
        self.currentTable = "X-R-ZU-ZL"
  
        #修改后台的数据
        if tModel is None:
            tModel = dcModel.dcModel('J6', '常规布局')
            tModel.setNamelist('BODY', 'NX', 0)            
        self.model = tModel   

        #配置界面
        tVlter = QIntValidator(self)
        tVlter.setRange(0, 20)
        self.NX.setValidator(tVlter)
        self.ELLIP.setValidator(QDoubleValidator(self))
        self.lineEdit_BLN.setValidator(QDoubleValidator(self))
        self.lineEdit_BLA.setValidator(QDoubleValidator(self))
        self.lineEdit_DS.setValidator(QDoubleValidator(self))
        
        self.popMenu = QMenu(self.Tab_ComboVariables)
        self.popMenu.addAction(self.actionAddRow)
        self.popMenu.addAction(self.actionDeleteRow)
        self.Tab_ComboVariables.setContextMenuPolicy(Qt.CustomContextMenu)
        self.tablePos = QPoint() #存储相对位置参数

        
        
        #self.NX.setEnabled(False)
        #初始化数据和内容
        self.InitDoc()
        
        
        
        
    def InitDoc(self):
        """
        分析并初始化后台数据
        """

        
        #根据doc内容执行初始化操作
#        bodyDict = self.model.getNamelist('BODY')
#        if bodyDict is None:    return 
        
        #判断是不是亚音速布局
#        ssList = ['BNOSE', 'BTAIL', 'BLN', 'BLA', 'DS']
#        aa = [i for i in ssList if i in bodyDict.keys()]
#        if len(aa)> 0: #没有高超声速的相关定义参数
#            return 
        #加载亚音速布局下的参数
        
        #NX
        tVar = self.model.getNamelistVar('BODY','NX')
        tVar = '0' if tVar is None else str(int(float(tVar)))
        self.NX.setText(tVar)
        #ELLIP
        tVar = self.model.getNamelistVar('BODY','ELLIP')
        if tVar is None:
            self.ELLIP.setEnabled(False)
            self.checkBox_ELLIP.setCheckState(Qt.Unchecked)
        else:
            self.ELLIP.setEnabled(True)
            self.checkBox_ELLIP.setCheckState(Qt.Checked)
            self.ELLIP.setText( str(tVar))
        #comboBox_METHED 默认方法0
        tVar = self.model.getNamelistVar('BODY','METHED')
        tVar = 1 if tVar is None else int(float(tVar))
        self.comboBox_METHED.setCurrentIndex(tVar-1)
        
        
        #comboBox_ITYPE
        tVar = self.model.getNamelistVar('BODY','ITYPE')
        tVar = 2 if tVar is None else int(float(tVar))
        if tVar is None:
            self.comboBox_ITYPE.setEnabled(False)
            self.checkBox_ITYPE.setCheckState(Qt.Unchecked)
            self.comboBox_ITYPE.setCurrentIndex(tVar -1)
        else:
            self.comboBox_ITYPE.setEnabled(True)
            self.checkBox_ITYPE.setCheckState(Qt.Checked)
            self.comboBox_ITYPE.setCurrentIndex(tVar -1)
            
        #comboBox_BNOSE 
        tVar = self.model.getNamelistVar('BODY','BNOSE')
        if tVar: 
            self.checkBox_BNOSE.setCheckState(Qt.Checked)
            self.comboBox_BNOSE.setEnabled(True)
            tVar = 1 if tVar is None else int(float(tVar))
            self.comboBox_BNOSE.setCurrentIndex(tVar -1)
        else :
            self.checkBox_BNOSE.setCheckState(Qt.Unchecked)
            self.comboBox_BNOSE.setEnabled(False)
            
        #comboBox_BTAIL
        tVar = self.model.getNamelistVar('BODY','BTAIL')
        if tVar: 
            self.checkBox_BTAIL.setCheckState(Qt.Checked)
            self.comboBox_BTAIL.setEnabled(True)
            tVar = 1 if tVar is None else int(tVar)
            self.comboBox_BTAIL.setCurrentIndex(tVar -1)
        else :
            self.checkBox_BTAIL.setCheckState(Qt.Unchecked)
            self.comboBox_BTAIL.setEnabled(False)
            
        #lineEdit_BLN
        tVar = self.model.getNamelistVar('BODY','BLN')
        if not tVar is None:
            self.checkBox_BLN.setCheckState(Qt.Checked)
            self.lineEdit_BLN.setEnabled(True)
            self.lineEdit_BLN.setText(str(tVar))
        else :
            self.checkBox_BLN.setCheckState(Qt.Unchecked)
            self.lineEdit_BLN.setText('0.0')
            self.lineEdit_BLN.setEnabled(False)          

        
        #lineEdit_BLA
        tVar = self.model.getNamelistVar('BODY','BLA')
        if not tVar is None:
            self.checkBox_BLA.setCheckState(Qt.Checked)
            self.lineEdit_BLA.setEnabled(True)
            self.lineEdit_BLA.setText(str(tVar))
        else :
            self.checkBox_BLA.setCheckState(Qt.Unchecked)
            self.lineEdit_BLA.setText('0.0')
            self.lineEdit_BLA.setEnabled(False)

        
        #lineEdit_DS
        tVar = self.model.getNamelistVar('BODY','DS')
        if tVar is None: 
            self.lineEdit_DS.setEnabled(False)
            self.checkBox_DS.setCheckState(Qt.Unchecked)
            self.lineEdit_DS.setText( '0.0')
        else :
            self.lineEdit_DS.setEnabled(True)
            self.checkBox_DS.setCheckState(Qt.Checked)
            self.lineEdit_DS.setText( str(tVar))
        
        #检测参数组合
        data ={}
        data['X'] = self.model.getNamelistVar('BODY','X')
        data['R'] = self.model.getNamelistVar('BODY','R')
        data['S'] = self.model.getNamelistVar('BODY','S')
        data['P'] = self.model.getNamelistVar('BODY','P')
        data['ZU'] = self.model.getNamelistVar('BODY','ZU')
        data['ZL'] = self.model.getNamelistVar('BODY','ZL')
        #判断模式
        modStr = ""
        for itr in data.keys():
            if data[itr]:
                modStr = modStr + "%s-"%itr
        if not modStr == "": #存在一种模式
            modStr = modStr[:-1]
            if self.comboBox_VarCombo.findText(modStr) == -1: #不存在这种模式
                self.comboBox_VarCombo.addItem(modStr)
                self.comboBox_VarCombo.setCurrentIndex(self.comboBox_VarCombo.count()-1)
            else : #存在这种模式
                self.comboBox_VarCombo.setCurrentIndex(self.comboBox_VarCombo.findText(modStr))
            
            #执行数据填充
            self.createTableWidget(modStr) #创建表头
            tHeader = modStr.split('-')
            for itC in range(0, len(tHeader)): 
                tData = data[tHeader[itC]]['Value'] #忘了index和value的事情了                
                for itR in range(0, len(tData)):
                    self.Tab_ComboVariables.setItem(itR, itC,
                                    QTableWidgetItem(str(tData[itR])))                    
            #填充完成  
            
        #如果没有模式输入则选择默认的全模式
        if modStr == "":
            self.comboBox_VarCombo.setCurrentIndex(2) #选择X-R-S-P-ZU-ZL 模式
            modStr = self.comboBox_VarCombo.currentText()
            self.createTableWidget(modStr) #创建表头
            self.logger.info("使用默认表头！%s"%modStr)
            
        
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
        
        if self.comboBox_SpeedRegime.currentIndex() <= 1: #亚音速和跨音速
            #执行数据 NX X R S P ELLIP ZU ZL
            self.model.setNamelist( 'BODY' , 'NX', float(self.NX.text()) )
            #ITYPE
            if self.checkBox_ITYPE.checkState() == Qt.Checked:
                if not self.comboBox_ITYPE.currentIndex() == 1:
                    self.model.setNamelist( 'BODY' , 'ITYPE', 
                                  '%d.'%(self.comboBox_ITYPE.currentIndex() + 1) )
            #Method            
            if not self.comboBox_METHED.currentIndex() == 0:
                self.model.setNamelist( 'BODY' , 'METHED', 
                                  '%d.'%(self.comboBox_METHED.currentIndex() + 1) )
            #ELLIP
            if self.checkBox_ELLIP.checkState() == Qt.Checked:
                self.model.setNamelist( 'BODY' , 'ELLIP', 
                                  self.ELLIP.text()  )
            #checkBox_BNOSE
            if self.checkBox_BNOSE.checkState() == Qt.Checked:                
                self.model.setNamelist( 'BODY' , 'BNOSE',
                '%d.'%(self.comboBox_BNOSE.currentIndex() + 1) )
            #checkBox_BTAIL
            if self.checkBox_BTAIL.checkState() == Qt.Checked:                
                self.model.setNamelist( 'BODY' , 'BTAIL',
                '%d.'%(self.comboBox_BTAIL.currentIndex() + 1) )
            #checkBox_BLN
            if self.checkBox_BLN.checkState() == Qt.Checked:                
                self.model.setNamelist( 'BODY' , 'BLN',
                self.lineEdit_BLN.text() )  
            #checkBox_BLA
            if self.checkBox_BLA.checkState() == Qt.Checked:                
                self.model.setNamelist( 'BODY' , 'BLA',
                self.lineEdit_BLA.text())     
            #checkBox_DS
            if self.checkBox_DS.checkState() == Qt.Checked:                
                self.model.setNamelist( 'BODY' , 'DS',
                self.lineEdit_DS.text() )        
     
            #X R S P ZU ZL
            for col in range(0, self.Tab_ComboVariables.columnCount()):
                varName = self.Tab_ComboVariables.horizontalHeaderItem(col).text()
                varLst =[]
                for rowN in range(0, self.Tab_ComboVariables.rowCount()):
                    #获得表格对应项的值
                    varLst.append(float(self.Tab_ComboVariables.item(rowN, col).text()))
                #写入到Model中
                self.model.setNamelist( 'BODY' , varName, varLst, 1)
                
                
                
        if self.comboBox_SpeedRegime.currentIndex() > 1: #超音速和高跨音速
            #执行数据 BNOSE BTAIL BLN BLA DS
            pass
        
        return self.model

        
    def UILogic(self):
        """
        在此刷新UI，需要根据不同的情况执行判断
        """
        
        #判断ITYPE是否使能
        if self.checkBox_ITYPE.checkState() == Qt.Checked:
            self.comboBox_ITYPE.setEnabled(True)
        else : self.comboBox_ITYPE.setEnabled(False)
        
        #checkBox_BNOSE
        if self.checkBox_BNOSE.checkState() == Qt.Checked: 
            self.comboBox_BNOSE.setEnabled(True)
        else:
            self.comboBox_BNOSE.setEnabled(False)
        #checkBox_BTAIL
        if self.checkBox_BTAIL.checkState() == Qt.Checked: 
            self.comboBox_BTAIL.setEnabled(True)
        else:
            self.comboBox_BTAIL.setEnabled(False)
        
        #checkBox_BLN
        if self.checkBox_BLN.checkState() == Qt.Checked: 
            self.lineEdit_BLN.setEnabled(True)
        else:
            self.lineEdit_BLN.setEnabled(False)
        
        #checkBox_BLA
        if self.checkBox_BLA.checkState() == Qt.Checked: 
            self.lineEdit_BLA.setEnabled(True)
        else:
            self.lineEdit_BLA.setEnabled(False)
        
        #checkBox_DS
        if self.checkBox_DS.checkState() == Qt.Checked: 
            self.lineEdit_DS.setEnabled(True)
        else:
            self.lineEdit_DS.setEnabled(False)
            
        #重新绘制表格
        if not self.currentTable == self.comboBox_VarCombo.currentText():
            #如果变量组合模式发生了变化则要重新创建表格
            self.changeTableModel( self.comboBox_VarCombo.currentText())
            self.currentTable = self.comboBox_VarCombo.currentText()
        
        #判断速度范围
        if self.comboBox_SpeedRegime.currentIndex() == 0: #亚音速
            #屏蔽高超声速输入
            #self.comboBox_BNOSE.setEnabled(False)
            #self.comboBox_BTAIL.setEnabled(False)
            #self.lineEdit_BLN.setEnabled(False)
            #self.lineEdit_BLA.setEnabled(False)
            #self.checkBox_DS.setEnabled(False)
            #self.lineEdit_DS.setEnabled(False)
            pass

        if self.comboBox_SpeedRegime.currentIndex() == 1: #跨音速
            #屏蔽高超声速输入
            #self.comboBox_BNOSE.setEnabled(False)
            #self.comboBox_BTAIL.setEnabled(False)
            #self.lineEdit_BLN.setEnabled(False)
            #self.lineEdit_BLA.setEnabled(False)
            #self.checkBox_DS.setEnabled(False)
            #self.lineEdit_DS.setEnabled(False)
            #使能RSPZUZL
            pass

        if self.comboBox_SpeedRegime.currentIndex() == 2: #超音速
            """"""
            #激活其他的表格项
#            self.comboBox_BNOSE.setEnabled(True)
#            self.comboBox_BTAIL.setEnabled(True)
#            self.lineEdit_BLN.setEnabled(True)
#            self.lineEdit_BLA.setEnabled(True)
#            self.checkBox_DS.setEnabled(True)
#            self.lineEdit_DS.setEnabled(True)
            
        if self.comboBox_SpeedRegime.currentIndex() == 3: #高超音速
            """"""
            #激活其他的表格项
#            self.comboBox_BNOSE.setEnabled(True)
#            self.comboBox_BTAIL.setEnabled(True)
#            self.lineEdit_BLN.setEnabled(True)
#            self.lineEdit_BLA.setEnabled(True)
#            self.checkBox_DS.setEnabled(True)
#            self.lineEdit_DS.setEnabled(True)
        
        
        #判断机身参数变量组合
        
    def createTableWidget(self, vars):
        """
        根据vars来创建表头
        """   
        varList = vars.split('-')
        self.Tab_ComboVariables.setColumnCount(len(varList))
        self.Tab_ComboVariables.setHorizontalHeaderLabels(varList)
        self.Tab_ComboVariables.setRowCount(int(self.NX.text()))

    def changeTableModel(self, modStr):
        """
        此处执行数据转换，暂未实现
        """ 
        #初步直接删除数据 
        if self.Tab_ComboVariables.rowCount() > 0:  
            button = QMessageBox.question(self, r"模型不为空是否清空重新写入",
                r"清空已经录入的数据吗?点击yes清除，放弃点击No",
                QMessageBox.Yes | QMessageBox.No)
            if button == QMessageBox.Yes:
                self.createTableWidget( modStr)  
            else :
                return       
            
        else:
            self.createTableWidget( modStr)
        
        
    
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
#        itype = "%d."%(self.comboBox_ITYPE.currentIndex() + 1)
#        self.model.setNamelist( 'BODY' , 'ITYPE', itype )
#        self.logger.info("选择了ITYPE：%s"%itype )
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
#        bnose = '%d.0'%(self.comboBox_BNOSE.currentIndex() +1)
#        self.model.setNamelist( 'BODY' , 'BNOSE', bnose )
#        self.logger.info("选择了BNOSE：%s"%bnose )
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
#        aVar = '%d.0'%(self.comboBox_BTAIL.currentIndex() +1)
#        self.model.setNamelist( 'BODY' , 'BTAIL', aVar )
#        self.logger.info("选择了BTAIL：%s"%aVar )
        
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
        
#        aVar = '%d.0'%(self.comboBox_METHED.currentIndex() +1)
#        self.model.setNamelist( 'BODY' , 'METHED', aVar )
#        self.logger.info("选择了METHED：%s"%aVar )
        
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
    
    @pyqtSlot()
    def on_NX_editingFinished(self):
        """
        这个信号是最终获得的NX的有效输入.
        """
        # TODO: not implemented yet
        
        nowRows = self.Tab_ComboVariables.rowCount()
        tNx = int(self.NX.text())
        if nowRows < tNx: 
            #if 当前行数少，考虑增加
            for itR in range(nowRows, tNx):
                self.Tab_ComboVariables.insertRow(itR)
        if nowRows == tNx:
            pass
        if nowRows > tNx:
            self.NX.setText(str(nowRows))
            strInfo = "尝试的NX小于现有数据行数，请手动从表格中删除对应行"
            QMessageBox.information(self, "提示" , strInfo)  
            self.logger.info(strInfo)
    
    @pyqtSlot(int)
    def on_checkBox_ELLIP_stateChanged(self, p0):
        """
        Slot documentation goes here.
        
        @param p0 DESCRIPTION
        @type int
        """
        # TODO: not implemented yet
        #刷新界面
        self.UILogic() 
    
    @pyqtSlot(int)
    def on_checkBox_BNOSE_stateChanged(self, p0):
        """
        Slot documentation goes here.
        
        @param p0 DESCRIPTION
        @type int
        """
        # TODO: not implemented yet
        #刷新界面
        self.UILogic()
    
    @pyqtSlot(int)
    def on_checkBox_BTAIL_stateChanged(self, p0):
        """
        Slot documentation goes here.
        
        @param p0 DESCRIPTION
        @type int
        """
        # TODO: not implemented yet
        #刷新界面
        self.UILogic()
    
    @pyqtSlot(int)
    def on_checkBox_DS_stateChanged(self, p0):
        """
        Slot documentation goes here.
        
        @param p0 DESCRIPTION
        @type int
        """
        # TODO: not implemented yet
        #刷新界面
        self.UILogic()
    
    @pyqtSlot()
    def on_actionAddRow_triggered(self):
        """
        响应表格新建行操作
        """
        # TODO: not implemented yet
        aItem = self.Tab_ComboVariables.indexAt(self.tablePos)
        rowIndex = 0
        if aItem is None :
            #没有命中
            rowIndex = self.Tab_ComboVariables.rowCount()
        else:
            rowIndex = aItem.row()

        if self.Tab_ComboVariables.rowCount() <20:
            self.Tab_ComboVariables.insertRow(rowIndex)
        else:
            self.logger.info("已经达到最大行数不能添加")
            
        self.NX.setText(str(self.Tab_ComboVariables.rowCount()))
        
    
    @pyqtSlot()
    def on_actionDeleteRow_triggered(self):
        """
        响应表格删除行操作.
        """
        # TODO: not implemented yet
        aItem = self.Tab_ComboVariables.indexAt(self.tablePos)
        if  not aItem  is None :            
            self.Tab_ComboVariables.removeRow(aItem.row())
        else:
            self.logger.info("没有命中任何行")
        
        self.NX.setText(str(self.Tab_ComboVariables.rowCount()))
    
    @pyqtSlot(QPoint)
    def on_Tab_ComboVariables_customContextMenuRequested(self, pos):
        """
        Slot documentation goes here.
        
        @param pos DESCRIPTION
        @type QPoint
        """
        # TODO: not implemented yet
        self.tablePos = pos
        posG = self.Tab_ComboVariables.mapToGlobal(pos)
        self.popMenu.exec(posG)
    
    @pyqtSlot(int)
    def on_checkBox_BLN_stateChanged(self, p0):
        """
        Slot documentation goes here.
        
        @param p0 DESCRIPTION
        @type int
        """
        # TODO: not implemented yet
        #刷新界面
        self.UILogic()
    
    @pyqtSlot(int)
    def on_checkBox_BLA_stateChanged(self, p0):
        """
        Slot documentation goes here.
        
        @param p0 DESCRIPTION
        @type int
        """
        # TODO: not implemented yet
        #刷新界面
        self.UILogic()
