# -*- coding: utf-8 -*-

"""
Module implementing FLTCON.
"""

from PyQt5.QtCore import pyqtSlot, Qt, QPoint, pyqtSignal
from PyQt5.QtWidgets import QWidget, QMenu, QTableWidgetItem, QMessageBox
from PyQt5.QtGui import QDoubleValidator, QIntValidator, QValidator

from PyDatcomLab.Core import dcModel
import logging

from Ui_FLTCON import Ui_Form


class FLTCON(QWidget, Ui_Form):
    """
    Class documentation goes here.
    """
    
    #定义各个Widget之间进行参数同步的信号
    emit_NMACHChanged = pyqtSignal(str, int)  #发送NMACH变化的信息
    emit_NACAChanged = pyqtSignal(str, bool)  #发送NACA选项卡的信号
    
    
    def __init__(self, parent=None, tModel = None):
        """
        Constructor
        
        @param parent reference to the parent widget
        @type QWidget
        """
        super(FLTCON, self).__init__(parent)
        self.setupUi(self)
        
        #创建日志
        self.logger = logging.getLogger(r'Datcomlogger')
        
        #修改后台的数据
        if tModel is None:
            tModel = dcModel.dcModel('J6', '常规布局')         
        self.model = tModel 
        #配置界面
        #NMACH
        tVd = QIntValidator(self)
        tVd.setRange(0, 20)
        self.NMACH.setValidator(tVd)
        self.NALPHA.setValidator(tVd)
        self.NALT.setValidator(tVd)
        tVd = QDoubleValidator(self)
        tVd.setBottom(0)
        self.WT.setValidator(tVd)
        tVd = QDoubleValidator(self)
        tVd.setRange(-180, 180)
        self.GAMMA.setValidator(QDoubleValidator(self))
        tVd = QDoubleValidator(self)
        tVd.setRange(0.6, .99)
        self.STMACH.setValidator(tVd)
        tVd = QDoubleValidator(self)
        tVd.setRange(1.01, 1.4)
        self.TSMACH.setValidator(tVd)
        
        #介于LOOP =2,3的逻辑实现机理不清楚，暂时只实现LOOP = 1.0
        self.LOOP.setEnabled(False)
        self.NALT.setEnabled(False)
        
        #创建表头
        #添加表头给对应控件        
        self.ALSCHD.setColumnCount(1)
        self.ALSCHD.setHorizontalHeaderLabels(['ALSCHD'])
        #
        self.paraList = ['MACH', 'RNNUB', 'VINF' , 'ALT', 'PINF' , 'TINF' ]
        #
        self.Speed_Atmospheric.setColumnCount(len(self.paraList))
        self.Speed_Atmospheric.setHorizontalHeaderLabels(self.paraList)
  
        #界面参数
        self.curPos = QPoint(0, 0)
        self.curWidget = None
        self.curN = None
        self.popMenu = None



        #初始化数据和内容
        self.InitDoc()   
        self.UILogic()  
        
    def InitDoc(self):
        """
        分析并初始化后台数据
        """
        
        #分析获得dict的数据信息
        #LOOP
        tVar = self.model.getNamelistVar('FLTCON','LOOP')
        tVar = 0 if tVar is None else int(float(tVar))
        if tVar >0 :#临时的
            self.logger.info("尝试LOOP的其他模式 %d"%tVar)
            tVar = 0
        self.LOOP.setCurrentIndex(tVar)
        
        #NMACH
        tVar = self.model.getNamelistVar('FLTCON','NMACH')
        tVar = 0 if tVar is None else int(float(tVar)) 
        self.NMACH.setText(str(tVar))
        #NALT
        tVar = self.model.getNamelistVar('FLTCON','NALT')
        tVar = 0 if tVar is None else int(float(tVar)) 
        self.NALT.setText(str(tVar)) 
        #NALPHA
        tVar = self.model.getNamelistVar('FLTCON','NALPHA')
        tVar = 0 if tVar is None else int(float(tVar)) 
        self.NALPHA.setText(str(tVar))   
        
        #WT
        tVar = self.model.getNamelistVar('FLTCON','WT')  
        if tVar is None: 
            self.WT.setEnabled(False)
            self.checkBox_WT.setCheckState(Qt.Unchecked)
            self.WT.setText( '0.0')
        else :
            self.WT.setEnabled(True)
            self.checkBox_WT.setCheckState(Qt.Checked)
            self.WT.setText( str(tVar))
        
        #GAMMA
        tVar = self.model.getNamelistVar('FLTCON','GAMMA')  
        if tVar is None: 
            self.GAMMA.setEnabled(False)
            self.checkBox_GAMMA.setCheckState(Qt.Unchecked)
            self.GAMMA.setText( '0.0')
        else :
            self.GAMMA.setEnabled(True)
            self.checkBox_GAMMA.setCheckState(Qt.Checked)
            self.GAMMA.setText( str(tVar))
        
        #STMACH
        tVar = self.model.getNamelistVar('FLTCON','STMACH')  
        if tVar is None: 
            self.STMACH.setEnabled(False)
            self.checkBox_STMACH.setCheckState(Qt.Unchecked)
            self.STMACH.setText( '0.6')
        else :
            self.STMACH.setEnabled(True)
            self.checkBox_STMACH.setCheckState(Qt.Checked)
            self.STMACH.setText( str(tVar)) 
            
        #TSMACH
        tVar = self.model.getNamelistVar('FLTCON','TSMACH')  
        if tVar is None: 
            self.TSMACH.setEnabled(False)
            self.checkBox_TSMACH.setCheckState(Qt.Unchecked)
            self.TSMACH.setText( '1.4')
        else :
            self.TSMACH.setEnabled(True)
            self.checkBox_TSMACH.setCheckState(Qt.Checked)
            self.TSMACH.setText( str(tVar))  

        #HYPERS
        tVar = self.model.getNamelistVar('FLTCON','HYPERS')  
        if tVar is None: 
            self.HYPERS.setEnabled(False)
            self.checkBox_HYPERS.setCheckState(Qt.Unchecked)
            self.HYPERS.setCurrentIndex(0)
        else :
            self.HYPERS.setEnabled(True)
            self.checkBox_HYPERS.setCheckState(Qt.Checked)
            self.HYPERS.setCurrentIndex( int(float(tVar)))  
        #TR
        tVar = self.model.getNamelistVar('FLTCON','TR')  
        if tVar is None: 
            self.TR.setEnabled(False)
            self.checkBox_TR.setCheckState(Qt.Unchecked)
            self.TR.setCurrentIndex(0)
        else :
            self.TR.setEnabled(True)
            self.checkBox_TR.setCheckState(Qt.Checked)
            self.TR.setCurrentIndex( int(float(tVar)))  
        
        #添加攻角 ALSCHD
        tVar = self.model.getNamelistVar('FLTCON','ALSCHD')
        if tVar is None:
            self.NALPHA.setText('0')
        else: #存在数据 
            tVar = tVar['Value']
            if not len(tVar) == int(float(self.NALPHA.text())):
                self.NALPHA.setText('%d'%len(tVar))
            self.ALSCHD.setRowCount(len(tVar))
            for itR in range(0, len(tVar)): #循环添加数据
                self.ALSCHD.setItem(itR, 0,
                            QTableWidgetItem(str(tVar[itR])))  
        
        #添加其他数据 
        #comboBox_Variables
        #paraList = ['MACH', 'RNNUB', 'VINF' , 'ALT', 'PINF' , 'TINF' ]
        #检测参数组合
        data ={}
        tLen = 0
        for pNm in self.paraList:
            data[pNm] = self.model.getNamelistVar('FLTCON',pNm)
            if data[pNm] is not None: 
                if tLen < len(data[pNm]['Value']):
                    tLen = len(data[pNm]['Value'])
        
        tCLen = len(self.paraList)
        self.Speed_Atmospheric.setRowCount(tLen)    #刷新表格
        
        for itC in range(tCLen):
            if data[self.paraList[itC]] is None :
                """尝试将该列禁用"""
                #self.Speed_Atmospheric.horizontalHeaderItem(itC).setCheckState(False)
            else :#存在数据
                tDataV = data[self.paraList[itC]]['Value']
                for itR in range(len(tDataV)):
                    self.Speed_Atmospheric.setItem(itR, itC,
                        QTableWidgetItem(str(tDataV[itR])))
        #判断模式
        #添加表头给对应控件        


        


        

    def setModel(self, tModel):
        """
        初始化本节点的xml描述文档
        """
        
        self.Model = tModel        
        #执行参数配置过程        
        self.InitDoc()
        
        self.UILogic()
        
    def UILogic(self):
        """
        在此刷新UI，需要根据不同的情况执行判断
        """
        
        #刷新CheckBox
        #checkBox_WT
        if self.checkBox_WT.checkState() == Qt.Checked:
            self.WT.setEnabled(True)
        else :
            self.WT.setEnabled(False)
        #checkBox_GAMMA GAMMA
        if self.checkBox_GAMMA.checkState() == Qt.Checked:
            self.GAMMA.setEnabled(True)
        else :
            self.GAMMA.setEnabled(False)
        #checkBox_STMACH STMACH
        if self.checkBox_STMACH.checkState() == Qt.Checked:
            self.STMACH.setEnabled(True)
        else :
            self.STMACH.setEnabled(False)
        #checkBox_TSMACH TSMACH
        if self.checkBox_TSMACH.checkState() == Qt.Checked:
            self.TSMACH.setEnabled(True)
        else :
            self.TSMACH.setEnabled(False)

        #checkBox_HYPERS HYPERS
        if self.checkBox_HYPERS.checkState() == Qt.Checked:
            self.HYPERS.setEnabled(True)
        else :
            self.HYPERS.setEnabled(False)
        #checkBox_TR
        if self.checkBox_TR.checkState() == Qt.Checked:
            self.TR.setEnabled(True)
        else :
            self.TR.setEnabled(False)
        
        #NACA
        if self.checkBox_UsingNACA.checkState() == Qt.Checked:
            self.comboBox_Variables.setCurrentIndex(0) #选中模式1
        else :
            self.comboBox_Variables.setCurrentIndex(1) #选中模式2,认为是默认值
            
        #分析LOOP执行相应的选择模式
        if self.LOOP.currentIndex() == 0:
            if self.NMACH.text() != self.NALT.text():
                self.NALT.setText(self.NMACH.text()) 
            pass
        elif self.LOOP.currentIndex() == 1:
            pass
        else:
            pass
        
        
        

    def getDoc(self):
        """
        将界面的内容刷新到变量model
        """
        
        #执行界面刷新
        #分析获得dict的数据信息
        
        #LOOP
        if self.LOOP.currentIndex() == 0:
            self.model.setNamelist( 'FLTCON' , 'LOOP', None) #移除
        else:
            self.model.setNamelist( 'FLTCON' , 'LOOP', self.LOOP.currentIndex() +1) #移除
       
        #NMACH
        if self.NMACH.isEnabled() :
            self.model.setNamelist( 'FLTCON' , 'NMACH', int(float(self.NMACH.text())))

        #NALT
        if self.NALT.isEnabled() :
            self.model.setNamelist( 'FLTCON' , 'NALT', int(float(self.NALT.text())))

        #NALPHA
        self.model.setNamelist( 'FLTCON' , 'NALPHA', int(float(self.NALPHA.text())))
        
        #WT
        if self.checkBox_WT.checkState() == Qt.Checked:
            self.model.setNamelist( 'FLTCON' , 'WT', float(self.WT.text()))
        else:
            self.model.setNamelist( 'FLTCON' , 'WT', None)
        
        #GAMMA
        if self.checkBox_GAMMA.checkState() == Qt.Checked:
            self.model.setNamelist( 'FLTCON' , 'GAMMA', float(self.GAMMA.text()))
        else:
            self.model.setNamelist( 'FLTCON' , 'GAMMA', None)
        
        #STMACH
        if self.checkBox_STMACH.checkState() == Qt.Checked:
            self.model.setNamelist( 'FLTCON' , 'STMACH', float(self.STMACH.text()))
        else:
            self.model.setNamelist( 'FLTCON' , 'STMACH', None)
            
        #TSMACH
        if self.checkBox_TSMACH.checkState() == Qt.Checked:
            self.model.setNamelist( 'FLTCON' , 'TSMACH', float(self.TSMACH.text()))
        else:
            self.model.setNamelist( 'FLTCON' , 'TSMACH', None)

        #HYPERS
        if self.checkBox_HYPERS.checkState() == Qt.Checked:
            self.model.setNamelist( 'FLTCON' , 'HYPERS', float(self.HYPERS.text()))
        else:
            self.model.setNamelist( 'FLTCON' , 'HYPERS', None)

        #TR
        if self.checkBox_TR.checkState() == Qt.Checked:
            self.model.setNamelist( 'FLTCON' , 'TR', float(self.TR.text()))
        else:
            self.model.setNamelist( 'FLTCON' , 'TR', None)
     
        #添加攻角 ALSCHD
        if self.ALSCHD.rowCount() == 0 :
            self.logger.info("没有输入足够的攻角，总数为0,移除原来的数据")
            self.model.setNamelist( 'FLTCON' , 'ALSCHD', None)
            self.model.setNamelist( 'FLTCON' , 'NALPHA', 0)            
        else:
            tVaList = []
            for itC in range(self.ALSCHD.rowCount()):
                tVaList.append(float(self.ALSCHD.item(itC, 0).text()))
            tDict = {'Index':1, 'Value':tVaList}
            self.model.setNamelist( 'FLTCON' , 'ALSCHD', tDict)
            
        #添加其他数据 
        #comboBox_Variables
        #paraList = ['MACH', 'RNNUB', 'VINF' , 'ALT', 'PINF' , 'TINF' ]
        #检测参数组合
        for itC in range(self.Speed_Atmospheric.columnCount()):
            pNm = self.Speed_Atmospheric.horizontalHeaderItem(itC).text()
            tVder = QDoubleValidator(self)
            tValList =[]
            for itR in range(self.Speed_Atmospheric.rowCount()):
                tItem = self.Speed_Atmospheric.item(itR, itC)
                if tItem is None : 
                    self.logger.info("%s 在%d,%d没有有效值"%(pNm, itR, itC))
                    continue
                #如果不为None
                st = tVder.validate(tItem.text(), 0)[0]
                if st == QValidator.Acceptable:
                    tValList.append(float(tItem.text()))
                else:
                    self.logger.error("%s 在%d,%d没有有效值"%(tItem.text(), itR, itC))
            if len(tValList) >0 : #有数据则加入到集合
                self.model.setNamelist( 'FLTCON' , pNm, 
                {'Index':1, 'Value':tValList})
            else : #没有数据则清除
                self.model.setNamelist( 'FLTCON' , pNm, None)
        
        #获取界面输入值
        return self.model
        
    

    
    @pyqtSlot(int)
    def on_comboBox_Variables_currentIndexChanged(self, index):
        """
        Slot documentation goes here.
        
        @param index DESCRIPTION
        @type int
        """
        # TODO: not implemented yet
        self.UILogic()  
    

    
    @pyqtSlot()
    def on_NMACH_editingFinished(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        #因为只实现LOOP =1

        
        nowRows = self.Speed_Atmospheric.rowCount()
        tNx = int(self.NMACH.text())
        if nowRows < tNx: 
            #if 当前行数少，考虑增加
            for itR in range(nowRows, tNx):
                self.Speed_Atmospheric.insertRow(itR)
        if nowRows == tNx:
            pass
        if nowRows > tNx:
            self.NMACH.setText(str(nowRows))
            strInfo = "尝试的NMACH小于现有数据行数，请手动从表格中删除对应行"
            QMessageBox.information(self, "提示" , strInfo)  
            self.logger.info(strInfo)    
            
        if self.NMACH.text() != self.NALT.text():
            self.NALT.setText(self.NMACH.text())        
        
        #因为只实现LOOP =1
        self.UILogic()  
    
    @pyqtSlot()
    def on_NALPHA_editingFinished(self):
        """
        Slot documentation goes here.
        当编辑攻角数
        
        """
        # TODO: not implemented yet
        nowRows = self.ALSCHD.rowCount()
        tNx = int(self.NALPHA.text())
        if nowRows < tNx: 
            #if 当前行数少，考虑增加
            for itR in range(nowRows, tNx):
                self.ALSCHD.insertRow(itR)
        if nowRows == tNx:
            pass
        if nowRows > tNx:
            self.NALPHA.setText(str(nowRows))
            strInfo = "尝试的NALPHA小于现有数据行数，请手动从表格中删除对应行"
            QMessageBox.information(self, "提示" , strInfo)  
            self.logger.info(strInfo)    
            
        
        self.UILogic()  
    
    @pyqtSlot()
    def on_NALT_editingFinished(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        #因为只实现LOOP =1
#        if self.NMACH.text() != self.NALT.text():
#            self.NMACH.setText(self.NALT.text())
        #因为只实现LOOP =1
        self.UILogic()  
    

    
    @pyqtSlot()
    def on_WT_editingFinished(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        self.UILogic()  
    
    @pyqtSlot(int)
    def on_checkBox_FlightPathAngle_stateChanged(self, p0):
        """
        Slot documentation goes here.
        
        @param p0 DESCRIPTION
        @type int
        """
        # TODO: not implemented yet
        self.UILogic()  
    
    @pyqtSlot(int)
    def on_checkBox_STMACH_stateChanged(self, p0):
        """
        Slot documentation goes here.
        
        @param p0 DESCRIPTION
        @type int
        """
        # TODO: not implemented yet
        self.UILogic()  
    
    @pyqtSlot()
    def on_STMACH_editingFinished(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        self.UILogic()  
    
    @pyqtSlot(int)
    def on_checkBox_TSMACH_stateChanged(self, p0):
        """
        Slot documentation goes here.
        
        @param p0 DESCRIPTION
        @type int
        """
        # TODO: not implemented yet
        self.UILogic()  
    
    @pyqtSlot()
    def on_TSMACH_editingFinished(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        self.UILogic()  
    
    @pyqtSlot(int)
    def on_checkBox_HYPERS_stateChanged(self, p0):
        """
        Slot documentation goes here.
        
        @param p0 DESCRIPTION
        @type int
        """
        # TODO: not implemented yet
        self.UILogic()  
    
    @pyqtSlot(int)
    def on_HYPERS_currentIndexChanged(self, index):
        """
        Slot documentation goes here.
        
        @param index DESCRIPTION
        @type int
        """
        # TODO: not implemented yet
        self.UILogic()  
    
    @pyqtSlot(int)
    def on_checkBox_TR_stateChanged(self, p0):
        """
        Slot documentation goes here.
        
        @param p0 DESCRIPTION
        @type int
        """
        # TODO: not implemented yet
        self.UILogic()  
    
    @pyqtSlot(int)
    def on_TR_currentIndexChanged(self, index):
        """
        Slot documentation goes here.
        
        @param index DESCRIPTION
        @type int
        """
        # TODO: not implemented yet
        self.UILogic()  
    
    @pyqtSlot()
    def on_actionAddRow_triggered(self):
        """
        增加新行的代码.
        """
        # TODO: not implemented yet
        #添加行
        aItem = self.curWidget.indexAt(self.curPos) #认为是表格 ，否则会异常
        rowIndex = 0
        if aItem.row() == -1 :
            #没有命中
            rowIndex = self.curWidget.rowCount()
        else:
            rowIndex = aItem.row()

        if self.curWidget.rowCount() <20:
            self.curWidget.insertRow(rowIndex)
        else:
            self.logger.info("%s已经达到最大行数不能添加"%self.curWidget.objectName())
            
        self.curN.setText(str(self.curWidget.rowCount()))
        
        self.UILogic()  
    
    @pyqtSlot()
    def on_actionDeleteRow_triggered(self):
        """
        删除行的代码.
        """
        # TODO: not implemented yet
        aItem = self.curWidget.indexAt(self.curPos)
        if  aItem.row() >= 0 :            
            self.curWidget.removeRow(aItem.row())
        else:
            self.logger.info("没有命中任何行")
        
        self.curN.setText(str(self.curWidget.rowCount()))

        self.UILogic()  
    
    @pyqtSlot(int)
    def on_checkBox_UsingNACA_stateChanged(self, p0):
        """
        此选项卡激活时，只接受马赫数和雷诺数.
        
        @param p0 DESCRIPTION
        @type int
        """
        # TODO: not implemented yet

        self.UILogic() 
    
    @pyqtSlot(int)
    def on_LOOP_currentIndexChanged(self, index):
        """
        Slot documentation goes here.
        
        @param index DESCRIPTION
        @type int
        """
        # TODO: not implemented yet

        self.UILogic() 
    
    @pyqtSlot(QPoint)
    def on_ALSCHD_customContextMenuRequested(self, pos):
        """
        Slot documentation goes here.
        
        @param pos DESCRIPTION
        @type QPoint
        """
        # TODO: not implemented yet
        self.curPos = pos
        self.curWidget = self.ALSCHD        
        posG = self.curWidget.mapToGlobal(pos)
        self.popMenu = QMenu(self.curWidget)
        self.popMenu.addAction(self.actionAddRow)
        self.popMenu.addAction(self.actionDeleteRow)
        self.curWidget.setContextMenuPolicy(Qt.CustomContextMenu)
        self.curN = self.NALPHA
        
        self.popMenu.exec(posG)
        #raise NotImplementedError
    
    @pyqtSlot(QPoint)
    def on_Speed_Atmospheric_customContextMenuRequested(self, pos):
        """
        Slot documentation goes here.
        
        @param pos DESCRIPTION
        @type QPoint
        """
        # TODO: not implemented yet
        self.curPos = pos
        self.curWidget = self.Speed_Atmospheric        
        posG = self.curWidget.mapToGlobal(pos)
        self.popMenu = QMenu(self.curWidget)
        self.popMenu.addAction(self.actionAddRow)
        self.popMenu.addAction(self.actionDeleteRow)
        self.curWidget.setContextMenuPolicy(Qt.CustomContextMenu)
        self.curN = self.NMACH
        
        self.popMenu.exec(posG)
    
    @pyqtSlot(int)
    def on_checkBox_WT_stateChanged(self, p0):
        """
        Slot documentation goes here.
        
        @param p0 DESCRIPTION
        @type int
        """
        # TODO: not implemented yet
        self.UILogic() 
    
    @pyqtSlot(int)
    def on_checkBox_GAMMA_stateChanged(self, p0):
        """
        Slot documentation goes here.
        
        @param p0 DESCRIPTION
        @type int
        """
        # TODO: not implemented yet
        self.UILogic() 
