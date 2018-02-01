# -*- coding: utf-8 -*-

"""
Module implementing FLTCON.
"""

from PyQt5.QtCore import pyqtSlot, Qt, QPoint, pyqtSignal
from PyQt5.QtWidgets import QWidget, QMenu  #,QMessageBox


from PyDatcomLab.Core import dcModel
from PyDatcomLab.GUIs.PlaneConfiguration import DatcomCARD as DC
import logging

from Ui_FLTCON import Ui_FLTCON


class FLTCON(QWidget, Ui_FLTCON):
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
        
        self.NameList = 'FLTCON'
        self.VariableList = {
                'LOOP':{  'TYPE':'List'  ,'Range':['1.0', '2.0', '3.0']   , 'Default':'1.0'},         
                'NMACH':{ 'TYPE':'INT'   ,'Range':[0, 20 ] }, 
                'NALT':{  'TYPE':'INT'   ,'Range':[0, 20 ] },
                'NALPHA':{'TYPE':'INT'   ,'Range':[0, 20 ] },
                'WT':{    'TYPE':'REAL'  ,'Range':[0, float('inf') ] },
                'GAMMA':{ 'TYPE':'REAL'  },
                'STMACH':{'TYPE':'REAL'  ,'Range':[0.6, 0.99 ]},    
                'TSMACH':{'TYPE':'REAL'  ,'Range':[1.01, 1.4 ]},  
                'HYPERS':{'TYPE':'List'  ,'Range':['.TRUE.', '.FALSE.']  , 'Default':'.TRUE.'}, 
                'TR':{    'TYPE':'List'  ,'Range':['0.0', '1.0']  , 'Default':'0.0'}, 
                'ALSCHD':{'TYPE':'Array', 'Limit':[0, 20] , 'Group':'ALSCHD'}, 
                'MACH':{  'TYPE':'Array', 'Limit':[0, 20] , 'Group':'Speed_Atmospheric'}, 
                'VINF':{  'TYPE':'Array', 'Limit':[0, 20] , 'Group':'Speed_Atmospheric'}, 
                'RNNUB':{ 'TYPE':'Array', 'Limit':[0, 20] , 'Group':'Speed_Atmospheric'},
                'ALT':{   'TYPE':'Array', 'Limit':[0, 20] , 'Group':'Speed_Atmospheric'},                
                'PINF':{  'TYPE':'Array', 'Limit':[0, 20] , 'Group':'Speed_Atmospheric'},  
                'TINF':{  'TYPE':'Array', 'Limit':[0, 20] , 'Group':'Speed_Atmospheric'},
        }        
        
        self.NMACHLinkTable = []   #['Lift' ]
        self.RuleNumToCount =[{'Num':'NMACH' , 'Group':'Speed_Atmospheric'}, 
                              {'Num':'NALPHA', 'Group':'ALSCHD'}]        
        self.RuleIndexToCombo = [{'Index':'Variables', 
                        'HowTo':{'1.0':['MACH', 'RNNUB'], 
                                 '2.0':['MACH', 'ALT'  ,'PINF', 'TINF' , 'RNNUB'], 
                                 '3.0':['VINF', 'ALT'  ,'PINF', 'TINF' , 'MACH', 'RNNUB'], 
                                 '4.0':['PINF', 'TINF', 'VINF', 'RNNUB', 'MACH'], 
                                 '5.0':['PINF', 'TINF', 'MACH', 'RNNUB', 'VINF'], 
                                 }, 
                        'Group':'Speed_Atmospheric'} 
                        ]
        
        #修改后台的数据
        if tModel is None:
            tModel = dcModel.dcModel('J6', '常规布局')  
        #定义数据
        self.DatcomCARD = DC.DatcomCARD(self)
        self.DatcomCARD.InitUi()
        self.DatcomCARD.setModel(tModel)   #设置模型
        
        #介于LOOP =2,3的逻辑实现机理不清楚，暂时只实现LOOP = 1.0
        self.comboBox_LOOP.setEnabled(False)
        self.NALT.setEnabled(False)
        
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
        self.DatcomCARD.UILogic()
        
        #NACA
#        if self.checkBox_UsingNACA.checkState() == Qt.Checked:
#            self.comboBox_Variables.setCurrentIndex(0) #选中模式1
#        else :
#            #self.comboBox_Variables.setCurrentIndex(1) #选中模式2,认为是默认值
#            pass
            
        if self.comboBox_Variables.currentIndex() == 0:
            self.checkBox_UsingNACA.setCheckState(Qt.Checked)
        else:
            self.checkBox_UsingNACA.setCheckState(Qt.Unchecked)
            
        #分析LOOP执行相应的选择模式
        if self.comboBox_LOOP.currentIndex() == 0:
            if self.NMACH.text() != self.NALT.text():
                self.NALT.setText(self.NMACH.text()) 
            pass
        elif self.comboBox_LOOP.currentIndex() == 1:
            pass
        else:
            pass
     
    

    
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

        
#        nowRows = self.tableWidget_Speed_Atmospheric.rowCount()
#        tNx = int(self.NMACH.text())
#        if nowRows < tNx: 
#            #if 当前行数少，考虑增加
#            for itR in range(nowRows, tNx):
#                self.tableWidget_Speed_Atmospheric.insertRow(itR)
#        if nowRows == tNx:
#            pass
#        if nowRows > tNx:
#            self.NMACH.setText(str(nowRows))
#            strInfo = "尝试的NMACH小于现有数据行数，请手动从表格中删除对应行"
#            QMessageBox.information(self, "提示" , strInfo)  
#            self.logger.info(strInfo)    
#            
#        if self.NMACH.text() != self.NALT.text():
#            self.NALT.setText(self.NMACH.text())        
        
        #因为只实现LOOP =1
        self.UILogic()  
    
    @pyqtSlot()
    def on_NALPHA_editingFinished(self):
        """
        Slot documentation goes here.
        当编辑攻角数
        
        """
        # TODO: not implemented yet
#        nowRows = self.tableWidget_ALSCHD.rowCount()
#        tNx = int(self.NALPHA.text())
#        if nowRows < tNx: 
#            #if 当前行数少，考虑增加
#            for itR in range(nowRows, tNx):
#                self.tableWidget_ALSCHD.insertRow(itR)
#        if nowRows == tNx:
#            pass
#        if nowRows > tNx:
#            self.NALPHA.setText(str(nowRows))
#            strInfo = "尝试的NALPHA小于现有数据行数，请手动从表格中删除对应行"
#            QMessageBox.information(self, "提示" , strInfo)  
#            self.logger.info(strInfo)    
            
        
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
