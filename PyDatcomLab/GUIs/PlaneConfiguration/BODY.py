# -*- coding: utf-8 -*-

"""
Module implementing BODY.
"""

from PyQt5.QtCore import pyqtSlot, QPoint
from PyQt5.QtWidgets import QWidget
#from PyQt5.QtGui import QIntValidator ,QDoubleValidator#, QStandardItemModel#, 

from PyDatcomLab.GUIs.PlaneConfiguration import DatcomCARD as DC
from PyDatcomLab.Core import dcModel 

import logging

from Ui_BODY import Ui_Form
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
        
        #开始核心数据的定义
        self.NameList = 'BODY'
        self.VariableList = {
                #
                'NX':{     'TYPE':'INT', 'DisplayName':'NX：机身横截面的个数'},
                'ELLIP':{  'TYPE':'REAL', 'DisplayName':'ELLIP：机身高宽比'},
                'BNOSE':{  'TYPE':'List','Range':['1.0', '2.0']          , 'Default':'1.0', 'DisplayName':'BNOSE：机头样式'},
                'BTAIL':{  'TYPE':'List','Range':['1.0', '2.0']          , 'Default':'1.0', 'DisplayName':'BTAIL：机尾样式'},
                'BLN':{    'TYPE':'REAL', 'DisplayName':'BLN：机身头部长度'},
                'BLA':{    'TYPE':'REAL', 'DisplayName':'BLA：机身后部长度'}, 
                'DS':{     'TYPE':'REAL', 'DisplayName':'DS：机头钝度'}, 
                'ITYPE':{  'TYPE':'List','Range':['1.0', '2.0', '3.0']   , 'Default':'2.0', 'DisplayName':'ITYPE:跨音速阻力分量马赫数计算方法'},
                'METHOD':{ 'TYPE':'List','Range':['1.0', '2.0']          , 'Default':'1.0', 'DisplayName':'METHOD：计算方法'},
             
                #Body参数定义
                'X':{  'TYPE':'Array' , 'Limit':[0, 20]  , 'Group':'BODY'}  , 
                'S':{  'TYPE':'Array' , 'Limit':[0, 20]  , 'Group':'BODY'}  , 
                'R':{  'TYPE':'Array' , 'Limit':[0, 20]  , 'Group':'BODY'}  , 
                'P':{  'TYPE':'Array' , 'Limit':[0, 20]  , 'Group':'BODY'}  , 
                'ZU':{ 'TYPE':'Array' , 'Limit':[0, 20]  , 'Group':'BODY'}  , 
                'ZL':{ 'TYPE':'Array' , 'Limit':[0, 20]  , 'Group':'BODY'}  , 

        }  
        self.NMACHLinkTable = []
        self.RuleNumToCount =[{'Num':'NX', 'Group':'BODY'}]
        self.RuleIndexToCombo = [{'Index':'VarCombo', 
                        'HowTo':{'1.0':['X', 'R', 'ZU', 'ZL'], 
                                 '2.0':['X', 'S', 'ZU', 'ZL'], 
                                 '3.0':['X', 'S', 'R', 'P', 'ZU', 'ZL']}, 
                        'Group':'BODY'} 
                        ]
     
  
        #修改后台的数据
        if tModel is None:
            tModel = dcModel.dcModel('J6', '常规布局')  
        #定义数据
        self.DatcomCARD = DC.DatcomCARD(self)
        self.InitComboVar(tModel)
        self.DatcomCARD.InitUi()
        self.DatcomCARD.setModel(tModel)   #设置模型

        #界面参数-表格逻辑
        self.curPos = QPoint(0, 0)
        self.curWidget = None
        self.curN = None
        self.popMenu = None
        
        #初始化数据和内容  
        self.UILogic()         
        
        
    def setModel(self, tModel):
        """
        初始化本节点的xml描述文档
        """
        
        #self.Model = tModel        
        #执行参数配置过程    
        self.InitComboVar(tModel)
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
        执行UI界面刷新操作
        """        
        self.DatcomCARD.UILogic()
        #其他表格的逻辑
        
    def InitComboVar(self, tModel):
        """
        分析数据的变量组合将结果添加到列表中
        """

        #检测参数组合
        data ={}
        data['X']  = tModel.getNamelistVar('BODY','X')
        data['R']  = tModel.getNamelistVar('BODY','R')
        data['S']  = tModel.getNamelistVar('BODY','S')
        data['P']  = tModel.getNamelistVar('BODY','P')
        data['ZU'] = tModel.getNamelistVar('BODY','ZU')
        data['ZL'] = tModel.getNamelistVar('BODY','ZL')
        #判断模式
        modStr = []
        for itr in data.keys():
            if data[itr]:
                modStr.append(itr) 
        isHas = False
        if modStr == [] :return
        for tMod in self.RuleIndexToCombo[0]['HowTo'].keys():
            tModList = self.RuleIndexToCombo[0]['HowTo'][tMod]
            if modStr == tModList:
                isHas = True
                break
        if isHas == False:
            tKey = '%d.0'%(len(self.RuleIndexToCombo[0]['HowTo']))
            self.RuleIndexToCombo[0]['HowTo'][tKey] = modStr

   
        
    
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
        self.UILogic()
            
    
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
        
#        nowRows = self.Tab_ComboVariables.rowCount()
#        tNx = int(self.NX.text())
#        if nowRows < tNx: 
#            #if 当前行数少，考虑增加
#            for itR in range(nowRows, tNx):
#                self.Tab_ComboVariables.insertRow(itR)
#        if nowRows == tNx:
#            pass
#        if nowRows > tNx:
#            self.NX.setText(str(nowRows))
#            strInfo = "尝试的NX小于现有数据行数，请手动从表格中删除对应行"
#            QMessageBox.information(self, "提示" , strInfo)  
#            self.logger.info(strInfo)
        self.UILogic()
    
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
