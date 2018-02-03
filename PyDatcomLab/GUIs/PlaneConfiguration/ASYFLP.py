# -*- coding: utf-8 -*-

"""
Module implementing ASYFLP.
"""

from PyQt5.QtCore import pyqtSlot, Qt, QPoint #, pyqtSignal
from PyQt5.QtWidgets import QWidget, QMenu  #, QLineEdit, QComboBox, QTableWidget
from PyDatcomLab.GUIs.PlaneConfiguration import DatcomCARD as DC


from PyDatcomLab.Core import dcModel 

import logging


from Ui_ASYFLP import Ui_ASYFLP


class ASYFLP(QWidget, Ui_ASYFLP):
    """
    Class documentation goes here.
    """
    def __init__(self, parent=None, tModel = None):
        """
        Constructor
        
        @param parent reference to the parent widget
        @type QWidget
        """
        super(ASYFLP, self).__init__(parent)
        self.setupUi(self)
        #创建日志
        self.logger = logging.getLogger(r'Datcomlogger')
        #开始核心数据的定义
        self.NameList = 'ASYFLP'
        self.VariableList = {
                'STYPE':{  'TYPE':'List'  ,'Range':['1.0', '2.0', '3.0','4.0', '5.0'], 'Default':'1.0'
                        , 'DisplayName':'STYPE  类别'},         
                'NDELTA':{ 'TYPE':'INT'   ,'Range':[0, 9 ] , 'DisplayName':'NDELTA 偏转角数量'}, 
                'SPANFI':{ 'TYPE':'REAL'  , 'DisplayName':'SPANFI  内侧端的横向距离'},    
                'SPANFO':{ 'TYPE':'REAL'  , 'DisplayName':'SPANFO  外侧端的横向距离'},
                'PHETE':{  'TYPE':'REAL'  , 'DisplayName':'PHETE  翼型后缘角正切值(90,99)'},               
                'CHRDFI':{ 'TYPE':'REAL'  , 'DisplayName':'CHRDFI  内侧弦长'},    
                'CHRDFO':{ 'TYPE':'REAL'  , 'DisplayName':'CHRDFO  外侧弦长'},  
                'XSPRME':{ 'TYPE':'REAL'  , 'DisplayName':'XSPRME  扰流板铰链到机翼前延的距离'},   
                'DELTAL':{ 'TYPE':'Array', 'Limit':[0, 9] , 'Group':'input', 'DisplayName':''}, 
                'DELTAR':{ 'TYPE':'Array', 'Limit':[0, 9] , 'Group':'input', 'DisplayName':''}, 
                'DELTAD':{ 'TYPE':'Array', 'Limit':[0, 9] , 'Group':'input', 'DisplayName':''}, 
                'DELTAS':{ 'TYPE':'Array', 'Limit':[0, 9] , 'Group':'input', 'DisplayName':''},
                'XSOC':{   'TYPE':'Array', 'Limit':[0, 9] , 'Group':'input', 'DisplayName':''}, 
                'HSOC':{   'TYPE':'Array', 'Limit':[0, 9] , 'Group':'input', 'DisplayName':''},
        }  
        #self.NMACHLinkTable = []
        self.RuleNumToCount = [{'Num':'NDELTA' , 'Group':'input'}, 
                               ]
        self.RuleIndexToCombo = [
           {'Index':'STYPE', 
            'HowTo':{'1.0':['DELTAS', 'XSOC',   'HSOC'], 
                     '2.0':['DELTAS', 'XSOC',   'HSOC'],  
                     '3.0':['DELTAD', 'DELTAS', 'HSOC'], 
                     '4.0':['DELTAL', 'DELTAR' ], 
                     '5.0':['DELTAL', 'DELTAR' ], 
                     }, 
            'Group':'input'} ,           
        ]        
        self.RuleVariableStatus = [ 
        {'ControlVar':'STYPE', 
         'HowTo':{
            # 0 : 1.0 机翼襟翼扰流板
            '1.0':{'Disabled':[
                    'DELTAL', 'DELTAR' , 
                    'CHRDFI', 'CHRDFO',
                    'DELTAD', 
                    ], 
                   'Enabled':[
                    'STYPE' , 'NDELTA', 
                    'SPANFI', 'SPANFO', 
                    'PHETE' , 'XSPRME', 
                    'DELTAS',
                    'XSOC'  , 'HSOC',
                    ]},
            # 1 : 2.0 机翼上的扰流板 
            '2.0':{'Disabled':[
                    'DELTAL', 'DELTAR' , 
                    'CHRDFI', 'CHRDFO',
                    'DELTAD', 
                    ], 
                   'Enabled':[
                    'STYPE' , 'NDELTA', 
                    'SPANFI', 'SPANFO', 
                    'PHETE' , 'XSPRME', 
                    'DELTAS',
                    'XSOC'  , 'HSOC',
                    ]},
            # 2 : 3.0 扰流板槽偏转翼
            '3.0':{'Disabled':[
                    'DELTAL', 'DELTAR' , 
                    'CHRDFI', 'CHRDFO',
                    'XSPRME', 
                    ], 
                   'Enabled':[
                    'STYPE' , 'NDELTA', 
                    'SPANFI', 'SPANFO', 
                    'PHETE' ,  
                    'DELTAD','DELTAS',
                    'XSOC'  , 'HSOC',
                    ]},
            # 3 : 4.0 简单襟翼副翼 
            '4.0':{'Disabled':[
                    'XSPRME', 
                    'XSOC'  , 'HSOC',
                    'DELTAD', 'DELTAS',
                    'PHETE' ,
                    ], 
                   'Enabled':[
                    'STYPE' , 'NDELTA', 
                    'DELTAL', 'DELTAR' ,
                    'CHRDFI', 'CHRDFO', 
                    'SPANFI', 'SPANFO', 
                    ]},
            # 4 : 5.0 差动平尾 
            '5.0':{'Disabled':[
                    'SPANFI', 'SPANFO', 
                    'XSPRME',  
                    'CHRDFI', 'CHRDFO',
                    'DELTAD', 'DELTAS',
                    'XSOC'  , 'HSOC',
                    ], 
                   'Enabled':[
                    'STYPE' , 'NDELTA', 
                    'DELTAL', 'DELTAR' ,
                    'PHETE' ,
                    ]},          
            }
            }
            ]

        #修改后台的数据
        if tModel is None:
            tModel = dcModel.dcModel('J6', '常规布局')  
        #定义数据
        self.DatcomCARD = DC.DatcomCARD(self)
        #self.InitComboVar(tModel)
        self.DatcomCARD.InitUi()
        self.DatcomCARD.setModel(tModel)   #设置模型
        
        #界面参数-表格逻辑
        self.curPos = QPoint(0, 0)
        self.curWidget = None
        self.curN = None
        self.popMenu = None        
        self.tableWidget_input.setContextMenuPolicy(Qt.CustomContextMenu)
    
        #初始化数据和内容  
        self.UILogic()   
        
    def setModel(self, tModel):
        """
        初始化本节点的xml描述文档
        """
      
        #执行参数配置过程    
        #self.InitComboVar(tModel)
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
    
    @pyqtSlot(int)
    def on_comboBox_STYPE_currentIndexChanged(self, index):
        """
        Slot documentation goes here.
        
        @param index DESCRIPTION
        @type int
        """
        self.UILogic()
    
    @pyqtSlot()
    def on_NDELTA_editingFinished(self):
        """
        Slot documentation goes here.
        """
        self.UILogic()
    
    @pyqtSlot(QPoint)
    def on_tableWidget_input_customContextMenuRequested(self, pos):
        """
        Slot documentation goes here.
        
        @param pos DESCRIPTION
        @type QPoint
        """
        self.curPos = pos
        self.curWidget = self.tableWidget_input        
        posG = self.curWidget.mapToGlobal(pos)
        self.popMenu = QMenu(self.curWidget)
        self.popMenu.addAction(self.actionAddRow)
        self.popMenu.addAction(self.actionDeleteRow)
        self.curWidget.setContextMenuPolicy(Qt.CustomContextMenu)
        self.curN = self.NDELTA
        
        self.popMenu.exec(posG)
    
    @pyqtSlot()
    def on_actionAddRow_triggered(self):
        """
        Slot documentation goes here.
        """
        
        #添加行
        aItem = self.curWidget.indexAt(self.curPos) #认为是表格 ，否则会异常
        rowIndex = 0
        if aItem.row() == -1 :
            #没有命中
            rowIndex = self.curWidget.rowCount()
        else:
            rowIndex = aItem.row()
            
        tLimit = 9    
        if self.curWidget.rowCount() < tLimit:
            self.curWidget.insertRow(rowIndex)
        else:
            self.logger.info("%s已经达到最大行数不能添加"%self.curWidget.objectName())
        if not self.curN is None: 
            self.curN.setText(str(self.curWidget.rowCount()))
        
        self.UILogic() 
    
    @pyqtSlot()
    def on_actionDeleteRow_triggered(self):
        """
        Slot documentation goes here.
        """
        
        aItem = self.curWidget.indexAt(self.curPos)
        if  aItem.row() >= 0 :            
            self.curWidget.removeRow(aItem.row())
        else:
            self.logger.info("没有命中任何行")
            
        if not self.curN is None:
            self.curN.setText(str(self.curWidget.rowCount()))
            
        self.UILogic() 
