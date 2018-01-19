# -*- coding: utf-8 -*-

"""
Module implementing SYMFLP.
"""

from PyQt5.QtCore import pyqtSlot, Qt, QPoint #, pyqtSignal
from PyQt5.QtWidgets import QWidget, QMenu  #, QLineEdit, QComboBox, QTableWidget
from PyDatcomLab.GUIs.PlaneConfiguration import DatcomCARD as DC


from PyDatcomLab.Core import dcModel 

import logging

from Ui_SYMFLP import Ui_SYMFLP


class SYMFLP(QWidget, Ui_SYMFLP):
    """
    Class documentation goes here.
    """
    def __init__(self, parent=None, tModel = None):
        """
        Constructor
        
        @param parent reference to the parent widget
        @type QWidget
        """
        super(SYMFLP, self).__init__(parent)
        self.setupUi(self)
        #创建日志
        self.logger = logging.getLogger(r'Datcomlogger')
        #开始核心数据的定义
        self.NameList = 'SYMFLP'
        self.VariableList = {
                'FTYPE':{  'TYPE':'List'  ,'Range':['1.0', '2.0', '3.0',
                                   '4.0', '5.0', '6.0', '7.0', '8.0', '9.0']   , 'Default':'1.0'},         
                'NDELTA':{ 'TYPE':'INT'   ,'Range':[0, 9 ] }, 
                'PHETE':{  'TYPE':'REAL'  },
                'PHETEP':{ 'TYPE':'REAL'  },
                'CHRDFI':{ 'TYPE':'REAL'  },    
                'CHRDFO':{ 'TYPE':'REAL'  },  
                'SPANFI':{ 'TYPE':'REAL'  },    
                'SPANFO':{ 'TYPE':'REAL'  },  
                'DOBCIN':{ 'TYPE':'REAL'  },    
                'DOBCOT':{ 'TYPE':'REAL'  },  
                'CB':{     'TYPE':'REAL'  },    
                'TC':{     'TYPE':'REAL'  },  
                'CMU':{    'TYPE':'REAL'  }, 
                'NTYPE':{  'TYPE':'List'  ,'Range':['1.0', '2.0', '3.0']          , 'Default':'1.0'},   
                'JETFLP':{ 'TYPE':'List'  ,'Range':['1.0', '2.0', '3.0', '4.0']   , 'Default':'1.0'}, 
                'DELTA':{  'TYPE':'Array', 'Limit':[0, 9] , 'Group':'flap'}, 
                'CPRMEI':{ 'TYPE':'Array', 'Limit':[0, 9] , 'Group':'flap'}, 
                'CPRMEO':{ 'TYPE':'Array', 'Limit':[0, 9] , 'Group':'flap'}, 
                'CAPINB':{ 'TYPE':'Array', 'Limit':[0, 9] , 'Group':'flap'},
                'CAPOUT':{ 'TYPE':'Array', 'Limit':[0, 9] , 'Group':'flap'},                
                'DOBDEF':{ 'TYPE':'Array', 'Limit':[0, 9] , 'Group':'flap'},  
                'SCLD':{   'TYPE':'Array', 'Limit':[0, 9] , 'Group':'delta'},
                'SCMD':{   'TYPE':'Array', 'Limit':[0, 9] , 'Group':'delta'},
                'DELJET':{ 'TYPE':'Array', 'Limit':[0, 9] , 'Group':'jet'},
                'EFFJET':{ 'TYPE':'Array', 'Limit':[0, 9] , 'Group':'jet'},
        }  
        #self.NMACHLinkTable = []
        self.RuleNumToCount = [{'Num':'NDELTA' , 'Group':'delta'}, 
                               {'Num':'NDELTA', 'Group':'flap'}]
        self.RuleIndexToCombo = [
           {'Index':'FTYPE', 
            'HowTo':{'1.0':['DELTA', ], 
                     '2.0':['DELTA', 'CPRMEI', 'CPRMEO' ,], 
                     '3.0':['DELTA', 'CPRMEI', 'CPRMEO' ,], 
                     '4.0':['DELTA', 'CPRMEI', 'CPRMEO' ,'CAPINB', 'CAPOUT','DOBDEF', ], 
                     '5.0':['DELTA', ], 
                     '6.0':['DELTA', ],  
                     '7.0':['DELTA', 'CPRMEI', 'CPRMEO' ,],   
                     '8.0':['DELTA', 'CPRMEI', 'CPRMEO' ,],      
                     '9.0':[], 
                     }, 
            'Group':'flap'} , 
            {'Index':'FTYPE', 
            'HowTo':{'1.0':[ ], 
                     '2.0':[ ], 
                     '3.0':[ ],
                     '4.0':[ ],
                     '5.0':[ ],
                     '6.0':[ ],
                     '7.0':[ ],  
                     '8.0':[ ],      
                     '9.0':['CPRMEI', 'CPRMEO' ,'DELJET', 'EFFJET',], 
                     }, 
            'Group':'jet'} 
        ]        
        self.RuleVariableStatus = [ 
        {'ControlVar':'FTYPE', 
         'HowTo':{
            # 0 : 1.0 平襟翼
            '1.0':{'Disabled':[
                    'CPRMEI', 'CPRMEO' , 
                    'CAPINB', 'CAPOUT',
                    'DOBDEF', 'DOBCIN', 'DOBCOT', 
                    'JETFLP', 'CMU', 'DELJET', 'EFFJET',], 
                   'Enabled':['FTYPE','NDELTA', 'DELTA', 
                    'PHETE', 'PHETEP',
                    'CHRDFI', 'CHRDFO',
                    'SPANFI' , 'SPANFO',
                    'CB' , 'TC', 'NTYPE']},
            # 1 : 2.0 单缝襟翼 
            '2.0':{'Disabled':[            
                    'CAPINB', 'CAPOUT',
                    'DOBDEF', 'DOBCIN', 'DOBCOT', 
                    'JETFLP', 'CMU',    'DELJET', 'EFFJET',
                    'CB'    , 'TC',     'NTYPE'], 
                   'Enabled':[
                    'FTYPE',  'NDELTA', 'DELTA', 
                    'PHETE',  'PHETEP', 
                    'CHRDFI', 'CHRDFO',
                    'SPANFI', 'SPANFO',
                    'CPRMEI', 'CPRMEO' ]}, 
            # 2 : 3.0 福勒襟翼
            '3.0':{'Disabled':[ 
                    'CAPINB', 'CAPOUT',
                    'DOBDEF', 'DOBCIN', 'DOBCOT', 
                    'CB' , 'TC',  'NTYPE',
                    'JETFLP', 'CMU', 'DELJET', 'EFFJET',], 
                   'Enabled':[
                    'FTYPE','NDELTA', 'DELTA',
                    'PHETE', 'PHETEP', 
                    'CHRDFI', 'CHRDFO',
                    'SPANFI', 'SPANFO',
                    'CPRMEI', 'CPRMEO' ]}, 
            # 3 : 4.0 双缝襟翼 
            '4.0':{'Disabled':[                  
                    'CB' , 'TC',  'NTYPE', 
                    'JETFLP', 'CMU', 'DELJET', 'EFFJET', ], 
                   'Enabled':[
                    'FTYPE','NDELTA', 'DELTA',
                    'PHETE', 'PHETEP', 
                    'CAPINB', 'CAPOUT',
                    'DOBDEF', 'DOBCIN', 'DOBCOT',
                    'CHRDFI', 'CHRDFO',
                    'SPANFI', 'SPANFO',
                    'CPRMEI', 'CPRMEO' ]}, 
            # 4 : 5.0 分裂襟翼 
            '5.0':{'Disabled':[                    
                    'CPRMEI', 'CPRMEO' , 
                    'CAPINB', 'CAPOUT',
                    'DOBDEF', 'DOBCIN', 'DOBCOT', 
                    'CB' , 'TC', 'NTYPE', 
                    'JETFLP', 'CMU', 'DELJET', 'EFFJET',], 
                   'Enabled':[
                        'FTYPE','NDELTA', 'DELTA',  
                        'PHETE', 'PHETEP',
                        'CHRDFI', 'CHRDFO',
                        'SPANFI' , 'SPANFO',
                   ]},
            # 5 : 6.0 前缘襟翼 
            '6.0':{'Disabled':[
                    'PHETE', 'PHETEP',
                    'CPRMEI', 'CPRMEO' , 
                    'CAPINB', 'CAPOUT',
                    'DOBDEF', 'DOBCIN', 'DOBCOT', 
                    'CB' , 'TC', 'NTYPE', 
                    'JETFLP', 'CMU', 'DELJET', 'EFFJET',], 
                   'Enabled':[
                        'FTYPE','NDELTA', 'DELTA',  
                        'CHRDFI', 'CHRDFO',
                        'SPANFI' , 'SPANFO',
                   ]},
            # 6 : 7.0 前缘缝翼
            '7.0':{'Disabled':[  
                    'PHETE',  'PHETEP',           
                    'CAPINB', 'CAPOUT',
                    'DOBDEF', 'DOBCIN', 'DOBCOT',
                    'CB'    , 'TC',     'NTYPE',  
                    'JETFLP', 'CMU',    'DELJET', 'EFFJET',
                    ], 
                   'Enabled':[
                    'FTYPE',  'NDELTA', 'DELTA', 
                    'CPRMEI', 'CPRMEO' ,                     
                    'CHRDFI', 'CHRDFO',
                    'SPANFI', 'SPANFO',
                    ]},             
            # 7 : 8.0 克鲁格襟翼
            '8.0':{'Disabled':[  
                    'PHETE',  'PHETEP',           
                    'CAPINB', 'CAPOUT',
                    'DOBDEF', 'DOBCIN', 'DOBCOT',
                    'CB'    , 'TC',     'NTYPE',  
                    'JETFLP', 'CMU',    'DELJET', 'EFFJET',
                    ], 
                   'Enabled':[
                    'FTYPE',  'NDELTA', 'DELTA', 
                    'CPRMEI', 'CPRMEO' ,                     
                    'CHRDFI', 'CHRDFO',
                    'SPANFI', 'SPANFO',
                    ]},
            # 8 : 9.0 克鲁格襟翼
            '9.0':{'Disabled':[  
                    'DELTA', 
                    'PHETE',  'PHETEP',           
                    'CAPINB', 'CAPOUT',
                    'DOBDEF', 'DOBCIN', 'DOBCOT',
                    'CB'    , 'TC',     'NTYPE',                      
                    ], 
                   'Enabled':[
                    'FTYPE',  'NDELTA', 
                    'CPRMEI', 'CPRMEO' ,                     
                    'CHRDFI', 'CHRDFO',
                    'SPANFI', 'SPANFO',
                    'JETFLP', 'CMU',   
                    'DELJET', 'EFFJET',
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
        self.tableWidget_delta.setContextMenuPolicy(Qt.CustomContextMenu)
        self.tableWidget_flap.setContextMenuPolicy(Qt.CustomContextMenu)
        self.tableWidget_jet.setContextMenuPolicy(Qt.CustomContextMenu)
        
        
        
        #初始化数据和内容  
        self.UILogic()   
        
    def setModel(self, tModel):
        """
        初始化本节点的xml描述文档
        """
        
        #self.Model = tModel        
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
    def on_comboBox_FTYPE_currentIndexChanged(self, index):
        """
        Slot documentation goes here.
        
        @param index DESCRIPTION
        @type int
        """
        self.UILogic() 
        if self.comboBox_FTYPE.currentIndex() == 8:
            self.tabWidget_input.setCurrentIndex(1)
        else:
            self.tabWidget_input.setCurrentIndex(0)
    
    @pyqtSlot(int)
    def on_comboBox_NTYPE_currentIndexChanged(self, index):
        """
        Slot documentation goes here.
        
        @param index DESCRIPTION
        @type int
        """
        self.UILogic() 
    
    @pyqtSlot(QPoint)
    def on_tableWidget_flap_customContextMenuRequested(self, pos):
        """
        Slot documentation goes here.
        
        @param pos DESCRIPTION
        @type QPoint
        """

        self.curPos = pos
        self.curWidget = self.tableWidget_flap        
        posG = self.curWidget.mapToGlobal(pos)
        self.popMenu = QMenu(self.curWidget)
        self.popMenu.addAction(self.actionAddRow)
        self.popMenu.addAction(self.actionDeleteRow)
        self.curWidget.setContextMenuPolicy(Qt.CustomContextMenu)
        self.curN = self.NDELTA
        
        self.popMenu.exec(posG)
    
    @pyqtSlot(QPoint)
    def on_tableWidget_delta_customContextMenuRequested(self, pos):
        """
        Slot documentation goes here.
        
        @param pos DESCRIPTION
        @type QPoint
        """

        self.curPos = pos
        self.curWidget = self.tableWidget_delta        
        posG = self.curWidget.mapToGlobal(pos)
        self.popMenu = QMenu(self.curWidget)
        self.popMenu.addAction(self.actionAddRow)
        self.popMenu.addAction(self.actionDeleteRow)
        self.curWidget.setContextMenuPolicy(Qt.CustomContextMenu)
        self.curN = self.NDELTA
        
        self.popMenu.exec(posG)
    
    @pyqtSlot()
    def on_NDELTA_editingFinished(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        self.UILogic() 
    
    @pyqtSlot(QPoint)
    def on_tableWidget_jet_customContextMenuRequested(self, pos):
        """
        Slot documentation goes here.
        
        @param pos DESCRIPTION
        @type QPoint
        """
        self.curPos = pos
        self.curWidget = self.tableWidget_jet        
        posG = self.curWidget.mapToGlobal(pos)
        self.popMenu = QMenu(self.curWidget)
        self.popMenu.addAction(self.actionAddRow)
        self.popMenu.addAction(self.actionDeleteRow)
        self.curWidget.setContextMenuPolicy(Qt.CustomContextMenu)
        #self.curN = self.NDELTA
        
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
        
        if self.curWidget.objectName() == 'tableWidget_flap' :
            tLimit = 9    
        if self.curWidget.objectName() == 'tableWidget_delta':
            tLimit = 9
        if self.curWidget.objectName() == 'tableWidget_jet':
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
