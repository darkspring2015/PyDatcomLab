# -*- coding: utf-8 -*-

"""
Module implementing CONTAB.
"""

from PyQt5.QtCore import pyqtSlot #, Qt, QPoint #, pyqtSignal
from PyQt5.QtWidgets import QWidget#, QMenu  #, QLineEdit, QComboBox, QTableWidget
from PyDatcomLab.GUIs.PlaneConfiguration import DatcomCARD as DC


from PyDatcomLab.Core import dcModel 

import logging

from Ui_CONTAB import Ui_CONTAB


class CONTAB(QWidget, Ui_CONTAB):
    """
    Class documentation goes here.
    """
    def __init__(self, parent=None, tModel = None):
        """
        Constructor
        
        @param parent reference to the parent widget
        @type QWidget
        """
        super(CONTAB, self).__init__(parent)
        self.setupUi(self)
        #创建日志
        self.logger = logging.getLogger(r'Datcomlogger')
        #开始核心数据的定义
        self.NameList = 'CONTAB'
        self.VariableList = {
                'CFITC':{  'TYPE':'REAL'  },    
                'CFOTC':{  'TYPE':'REAL'  },
                'BITC':{  'TYPE':'REAL'  },    
                'BOTC':{  'TYPE':'REAL'  },                
                'CFITT':{  'TYPE':'REAL'  },    
                'CFOTT':{  'TYPE':'REAL'  },
                'BITT':{  'TYPE':'REAL'  },    
                'BOTT':{  'TYPE':'REAL'  }, 
                'B1':{  'TYPE':'REAL'  },    
                'B2':{  'TYPE':'REAL'  },
                'B3':{  'TYPE':'REAL'  },    
                'B4':{  'TYPE':'REAL'  },   
                'D1':{  'TYPE':'REAL'  },    
                'D2':{  'TYPE':'REAL'  },
                'D3':{  'TYPE':'REAL'  },                 
                'CGMAX':{'TYPE':'REAL'  },               
                'KS':{   'TYPE':'REAL'  },    
                'RL':{  'TYPE':'REAL'  },               
                'BGR':{ 'TYPE':'REAL'  }, 
                'DELR':{'TYPE':'REAL'  }, 
                'TTYPE':{  'TYPE':'List'  ,'Range':['1.0', '2.0', '3.0'], 'Default':'1.0'}, 
        }  
        #self.NMACHLinkTable = []
        #tableWidget_controlAngle
        #self.RuleNumToCount = [{'Num':'HNDLTA' , 'Group':'controlAngle'},]
        #self.RuleIndexToCombo = [   ]        
        self.RuleVariableStatus = [ 
        {'ControlVar':'TTYPE', 
         'HowTo':{
            # 0 : 1.0 control tab
            '1.0':{'Disabled':[
                    'CFITT', 'CFOTT' , 'BITT', 'BOTT',
                    ], 
                   'Enabled':['TTYPE',
                    'CFITC', 'CFOTC', 'BITC', 'BOTC',
                    'B1', 'B2','B3' , 'B4',
                    'D1' , 'D2', 'D3', 
                    'CGMAX' , 'KS', 'RL', 'BGR', 'DELR', 
                    ]},
            # 1 : 2.0 trim tab 
            '2.0':{'Disabled':[
                    'CFITC', 'CFOTC', 'BITC', 'BOTC',                    
                    ], 
                   'Enabled':['TTYPE',
                    'CFITT', 'CFOTT' , 'BITT', 'BOTT',
                    'B1', 'B2','B3' , 'B4',
                    'D1' , 'D2', 'D3', 
                    'CGMAX' , 'KS', 'RL', 'BGR', 'DELR', 
                    ]}, 
            # 2 : 3.0 both
            '3.0':{'Disabled':[
                    
                    ], 
                   'Enabled':['TTYPE',
                    'CFITC', 'CFOTC', 'BITC', 'BOTC',
                    'CFITT', 'CFOTT' , 'BITT', 'BOTT',
                    'B1', 'B2','B3' , 'B4',
                    'D1' , 'D2', 'D3', 
                    'CGMAX' , 'KS', 'RL', 'BGR', 'DELR', 
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
#        self.curPos = QPoint(0, 0)
#        self.curWidget = None
#        self.curN = None
#        self.popMenu = None        
#        self.tableWidget_controlAngle.setContextMenuPolicy(Qt.CustomContextMenu)
    
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
    def on_comboBox_TTYPE_currentIndexChanged(self, index):
        """
        Slot documentation goes here.
        
        @param index DESCRIPTION
        @type int
        """
        # TODO: not implemented yet
        self.UILogic()
