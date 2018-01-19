# -*- coding: utf-8 -*-

"""
Module implementing LARWB.
"""

from PyQt5.QtCore import pyqtSlot #, Qt, QPoint #, pyqtSignal
from PyQt5.QtWidgets import QWidget #, QMenu  #, QLineEdit, QComboBox, QTableWidget
from PyDatcomLab.GUIs.PlaneConfiguration import DatcomCARD as DC


from PyDatcomLab.Core import dcModel 

import logging

from Ui_LARWB import Ui_LARWB


class LARWB(QWidget, Ui_LARWB):
    """
    Class documentation goes here.
    """
    def __init__(self, parent=None, tModel = None):
        """
        Constructor
        
        @param parent reference to the parent widget
        @type QWidget
        """
        super(LARWB, self).__init__(parent)
        self.setupUi(self)
        #创建日志
        self.logger = logging.getLogger(r'Datcomlogger')
        #开始核心数据的定义
        self.NameList = 'LARWB'
        self.VariableList = {                         
                'ZB':{     'TYPE':'REAL'  }, 
                'SREF':{   'TYPE':'REAL'  },    
                'DELTEP':{ 'TYPE':'REAL'  },
                'SFRONT':{ 'TYPE':'REAL'  },               
                'AR':{     'TYPE':'REAL'  },    
                'R3LEOB':{ 'TYPE':'REAL'  },  
                'DELTAL':{ 'TYPE':'REAL'  },   
                'L':{      'TYPE':'REAL'  }, 
                'SWET':{   'TYPE':'REAL'  }, 
                'PERBAS':{ 'TYPE':'REAL'  }, 
                'SBASE':{  'TYPE':'REAL'  }, 
                'HB':{     'TYPE':'REAL'  }, 
                'BB':{     'TYPE':'REAL'  }, 
                'BLF':{    'TYPE':'List'  ,'Range':['.TRUE.', '.FALSE.'], 'Default':'.TRUE.'}, 
                'XCG':{    'TYPE':'REAL'  }, 
                'THETAD':{ 'TYPE':'REAL'  }, 
                'ROUNDN':{ 'TYPE':'List'  ,'Range':['.TRUE.', '.FALSE.'], 'Default':'.TRUE.'}, 
                'SBS':{    'TYPE':'REAL'  }, 
                'SBSLB':{  'TYPE':'REAL'  }, 
                'XCENSB':{ 'TYPE':'REAL'  }, 
                'XCENW':{  'TYPE':'REAL'  }, 
        }  
        #self.NMACHLinkTable = []
        #self.RuleNumToCount = [{'Num':'NDELTA' , 'Group':'input'}, ]
#        self.RuleIndexToCombo = [
#           {'Index':'STYPE', 
#            'HowTo':{'1.0':['DELTAS', 'XSOC',   'HSOC'], 
#                     '2.0':['DELTAS', 'XSOC',   'HSOC'],  
#                     '3.0':['DELTAD', 'DELTAS', 'HSOC'], 
#                     '4.0':['DELTAL', 'DELTAR' ], 
#                     '5.0':['DELTAL', 'DELTAR' ], 
#                     }, 
#            'Group':'input'} ,           
#        ]        
#        self.RuleVariableStatus = []

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
#        self.tableWidget_input.setContextMenuPolicy(Qt.CustomContextMenu)
    
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
    def on_comboBox_BLF_currentIndexChanged(self, index):
        """
        Slot documentation goes here.
        
        @param index DESCRIPTION
        @type int
        """
        self.UILogic()
    
    @pyqtSlot(int)
    def on_comboBox_ROUNDN_currentIndexChanged(self, index):
        """
        Slot documentation goes here.
        
        @param index DESCRIPTION
        @type int
        """
        self.UILogic()
