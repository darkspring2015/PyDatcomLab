# -*- coding: utf-8 -*-

"""
Module implementing FLTCON.
"""

from PyQt5.QtCore import pyqtSlot, QPoint, QMetaObject
from PyQt5.QtWidgets import QWidget,  QTableWidget , QLineEdit #,QMessageBox


from PyDatcomLab.Core import dcModel
from PyDatcomLab.Core.DictionaryLoader import  defaultDatcomDefinition as DDefine  
from PyDatcomLab.GUIs.InputCard import DatcomCARDLogicBase as DCLogic  
from PyDatcomLab.GUIs.InputCard.DatcomCARDUiBase import DatcomCARDUIBase 
import logging




class DatcomWidgetBase(QWidget, DatcomCARDUIBase):
    """
    Datcom 输入选项卡的基础类.
    """
    
    #定义各个Widget之间进行参数同步的信号
    #emit_NMACHChanged = pyqtSignal(str, int)  #发送NMACH变化的信息
    #emit_NACAChanged = pyqtSignal(str, bool)  #发送NACA选项卡的信号
    
    
    def __init__(self, parent=None, tCARD = 'FLTCON' , tModel = None):
        """
        Constructor
        
        @param parent reference to the parent widget
        @type QWidget
        """
        super(DatcomWidgetBase, self).__init__(parent)
        #self.setupUi(self)
        
        #创建日志
        self.logger = logging.getLogger(r'Datcomlogger')
        #获得Datcom的界面定义
        self.NameList = tCARD
        self.VariableList       = DDefine.getNamelistDefineByName(self.NameList) 
        self.NMACHLinkTable     = DDefine.getCARDAddtionalInformation(self.NameList, 'NMACHLinkTable') 
        self.RuleNumToCount     = DDefine.getCARDAddtionalInformation(self.NameList, 'RuleNumToCount')        
        self.RuleIndexToCombo   = DDefine.getCARDAddtionalInformation(self.NameList, 'RuleIndexToCombo')  
        self.GroupDefine        = DDefine.getCARDAddtionalInformation(self.NameList, 'GroupDefine')  
        self.RuleVariableStatus = DDefine.getCARDAddtionalInformation(self.NameList, 'RuleVariableStatus')
        self.HashVaribles       = {}
        
        
        #配置完成后再调用界面初始化
        self.setupUi(self)
        
        #修改后台的数据
        if tModel is None:
            tModel = dcModel.dcModel()  
        #定义数据
        self.DatcomCARD = DCLogic.DatcomCARDLogicBase(self)
        self.DatcomCARD.InitUi()
        self.DatcomCARD.setModel(tModel)   #设置模型
        
        #绑定处理逻辑
        self.Singal_RuleIndexToCombo.connect(self.DatcomCARD.on_Singal_RuleIndexToCombo)
        self.Singal_CheckboxStateChanged.connect(self.DatcomCARD.on_Singal_CheckboxStateChanged)
        #self.Singal_TbLength_editingFinished.connect(self.DatcomCARD.on_Singal_TbLength_editingFinished)
        self.Singal_TbLength_editingFinished.connect(self.on_Singal_TbLength_editingFinished)
        self.Singal_TBRowCountChanged.connect(self.on_Singal_TBRowCountChanged)
      
        self.Singal_CommonUIChanged.connect(self.DatcomCARD.on_Singal_CommonUIChanged)
        self.Singal_RuleVariableStatus.connect(self.DatcomCARD.on_Singal_RuleVariableStatus)   
        
        #界面参数
        self.curPos = QPoint(0, 0)
        self.curWidget = None
        self.curN = None
        self.popMenu = None
        
        #再次执行绑定
        QMetaObject.connectSlotsByName(self)
        
        #刷新界面
        self.UILogic()  
        
    @pyqtSlot(str, str, int) 
    def on_Singal_TbLength_editingFinished(self, sName, rName, tNum):
        """
        响应表格长度控制变量的改变
        """            
        tTWidget = self.findChild(QTableWidget, rName)
        if tTWidget:
            tTWidget.Singal_RuleNumToCount.emit(tNum)

    @pyqtSlot(int, str)
    def on_Singal_TBRowCountChanged(self, tNum, rName):
        """
        响应表格行数变化
        """
        tCWidget = self.findChild(QLineEdit, rName)
        if  tCWidget : tCWidget.setText(str(tNum))
        
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
        
    

    
if __name__ == "__main__":
    import sys
    from PyQt5.QtWidgets import QApplication
    app = QApplication(sys.argv)
    card = DatcomWidgetBase(tCARD = 'FLTCON')    
    #ui = DatcomBaseUI()
    #ui.setupUi(card)
    card.show()
    sys.exit(app.exec_())
    
