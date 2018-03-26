# -*- coding: utf-8 -*-

"""
Module implementing DatcomInputSingle.
"""
from PyQt5 import QtCore,  QtWidgets 
from PyQt5.QtCore import pyqtSlot,  pyqtSignal
from PyQt5.QtWidgets import QWidget

from PyDatcomLab.Core.DictionaryLoader import  defaultDatcomDefinition as DDefine 
import logging
#from collections import OrderedDict

class DatcomInputComboChooser(QWidget):
    """
    Class documentation goes here.
    """
    varComboChanged = pyqtSignal(str, str)      #将变量组合结构发送出去 <self.vUrl,Howto-ChosedKey>
    def __init__(self, namelist, VarName,  parent=None, DDefinition = DDefine ):
        """
        Constructor
        
        @param parent reference to the parent widget
        @type QWidget
        """
        super(DatcomInputComboChooser, self).__init__( parent = parent)
        
        #创建日志
        self.logger = logging.getLogger(r'Datcomlogger')        
        #配置分析
        if DDefinition is  None or namelist is None or VarName is None:
            self.logger.error("没有有效的配置文件，无法初始化")
            return
        
        self.Namelist       = namelist
        self.VarName        = VarName
        self.vUrl           = "%s/%s"%(self.Namelist, self.VarName)
        self.ruleDefine     = DDefinition.getRuleIndexToComboByComboVariable( self.Namelist, self.VarName)
        if self.ruleDefine is None or \
                self.ruleDefine == {} or\
                'Group' not in self.ruleDefine.keys() or\
                'Index' not in self.ruleDefine.keys() or\
                'HowTo' not in self.ruleDefine.keys():
            self.logger.error("无法查找到对应的定义Namelist：%s，RuleVar：%s"%(self.Namelist, self.VarName))
            return 
        self.ruleHowtoKeyList   =  list(self.ruleDefine['HowTo'].keys())
        #显示名称
        self.ruleVarDisplayName = self.ruleDefine['DisplayName'] if 'DisplayName' in self.ruleDefine.keys() else self.VarName
        self.ruleVarTooltips    = self.ruleDefine['Tooltips'] if 'Tooltips' in  self.ruleDefine.keys() else self.ruleVarDisplayName
        self.ruleGroupName      = self.ruleDefine['Group'] 
        self.objUrl             = '%s/%s'%(self.Namelist,self.ruleGroupName  )
        
        #基本几何尺寸
        self.labelIndent    = 20
        self.baseSize       = [400, 25]
        self.baseSplitterSize = [200, 200]
        self.baseStretchFactor = [1, 1]
        #初始化界面        
        self.setupUi(self)


    def setupUi(self, Form):
        """
        自动界面构造函数
        """
        Form.setObjectName(self.VarName )
        #Form.resize(self.baseSize[0] , self.baseSize [1])
        self.verticalLayout = QtWidgets.QVBoxLayout(Form)
        self.verticalLayout.setContentsMargins(1, 1, 1, 1)
        self.verticalLayout.setSpacing(2)
        self.verticalLayout.setObjectName("TopLayout_%s"%self.VarName)
        
        self.splitter_Top = QtWidgets.QSplitter(Form)
        self.splitter_Top.setOrientation(QtCore.Qt.Horizontal)
        self.splitter_Top.setObjectName("TopSplitter_%s"%self.VarName)
        

        #添加Label或者checkBox
        self.LabelItem = QtWidgets.QLabel(self.splitter_Top)
        self.LabelItem.setObjectName("label_Var_%s"%self.VarName)
        #给Label赋值
        self.LabelItem.setText(self.ruleVarDisplayName)  
        self.LabelItem.setIndent(self.labelIndent)
        #调节
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.LabelItem.sizePolicy().hasHeightForWidth())
        self.LabelItem.setSizePolicy(sizePolicy)

        #创建Combo对象
        self.ruleComboWidget = QtWidgets.QComboBox(self.splitter_Top)
        self.ruleComboWidget.setObjectName("comboBox_Variables_%s"%self.VarName)

        #如果存在DisplayRange，优先添加说明信息
        for iR in self.ruleHowtoKeyList:
             self.ruleComboWidget.addItem('-'.join(self.ruleDefine['HowTo'][iR]))  
        #设置风格
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.ruleComboWidget.sizePolicy().hasHeightForWidth())
        self.ruleComboWidget.setSizePolicy(sizePolicy)
        #设置连接关系
        self.ruleComboWidget.currentIndexChanged.connect(self.on_comboBox_Variables_currentIndexChanged)

        #添加分裂器
        self.verticalLayout.addWidget(self.splitter_Top)   
        
        #执行分裂期附加配置
        self.splitter_Top.setStretchFactor(0, self.baseStretchFactor[0])
        self.splitter_Top.setStretchFactor(1, self.baseStretchFactor[1])
        #self.splitter_Top.setSizes(self.baseSplitterSize)
        
        #添加单位
        self.retranslateUi(Form)
        
        QtCore.QMetaObject.connectSlotsByName(Form)

    def retranslateUi(self, Form):
        _translate = QtCore.QCoreApplication.translate
        Form.setWindowTitle(_translate("Form", self.ruleVarDisplayName))
        self.LabelItem.setText(_translate("Form", self.ruleVarDisplayName))
        self.ruleComboWidget.setToolTip(_translate("Form", self.ruleVarTooltips))
        
    def currentIndex(self):
        """
        获得当前的选中项
        """
        return self.ruleComboWidget.currentIndex()
    def setCurrentIndex(self, index):
        """
        """
        self.ruleComboWidget.setCurrentIndex(index)
        #self.varComboChanged.emit(self.CARDName, self.ruleGroupName, index)

    def dt_setStretchFactor(self, tIndex, factor = 0):
        """
        设置中央分割器的比例关系
        """
        self.splitter_Top.setStretchFactor(tIndex, factor)
        
    def dt_setSizes(self, tLift, tRight):
        """
        设置中央分割器的比例关系
        tLift  左侧宽度
        tRight 右侧宽度
        """ 
        self.splitter_Top.setSizes([tLift, tRight])

    def resizeEvent(self, event):
        """
        """
        all = sum(self.baseStretchFactor)
        if all != 0:
            tStretchSize = []
            for iS in self.baseStretchFactor:
                tStretchSize.append(self.width() * iS/ all)
            self.splitter_Top.setSizes(tStretchSize)        
  
    @pyqtSlot(int)
    def on_comboBox_Variables_currentIndexChanged(self, index):
        """
        响应量纲变化.
        
        @param index Combo控件中的索引
        @type int
        #发出的信号应该包含 self.vUrl  key 两部分
        """
        self.varComboChanged.emit(self.vUrl, self.ruleHowtoKeyList[index])
    
    def sendCurrentIndex(self): 
        """
        发送当前的选择，用于同步控件逻辑
        """
        self.varComboChanged.emit(self.vUrl, self.ruleHowtoKeyList[self.currentIndex()])
        
    @pyqtSlot(str, str)
    def on_Singal_varComboChangedFromTable(self, vUrl, tCombo):
        """
        响应表格发来的变量组合关系的变化，此函数应该在加载外部数据文件时被激活，以形成正确选择变量组
        @param vUrl 发来的
        @type str        
        """
        if vUrl != self.objUrl or tCombo is None or tCombo == '':
            return 
        #解析符合要求的
        tComboList = None
        try:
            tComboList = eval(tCombo)
            if tComboList is  None or not type(tComboList) is list:
                return
        except :
            self.logger.error("解析变量组合出错！%s"%tCombo)
        #比对该组合属于哪一个分组
        tIndex = -1
        for iR in self.ruleHowtoKeyList:
            tListI = self.ruleDefine['HowTo'][iR]
            tId = dict(zip(tListI, tListI))
            tOd = dict(zip(tComboList, tComboList))
            if tId == tOd:
                tIndex = self.ruleHowtoKeyList.index(iR)
                break
        if tIndex > -1:
            self.ruleComboWidget.setCurrentIndex(tIndex)
            
                

        
if __name__ == "__main__":
    import sys
    app = QtWidgets.QApplication(sys.argv)
    tMain = QtWidgets.QWidget()  
    LiftLayout = QtWidgets.QVBoxLayout()
    tMain.setLayout(LiftLayout)
    tC = DatcomInputComboChooser( 'FLTCON', 'Variables',  parent=tMain, DDefinition = DDefine )
    LiftLayout.addWidget(tC) 
    tC.varComboChanged.connect(lambda nm, gn, index: print("%s-%s-%d"%(nm, gn, index)) )

    tMain.show()
    sys.exit(app.exec_())
