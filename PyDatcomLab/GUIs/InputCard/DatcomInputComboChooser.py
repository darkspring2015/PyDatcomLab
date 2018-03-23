# -*- coding: utf-8 -*-

"""
Module implementing DatcomInputSingle.
"""
from PyQt5 import QtCore,  QtWidgets 
from PyQt5.QtCore import pyqtSlot,  pyqtSignal
from PyQt5.QtWidgets import QWidget

from PyDatcomLab.Core.DictionaryLoader import  defaultDatcomDefinition as DDefine 
import logging


class DatcomInputComboChooser(QWidget):
    """
    Class documentation goes here.
    """
    varComboChanged = pyqtSignal(str ,str, int)      #将变量组合结构发送出去 Namelist Group Index
    def __init__(self, CARD, VarName,  parent=None, DDefinition = DDefine ):
        """
        Constructor
        
        @param parent reference to the parent widget
        @type QWidget
        """
        super(DatcomInputComboChooser, self).__init__( parent = parent)
        
        #创建日志
        self.logger = logging.getLogger(r'Datcomlogger')        
        #配置分析
        if DDefinition is  None or CARD is None or VarName is None:
            self.logger.error("没有有效的配置文件，无法初始化")
            return
        
        self.CARDName       = CARD
        self.VarName        = VarName
        self.vUrl           = "%s/%s"%(self.CARDName, self.VarName)
        self.ruleDefine      = DDefinition.getRuleIndexToComboByComboVariable( self.CARDName, self.VarName)
        if self.ruleDefine is None or \
                self.ruleDefine == {} or\
                'Group' not in self.ruleDefine.keys() or\
                'Index' not in self.ruleDefine.keys() or\
                'HowTo' not in self.ruleDefine.keys():
            self.logger.error("无法查找到对应的定义Namelist：%s，RuleVar：%s"%(self.CARDName, self.VarName))
            return 
        #显示名称
        self.ruleVarDisplayName = self.ruleDefine['DisplayName'] if 'DisplayName' in self.ruleDefine.keys() else self.VarName
        self.ruleVarTooltips    = self.ruleDefine['Tooltips'] if 'Tooltips' in  self.ruleDefine.keys() else self.ruleVarDisplayName
        self.ruleGroupName      = self.ruleDefine['Group'] 
        self.setupUi(self)


    def setupUi(self, Form):
        Form.setObjectName(self.VarName)       

        self.horizontalLayout = QtWidgets.QHBoxLayout(Form)
        self.horizontalLayout.setObjectName("horizontalLayout")
        self.horizontalLayout.setContentsMargins(0, 0, 0, 0)

        #添加Label或者checkBox
        self.LabelItem = QtWidgets.QLabel(Form)
        self.LabelItem.setObjectName("label_Var")
        #给Label赋值
        self.LabelItem.setText(self.ruleVarDisplayName)  
        #调节
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.LabelItem.sizePolicy().hasHeightForWidth())
        self.LabelItem.setSizePolicy(sizePolicy)
        self.horizontalLayout.addWidget(self.LabelItem)
        #添加分割气
        self.spacerItem = QtWidgets.QSpacerItem(55, 20, QtWidgets.QSizePolicy.Expanding, QtWidgets.QSizePolicy.Minimum)
        #spacerItem.setObjectName('spacerItem')
        self.horizontalLayout.addItem(self.spacerItem)
        #添加录入框
        #self.InputWidget = QtWidgets.QWidget(Form)

        #创建Combo对象
        self.ruleComboWidget = QtWidgets.QComboBox(Form)
        self.ruleComboWidget.setObjectName("comboBox_Variables")

        #如果存在DisplayRange，优先添加说明信息
        for iR in self.ruleDefine['HowTo'].keys():
             self.ruleComboWidget.addItem('-'.join(self.ruleDefine['HowTo'][iR]))  
        self.horizontalLayout.addWidget(self.ruleComboWidget) 
      
        #添加单位
        self.retranslateUi(Form)
        
        QtCore.QMetaObject.connectSlotsByName(Form)

    def retranslateUi(self, Form):
        _translate = QtCore.QCoreApplication.translate
        Form.setWindowTitle(_translate("Form", self.ruleVarDisplayName))
        self.LabelItem.setText(_translate("Form", self.ruleVarDisplayName))
        self.ruleComboWidget.setToolTip(_translate("Form", self.ruleVarTooltips))

  
    @pyqtSlot(int)
    def on_comboBox_Variables_currentIndexChanged(self, index):
        """
        响应量纲变化.
        
        @param index 单位的索引
        @type int
        """
        self.varComboChanged.emit(self.CARDName, self.ruleGroupName, index)




        
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
