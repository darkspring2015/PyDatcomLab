# -*- coding: utf-8 -*-

"""
Module implementing DatcomInputSingle.
"""
from PyQt5 import QtCore,  QtWidgets , QtGui
from PyQt5.QtCore import pyqtSlot, Qt, pyqtSignal
from PyQt5.QtWidgets import QWidget

from PyDatcomLab.Core.DictionaryLoader import  defaultDatcomDefinition as DDefine 
from PyDatcomLab.Core.datcomDimension import Dimension, unitTransformation
import logging
import math

class DatcomInputSingle(QWidget):
    """
    Class documentation goes here.
    """
    editingFinished = pyqtSignal(str ,str)      #将编辑结构发送出去
    def __init__(self, CARD, VarName,  parent=None, DDefinition = DDefine ):
        """
        Constructor
        
        @param parent reference to the parent widget
        @type QWidget
        """
        super(DatcomInputSingle, self).__init__( parent = parent)
        
        #创建日志
        self.logger = logging.getLogger(r'Datcomlogger')        
        #配置分析
        if DDefinition is  None or CARD is None or VarName is None:
            self.logger.error("没有有效的配置文件，无法初始化")
            return
        
        self.CARDName       = CARD
        self.VarName        = VarName
        self.vUrl           = "%s/%s"%(self.CARDName, self.VarName)
        self.VarDefine      = DDefinition.getVariableDefineByName( self.CARDName, self.VarName)            
        self.VarDisplayName = self.VarDefine['DisplayName'] if 'DisplayName' in self.VarDefine.keys() else self.VarName
        self.VarTooltips    = self.VarDefine['Tooltips'] if 'Tooltips' in  self.VarDefine.keys() else self.VarDisplayName
        self.vDimension     = self.VarDefine['Dimension'] if 'Dimension' in  self.VarDefine.keys() else ''
        self.vCurrentUnit   = ''  #保存当前的单位
        self.labelIndent    = 20
        self.baseSize       = [400, 25]
        self.baseSplitterSize = [200, 200]
        self.baseStretchFactor = [1, 1]
        #
        self.vFloatFormat   = '%.f'
        if self.VarDefine['TYPE'] not in ['INT', 'REAL'] : 
            self.logger.error('尝试创建的%s变量不是INT或者REAL类型'%self.vUrl )
        #规划界面
        self.setupUi(self)
        self.InitializeUILogic()

    def setupUi(self, Form):
        """
        配置界面元素
        """
        
        Form.setObjectName(self.VarName )
        #Form.resize(self.baseSize[0] , self.baseSize [1])
        self.verticalLayout = QtWidgets.QVBoxLayout(Form)
        self.verticalLayout.setContentsMargins(1, 1, 1, 1)
        self.verticalLayout.setSpacing(2)
        self.verticalLayout.setObjectName("TopLayout")
        
        self.splitter_Top = QtWidgets.QSplitter(Form)
        self.splitter_Top.setOrientation(QtCore.Qt.Horizontal)
        self.splitter_Top.setObjectName("TopSplitter")
        #添加Label或者checkBox
        #self.LabelItem = None
        if 'MustInput' in self.VarDefine.keys() and self.VarDefine['MustInput' ] in ['UnChecked', 'Checked'] :
            #存在可选项
            self.LabelItem = QtWidgets.QCheckBox(self.splitter_Top)
            self.LabelItem.setObjectName("checkBox_Var")
            #绑定值变换信号到自身信号 先绑定以响应对应的状态确认
            self.LabelItem.stateChanged.connect(self.on_checkBox_Var_stateChanged) 
            if self.VarDefine['MustInput' ] == 'UnChecked':
                self.LabelItem.setCheckState(Qt.Unchecked)
            elif  self.VarDefine['MustInput' ] == 'Checked':
                self.LabelItem.setCheckState(Qt.Checked)
            else:
                self.LabelItem.setCheckState(Qt.Unchecked)
           
        else: #没有选项卡
            self.LabelItem = QtWidgets.QLabel(self.splitter_Top)
            self.LabelItem.setObjectName("label_Var")
            self.LabelItem.setIndent(self.labelIndent )
        #给Label赋值
        if 'DisplayName' in self.VarDefine.keys():
            self.LabelItem.setText(self.VarDefine['DisplayName'])
        else:
            self.LabelItem.setText(self.VarName)  

        #创建右半部分的结构
        self.splitter_Inner = QtWidgets.QSplitter(self.splitter_Top)
        self.splitter_Inner.setOrientation(QtCore.Qt.Horizontal)
        self.splitter_Inner.setObjectName("InnerSplitter")
        #添加录入框
        self.InputWidget = QtWidgets.QLineEdit(self.splitter_Inner)
        self.InputWidget.setObjectName('InputWidget') #设置输入组件的名称为变量名
        #调节录入框的策略
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.InputWidget.sizePolicy().hasHeightForWidth())
        self.InputWidget.setSizePolicy(sizePolicy)
        
        #增加对Array单值的支持
        tType = ""
        if self.VarDefine['TYPE'] in ['REAL','INT' ] :
            tType = self.VarDefine['TYPE']
        elif self.VarDefine['TYPE']  == 'Array' :
            if 'SubType' in self.VarDefine.keys():
                tType = self.VarDefine['SubType']
            else:
                tType = 'REAL'
        else:
            self.logger.error("尝试使用DatcomInputSingle来展示非物理量！%s-%s"%(self.vUrl, self.VarDefine['TYPE']))
                    
            
            

        #添加验证器
        if tType == 'REAL':
            #给控件设置属性
            if self.InputWidget is None:
                self.logger.error("访问的变量：%s 不在本窗体"%self.VarName)
            else:
                tVd = dtQDoubleValidator()
                tVd.setObjectName('DoubleValidator')
                #tVd.setNotation(QtGui.QDoubleValidator.StandardNotation) #否则无法限制
                if 'Range' in self.VarDefine.keys():
                    tRange = self.VarDefine['Range']
                    if  self.isNotNanInf(tRange[0]) :
                        tVd.setBottom(tRange[0])                        
                    if  self.isNotNanInf(tRange[1]):
                        tVd.setTop(tRange[1])

                    if 'Decimals' in self.VarDefine.keys()and \
                        QtGui.QIntValidator().validate(self.VarDefine['Decimals'], 0)[0] == QtGui.QValidator.Acceptable:
                            tVd.setDecimals(int(self.VarDefine['Decimals']))  
                            self.vFloatFormat = '%%.%df'%tVd.decimals()
                            tFStr  = '%s - %s'%(self.vFloatFormat, self.vFloatFormat)
                            self.InputWidget.setPlaceholderText(tFStr%(tVd.bottom(), tVd.top()))
                    else:
                        self.InputWidget.setPlaceholderText('%f-%f'%(tVd.bottom(), tVd.top()))
                self.InputWidget.setValidator(tVd)  
        #给INT类型绑定验证器        
        elif tType == 'INT':
            #给控件设置属性
            if self.InputWidget is None:
                self.logger.error("访问的变量：%s 不在本窗体"%self.VarName)
            else:
                tVd =  QtGui.QIntValidator()
                tVd.setObjectName('IntValidator')
                tVd.setBottom(0)
                if 'Range' in self.VarDefine.keys():
                    tRange = self.VarDefine['Range']
                    tVd.setRange(tRange[0], tRange[1])
                    self.InputWidget.setPlaceholderText('%d - %d'%(tVd.bottom(), tVd.top()))
                self.InputWidget.setValidator(tVd) 
        #调整默认策略
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.InputWidget.sizePolicy().hasHeightForWidth())
        self.InputWidget.setSizePolicy(sizePolicy)                 

        #添加单位
        if 'Dimension' in self.VarDefine.keys() and self.VarDefine['Dimension'] not in ['']:
            self.comboBox_VarUnit = QtWidgets.QComboBox(self.splitter_Inner)
            self.comboBox_VarUnit.setObjectName("comboBox_Unit")
            if self.VarDefine['Dimension'] in Dimension.keys():
                for itUnit in Dimension[self.VarDefine['Dimension']]:
                    self.comboBox_VarUnit.addItem(itUnit)
            else:
                self.logger.error("尝试添加不存在的量纲%s"%(self.VarDefine['Dimension']))
            #选择默认单位
            if self.comboBox_VarUnit.count() >0: self.comboBox_VarUnit.setCurrentIndex(0)    
            #调整默认策略
            sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
            sizePolicy.setHorizontalStretch(0)
            sizePolicy.setVerticalStretch(0)
            sizePolicy.setHeightForWidth(self.comboBox_VarUnit.sizePolicy().hasHeightForWidth())
            self.comboBox_VarUnit.setSizePolicy(sizePolicy)    
        
        #添加分裂器
        self.verticalLayout.addWidget(self.splitter_Top)   
        
        #执行分裂期附加配置
        self.splitter_Top.setStretchFactor(0, self.baseStretchFactor[0])
        self.splitter_Top.setStretchFactor(1, self.baseStretchFactor[1])
        #self.splitter_Top.setSizes(self.baseSplitterSize)

        #执行其他逻辑
        self.retranslateUi(Form)        
        QtCore.QMetaObject.connectSlotsByName(Form)
    
    def resizeEvent(self, event):
        """
        """
        all = sum(self.baseStretchFactor)
        if all != 0:
            tStretchSize = []
            for iS in self.baseStretchFactor:
                tStretchSize.append(self.width() * iS/ all)
            self.splitter_Top.setSizes(tStretchSize)

        #super(QWidget, self).resizeEvent(event)
        
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

    def retranslateUi(self, Form):
        _translate = QtCore.QCoreApplication.translate
        Form.setWindowTitle(_translate("Form", self.VarDisplayName))
        self.LabelItem.setText(_translate("Form", self.VarDisplayName))
        self.InputWidget.setToolTip(_translate("Form", self.VarTooltips))
        tUnitWidget = self.findChild(QtWidgets.QComboBox,'comboBox_Unit')
        if tUnitWidget :  tUnitWidget.setToolTip(_translate("Form", self.vDimension))
    
    def isNotNanInf(self, tF):
        """
        """
        if tF is not  None and  type(tF) in [float, int]  and \
        not math.isinf(tF) and not math.isnan(tF):
            return True
        return False
        
    def setDelegateData(self, tData):
        """
        直接设置数据值，将在作为Delegate是被使用
        """
        tVd = self.InputWidget.validator()
        if tVd is None:
            self.InputWidget.setText(str(tData))
        else:
            if tVd.validate(str(tData), 0)[0] == QtGui.QValidator.Acceptable:
                self.InputWidget.setText(str(tData))
            else:
                self.logger.error("传入数据无法通过验证：%s：%s"%(self.vUrl, str(tData)))

    def getDelegateData(self):
        """
        返回编辑控件当前的值，将在作为Delegate是被使用
        """
        return self.InputWidget.text()
                

    def setData(self, dtModel):
        """
        设置控件的值
        """
        if dtModel is None : return         
        if self.VarDefine['TYPE'] not in ['INT', 'REAL']:
            self.logger.error('尝试创建的%s变量不是INT或者REAL类型'%self.vUrl )
            return 
            
        tVar = dtModel.getContinuousVariableValueByName(self.CARDName, self.VarName)
        #设置单位           
        if 'Unit' in tVar.keys() and tVar['Unit'] not in ['']:
            tIndex = self.comboBox_VarUnit.findText(tVar['Unit'])
            if tIndex >=0 : #匹配的准确性
                self.comboBox_VarUnit.setCurrentIndex(tIndex)
                self.comboBox_VarUnit.setEnabled(True)
            else:
                self.comboBox_VarUnit.setEnabled(False)
                self.logger.error("传递的参数具有错误的量纲信息：%s，U:%s"%(tVar['Dimension'], tVar['Unit']))
        #设置值
        if 'Value' in tVar.keys() and tVar['Value'] is not None:        
            try:
                if self.VarDefine['TYPE'] == 'INT':
                    tV = int(float(tVar['Value']))
                elif self.VarDefine['TYPE'] == 'REAL':
                    tV = float(tVar['Value'])
                else:
                    tV = tVar['Value']
            except Exception as e:
                self.logger.error("传递的参数转换为字符串过程出错：！%s"%e)
            finally:
                self.InputWidget.setText(str(tV))
        else:
            self.logger.error("传递的参数对应的控件信息异常：！")

                
    
    def getData(self, dtModel):
        """
        将控件的值写入到模型中
        """
        tV = {'Dimension':self.vDimension, 'Value':'' , 'Unit':self.vCurrentUnit }
        if self.InputWidget.isEnabled():
            tV = {'Dimension':self.vDimension, 'Value':float(self.InputWidget.text()) , 'Unit':self.vCurrentUnit }            
        else:
            tV = {'Dimension':self.vDimension, 'Value':None , 'Unit':self.vCurrentUnit }
        #写入到响应的数据中
        dtModel.setContinuousVariableValueByName(self.CARDName, self.VarName, tV)  
        
    @pyqtSlot(str, int)  #标示和值
    def on_Signal_rowCountChanged(self, vUrl, vCount):
        """
        将控件的显示值重置为vText
        """
        if self.vUrl == vUrl and type(self.InputWidget) is QtWidgets.QLineEdit:
            self.InputWidget.setText(str(vCount))
            
        

    def InitializeUILogic(self):
        """
        执行基本的UI逻辑同步
        """
        #初始化界面默认值
        #tVarDefine = self.VarDefine
        tCklabel = self.findChild(QtWidgets.QCheckBox,'checkBox_Var')
        if tCklabel :
            tUnitWidget = self.findChild(QtWidgets.QComboBox,'comboBox_Unit')
            if tCklabel.checkState() == Qt.Unchecked:
                self.InputWidget.setEnabled(False)
                if tUnitWidget: tUnitWidget.setEnabled(False)                
            elif  tCklabel.checkState() == Qt.Checked:
                self.InputWidget.setEnabled(True)
                if tUnitWidget: tUnitWidget.setEnabled(True)
            else:
                self.InputWidget.setEnabled(False)
                if tUnitWidget: tUnitWidget.setEnabled(False)
                

    
    @pyqtSlot(int)
    def on_checkBox_Var_stateChanged(self, p0):
        """
        Slot documentation goes here.
        
        @param p0 DESCRIPTION
        @type int
        """
        tUnitWidget = self.findChild(QtWidgets.QComboBox,'comboBox_Unit')
        if p0 == Qt.Checked:
            self.InputWidget.setEnabled(True)
            if tUnitWidget: tUnitWidget.setEnabled(True)
        else:
            self.InputWidget.setEnabled(False)
            if tUnitWidget: tUnitWidget.setEnabled(False)            
                

    
    @pyqtSlot()
    def on_InputWidget_editingFinished(self):
        """
        转发输入控件的编辑结果.
        """
        self.editingFinished.emit(self.vUrl , self.InputWidget.text())
        

    
    @pyqtSlot(int)
    def on_comboBox_Unit_currentIndexChanged(self, index):
        """
        响应量纲变化.
        
        @param index 单位的索引
        @type int
        """

        tUint = self.comboBox_VarUnit.currentText()
        tValue = None if self.InputWidget.text() == '' else float(self.InputWidget.text())
        tVar = {'Dimension':self.vDimension, 'Value':tValue , 'Unit':self.vCurrentUnit }
        tRVar = unitTransformation(tVar, tUint)
        if tRVar['Value'] is  None :
            self.InputWidget.setText("")
        elif  not (math.isinf(tRVar['Value']) or math.isnan(tRVar['Value']))  :
            self.InputWidget.setText(self.vFloatFormat%(tRVar['Value']))
            
    @pyqtSlot(str)
    def on_DoubleValidator_fixuping(self, tInput):       
        """
        实现更正函数，确保实现效果符合要求
        """
        tF = self.sender().bottom() 
        try:
            tF = float(tInput)
            if tF > self.sender().top():     tInput = self.vFloatFormat%(self.sender().top())
            if tF < self.sender().bottom() : tInput = self.vFloatFormat%(self.sender().bottom())
        except Exception as  e:
            #tInput = str((self.sender().top() +self.sender().bottom())/2 )
            tInput = ''
        finally:
            self.InputWidget.setText(tInput)
            


class dtQDoubleValidator(QtGui.QDoubleValidator):   
    """
    自定义拓展的验证器，具备修正功能
    """    
    fixuping = pyqtSignal(str )      #将编辑结构发送出去        
    def fixup(self, tInput):
        """
        """
        self.fixuping.emit(tInput)




class DatcomInputSingleNoLabel(DatcomInputSingle):
    """
    没有Label栏的输入框
    """
    def __init__(self, CARD, VarName,  parent=None, DDefinition = DDefine, isRemoveSpacer = True):
        """
        """
        super(DatcomInputSingleNoLabel, self).__init__(CARD, VarName,  parent, DDefinition)
        #删除对应的控件
        tLabel = self.findChild(QtWidgets.QCheckBox, 'checkBox_Var')
        tLabel2 = self.findChild(QtWidgets.QLabel, 'label_Var')
        if tLabel is not None :
            tLabel.setCheckState(Qt.Checked)
            tLabel.deleteLater()
        elif tLabel2 is not None:
            tLabel2.deleteLater()  
        else:
            self.logger.error("并不存在对应的控件")
        

        
if __name__ == "__main__":
    import sys
    app = QtWidgets.QApplication(sys.argv)
    tMain = QtWidgets.QWidget()  
    LiftLayout = QtWidgets.QVBoxLayout()
    #LiftLayout.setContentsMargins(5, 0, 0, 5)
    tMain.setLayout(LiftLayout)

    LiftLayout.addWidget(DatcomInputSingle( 'FLTCON', 'TSMACH',  parent=tMain, DDefinition = DDefine )) 
    LiftLayout.addWidget(DatcomInputSingleNoLabel( 'FLTCON', 'TSMACH',  parent=tMain, DDefinition = DDefine )) 
    LiftLayout.addWidget(DatcomInputSingle( 'FLTCON', 'HYPERS',  parent=tMain, DDefinition = DDefine ))  
    LiftLayout.addWidget(DatcomInputSingleNoLabel( 'FLTCON', 'HYPERS',  parent=tMain, DDefinition = DDefine ))  
    LiftLayout.addWidget(DatcomInputSingle( 'FLTCON', 'WT'    ,  parent=tMain, DDefinition = DDefine )) 
    LiftLayout.addWidget(DatcomInputSingleNoLabel( 'FLTCON', 'WT'    ,  parent=tMain, DDefinition = DDefine )) 
    LiftLayout.addWidget(DatcomInputSingle( 'FLTCON', 'NALPHA',  parent=tMain, DDefinition = DDefine )) 
    LiftLayout.addWidget(DatcomInputSingleNoLabel( 'FLTCON', 'NALPHA',  parent=tMain, DDefinition = DDefine , isRemoveSpacer = False)) 

    tMain.show()
    sys.exit(app.exec_())
