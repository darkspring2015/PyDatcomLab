# -*- coding: utf-8 -*-

"""
Module implementing DatcomInputSingle.
"""
from PyQt5 import QtCore,  QtWidgets , QtGui
from PyQt5.QtCore import pyqtSlot, Qt, pyqtSignal
from PyQt5.QtWidgets import QWidget

from PyDatcomLab.Core.DictionaryLoader import  defaultDatcomDefinition as DDefine 
from PyDatcomLab.Core.datcomDimension import Dimension, unitTransformation
from PyDatcomLab.Core import datcomDimension as dtDimension
import logging
import math

class DatcomInputSingle(QWidget):
    """
    Class documentation goes here.
    """
    editingFinished = pyqtSignal(str ,str)      #将编辑结构发送出去
    def __init__(self, iUrl, parent=None, iDefinition = DDefine ):
        """
        Constructor
        DatcomInputSingle是一个QWidget控件，用来输出和显示一个REAL、INT类型的值
        @param parent reference to the parent widget
        @type QWidget
        @param iUrl 是需要显示的变量的Url限定符 NAMELIST/VARIABLE
        @type str
        @param iDefinition reference to DTdictionary的默认实例defaultDatcomDefinition
        @type DTdictionary
        """
        #创建父类初始化
        super(DatcomInputSingle, self).__init__( parent = parent)
        
        #创建日志
        self.logger = logging.getLogger(r'Datcomlogger')        
        #配置分析
        
        if iDefinition is  None or iUrl is None:
            self.logger.error("无效的配置，无法初始化！ DTdictionary ：%s；Url：%s"%(str(iDefinition), str(iUrl)))
            return
        self.dtDefine       = iDefinition   #设置Datcom配置文件
        self.vUrl             = iUrl
        #获得变量定义
        self.VarDefine     = self.dtDefine.checkUrl(self.vUrl )  
        if self.VarDefine is None :
            self.logger.error("无法处理不存在的定义，URL：%s"%iUrl)
            return
        #分析其他的附加信息
        self.namelist , self.VarName    =    iUrl.split('/')[-2:]                 
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
        if  not self.isValidateType(): 
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
            self.LabelItem.setObjectName("checkBox_Var%s"%self.VarName)
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
            self.LabelItem.setObjectName("label_Var%s"%self.VarName)
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
        self.InputWidget.setObjectName('InputWidget%s'%self.VarName) #设置输入组件的名称为变量名
        self.InputWidget.editingFinished.connect(self.on_InputWidget_editingFinished)
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
            self.comboBox_VarUnit.setObjectName("comboBox_Unit_%s"%self.VarName)
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
            #在此手动连接slot
            self.comboBox_VarUnit.currentIndexChanged.connect(self.on_my_currentIndexChanged)
        
        #添加分裂器
        self.verticalLayout.addWidget(self.splitter_Top)   
        
        #执行分裂期附加配置
        self.splitter_Top.setStretchFactor(0, self.baseStretchFactor[0])
        self.splitter_Top.setStretchFactor(1, self.baseStretchFactor[1])
        #self.splitter_Top.setSizes(self.baseSplitterSize)

        #执行其他逻辑
        self.retranslateUi(Form)        
        QtCore.QMetaObject.connectSlotsByName(Form)
        
    def isValidateType(self):
        """
        验证是否有效的变量类型
        """
        tReslut = False
        if self.VarDefine['TYPE']  in ['INT', 'REAL']:
            tReslut = True
        if self.VarDefine['TYPE'] == 'Array' and 'SubType' not in self.VarDefine.keys():
            tReslut = True
        if self.VarDefine['TYPE'] == 'Array' and 'SubType'  in self.VarDefine.keys() and self.VarDefine['SubType']  in ['', 'INT', 'REAL']:
            tReslut = True           
            
        return tReslut
        


    def InitializeUILogic(self):
        """
        执行基本的UI逻辑同步
        """
        #初始化界面默认值
        #tVarDefine = self.VarDefine
        tCklabel      = self.findChild(QtWidgets.QCheckBox,"checkBox_Var%s"%self.VarName)
        tUnitWidget = self.findChild(QtWidgets.QComboBox,"comboBox_Unit_%s"%self.VarName)
        if tCklabel :            
            if tCklabel.checkState() == Qt.Unchecked:
                self.InputWidget.setEnabled(False)
                if tUnitWidget is not None:
                    tUnitWidget.setEnabled(False)                
            elif  tCklabel.checkState() == Qt.Checked:
                self.InputWidget.setEnabled(True)
                if tUnitWidget is not None:
                    tUnitWidget.setEnabled(True)
            else:
                self.InputWidget.setEnabled(False)
                if tUnitWidget is not None:
                    tUnitWidget.setEnabled(False)
        
        #配置变量的缺省值
        #tUnitWidget = self.findChild(QtWidgets.QWidget,"comboBox_Unit_%s"%self.VarName)
        if tUnitWidget is not None :
            self.vCurrentUnit = tUnitWidget.currentText()
            
    
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
        tUnitWidget = self.findChild(QtWidgets.QComboBox,'comboBox_Unit_%s'%self.VarName)
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
        tData 是包含量纲和单位的单值、多值中的一个 etc：{'Dimension': 'L', 'Unit': 'feet', 'Value': 11111.0}
        """
        
        if type(tData) is  dict and 'Value' in  tData.keys():
            tDValue = tData
        else :
            tDValue = {'Dimension': '', 'Unit': '', 'Value': tData}
        #写入到结论中
        self.delegateData = tDValue
        #验证器
        tVd = self.InputWidget.validator()
        if tVd is None:
            self.InputWidget.setText(str(tDValue['Value']))
        else:
            if tVd.validate(str(tDValue['Value']), 0)[0] == QtGui.QValidator.Acceptable:
                self.InputWidget.setText(str(tDValue['Value']))
            else:
                self.logger.error("传入数据无法通过验证：%s：%s,修正为默认值"%(self.vUrl, str(tDValue['Value'])))
                #linger 修正算法
                tVatTmp = self.dtDefine.getVariableTemplateByUrl(self.vUrl, True)
                tDValue['Value'] = tVatTmp['Value']
                
            tUWidget = self.findChild(QWidget, "comboBox_Unit_%s"%self.VarName)
            if tUWidget is not None :
                tIndex = tUWidget.findText(tData['Unit'])
                if tIndex >=0 : tUWidget.setCurrentIndex(tIndex)
     

    def getDelegateData(self):
        """
        返回编辑控件当前的值，将在作为Delegate是被使用
        """
        if self.InputWidget.text() is None or self.InputWidget.text() =="":
            return self.delegateData
            
        
        if self.VarDefine['TYPE'] == 'INT':
            self.delegateData['Value'] = int(self.InputWidget.text())
        elif self.VarDefine['TYPE'] == 'REAL' :
            self.delegateData['Value'] = float(self.InputWidget.text())
        elif self.VarDefine['TYPE'] == 'Array' and ('SubType' not in self.VarDefine.keys() or\
             self.VarDefine['SubType' ] in ['', 'INT', 'REAL']) :
                self.delegateData['Value'] = float(self.InputWidget.text())
        else:
            self.delegateData['Value'] =  self.InputWidget.text()
        tUWidget = self.findChild(QWidget, "comboBox_Unit_%s"%self.VarName)  
        self.delegateData['Unit'] = tUWidget.currentText() if tUWidget is not None else ''
        

        return self.delegateData
     
    def setDataByVariable(self, tVar):
        """
        设置控件的值，
        tVar 是dict型的变量，是dcModel的成员
        """     
        tMust = ['VarName','Namelist', 'Url', 'Unit', 'Value' ]
        if tVar is None or type(tVar) != dict  :
            self.logger.error("设置%s的参数类型不合法"%self.vUrl)
            return 
        for iK in tMust:
            if iK not in tVar.keys():
                self.logger.error("设置%s的参数类型不合法,缺少%s"%(self.vUrl, iK))
                return 
        if tVar['Url'] != self.vUrl:
            self.logger.error("设置%s的参数目标不正确%s"%(self.vUrl, tVar['Url']))
            return 
        tVarTp = self.dtDefine.getVariableTemplateByUrl(self.vUrl)
        #开始执行设置
        if 'Dimension' in tVarTp.keys():
            if tVar['Unit'] not in dtDimension.getUnitListByDimension(tVarTp['Dimension']):
                self.logger.error("设置%s的参数的单位：%s 不合适"%(self.vUrl, tVar['Unit']))
                return 
            else:
                tUWidget = self.findChild(QWidget, "comboBox_Unit_%s"%self.VarName)
                if tUWidget is None : 
                    self.logger.error("控件并未设置单位控件：%s "%(self.vUrl))
                    return 
                tIndex = tUWidget.findText(tVar['Unit'])
                if tIndex <0 : 
                    self.logger.error("设置的%s单位%s,并不在当前单位控件列表中"%(self.vUrl,tVar['Unit'] ))
                    return 
                tUWidget.setCurrentIndex(tIndex)
        #设置值
        if tVar['Value'] is None :
            self.logger.error("传递的变量%s值为None,忽略设置过程"%self.vUrl)
        else:
            self.InputWidget.setText(str(tVar['Value']))
        
    def getDataByVariable(self):
        """
        获得当前控制的值
        如果当前值无法通过验证返回None
        
        """
        tVd = self.InputWidget.validator()
        if tVd is not None  and tVd.validate(self.InputWidget.text()) != Qt.Acceptable:
            self.logger.error("%s录入控件的的当前值：%s，无法通过验证"%(self.vUrl,self.InputWidget.text() ))
            return None
        #获得数据
        tDefault = self.dtDefine.getVariableTemplateByUrl(self.vUrl) #每次都是独立的实例
        tDf      = self.VarDefine
        if not self.InputWidget.isEnabled():
            tDefault.update({'InUsed':'False'})
        else:
            tDefault.update({'InUsed':'True'})
            if  self.InputWidget.text() not in [None, '']:
                try:
                    if tDf['TYPE'] == 'INT':
                        tDefault.update({'Value':int(float(self.InputWidget.text()))})
                    if tDf['TYPE'] == 'REAL':
                        tDefault.update({'Value':float(self.InputWidget.text())})
                except Exception as e:
                    self.logger.error("进行类型转换时失败，text：%s!"%self.InputWidget.text())
        #更新单位
        tUWidget = self.findChild(QWidget, "comboBox_Unit_%s"%self.VarName)
        if tUWidget is not None:
            tDefault.update({'Unit':tUWidget.currentText()})
        
        return tDefault
        
        

    def loadData(self, dtModel):
        """
        设置控件的值，从dtModel中加载对应值
        """
        if dtModel is None : return         
        if not self.isValidateType() :
            self.logger.error('尝试创建的%s变量不是INT或者REAL类型'%self.vUrl )
            return 
            
        tVar = dtModel.getContinuousVariableValueByName(self.vUrl)
        if tVar is None:  #处理tVar不存在与Model中的情况
            tVar = self.dtDefine.getVariableTemplateByUrl(self.vUrl)
            tVar['Value'] = None
            
        #设置单位           
        if 'Unit' in tVar.keys() and tVar['Unit'] not in ['', '/']:
            tIndex = self.comboBox_VarUnit.findText(tVar['Unit'])
            if tIndex >=0 : #匹配的准确性
                self.comboBox_VarUnit.setCurrentIndex(tIndex)
                #self.comboBox_VarUnit.setEnabled(True)
            else:
                #self.comboBox_VarUnit.setEnabled(False)
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
            if 'Default' in  self.VarDefine.keys() and self.VarDefine['Default'] is not None:
                self.InputWidget.setText(str(self.VarDefine['Default']))
                if  self.InputWidget.isEnabled():
                    self.logger.warning("控件loadData(%s)异常：dcModel中没有有效的值，使用默认值：%s修正！"%(self.vUrl , str(self.VarDefine['Default'])))
            else:
                if  self.InputWidget.isEnabled():
                    self.logger.error("控件loadData(%s)异常：dcModel中没有有效的值，且没有默认值！"%self.vUrl )

                
    
    def saveData(self, dtModel):
        """
        将控件的值写入到模型中
        """
        #tV = {'Dimension':self.vDimension, 'Value':'' , 'Unit':self.vCurrentUnit }
        tV = self.dtDefine.getVariableTemplateByUrl(self.vUrl)
        if not self.InputWidget.isEnabled() or self.InputWidget.text()  in [None, '']:
            tV['Value'] = None
        else  :
            try:
                tV.update({'Value':float(self.InputWidget.text())})
            except Exception as e:
                self.logger.error("DatcomInputSingle.saveData() 执行数据识别时异常：%s!"%self.InputWidget.text())
        tUWidget = self.findChild(QWidget, "comboBox_Unit_%s"%self.VarName)
        if tUWidget is not None:
            tV.update({'Unit':self.vCurrentUnit})

        #写入到响应的数据中
        dtModel.setContinuousVariableValueByName(self.vUrl, tV)  
        
    @pyqtSlot(str, int)  #标示和值
    def on_Signal_rowCountChanged(self, vUrl, vCount):
        """
        将控件的显示值重置为vText
        """
        if self.vUrl == vUrl and type(self.InputWidget) is QtWidgets.QLineEdit:
            self.InputWidget.setText(str(vCount))      

    
    @pyqtSlot(int)
    def on_checkBox_Var_stateChanged(self, p0):
        """
        Slot documentation goes here.
        
        @param p0 DESCRIPTION
        @type int
        """
        tUnitWidget = self.findChild(QtWidgets.QComboBox,"comboBox_Unit_%s"%self.VarName)
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
    def on_my_currentIndexChanged(self, index):
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
    def __init__(self, iUrl,  parent=None, iDefinition = DDefine, isRemoveSpacer = True):
        """
        """
        super(DatcomInputSingleNoLabel, self).__init__(iUrl,  parent, iDefinition)
        #删除对应的控件
        tLabel = self.findChild(QtWidgets.QCheckBox, 'checkBox_Var%s'%self.VarName)
        tLabel2 = self.findChild(QtWidgets.QLabel, 'label_Var%s'%self.VarName)
        if tLabel is not None :
            tLabel.setCheckState(Qt.Checked)
            tLabel.deleteLater()
        elif tLabel2 is not None:
            tLabel2.deleteLater()  
        else:
            self.logger.error("并不存在对应的控件")
        

        
if __name__ == "__main__":
    import sys, os
    from PyDatcomLab.Core import  datcomModel as dcModel
    app = QtWidgets.QApplication(sys.argv)
    tMain = QtWidgets.QWidget()  
    LiftLayout = QtWidgets.QVBoxLayout()
    #LiftLayout.setContentsMargins(5, 0, 0, 5)
    tMain.setLayout(LiftLayout)
    tMPath = os.path.join(os.path.expanduser('~'), r'.PyDatcomLab\extras\PyDatcomProjects\1\case2.xml')
    tModel = dcModel.dcModel(tMPath)

    LiftLayout.addWidget(DatcomInputSingle( 'FLTCON/TSMACH',  parent=tMain, iDefinition = DDefine )) 
    LiftLayout.addWidget(DatcomInputSingleNoLabel( 'FLTCON/TSMACH',  parent=tMain, iDefinition = DDefine )) 
    LiftLayout.addWidget(DatcomInputSingle( 'FLTCON/HYPERS',  parent=tMain, iDefinition = DDefine ))  
    LiftLayout.addWidget(DatcomInputSingleNoLabel( 'FLTCON/HYPERS',  parent=tMain, iDefinition = DDefine ))  
    LiftLayout.addWidget(DatcomInputSingle( 'FLTCON/WT'    ,  parent=tMain, iDefinition = DDefine )) 
    LiftLayout.addWidget(DatcomInputSingle( 'FLTCON/WT'    ,  parent=tMain, iDefinition = DDefine )) 
    LiftLayout.addWidget(DatcomInputSingle( 'FLTCON/WT'    ,  parent=tMain, iDefinition = DDefine )) 
    LiftLayout.addWidget(DatcomInputSingle( 'FLTCON/WT'    ,  parent=tMain, iDefinition = DDefine )) 
    LiftLayout.addWidget(DatcomInputSingleNoLabel( 'FLTCON/WT'    ,  parent=tMain, iDefinition = DDefine )) 
    LiftLayout.addWidget(DatcomInputSingle( 'FLTCON/NALPHA',  parent=tMain, iDefinition = DDefine )) 
    LiftLayout.addWidget(DatcomInputSingleNoLabel( 'FLTCON/NALPHA',  parent=tMain, iDefinition = DDefine , isRemoveSpacer = False)) 

    tMain.show()
    sys.exit(app.exec_())
