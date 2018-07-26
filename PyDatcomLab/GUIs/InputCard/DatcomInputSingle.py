# -*- coding: utf-8 -*-

"""
Module implementing DatcomInputSingle.
"""
from PyQt5 import QtCore,  QtWidgets , QtGui
from PyQt5.QtCore import pyqtSlot, Qt, pyqtSignal
from PyQt5.QtWidgets import QWidget

#from PyDatcomLab.Core.DictionaryLoader import  defaultDatcomDefinition as DDefine 
from PyDatcomLab.Core.DictionaryLoader import   DTdictionary 
from PyDatcomLab.Core.datcomDimension import Dimension, unitTransformation
#from PyDatcomLab.Core import datcomDimension as dtDimension
from PyDatcomLab.Core.datcomModel import dcModel

import logging
import math



class DatcomInputSingle(QWidget):
    """
    Class documentation goes here.
    """
    editingFinished             =  pyqtSignal(str ,str)      #将编辑结构发送出去
    Signal_NMACHChanged  =  pyqtSignal(int)            #发送NMACH变化的结果
    Signal_VariableChanged =  pyqtSignal(str)            #控件对应的datcom变量发生变化时触发 str为变量的iUrl，用于通知其他的相关控件
    
    def __init__(self, iUrl, parent=None, iDefinition = DTdictionary.defaultConfig, iModel =None , isDelegate= False):
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
        #获得变量的基础模板
        self.VarTemplate = self.dtDefine.getVariableTemplateByUrl(self.vUrl)
        #分析其他的附加信息
        self.namelist , self.VarName    =    iUrl.split('/')[-2:]                 
        self.VarDisplayName = self.VarDefine['DisplayName'] if 'DisplayName' in self.VarDefine.keys() else self.VarName
        self.VarTooltips    = self.VarDefine['Tooltips'] if 'Tooltips' in  self.VarDefine.keys() else self.VarDisplayName
        self.setToolTip(self.VarTooltips  )
        self.vDimension     = self.VarDefine['Dimension'] if 'Dimension' in  self.VarDefine.keys() else ''
        self.vCurrentUnit   = ''  #保存当前的单位
        self.labelIndent    = 20
        self.baseSize       = [400, 25]
        self.baseSplitterSize = [200, 200]
        self.baseStretchFactor = [1, 1]
        #数据显示模式
        self.vFloatFormat   = '%.f'
        if  not self.isValidateType(): 
            self.logger.error('尝试创建的%s变量不是INT或者REAL类型'%self.vUrl )
        #是否为通用代理模式
        self.isDelegate = isDelegate
        self.delegateData =  self.dtDefine.getVariableTemplateByUrl(self.vUrl, True)
        #规划界面
        self.setupUi(self)
        self.InitializeUILogic()
        #执行模型加载,如果是代理模式，则不加载模型
        if  self.isDelegate == False:
            if iModel is None or type(iModel) != dcModel:
                #self.logger.warning("初始化DatcomInputSingle时传入的dtModel的类型为%s，应该为%s"%(str(type(iModel)), str(type(dcModel))))
                iModel = dcModel()
                iModel.setVariable(self.VarTemplate)        
            self.dtModel        = iModel       
            #为数据模型调用初始化函数
            self.setModel(self.dtModel)              
        #联结部分的slot
        self.InputWidget.installEventFilter(self)       
        
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
        #self.InputWidget.editingFinished.connect(self.on_InputWidget_editingFinished)
        self.InputWidget.textChanged.connect(self.on_InputWidget_textChanged)
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
            self.comboBox_VarUnit.currentIndexChanged.connect(self.on_Unit_currentIndexChanged)
        
        #添加分裂器
        self.verticalLayout.addWidget(self.splitter_Top)   
        
        #执行分裂期附加配置
        self.splitter_Top.setStretchFactor(0, self.baseStretchFactor[0])
        self.splitter_Top.setStretchFactor(1, self.baseStretchFactor[1])
        #self.splitter_Top.setSizes(self.baseSplitterSize)

        #执行其他逻辑
        self.retranslateUi(Form)        
        QtCore.QMetaObject.connectSlotsByName(Form)           
        
        
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
        判断是否是Nan
        """
        if tF is not  None and  type(tF) in [float, int]  and \
        not math.isinf(tF) and not math.isnan(tF):
            return True
        return False
    
    
    def eventFilter(self, watched, event):
        """
        重载eventFilter(QObject *o, QEvent *e)函数，实现过滤功能，在实现进入时给与 激活focus的功能
        """
        if watched == self.InputWidget:
            if QtCore.QEvent.HoverEnter == event.type():
                 self.setFocus_onWindowActivate()
                 return True
            if QtCore.QEvent.HoverLeave == event.type():
                self.cancelSelection_onWindowDeactivate()
                return True
             
        return False                 


    def setFocus_onWindowActivate(self):
        """
        用以在外部给输入控件设置焦点
        """
        if self.InputWidget is None:return
        #设置焦点
        if self.InputWidget.isEnabled() :
            #捕获焦点，全选，设置光标为最后
            self.InputWidget.setFocus()
            #self.InputWidget.setSelection(0,len(self.InputWidget.text()) )
            self.InputWidget.setCursorPosition(len(self.InputWidget.text()) )
                     
    def cancelSelection_onWindowDeactivate(self):
        """
        当窗口不再激活的时候，设置取消全选
        """
        self.InputWidget.deselect()
        
    def setDelegateData(self, tData):
        """
        直接设置数据值，将在作为Delegate是被使用
        tData 是包含量纲和单位的单值、多值中的一个 etc：{'Dimension': 'L', 'Unit': 'feet', 'Value': 11111.0}
        """
        #激活设置窗口
        self.InputWidget.setEnabled(True)
        self.isDelegate =True
        #执行逻辑
        if type(tData) is  dict and 'Value' in  tData.keys():
            tDValue = tData
        else :
            #使用全局模板，防止数据异常
            tDValue = self.dtDefine.getVariableTemplateByUrl(self.vUrl, True).copy()
            tDValue.update({ 'Value': tData})
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
            #设置单位系统
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

    #下面开始的是控件的读写逻辑部分

    def setModel(self, iModel):
        """
        设置控件的dtModel
        注意：
        1.控件的更改将直接写入到iModel中
        2.函数触发一次加载
        """        
        #为数据模型调用初始化函数
        if iModel is not None and  type(iModel) == dcModel:
            self.dtModel = iModel
            self._loadData()
        else:
            self.logger.warning("DatcomInputSingle.setModel()传入的dtModel的类型为%s，应该为%s"%(str(type(self.dtModel)), str(type(dcModel))))        
    
    def getModel(self):
        """
        返回模型的Model
        不推荐使用该方法        
        """
        return self.dtModel 

    def _loadData(self):
        """
        设置控件的值，从dtModel中加载对应值
        """
        if self.dtModel is None : 
            self.logger.error("dtModel为None！")
            return     
        #tVar = self._getCurrentValueInModel()        
        tVar = self.dtModel.getVariableByUrl(self.vUrl)  #获取模型中的具体数值
        if tVar is None:
            tVar = self.dtDefine.getVariableTemplateByUrl(self.vUrl, isSubType=True)
            tVar.update({"InUsed":'False'})    
            self.on_EnabledStatusChanged(Qt.Unchecked)
        else:
            self.on_EnabledStatusChanged(Qt.Checked)
            #将结果加载到控件
            self._loadDataToWidget(tVar)

                    
    def _loadDataToWidget(self, iVariable):
        """
        根据输入变量iVariable修改界面信息
        @param iVariable 输入的变量值
        @type dict
        """
        #检查输入类型
        if iVariable is None or type(iVariable) != dict  :
            self.logger.error("_loadDataToWidget()设置%s的输入的参数类型不合法：%s !"%(self.vUrl, type(iVariable)))
            return False
        #分析输入合规性
        #tMust = ['VarName','Namelist', 'Url', 'Unit', 'Value' ]
        tMust = [ 'Url', 'Unit', 'Value' ]
        for iK in tMust:
            if iK not in iVariable.keys():
                self.logger.error("_loadDataToWidget()设置%s的输入参数项不足,缺少%s"%(self.vUrl, iK))
                return False
        #检查变量名称
        if iVariable['Url'] != self.vUrl:
            self.logger.error("_loadDataToWidget()输入的变量名为 %s，应该为%s"%(self.vUrl, iVariable['Url']))
            return False
        if not self.isValidateType():
            self.logger.error("_loadDataToWidget()输入的变量%s的datcom类型不被支持"%(self.vUrl))
            
        #开始加载逻辑
        #设置单位           
        if 'Unit' in iVariable.keys() and iVariable['Unit'] not in ['', '/']:
            tUWidget = self.findChild(QWidget, "comboBox_Unit_%s"%self.VarName)
            if tUWidget is not None :
                tIndex = tUWidget.findText(iVariable['Unit'])
                if tIndex >=0 : 
                    tUWidget.setCurrentIndex(tIndex)
                else:
                    self.logger.error("_loadDataToWidget()无法找到对应的单位！")
            else:
                #self.logger.error("输入存在单位项但控件初始化部分没有单位选项！")
                self.logger.error("_loadDataToWidget()传递的参数具有错误的量纲信息：%s，U:%s"%(iVariable['Dimension'], iVariable['Unit']))
   
        #设置值
        if 'Value' in iVariable.keys() and iVariable['Value'] is not None:    
            #检查内容值
            tVd = self.InputWidget.validator()
            if tVd is None:
                self.InputWidget.setText(str(iVariable['Value']))
            else:
                #正常模式下进行分析
                if not self.isDelegate:
                    if tVd.validate(str(iVariable['Value']), 0)[0] == QtGui.QValidator.Acceptable:
                        self.InputWidget.setText(str(iVariable['Value']))
                    else:
                        #没有通过验证的情况，则输入默认值
                        self.logger.error("_loadDataToWidget()传入数据无法通过验证：%s：%s,修正为默认值"%(self.vUrl, str(iVariable['Value'])))
                        #linger 修正算法
                        tVatTmp = self.dtDefine.getVariableTemplateByUrl(self.vUrl, True)
                        iVariable['Value'] = tVatTmp['Value']
                        self.InputWidget.setText(str(iVariable['Value']))
        else:
            #没有有效的值
            if 'Default' in  self.VarDefine.keys() and self.VarDefine['Default'] is not None:
                self.InputWidget.setText(str(self.VarDefine['Default']))
                if  self.InputWidget.isEnabled():
                    self.logger.warning("控件loadData(%s)异常：dcModel中没有有效的值，使用默认值：%s修正！"%(self.vUrl , str(self.VarDefine['Default'])))
            else:
                if  self.InputWidget.isEnabled():
                    #这里的逻辑是一个顽固的bug，因为控件状态是不可靠的！
                    self.logger.warning("控件loadData(%s)异常：dcModel中没有有效的值，且没有默认值！"%self.vUrl )       

        return True
 
    def setDataByVariable(self, iVar):
        """
        设置控件的值  
        iVar 是dict型的变量，是dcModel的子项
        注意事项:
        1. 如果控件设置了dcModel，将触发合理的输入值被保存到dcModel
        """             
        if self._loadDataToWidget(iVar):
            self.dtModel.setVariable(iVar)
        else:
            self.logger.warning("设置控件值得过程出错！")

        
    def getDataByVariable(self):
        """
        获得当前控制的值，负责从外部调用，
        如果当前值无法通过验证返回None
        如果正常，返回Model中的一个具体实例
        返回值为独立的副本
        """
        return self._getDatcomData().copy()


    def _getDatcomData(self):
        """
        分析获取控件的当前输入值，生成一个dcModel的子项（DatcomData）
        """        
        tVd = self.InputWidget.validator()
        if tVd is not None  and tVd.validate(self.InputWidget.text(), 0)[0] == QtGui.QValidator.Invalid:
            self.logger.warning("%s录入控件的的当前值：%s，无法通过验证"%(self.vUrl,self.InputWidget.text() ))
            return None
        #获得数据
        tDefault = self._getCurrentValueInModel().copy()  #获取模型中的具体数值,并且只能是副本
        if not self.InputWidget.isEnabled():
            tDefault.update({'InUsed':'False'})
        else:
            tDefault.update({'InUsed':'True'})
            if  self.InputWidget.text() not in [None, '']:
                try:
                    if  self.VarDefine['TYPE'] == 'INT':
                        tDefault.update({'Value':int(float(self.InputWidget.text()))})
                    if  self.VarDefine['TYPE'] == 'REAL':
                        tDefault.update({'Value':float(self.InputWidget.text())})
                    if self.VarDefine['TYPE'] == 'Array' and ('SubType' not in self.VarDefine.keys() or\
                        self.VarDefine['SubType' ]  in ['', 'INT', 'REAL']) :
                        #tDefault.update({'Value':float(self.InputWidget.text())})
                        self.delegateData['Value'] = float(self.InputWidget.text()) 
                        if not self.isDelegate:
                            self.logger.error("作为Array的Delegate的内容，text：%s!"%self.InputWidget.text())                   
                except Exception as e:
                    self.logger.error("进行类型转换时异常，text：%s! Exception：%s"%(self.InputWidget.text(), e))
        #更新单位
        tUWidget = self.findChild(QWidget, "comboBox_Unit_%s"%self.VarName)
        if tUWidget is not None:
            tDefault.update({'Unit':tUWidget.currentText()})
        
        return tDefault
    
    def _getCurrentValueInModel(self):
        """
        内部函数，从self.dtModel中获得当前值
        如果model中没有当前变量，则使用datcomDefine中的默认值
        返回值是获得变量值
        如果使用模板值，则设置标记位"InUsed"为False
        """
        tV = self.dtModel.getVariableByUrl(self.vUrl)  #获取模型中的具体数值
        if tV is None:
            tV = self.dtDefine.getVariableTemplateByUrl(self.vUrl, isSubType=True)
            tV.update({"InUsed":'False'})        
        return tV
        
    def _UpdateUsedFlags(self, isUsed = 'True' ):
        """
        更新变量的值
        """
        #获取模型值
        tV = self._getCurrentValueInModel()  #获取模型中的具体数值
        #更新标志
        tV.update({'InUsed':isUsed})
        #回写到数据模型
        self.dtModel.setVariable( tV)        

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
        
    #输入刷新逻辑结束

        
    @pyqtSlot()
    def on_InputWidget_editingFinished(self):
        """
        输入控件InputWidget的编辑完成事件.
        用户消息的响应
        """        
        self.emit_InputWidget_text()       


    @pyqtSlot(str)
    def on_InputWidget_textChanged(self, iText):
        """
        输入控件InputWidget的内容改变事件.
        可以程序改变文本值的逻辑
        对于程序更改变量值得情况，应该在 setDataByVariable中进行处理
        """
        if self.InputWidget.hasFocus():return 
        tVd = self.InputWidget.validator()
        if tVd is not None  and tVd.validate(self.InputWidget.text(), 0)[0] == QtGui.QValidator.Acceptable:
            self.emit_InputWidget_text()
    
    
    def emit_InputWidget_text(self):
        """
        发射文本改变信号
        """
        #如果处于代理模式，则取消更新逻辑
        #如果处于Loading模式，则取消更新逻辑
        if self.isDelegate :#or self.isLoading  : 
            return 
        #获得当前控件的输入值
        tVar = self._getDatcomData()       
        tNowVar = self.dtModel.getVariableByUrl(self.vUrl)
        #判断减少不必要的修改和发送  
        if tNowVar is not None and tVar is not None and tNowVar['Value'] == tVar['Value']  and tNowVar['Unit'] == tVar['Unit']:
            #此处认为Unit是‘’而不是None
            return 
        #写入到响应的数据中
        self.dtModel.setVariable(tVar)          
        #发送信号
        self.editingFinished.emit(self.vUrl , self.InputWidget.text())
        self.Signal_VariableChanged.emit(self.vUrl)  #发送变量改变的信号        
        if self.vUrl == 'FLTCON/NMACH':
            try:
                tNMACH = int(float(self.InputWidget.text()))
                self.Signal_NMACHChanged.emit(tNMACH)
            except Exception as e:
                self.logger.error("编辑控件不发转换输入文本到NMACH(int):%s"%self.InputWidget.text())

        
    @pyqtSlot(int)  #标示和值
    def on_EnabledStatusChanged(self, iStatus = Qt.Checked):
        """
        响应外部触发的Enable和DisEnable信号或者需求
        @param  iStatus 控件的基本状态
        @Type  int  Qt.Checked,Qt.Unchecked
        """
        #TODO 实现逻辑待定        
        if 'MustInput' in self.VarDefine.keys() and self.VarDefine['MustInput' ] in ['UnChecked', 'Checked'] :
                self.LabelItem.setCheckState(iStatus)
        else:
            if iStatus == Qt.Checked:
                self.InputWidget.setEnabled(True)
                self._UpdateUsedFlags('True')                
            else:
                self.InputWidget.setEnabled(False) 
                self._UpdateUsedFlags('False') 
   
        
    @pyqtSlot(str, int)  #控件是Group长度的输入项
    def on_Signal_rowCountChanged(self, vUrl, vCount):
        """
        将控件的显示值重置为vText
        """
        if self.vUrl == vUrl and type(self.InputWidget) is QtWidgets.QLineEdit:
            tDefault = self.dtModel.getVariableByUrl(self.vUrl) 
            tDefault.update({'Value':vCount})  #利用高级Python特性，代替了回写操作
            self.dtModel.setVariable( tDefault)
            self.InputWidget.setText(str(vCount))   
        else:
            self.logger.error("on_Signal_rowCountChanged处理异常:%s-%d"%(vUrl, vCount))

    
    @pyqtSlot(int)
    def on_checkBox_Var_stateChanged(self, p0):
        """
        控件的选项卡状态变换信号的响应函数
        
        @param p0 选项 Qt.Checked ,Qt.Unchecked
        @type int
        """
        tUnitWidget = self.findChild(QtWidgets.QComboBox,"comboBox_Unit_%s"%self.VarName)
        if p0 == Qt.Checked:
            #调整界面状态
            self.InputWidget.setEnabled(True)
            if tUnitWidget: 
                tUnitWidget.setEnabled(True)
            #获得并写入到Model
            tVar = self._getDatcomData()
            tVar.update({'InUsed':'True'})
            self.dtModel.setVariable(tVar)
        else:
            #调整界面状态
            self.InputWidget.setEnabled(False)
            if tUnitWidget: 
                tUnitWidget.setEnabled(False)     
            #获得并写入到Model
            tVar = self._getDatcomData()
            tVar.update({'InUsed':'False'})
            self.dtModel.setVariable(tVar)

    @pyqtSlot(int)
    def on_Unit_currentIndexChanged(self, index):
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
        #更新内部状态
        self.vCurrentUnit  = tUint
        #如果是代理模式，回写变量
        if not self.isDelegate:
            tDefault = self._getCurrentValueInModel()
            tDefault.update(tRVar)
            self.dtModel.setVariable(tDefault)  
            
            
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
    def __init__(self, iUrl,  parent=None, iDefinition = DTdictionary.defaultConfig, isRemoveSpacer = True):
        """
        """
        super(DatcomInputSingleNoLabel, self).__init__(iUrl,  parent, iDefinition, isDelegate= True)
        #删除对应的控件
        tLabel = self.findChild(QtWidgets.QCheckBox, 'checkBox_Var%s'%self.VarName)
        tLabel2 = self.findChild(QtWidgets.QLabel, 'label_Var%s'%self.VarName)
        if tLabel is not None :
            #tLabel.setCheckState(Qt.Checked)  #此句破坏了内部结构，要求具有dtModel
            self.InputWidget.setEnabled(True)
            tLabel.deleteLater()
        elif tLabel2 is not None:
            tLabel2.deleteLater()  
        else:
            self.logger.error("并不存在对应的控件")
        

        
if __name__ == "__main__":
    import sys, os
    #from PyDatcomLab.Core import  datcomModel as dcModel
    app = QtWidgets.QApplication(sys.argv)
    tMain = QtWidgets.QWidget()  
    LiftLayout = QtWidgets.QVBoxLayout()
    #LiftLayout.setContentsMargins(5, 0, 0, 5)
    tMain.setLayout(LiftLayout)
    tMPath = os.path.join(os.path.expanduser('~'), r'.PyDatcomLab\extras\PyDatcomProjects\1\case2.xml')
    tModel = dcModel(tMPath)

    LiftLayout.addWidget(DatcomInputSingle( 'FLTCON/TSMACH',  parent=tMain, iDefinition = DTdictionary.defaultConfig )) 
    LiftLayout.addWidget(DatcomInputSingleNoLabel( 'FLTCON/TSMACH',  parent=tMain, iDefinition = DTdictionary.defaultConfig )) 
    LiftLayout.addWidget(DatcomInputSingle( 'FLTCON/HYPERS',  parent=tMain, iDefinition = DTdictionary.defaultConfig ))  
    LiftLayout.addWidget(DatcomInputSingleNoLabel( 'FLTCON/HYPERS',  parent=tMain, iDefinition = DTdictionary.defaultConfig ))  
    LiftLayout.addWidget(DatcomInputSingle( 'FLTCON/WT'    ,  parent=tMain, iDefinition = DTdictionary.defaultConfig , iModel = tModel)) 
    LiftLayout.addWidget(DatcomInputSingle( 'FLTCON/WT'    ,  parent=tMain, iDefinition = DTdictionary.defaultConfig , iModel = tModel)) 
    LiftLayout.addWidget(DatcomInputSingle( 'FLTCON/WT'    ,  parent=tMain, iDefinition = DTdictionary.defaultConfig , iModel = tModel)) 
    LiftLayout.addWidget(DatcomInputSingle( 'FLTCON/WT'    ,  parent=tMain, iDefinition = DTdictionary.defaultConfig , iModel = tModel)) 
    LiftLayout.addWidget(DatcomInputSingleNoLabel( 'FLTCON/WT'    ,  parent=tMain, iDefinition = DTdictionary.defaultConfig )) 
    LiftLayout.addWidget(DatcomInputSingle( 'FLTCON/NALPHA',  parent=tMain, iDefinition = DTdictionary.defaultConfig )) 
    LiftLayout.addWidget(DatcomInputSingleNoLabel( 'FLTCON/NALPHA',  parent=tMain, iDefinition = DTdictionary.defaultConfig , isRemoveSpacer = False)) 

    tMain.show()
    sys.exit(app.exec_())
