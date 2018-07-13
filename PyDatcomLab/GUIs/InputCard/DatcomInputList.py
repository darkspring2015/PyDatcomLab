# -*- coding: utf-8 -*-

"""
Module implementing DatcomInputSingle.
"""
from PyQt5 import QtCore,  QtWidgets #, QtGui
from PyQt5.QtCore import pyqtSlot, Qt, pyqtSignal
from PyQt5.QtWidgets import QWidget

from PyDatcomLab.Core.datcomModel import dcModel
from PyDatcomLab.Core.DictionaryLoader import  defaultDatcomDefinition as DDefine 
import logging


class DatcomInputList(QWidget):
    """
    用于输入Datcom中的List类型的参数.其中将增加一些特殊的转换逻辑
    """
    currentIndexChanged = pyqtSignal(str, str)  #将编辑结构发送出去  当前索引 int   # (Url,index在Range中的具体值）
    Signal_VariableChanged =  pyqtSignal(str)            #控件对应的datcom变量发生变化时触发 str为变量的iUrl，用于通知其他的相关控件
    
    def __init__(self, iUrl,  parent=None, iDefinition = DDefine , iModel =None):
        """
        Constructor
        DatcomInputList是一个QWidget控件，用来输出和显示一个List类型的值,list的内容默认为str
        @param parent reference to the parent widget
        @type QWidget
        @param iUrl 是需要显示的变量的Url限定符 NAMELIST/VARIABLE
        @type str
        @param iDefinition reference to DTdictionary的默认实例defaultDatcomDefinition
        @type DTdictionary
        @param parent reference to the parent widget
        @type QWidget
        注意事项：
        1.请合理修改配置文件中的Range,DisplayRange 选项，否则将导致初始化失败。
        2.函数将会将iUrl中的变量名设置为自身的ObjectName
        """
        super(DatcomInputList, self).__init__( parent = parent)
        
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
        self.Namelist  , self.VarName    =    iUrl.split('/')[-2:]        
        if iModel is None:
            iModel = dcModel()
        self.dtModel        = iModel
        #分析其他的附加信息
        self.VarDisplayName = self.VarDefine['DisplayName'] if 'DisplayName' in self.VarDefine.keys() else self.VarName
        self.VarTooltips    = self.VarDefine['Tooltips'] if 'Tooltips' in  self.VarDefine.keys() else self.VarDisplayName
        self.setToolTip(self.VarTooltips  )
        self.vRange         = []
        if 'Range' in self.VarDefine.keys():
            self.vRange     = self.VarDefine['Range'] 
        else:
            self.logger.error("没有有效的配置文件，无法初始化,Range不能为空")
            return
        self.vDisplayRange  = self.VarDefine['DisplayRange'] if 'DisplayRange' in self.VarDefine.keys() else self.vRange
        self.isMustInput = self.dtDefine.getVariableMustInput(self.vUrl)
        #基本几何尺寸
        self.labelIndent    = 20
        self.baseSize       = [400, 25]
        self.baseSplitterSize = [200, 200]
        self.baseStretchFactor = [1, 1]
        #其他参数定义
        self.isDatcom = True
        #初始化界面
        self.setupUi(self)
        self.InitializeUILogic()
        
        #为数据模型调用初始化函数
        if self.dtModel is not None :
            if type(self.dtModel) == dcModel:
                self.setModel(self.dtModel)
            else:
                self.logger.warning("初始化DatcomInputSingle时传入的dtModel的类型为%s，应该为%s"%(str(type(self.dtModel)), str(type(dcModel))))
                self.dtModel = dcModel()
                
        #联结部分的slot
        self.installEventFilter(self)
        

    def setupUi(self, Form):
        """
        自动界面构造函数
        主要功能：
        1.创建控件框架
        """
        if  self.VarDefine['TYPE'] not in ['List'] : 
            self.logger.error("尝试在DatcomInputList控件中录入非List值")
            return
        #设置自身的名称为变量名
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
        #self.LabelItem = None
        if 'MustInput' in self.VarDefine.keys() and self.VarDefine['MustInput' ] in ['UnChecked', 'Checked'] :
            #存在可选项
            self.LabelItem = QtWidgets.QCheckBox(self.splitter_Top)
            self.LabelItem.setObjectName("checkBox_Var%s"%self.VarName)
            #绑定值变换信号到自身信号 先绑定以响应对应的状态确认
            self.LabelItem.stateChanged.connect(self.on_checkBoxWidget_stateChanged) 
            if self.VarDefine['MustInput' ] == 'UnChecked':
                self.LabelItem.setCheckState(Qt.Unchecked)
            elif  self.VarDefine['MustInput' ] == 'Checked':
                self.LabelItem.setCheckState(Qt.Checked)
            else:
                self.LabelItem.setCheckState(Qt.Unchecked)
           
        else: #没有选项卡
            self.LabelItem = QtWidgets.QLabel(self.splitter_Top)
            self.LabelItem.setObjectName("label_Var%s"%self.VarName)
            self.LabelItem.setIndent(self.labelIndent)
        #给Label赋值
        if 'DisplayName' in self.VarDefine.keys():
            self.LabelItem.setText(self.VarDefine['DisplayName'])
        else:
            self.LabelItem.setText(self.VarName)  
        #调节
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.LabelItem.sizePolicy().hasHeightForWidth())
        self.LabelItem.setSizePolicy(sizePolicy)


        #创建右半部分的结构
        #添加录入框
        self.InputWidget = QtWidgets.QComboBox(self.splitter_Top)
        self.InputWidget.setObjectName("InputWidget%s"%self.VarName) #设置输入组件的名称为变量名 
        #配置策略
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.InputWidget.sizePolicy().hasHeightForWidth())
        self.InputWidget.setSizePolicy(sizePolicy)
        #self.splitter_Top.addWidget(self.InputWidget )
 
        #如果存在DisplayRange，优先添加说明信息
        if 'DisplayRange' in self.VarDefine.keys() and 'Range' in self.VarDefine.keys() and\
            len(self.VarDefine['Range']) == len(self.VarDefine['DisplayRange']):
                for iT in self.VarDefine['DisplayRange']:
                    self.InputWidget.addItem( iT ) 
        else:
            for iT in self.VarDefine['Range']:
                self.InputWidget.addItem( iT ) 
        #配置默认值
        if 'Default' in self.VarDefine.keys() :
            tIndex =self.vRange.index(self.VarDefine['Default'])
            if tIndex >-1:
                self.InputWidget.setCurrentIndex(tIndex)
        #绑定信号槽关系 ,自动绑定机制将导致逻辑混乱的问题
        self.InputWidget.currentIndexChanged.connect(self.on_ListWidget_currentIndexChanged)
        #添加分裂器
        self.verticalLayout.addWidget(self.splitter_Top)   
        
        #执行分裂期附加配置
        self.splitter_Top.setStretchFactor(0, self.baseStretchFactor[0])
        self.splitter_Top.setStretchFactor(1, self.baseStretchFactor[1])
        #self.splitter_Top.setSizes(self.baseSplitterSize)
        
        #添加单位
        self.retranslateUi(Form)
        
        QtCore.QMetaObject.connectSlotsByName(Form)
        


    def InitializeUILogic(self):
        """
        执行UI的初始化逻辑同步
        """
        #初始化界面默认值
        #tVarDefine = self.VarDefine
        tCklabel = self.findChild(QtWidgets.QCheckBox,'checkBox_Var%s'%self.VarName)
        if tCklabel :
            if tCklabel.checkState() == Qt.Unchecked:
                self.InputWidget.setEnabled(False)              
            elif  tCklabel.checkState() == Qt.Checked:
                self.InputWidget.setEnabled(True)
            else:
                self.InputWidget.setEnabled(False)   
                

    def retranslateUi(self, Form):
        _translate = QtCore.QCoreApplication.translate
        Form.setWindowTitle(_translate("Form", self.VarDisplayName))
        self.LabelItem.setText(_translate("Form", self.VarDisplayName))
        self.InputWidget.setToolTip(_translate("Form", self.VarTooltips))       


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
            
    def eventFilter(self, watched, event):
        """
        重载eventFilter(QObject *o, QEvent *e)函数，实现过滤功能，在实现进入时给与 激活focus的功能
        """
#        if watched == self.InputWidget:
#            if QtCore.QEvent.HoverEnter == event.type():
#                 self.setFocus_onWindowActivate()
#                 return True
#            if QtCore.QEvent.HoverLeave == event.type():
#                self.cancelSelection_onWindowDeactivate()
#                return True
             
        return False            
    

    def setDelegateData(self, tData):
        """
        直接设置数据值，将在作为Delegate是被使用
        tData发送来的是显示值 DisplayRange的值
        """
        tIndex = self.vDisplayRange.index(str(tData))
        if tIndex < 0 :
            self.logger.error("传入数据无法通过验证：%s：%s"%(self.vUrl, str(tData)))
            return 
        self.InputWidget.setCurrentIndex(tIndex)


    def getDelegateData(self):
        """
        返回编辑控件当前的显示值，将在作为Delegate是被使用
        """
        if self.vRange is None :return None
        return self.vDisplayRange[self.InputWidget.currentIndex()] 


    def setModel(self, iModel):
        """
        设置控件的dtModel
        注意：
        1.控件的更改将直接写入到iModel中
        2.函数触发一次加载
        """        
        #为数据模型调用初始化函数
        if iModel is not None and  type(self.dtModel) == dcModel:
            self.dtModel = iModel
            self._loadData()
        else:
            self.logger.warning("DatcomInputList.setModel()传入的dtModel的类型为%s，应该为%s"%(str(type(self.dtModel)), str(type(dcModel))))        
    
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
        tVar = self._getCurrentValueInModel()
        self._loadDataToWidget(tVar)

                    
    def _loadDataToWidget(self, iVariable):
        """
        根据输入变量iVariable修改界面信息
        @param iVariable 输入的变量值
        @type dict
        """
        #检查输入类型
        if iVariable is None or type(iVariable) != dict  :
            self.logger.error("setDataByVariable()设置%s的输入的参数类型不合法：%s !"%(self.vUrl, type(iVariable)))
            return False
            
        tMust = [ 'Url', 'Value' ]
        for iK in tMust:
            if iK not in iVariable.keys():
                self.logger.error("setDataByVariable()设置%s的输入参数项不足,缺少%s"%(self.vUrl, iK))
                return False
        #检查变量名称
        if iVariable['Url'] != self.vUrl:
            self.logger.error("setDataByVariable()输入的变量名为 %s，应该为%s"%(self.vUrl, iVariable['Url']))
            return False            

        #分析输入合规性,获得输入的键值
        if iVariable is not None and type(iVariable) is dict:
            if type(iVariable['Value']) is str:
                tKey = iVariable['Value']
            elif type(iVariable['Value']) in [float, int] and self.isDatcom:
                tKey = '%.1f'%(float(iVariable['Value']))
            else:
                tKey = iVariable['Value']  #None 
                self.logger.error("值类型错误: %s "%(str(iVariable)))                   
        else:
            self.logger.error("值类型错误")
        #根据键值选择控件的选项，并激活或关闭控件
        if tKey in self.VarDefine['Range']:
            self.InputWidget.setCurrentIndex(self.VarDefine['Range'].index(tKey))
            #判断是否需要激活控件
            if 'MustInput' in self.VarDefine.keys() and self.VarDefine['MustInput' ] in ['UnChecked', 'Checked'] :
                if 'Default' in self.VarDefine and self.VarDefine['Default'] != tKey :
                    self.on_EnabledStatusChanged(True)
                else:
                    self.on_EnabledStatusChanged(False)
        else:
            if 'Default' in self.VarDefine.keys():
                self.InputWidget.setCurrentIndex(self.VarDefine['Range'].index(self.VarDefine['Default']))
                self.on_EnabledStatusChanged(False)
                self.logger.error("传递的参数具有的值不在预设范围之内：%s,使用默认值修正：%s"%(tKey, self.VarDefine['Default'])) 
            else:
                self.logger.error("传递的参数具有的值不在预设范围之内：%s"%(iVariable['Value'])) 

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
        if self.InputWidget is None :return 
        tDefault = self._getCurrentValueInModel()
        #开始读取逻辑
        tDefault.update({'Value':self.getCurrentKey()})
        if self.InputWidget.isEnabled():
            tDefault.update({'InUsed':'True'})
        else:
            tDefault.update({'InUsed':'False'})
        return tDefault

    
    def _getCurrentValueInModel(self):
        """
        内部函数，从self.dtModel中获得当前值
        如果model中没有当前变量，则使用datcomDefine中的默认值
        """
        tV = self.dtModel.getVariableByUrl(self.vUrl)  #获取模型中的具体数值
        if tV is None:
            tV = self.dtDefine.getVariableTemplateByUrl(self.vUrl, isSubType=True)
            tV.update({"InUsed":'False'})   
        return tV
        
    def _UpdateUsedFlags(self, isUsed = 'True'):
        """
        更新变量的值
        """
        #获取模型值
        tV = self._getCurrentValueInModel()  #获取模型中的具体数值
        #更新标志
        tV.update({'InUsed':isUsed})
        #回写到数据模型
        self.dtModel.setVariable( tV)       
        
    def getCurrentKey(self):
        """
        获得当前控件对应的键
        """        
        return self.vRange[self.InputWidget.currentIndex()]  
    
#    def isEqualDefault(self):
#        """
#        判断是否等于默认值，
#        1. 没有默认值 返回 'Unknown'
#        2. 等于默认值 返回 'TRUE'
#        3. 不等于默认值 返回 'FALSE'
#        """
#        if 'Default' not in self.VarDefine.keys():
#            return  'Unknown'
#        else:
#            if self.VarDefine['Range'].index(self.VarDefine['Default']) == self.InputWidget.currentIndex():
#                return 'TRUE'
#            else:
#                return 'FALSE'
   
        
    @pyqtSlot(int)  #标示和值
    def on_EnabledStatusChanged(self, iStatus = Qt.Checked):
        """
        响应外部触发的Enable和DisEnable信号或者需求
        @param  iStatus 控件的基本状态
        @Type  int  Qt.Checked,Qt.Unchecked
        """ 
        if 'MustInput' in self.VarDefine.keys() and self.VarDefine['MustInput' ] in ['UnChecked', 'Checked'] :
            self.LabelItem.setCheckState(iStatus)
        else:
            if iStatus == Qt.Checked:
                self.InputWidget.setEnabled(True)
                self._UpdateUsedFlags('True')   
            else:
                self.InputWidget.setEnabled(False)    
                self._UpdateUsedFlags('False')   

    @pyqtSlot(int)
    def on_checkBoxWidget_stateChanged(self, p0):
        """
        Slot documentation 
        这里有一个陷阱，on_checkBox_Var_stateChanged 作为函数
        将导致所有的List之间形成不确定的路由关系，为此不使用和控件名称一致的函数名
        要求：
        1.因为控件的激活状态确定了是否使用，因此在checkBoxWidget进入False时应该设置为默认值
        
        @param p0 DESCRIPTION
        @type int
        """
        if p0 == Qt.Checked:
            self.InputWidget.setEnabled(True)
        else:
            self.InputWidget.setEnabled(False)
            #改变当前值到默认值
            if 'Default' in self.VarDefine.keys():
                self.InputWidget.setCurrentIndex(self.VarDefine['Range'].index(self.VarDefine['Default']))

    @pyqtSlot(int)
    def on_ListWidget_currentIndexChanged(self, index):
        """
        选项发生变化时触发新的消息
        这里有一个陷阱，如果使用on_InputWidget_currentIndexChanged 作为函数
        将导致所有的List之间形成不确定的路由关系，为此不使用和控件名称一致的函数名
        
        @param index 选项的索引
        @type int
        """
        self.currentIndexChanged.emit(self.vUrl, self.vRange[index])
        self.Signal_VariableChanged.emit(self.vUrl)


class DatcomInputListNoLabel(DatcomInputList):
    """
    没有Label栏的输入框的List输入控件
    """
    def __init__(self, iUrl,  parent=None, iDefinition = DDefine, isRemoveSpacer = True):
        """
        """
        #super(DatcomInputListNoLabel, self).__init__(CARD, VarName,  parent=None, DDefinition = DDefine)
        #这是一个非常经典的错误
        super(DatcomInputListNoLabel, self).__init__(iUrl,  parent, iDefinition )
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
    import sys
    app = QtWidgets.QApplication(sys.argv)
    tMain = QtWidgets.QWidget()  
    LiftLayout = QtWidgets.QVBoxLayout()
    #LiftLayout.setContentsMargins(5, 0, 0, 5)
    tMain.setLayout(LiftLayout)
    tModel = dcModel()

    LiftLayout.addWidget(DatcomInputList(        'FLTCON/HYPERS',  parent=tMain, iDefinition = DDefine , iModel =tModel))  
    LiftLayout.addWidget(DatcomInputListNoLabel( 'FLTCON/HYPERS',  parent=tMain, iDefinition = DDefine ))  
 
    tMain.show()
    sys.exit(app.exec_())
