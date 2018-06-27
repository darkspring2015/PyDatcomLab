# -*- coding: utf-8 -*-

"""
Module implementing DatcomInputSingle.
"""
from PyQt5 import QtCore,  QtWidgets #, QtGui
from PyQt5.QtCore import pyqtSlot, Qt, pyqtSignal
from PyQt5.QtWidgets import QWidget

from PyDatcomLab.Core.DictionaryLoader import  defaultDatcomDefinition as DDefine 
import logging


class DatcomInputList(QWidget):
    """
    用于输入Datcom中的List类型的参数.其中将增加一些特殊的转换逻辑
    """
    currentIndexChanged = pyqtSignal(str , str)  #将编辑结构发送出去 (Url,index在Range中的具体值）
    
    def __init__(self, iUrl,  parent=None, iDefinition = DDefine ):
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
        self.CARDName , self.VarName    =    iUrl.split('/')[-2:]        
        #分析其他的附加信息
        self.VarDisplayName = self.VarDefine['DisplayName'] if 'DisplayName' in self.VarDefine.keys() else self.VarName
        self.VarTooltips    = self.VarDefine['Tooltips'] if 'Tooltips' in  self.VarDefine.keys() else self.VarDisplayName
        self.vRange         = []
        if 'Range' in self.VarDefine.keys():
            self.vRange     = self.VarDefine['Range'] 
        else:
            self.logger.error("没有有效的配置文件，无法初始化,Range不能为空")
        self.vDisplayRange  = self.VarDefine['DisplayRange'] if 'DisplayRange' in self.VarDefine.keys() else self.vRange
        
        
        #基本几何尺寸
        self.labelIndent    = 20
        self.baseSize       = [400, 25]
        self.baseSplitterSize = [200, 200]
        self.baseStretchFactor = [1, 1]
        
        #初始化界面
        self.setupUi(self)
        self.InitializeUILogic()

    def setupUi(self, Form):
        """
        自动界面构造函数
        """
        if  self.VarDefine['TYPE'] not in ['List'] : 
            self.logger.error("尝试在DatcomInputList控件中录入非List值")
            return
            
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


    def setDataByVariable(self, tVar):
        """
        设置控件的值，
        tVar 是dict型的变量，是dcModel的成员
        """     
        tMust = ['VarName','Namelist', 'Url',  'Value' ]
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
        设置控件的值,采用的是datcomModel的接口
        从dtModel中加载控件的值
        """
        if dtModel is None : return 
        #获得变量的值 
        tVar = dtModel.getDiscreteVariableValueByName(self.vUrl)
        if tVar is not None :
            if type(tVar) in [float, int]:
                tKey = '%.1f'%(float(tVar))
            elif type(tVar) is str:
                tKey = tVar
            elif type(tVar) is dict and 'Value' in tVar.keys():
                if type(tVar['Value']) is str:
                    tKey = tVar['Value']
                elif type(tVar['Value']) in [float, int]:
                    tKey = '%.1f'%(float(tVar['Value']))
                else:
                    tKey = tVar['Value']  #None 
                    self.logger.error("值类型错误: %s "%(str(tVar)))                    
            else:
                self.logger.error("值类型错误")
  
            if tKey in self.VarDefine['Range']:
                self.InputWidget.setCurrentIndex(self.VarDefine['Range'].index(tKey))
            else:
                if 'Default' in self.VarDefine.keys():
                    self.InputWidget.setCurrentIndex(self.VarDefine['Range'].index(self.VarDefine['Default']))
                    self.logger.error("传递的参数具有的值不在预设范围之内：%s,使用默认值修正：%s"%(tKey, self.VarDefine['Default'])) 
                else:
                    self.logger.error("传递的参数具有的值不在预设范围之内：%s"%(tVar['Value'])) 
        else:
            if type(self.LabelItem )is QtWidgets.QCheckBox :
                self.LabelItem.setCheckState(Qt.Unchecked)
            else:
                self.logger.error("没有为该List传递的有效的参数，而控件又是必须录入信息的控件！")
    
    def saveData(self, dtModel):
        """
        将控件的值写入到模型dtModel中
        """
        tV = self.dtDefine.getVariableTemplateByUrl(self.vUrl)
        tValue = self.vRange[self.InputWidget.currentIndex()]
        
        if not self.InputWidget.isEnabled() or self.InputWidget.currentIndex() < 0:
            tV['Value'] = None
        else  :
            try:
                tV.update({'Value':tValue})
            except Exception as e:
                self.logger.error("DatcomInputList.saveData（）没有找到%s对应的项目索引！"%self.vUrl)
        #写入到结果之中        
        dtModel.setDiscreteVariableValueByName(self.vUrl, tV) 
        

 
    @pyqtSlot(int)
    def on_checkBoxWidget_stateChanged(self, p0):
        """
        Slot documentation goes here.
        这里有一个陷阱，on_checkBox_Var_stateChanged 作为函数
        将导致所有的List之间形成不确定的路由关系，为此不使用和控件名称一致的函数名
        
        @param p0 DESCRIPTION
        @type int
        """
        if p0 == Qt.Checked:
            self.InputWidget.setEnabled(True)
        else:
            self.InputWidget.setEnabled(False)
 
    
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

    LiftLayout.addWidget(DatcomInputList(        'FLTCON/HYPERS',  parent=tMain, iDefinition = DDefine ))  
    LiftLayout.addWidget(DatcomInputListNoLabel( 'FLTCON/HYPERS',  parent=tMain, iDefinition = DDefine ))  
 
    tMain.show()
    sys.exit(app.exec_())
