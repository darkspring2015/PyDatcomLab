# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'E:\Projects\PyDatcomLab\PyDatcomLab\GUIs\PlaneConfiguration\ASYFLP.ui'
#
# Created by: PyQt5 UI code generator 5.9.1
#
# WARNING! All changes made in this file will be lost!

from PyQt5 import QtCore, QtGui, QtWidgets
from PyQt5.QtCore import pyqtSignal, Qt, pyqtSlot
from PyQt5.QtWebEngineWidgets import QWebEngineView
from PyDatcomLab.Core.datcomDefine import Dimension , groupDefine
from PyDatcomLab.GUIs.InputCard.DatcomTableBase import DatcomTableBase  as TB
from PyDatcomLab.Core.DictionaryLoader import  defaultDatcomDefinition as DDefine

import logging

class DatcomCARDUIBase(object):
    """
    class DatcomCARD 是提供CARD录入的基础类
    """
    Singal_RuleIndexToCombo              = pyqtSignal(int,str)      #处理变量组个的同步问题
    Singal_CheckboxStateChanged          = pyqtSignal(int,str)      #处理checkbox的同步问题
    Singal_TbLength_editingFinished      = pyqtSignal(str, str, int)          #处理表格长度控制变量的触发逻辑
    Singal_CommonUIChanged               = pyqtSignal(str)          #通用的输入状态变化规则
    Singal_RuleVariableStatus            = pyqtSignal(str)          #通用的RuleVariableStatus状态变化规则 
    Singal_TBRowCountChanged             = pyqtSignal(int, str)     #用于通知表格的行数发生了变化
    def __init__(self):
        """
        初始化所有必需的操作
        """    
        #创建日志
        self.logger = logging.getLogger(r'Datcomlogger') 
        self.RuleIndexToComboCache =[]   

    
    def setupUi(self, CARD):
        """
        通用CARD的界面生成器
        CARD： 包括定义表dict 
        """
        #存储临时的缓存信息
             
        CARD.setObjectName(CARD.NameList)
        CARD.resize(1061, 467)
        
        #创建框架
        #创建顶层布局
        self.horizontalLayout_CARD = QtWidgets.QHBoxLayout(CARD)
        self.horizontalLayout_CARD.setObjectName("horizontalLayout_CARD")
        #创建左侧布局
        self.LiftLayout = QtWidgets.QVBoxLayout(CARD)
        self.LiftLayout.setObjectName("LiftLayout")

        #循环遍历Group框架定义 
        tWidget = CARD
        tableCache ={}
        if hasattr(tWidget,'VariableList'):
            #开始参数配置过程
            for tVarName in tWidget.VariableList.keys(): 
                #创建一个水平布局器
                aLayout = QtWidgets.QHBoxLayout()
                aLayout.setObjectName("horizontalLayout_%s"%(tVarName))
                
                tVarDefine = tWidget.VariableList[tVarName]
                #判断类型
                if tVarDefine['TYPE'] == 'Array':
                    #对于表格类型不在这里创建信息
                    groupName = tVarDefine['Group']
                    if groupName in tableCache.keys() :
                        tableCache[groupName].append(tVarName)
                    else:
                        tableCache[groupName] =[tVarName]
                    CARD.HashVaribles[tVarName] = "tableWidget_%s"%(groupName) #保存记录
                    continue
                else:
                    #判断是否需要Check
                    tLabelItem = None
                    if 'MustInput' in tVarDefine.keys() and tVarDefine['MustInput' ] in ['UnChecked', 'Checked'] :
                        #存在可选项
                        tLabelItem = QtWidgets.QCheckBox(CARD)
                        tLabelItem.setObjectName("checkBox_%s"%(tVarName))
                        if tVarDefine['MustInput' ] == 'UnChecked':
                            tLabelItem.setCheckState(Qt.Unchecked)
                        elif  tVarDefine['MustInput' ] == 'Checked':
                            tLabelItem.setCheckState(Qt.Checked)
                        else:
                            tLabelItem.setCheckState(Qt.Unchecked)
                        #绑定值变换信号到自身信号
                        tLabelItem.stateChanged.connect(self.emit_CheckBoxStateChanged)                        
                    else: #没有选项卡
                        tLabelItem = QtWidgets.QLabel(CARD)
                        tLabelItem.setObjectName("label_%s"%(tVarName))
                    #给Label赋值
                    if 'DisplayName' in tWidget.VariableList[tVarName].keys():
                        tLabelItem.setText(tVarDefine['DisplayName'])
                    else:
                        tLabelItem.setText(tVarName)                        
                    aLayout.addWidget(tLabelItem)  
                    self.__dict__[tLabelItem.objectName()] =  tLabelItem #向类注册
                    #增加sprizer
                    aSpacerItem = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
                    aLayout.addItem(aSpacerItem)  
                    
                    #根据不同的类型添加对应的项目
                    tVarWidget = None
                    if tVarDefine['TYPE'] == 'INT':
                        tVarWidget = QtWidgets.QLineEdit(CARD)
                        tVarWidget.setObjectName("%s"%(tVarName))
                        CARD.HashVaribles[tVarName] = tVarName #保存记录
                        #验证器
                        tVd = QtGui.QIntValidator(CARD)
                        tVd.setBottom(0)
                        if 'Range' in tVarDefine.keys():
                            tRange = tVarDefine['Range']
                            tVd.setRange(tRange[0], tRange[1])
                        tVarWidget.setValidator(tVd) 
                        #默认值
                        #查询默认值
                        if 'Default' in tVarDefine.keys():
                            tVarWidget.setText(str(tVarDefine['Default'] ))
                    elif tVarDefine['TYPE'] == 'REAL':
                        tVarWidget = QtWidgets.QLineEdit(CARD)
                        tVarWidget.setObjectName("%s"%(tVarName))
                        CARD.HashVaribles[tVarName] = tVarName #保存记录
                        #验证器
                        tVd = QtGui.QDoubleValidator(CARD)
                        if 'Range' in tVarDefine.keys():
                            tRange = tVarDefine['Range']
                            if tRange[0] not in [float('-inf'), float('inf'), float('nan')] :
                                tVd.setBottom(tRange[0])
                            if tRange[1] not in [float('-inf'), float('inf'), float('nan')] :
                                tVd.setTop(tRange[1])
                        tVarWidget.setValidator(tVd) 
                        #查询默认值
                        if 'Default' in tVarDefine.keys():
                            tVarWidget.setText(str( tVarDefine['Default'] ))
                    elif tVarDefine['TYPE'] == 'List':
                        tVarWidget = QtWidgets.QComboBox(CARD)
                        tVarWidget.setObjectName("comboBox_%s"%(tVarName))
                        CARD.HashVaribles[tVarName] = "comboBox_%s"%(tVarName) #保存记录
                        #如果存在DisplayRange，优先添加说明信息
                        if 'DisplayRange' in tVarDefine.keys():
                            for itIndex in tVarDefine['DisplayRange']:
                                tVarWidget.addItem(itIndex)                         
                        elif 'Range' in tVarDefine.keys():
                            for itIndex in tVarDefine['Range']:
                                tVarWidget.addItem(itIndex)   

                    #限制var的格式
                    sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
                    sizePolicy.setHorizontalStretch(0)
                    sizePolicy.setVerticalStretch(0)
                    sizePolicy.setHeightForWidth(tVarWidget.sizePolicy().hasHeightForWidth())
                    tVarWidget.setSizePolicy(sizePolicy)

                    #创建ToolTips
                    if 'ToolTips' in tVarDefine.keys():
                        tVarWidget.setToolTip(tVarDefine['ToolTips'])
                    #添加到系统
                    aLayout.addWidget(tVarWidget)
                    #self.__dict__[tVarWidget.objectName()] =  tVarWidget #向类注册
                    
                    #创建量纲
                    if 'Dimension' in tVarDefine.keys():
                        aDimension = QtWidgets.QComboBox(CARD)
                        aDimension.setObjectName("comboBox_Dimension_%s"%(tVarName))
                        if tVarDefine['Dimension'] in Dimension.keys():
                            for itUnit in Dimension[tVarDefine['Dimension']]:
                                aDimension.addItem(itUnit)
                        else:
                            self.logger.error("尝试添加不存在的量纲%s"%(tVarDefine['Dimension']))
                        #选择默认单位
                        if aDimension.count() >0: aDimension.setCurrentIndex(0)
                        aLayout.addWidget(aDimension)   
                    
                # 结束单值工程量创建
  
                #添加项目具体输入框 END                        
                self.LiftLayout.addLayout(aLayout)
                
        #创建附加控件 如果定义筛选变量组的控件
        if hasattr(tWidget,'RuleIndexToCombo'):
            #逐条创建附加筛选逻辑
            for tCombo in tWidget.RuleIndexToCombo:
                if  tCombo is None or tCombo == {}:
                    continue
                if not 'Index'  in tCombo.keys():
                    continue
                #创建一个水平布局器
                tVarName   = tCombo['Index']
                tGroupName = tCombo['Group']
                aLayout = QtWidgets.QHBoxLayout()
                aLayout.setObjectName("horizontalLayout_%s"%(tVarName))
                tLabelItem = QtWidgets.QLabel(CARD)
                tLabelItem.setObjectName("label_%s"%(tVarName))
                #给Label赋值
                if 'DisplayName' in tCombo.keys():
                    tLabelItem.setText(tCombo['DisplayName'])
                else:
                    tLabelItem.setText(tVarName)  
                aLayout.addWidget(tLabelItem)  
                aSpacerItem = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
                aLayout.addItem(aSpacerItem) 
                #创建Combo对象
                tVarWidget = QtWidgets.QComboBox(CARD)
                tVarWidget.setObjectName("comboBox_%s"%(tVarName))
                CARD.HashVaribles[tVarName] = "comboBox_%s"%(tVarName) #保存记录
                #如果存在DisplayRange，优先添加说明信息
                for iR in tCombo['HowTo'].keys():
                    tVarWidget.addItem('-'.join(tCombo['HowTo'][iR]))  
                aLayout.addWidget(tVarWidget)
                #绑定值变换信号到自身信号 因为尚未创建对象TableWidget无法直接绑定
                self.RuleIndexToComboCache.append({'Sender':"comboBox_%s"%(tVarName),
                                               'Receiver':"tableWidget_%s"%(tGroupName)})
                #tVarWidget.currentIndexChanged.connect(self.emit_currentIndexChanged)
                #添加到左侧兰  
                self.LiftLayout.addLayout(aLayout)

        #创建下方的空间分割器
        spacerItem_LiftBottom = QtWidgets.QSpacerItem(20, 40, QtWidgets.QSizePolicy.Minimum, QtWidgets.QSizePolicy.Expanding)
        self.LiftLayout.addItem(spacerItem_LiftBottom)
        #完成左侧布局结构                    
        self.horizontalLayout_CARD.addLayout(self.LiftLayout)

        #创建右侧结构
        aLayout = QtWidgets.QHBoxLayout()
        aLayout.setObjectName("horizontalLayout_right")
        
        tabWidget_right = QtWidgets.QTabWidget(CARD)
        tabWidget_right.setObjectName("tabWidget_right")
        
        #创建多值工程量的输入结构      
        for tGroup in tableCache.keys():
            tTab = QtWidgets.QWidget(CARD)
            tTab.setObjectName("tab_%s"%(tGroup))
            tHorizontalLayout = QtWidgets.QHBoxLayout(tTab)
            tHorizontalLayout.setObjectName("horizontalLayout_%s"%tGroup)
            #tTabTable = QtWidgets.QTableWidget(CARD)
            tTabTable = TB(CARD)
            tTabTable.setDefinition( CARD.NameList, tGroup, DDefine)
            sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Expanding, QtWidgets.QSizePolicy.Expanding)
            sizePolicy.setHorizontalStretch(0)
            sizePolicy.setVerticalStretch(0)
            sizePolicy.setHeightForWidth(tTabTable.sizePolicy().hasHeightForWidth())
            tTabTable.setSizePolicy(sizePolicy)
            tTabTable.setMaximumSize(QtCore.QSize(16777215, 16777215))
            tTabTable.setContextMenuPolicy(QtCore.Qt.CustomContextMenu)
            tTabTable.setObjectName("tableWidget_%s"%tGroup)
            tTabTable.setColumnCount(0)
            tTabTable.setRowCount(0)
            tHorizontalLayout.addWidget(tTabTable)
            #tTabName     = tGroup
            tTabTooltips = tGroup
            tDisplayName = tGroup
            if hasattr(CARD,'GroupDefine'):
                if not CARD.GroupDefine is None and  tGroup in CARD.GroupDefine.keys():    
                    if 'ToolTips' in CARD.GroupDefine[tGroup].keys():
                        tTabTooltips = CARD.GroupDefine[tGroup]['ToolTips']
                        #tTab.setToolTip(CARD.groupDefine[tGroup]['ToolTips'])
                    if 'DisplayName' in CARD.GroupDefine[tGroup].keys():
                        tDisplayName = CARD.GroupDefine[tGroup]['DisplayName']
                        #tabWidget_right.addTab(tTab, CARD.groupDefine[tGroup]['DisplayName']) 
            tTab.setToolTip(tTabTooltips)
            tabWidget_right.addTab(tTab, tDisplayName)
                
        #创建说明文档结构
        self.tab_Help = QWebEngineView()
        self.tab_Help.load(QtCore.QUrl("https://github.com/darkspring2015/PyDatcomLab/blob/master/wiki/%E5%8F%82%E6%95%B0%E8%AF%B4%E6%98%8E%E6%96%87%E4%BB%B6.md"))
        tabWidget_right.addTab(self.tab_Help, "说明文档")
        
        self.horizontalLayout_CARD.addWidget(tabWidget_right)     

        
        #调用控件信号槽绑定逻辑
        self.connectSlotsByRule(CARD) #绑定自定义的信号槽关系
        #QtCore.QMetaObject.connectSlotsByName(CARD)
        
    def connectSlotsByRule(self, tCARD):
        """
        根据定义的Rule来绑定对应的信号槽
        """
        
        #执行附加的信号绑定关系
        for iCn in self.RuleIndexToComboCache:
            tSWidget = tCARD.findChild(QtWidgets.QComboBox   ,iCn['Sender']) 
            tRWidget = tCARD.findChild(QtWidgets.QTableWidget,iCn['Receiver']) 
            if tSWidget is None or tRWidget is None:
                self.logger.error("尝试绑定tRuleIndexToComboCache逻辑失败")
                continue
            tSWidget.currentIndexChanged.connect(tRWidget.on_Singal_RuleIndexToCombo)
            
        #添加变量组合控制逻辑
        if hasattr(tCARD,'RuleVariableStatus'):
            for iR in tCARD.RuleVariableStatus:#[]
                tCWidget  = tCARD.findChild(QtWidgets.QComboBox, "comboBox_%s"%iR['ControlVar'])
                if tCWidget is None :
                    self.logger.error("变量%s对应的控件不存在"%(iR['ControlVar']))
                    continue
                #变量num->Table
                tCWidget.currentIndexChanged.connect(self.emit_Singal_RuleVariableStatus)   
        #添加表格长度控制逻辑
        if hasattr(tCARD,'RuleNumToCount'):
            for iR in tCARD.RuleNumToCount:#[]
                tCWidget  = tCARD.findChild(QtWidgets.QLineEdit, iR['Num'])
                tTbWidget = tCARD.findChild(QtWidgets.QTableWidget,'tableWidget_%s'%iR['Group'])
                if tCWidget is None or tTbWidget is None:
                    self.logger.error("变量%s对应的控件不存在"%(iR['Num']))
                    continue
                #变量num->Table
                tCWidget.editingFinished.connect(lambda : self.emit_TbLength_editingFinished(
                                       iR['Num'], 'tableWidget_%s'%iR['Group']))
                #表格长度到-NUM
                tTbWidget.Signal_rowCountChanged.connect(self.Singal_TBRowCountChanged)

    
        
    def emit_currentIndexChanged(self, tIndex):
        """
        用来触发新的信号函数
        """
        tName = self.sender().objectName()
        self.Singal_RuleIndexToCombo.emit(tIndex, tName)
        
    def emit_CheckBoxStateChanged(self,tIndex ):
        """
        用来触发新的信号函数,转发所有的checkbox状态变化的
        """
        tName = self.sender().objectName()
        self.Singal_CheckboxStateChanged.emit(tIndex, tName)
        
    #@pyqtSlot(str, str)        
    def emit_TbLength_editingFinished(self , sName, rName):
        """
        用来触发新的信号函数,转发影响表格长度的变量的变化
        """
        tNum = 0 if self.sender().text() == "" else int(float(self.sender().text()))
        self.Singal_TbLength_editingFinished.emit( sName, rName, tNum)
        
    def emit_Singal_RuleVariableStatus(self ):
        """
        用来触发新的信号函数,转发Singal_RuleVariableStatus
        """
        tName = self.sender().objectName()
        self.Singal_RuleVariableStatus.emit( tName)




      
    def retranslateUi(self, CARD):
        _translate = QtCore.QCoreApplication.translate    
        
        
        
if __name__ == "__main__":
    import sys
    app = QtWidgets.QApplication(sys.argv)
    card = QtWidgets.QWidget()
    card.NameList = 'FLTCON'
    card.VariableList = {
                'LOOP':{  'TYPE':'List'  ,'Range':['1.0', '2.0', '3.0']   , 'Default':'1.0'},         
                'NMACH':{ 'TYPE':'INT'   ,'Range':[0, 20 ] }, 
                'NALT':{  'TYPE':'INT'   ,'Range':[0, 20 ], 'Dimension':'L' },
                'NALPHA':{'TYPE':'INT'   ,'Range':[0, 20 ] , 'Dimension':'DEG','Label':'NALPHA：攻角数' },
                'WT':{    'TYPE':'REAL'  ,'Range':[0, float('inf') ] , 'ToolTips':'飞行器重量'},
                'GAMMA':{ 'TYPE':'REAL'  },
                'STMACH':{'TYPE':'REAL'  ,'Range':[0.6, 0.99 ]},    
                'TSMACH':{'TYPE':'REAL'  ,'Range':[1.01, 1.4 ]},  
                'HYPERS':{'TYPE':'List'  ,'Range':['.TRUE.', '.FALSE.']  , 'Default':'.TRUE.'}, 
                'TR':{    'TYPE':'List'  ,'Range':['0.0', '1.0']  , 'Default':'0.0'}, 
                'ALSCHD':{'TYPE':'Array', 'Limit':[0, 20] , 'Group':'ALSCHD'}, 
                'MACH':{  'TYPE':'Array', 'Limit':[0, 20] , 'Group':'Speed_Atmospheric'}, 
                'VINF':{  'TYPE':'Array', 'Limit':[0, 20] , 'Group':'Speed_Atmospheric'}, 
                'RNNUB':{ 'TYPE':'Array', 'Limit':[0, 20] , 'Group':'Speed_Atmospheric'},
                'ALT':{   'TYPE':'Array', 'Limit':[0, 20] , 'Group':'Speed_Atmospheric'},                
                'PINF':{  'TYPE':'Array', 'Limit':[0, 20] , 'Group':'Speed_Atmospheric'},  
                'TINF':{  'TYPE':'Array', 'Limit':[0, 20] , 'Group':'Speed_Atmospheric'},
        }   
    card.groupDefine = groupDefine
    card.RuleIndexToCombo = [{'Index':'Variables', 
                        'HowTo':{'1.0':['MACH', 'RNNUB'], 
                                 '2.0':['MACH', 'ALT'  ,'PINF', 'TINF' , 'RNNUB'], 
                                 '3.0':['VINF', 'ALT'  ,'PINF', 'TINF' , 'MACH', 'RNNUB'], 
                                 '4.0':['PINF', 'TINF', 'VINF', 'RNNUB', 'MACH'], 
                                 '5.0':['PINF', 'TINF', 'MACH', 'RNNUB', 'VINF'], 
                                 }, 
                        'Group':'Speed_Atmospheric'} 
                        ]    
    ui = DatcomCARDUIBase()
    ui.setupUi(card)
    if hasattr(ui,'NALT'):
        print(ui.NALT)
    if card.findChild(QtWidgets.QLineEdit,'NALT'):
        print(card.findChild(QtWidgets.QLineEdit,'NALT'))
    card.show()
    sys.exit(app.exec_())
