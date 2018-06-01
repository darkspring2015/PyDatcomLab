# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'E:\Projects\PyDatcomLab\PyDatcomLab\GUIs\PlaneConfiguration\ASYFLP.ui'
#
# Created by: PyQt5 UI code generator 5.9.1
#
# WARNING! All changes made in this file will be lost!

from PyQt5 import QtCore, QtWidgets

from PyQt5.QtWebEngineWidgets import QWebEngineView
from PyDatcomLab.Core.datcomDefine import groupDefine #,  Dimension 
from PyDatcomLab.GUIs.InputCard.DatcomInputTable import DatcomInputTable  as TB
from PyDatcomLab.GUIs.InputCard.DatcomInputComboChooser import DatcomInputComboChooser
from PyDatcomLab.Core.DictionaryLoader import  defaultDatcomDefinition as DDefine
from PyDatcomLab.GUIs.InputCard import DatcomInputSingle as SInput,  DatcomInputList as LInput
import logging

class DatcomWidgetBaseUi(object):
    """
    class DatcomCARD 是提供CARD录入的基础类
    """

    
    def __init__(self):
        """
        初始化所有必需的操作
        """    
        #创建日志
        self.logger = logging.getLogger(r'Datcomlogger')  
        self.baseSize       = [900, 600]
        self.baseSplitterSize = [300, 600]
        self.baseStretchFactor = [1, 2]
    
    def setupUi(self, CARD):
        """
        通用CARD的界面生成器
        CARD： 包括定义表dict 
        """
        #存储临时的缓存信息
             
        CARD.setObjectName(CARD.NameList)
        #CARD.resize(self.baseSize[0], self.baseSize[1])

        #创建框架
        #创建顶层布局
        self.horizontalLayout_CARD = QtWidgets.QHBoxLayout(CARD)
        self.horizontalLayout_CARD.setObjectName("horizontalLayout_CARD")
        #加入分割器
        self.splitter_H1 = QtWidgets.QSplitter(CARD)
        self.splitter_H1.setOrientation(QtCore.Qt.Horizontal)
        self.splitter_H1.setObjectName("splitter_H1")

        #创建左侧布局
        self.groupBox_lift = QtWidgets.QGroupBox(self.splitter_H1)
        self.groupBox_lift.setTitle("")
        self.groupBox_lift.setObjectName("groupBox_lift")        
        self.LiftLayout = QtWidgets.QVBoxLayout(self.groupBox_lift)
        self.LiftLayout.setObjectName("LiftLayout")
        #限制groupBox_lift的格式
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.groupBox_lift.sizePolicy().hasHeightForWidth())
        self.groupBox_lift.setSizePolicy(sizePolicy)
        #循环遍历Group框架定义 

        tableCache ={}
        if hasattr(CARD,'VariableList'):
            #开始参数配置过程
            for tVarName in CARD.VariableList.keys():              
                tVarDefine = CARD.VariableList[tVarName]
                tUrl = '%s/%s'%(CARD.NameList, tVarName)
                #判断类型
                if tVarDefine['TYPE'] == 'Array':
                    #对于表格类型不在这里创建信息
                    groupName = tVarDefine['Group']
                    if groupName in tableCache.keys() :
                        tableCache[groupName].append(tVarName)
                    else:
                        tableCache[groupName] =[tVarName]
                    continue
                elif tVarDefine['TYPE'] in ['INT', 'REAL'] :
                    # 开始单值工程量创建
                    tVarWidget = SInput.DatcomInputSingle(tUrl, parent=self.groupBox_lift )                    
                    tVarWidget.setObjectName(tVarName)
                    #限制var的格式
                    sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
                    sizePolicy.setHorizontalStretch(0)
                    sizePolicy.setVerticalStretch(0)
                    sizePolicy.setHeightForWidth(tVarWidget.sizePolicy().hasHeightForWidth())
                    tVarWidget.setSizePolicy(sizePolicy)
                    self.LiftLayout.addWidget(tVarWidget)
                elif tVarDefine['TYPE'] in ['List'] :
                    # 结束单值工程量创建
                    tVarWidget = LInput.DatcomInputList(tUrl, parent=self.groupBox_lift )
                    tVarWidget.setObjectName(tVarName)
                    #限制var的格式
                    sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
                    sizePolicy.setHorizontalStretch(0)
                    sizePolicy.setVerticalStretch(0)
                    sizePolicy.setHeightForWidth(tVarWidget.sizePolicy().hasHeightForWidth())
                    tVarWidget.setSizePolicy(sizePolicy)
                    self.LiftLayout.addWidget(tVarWidget)
                
            
        #创建附加控件 如果定义筛选变量组的控件
        if hasattr(CARD,'RuleIndexToCombo'):
            #逐条创建附加筛选逻辑
            for tCombo in CARD.RuleIndexToCombo:
                if  tCombo is None or tCombo == {}:
                    continue
                if not 'Index'  in tCombo.keys():
                    continue
                #创建一个水平布局器                
                tVarName   = tCombo['Index']
                tUrl = '%s/%s'%(CARD.NameList, tVarName)
                tComboWidget = DatcomInputComboChooser(tUrl,
                               parent=self.groupBox_lift, DDefinition = DDefine)    
                tComboWidget.setObjectName('Chooser_%s'%tVarName)
                #绑定值变换信号到自身信号 因为尚未创建对象TableWidget无法直接绑定 
                #连接变量变化信号到本Widget的转发信号
                tComboWidget.varComboChanged.connect(CARD.Singal_RuleIndexToCombo)
                #连接当前转发的表格变化信号
                self.Singal_varComboChangedFromTable.connect(tComboWidget.on_Singal_varComboChangedFromTable)
                
                self.LiftLayout.addWidget(tComboWidget)
                
        #创建下方的空间分割器
        spacerItem_LiftBottom = QtWidgets.QSpacerItem(20, 40, QtWidgets.QSizePolicy.Minimum, QtWidgets.QSizePolicy.Expanding)
        self.LiftLayout.addItem(spacerItem_LiftBottom)
        #完成左侧布局结构                    


        #创建右侧结构        
        tabWidget_right = QtWidgets.QTabWidget(self.splitter_H1)
        tabWidget_right.setObjectName("tabWidget_right")
        #设置表格的大小策略
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Expanding, QtWidgets.QSizePolicy.Expanding)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(tabWidget_right.sizePolicy().hasHeightForWidth())
        tabWidget_right.setSizePolicy(sizePolicy)        
        #创建多值工程量的输入结构      
        for tGroup in tableCache.keys():
            #创建表单
            tTabTable = TB(iNameList = CARD.NameList, iGroup = tGroup , iDefine = CARD.DDefine, parent = CARD)
            tTabTable.setObjectName("tableWidget_%s"%tGroup)
            #设置表格的大小策略
            sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Expanding, QtWidgets.QSizePolicy.Expanding)
            sizePolicy.setHorizontalStretch(0)
            sizePolicy.setVerticalStretch(0)
            sizePolicy.setHeightForWidth(tTabTable.sizePolicy().hasHeightForWidth())
            tTabTable.setSizePolicy(sizePolicy)
            tTabTable.setMaximumSize(QtCore.QSize(16777215, 16777215))

            #连接变量组合变化信号
            CARD.Singal_RuleIndexToCombo.connect(tTabTable.on_Singal_RuleIndexToCombo)
            tTabTable.Singal_variableComboChanged.connect(self.Singal_varComboChangedFromTable)
            #长度控制信号和长度变化信号在RuleNumToCount中绑定
            #tHorizontalLayout.addWidget(tTabTable)
            #tTabName     = tGroup
            tTabTooltips = tGroup
            tDisplayName = tGroup
            if hasattr(CARD,'GroupDefine'):
                if not CARD.GroupDefine is None and  tGroup in CARD.GroupDefine.keys():    
                    if 'ToolTips' in CARD.GroupDefine[tGroup].keys():
                        tTabTooltips = CARD.GroupDefine[tGroup]['ToolTips']
                    if 'DisplayName' in CARD.GroupDefine[tGroup].keys():
                        tDisplayName = CARD.GroupDefine[tGroup]['DisplayName']
            tTabTable.setToolTip(tTabTooltips)
            tabWidget_right.addTab(tTabTable, tDisplayName)
                
        #创建说明文档结构
        self.tab_Help = QWebEngineView()
        self.tab_Help.load(QtCore.QUrl(CARD.HelpUrl))
        tabWidget_right.addTab(self.tab_Help, "说明文档")
        #总括布局
        self.horizontalLayout_CARD.addWidget(self.splitter_H1)
        self.splitter_H1.setStretchFactor(0, self.baseStretchFactor[0])
        self.splitter_H1.setStretchFactor(1, self.baseStretchFactor[1])
        #self.splitter_H1.setSizes(self.baseSplitterSize)
        
        #调用控件信号槽绑定逻辑
        self.connectSlotsByRule(CARD) #绑定自定义的信号槽关系
        QtCore.QMetaObject.connectSlotsByName(CARD)
      

        
    def connectSlotsByRule(self, tCARD):
        """
        根据定义的Rule来绑定对应的信号槽
        """
           
        #添加变量组合控制逻辑
        if hasattr(tCARD,'RuleVariableStatus'):
            for iR in tCARD.RuleVariableStatus:#[]
                tCWidget  = tCARD.findChild(QtWidgets.QWidget, "comboBox_%s"%iR['ControlVar'])
                if tCWidget is None :
                    self.logger.error("变量%s对应的控件不存在"%(iR['ControlVar']))
                    continue
                #变量num->Table
                tCWidget.currentIndexChanged.connect(self.emit_Singal_RuleVariableStatus) 
                
        #添加表格长度控制逻辑
        if hasattr(tCARD,'RuleNumToCount'):
            for iR in tCARD.RuleNumToCount:#[]
                tCWidget  = tCARD.findChild(SInput.DatcomInputSingle, iR['Num'])
                tTbWidget = tCARD.findChild(TB,'tableWidget_%s'%iR['Group'])
                if tCWidget is None or tTbWidget is None:
                    self.logger.error("变量%s对应的控件不存在"%(iR['Num']))
                    continue
                #变量num->Table
                #直接进行绑定不经过Mainwindo的转发了
                tCWidget.editingFinished.connect(tTbWidget.on_TbLength_editingFinished) 
                #表格长度到-NUM
                tTbWidget.Signal_rowCountChanged.connect(tCWidget.on_Signal_rowCountChanged)

      
    def retranslateUi(self, CARD):
        """"""
        #_translate = QtCore.QCoreApplication.translate    
        
    def dt_setStretchFactor(self, tIndex, factor = 0):
        """
        设置中央分割器的比例关系
        """
        self.splitter_H1.setStretchFactor(tIndex, factor)
        
    def dt_setSizes(self, tLift, tRight):
        """
        设置中央分割器的比例关系
        tLift  左侧宽度
        tRight 右侧宽度
        """ 
        self.splitter_H1.setSizes([tLift, tRight])
        
    def resizeEvent(self, event):
        """
        """
        all = sum(self.baseStretchFactor)
        if all != 0:
            tStretchSize = []
            for iS in self.baseStretchFactor:
                tStretchSize.append(self.width() * iS/ all)
            self.splitter_H1.setSizes(tStretchSize)
            
        
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
