# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'E:\Projects\PyDatcomLab\PyDatcomLab\GUIs\PlaneConfiguration\ASYFLP.ui'
#
# Created by: PyQt5 UI code generator 5.9.1
#
# WARNING! All changes made in this file will be lost!

from PyQt5 import QtCore, QtWidgets

from PyQt5.QtWebEngineWidgets import QWebEngineView
from PyDatcomLab.Core.datcomDefine import groupDefine #,  Dimension 
from PyDatcomLab.GUIs.InputCard.DatcomTableBase import DatcomTableBase  as TB
from PyDatcomLab.GUIs.InputCard.DatcomInputComboChooser import DatcomInputComboChooser
from PyDatcomLab.Core.DictionaryLoader import  defaultDatcomDefinition as DDefine
from PyDatcomLab.GUIs.InputCard import DatcomInputSingle as SInput,  DatcomInputList as LInput
import logging

class DatcomCARDUIBase(object):
    """
    class DatcomCARD 是提供CARD录入的基础类
    """

    
    def __init__(self):
        """
        初始化所有必需的操作
        """    
        #创建日志
        self.logger = logging.getLogger(r'Datcomlogger')  

    
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

        tableCache ={}
        if hasattr(CARD,'VariableList'):
            #开始参数配置过程
            for tVarName in CARD.VariableList.keys():              
                tVarDefine = CARD.VariableList[tVarName]
                #判断类型
                if tVarDefine['TYPE'] == 'Array':
                    #对于表格类型不在这里创建信息
                    groupName = tVarDefine['Group']
                    if groupName in tableCache.keys() :
                        tableCache[groupName].append(tVarName)
                    else:
                        tableCache[groupName] =[tVarName]
                    #CARD.HashVaribles[tVarName] = "tableWidget_%s"%(groupName) #保存记录
                    continue
                elif tVarDefine['TYPE'] in ['INT', 'REAL'] :
                    # 开始单值工程量创建
                    # 'FLTCON', 'NALPHA',  parent=None, DDefinition = DDefine
                    tVarWidget = SInput.DatcomInputSingle(CARD.NameList, tVarName, parent=CARD )
                    tVarWidget.setObjectName(tVarName)
                    #限制var的格式
                    sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Expanding, QtWidgets.QSizePolicy.Fixed)
                    sizePolicy.setHorizontalStretch(0)
                    sizePolicy.setVerticalStretch(0)
                    sizePolicy.setHeightForWidth(tVarWidget.sizePolicy().hasHeightForWidth())
                    tVarWidget.setSizePolicy(sizePolicy)
                    self.LiftLayout.addWidget(tVarWidget)
                    # 结束单值工程量创建
                elif tVarDefine['TYPE'] in ['List'] :
                    tVarWidget = LInput.DatcomInputList(CARD.NameList, tVarName, parent=CARD )
                    tVarWidget.setObjectName(tVarName)
                    #限制var的格式
                    sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Expanding, QtWidgets.QSizePolicy.Fixed)
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
                #tGroupName = tCombo['Group']
                tComboWidget = DatcomInputComboChooser(CARD.NameList, tVarName,
                         parent=CARD, DDefinition = DDefine)  
                tComboWidget.setObjectName('Chooser_%s'%tVarName) 
                tComboWidget.varComboChanged.connect(CARD.Singal_RuleIndexToCombo)
                self.LiftLayout.addWidget(tComboWidget)
                #绑定值变换信号到自身信号 因为尚未创建对象TableWidget无法直接绑定



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
            tTabTable = TB(iNameList = CARD.NameList, iGroup = tGroup , iDefine = CARD.DDefine, parent = CARD)
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
            #连接信号和槽
            #连接变量组合变化信号
            CARD.Singal_RuleIndexToCombo.connect(tTabTable.on_Singal_RuleIndexToCombo)
            #长度控制信号和长度变化信号在RuleNumToCount中绑定
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
        self.tab_Help.load(QtCore.QUrl(CARD.HelpUrl))
        tabWidget_right.addTab(self.tab_Help, "说明文档")
        
        self.horizontalLayout_CARD.addWidget(tabWidget_right)     

        
        #调用控件信号槽绑定逻辑
        self.connectSlotsByRule(CARD) #绑定自定义的信号槽关系
        #QtCore.QMetaObject.connectSlotsByName(CARD)
        
    def connectSlotsByRule(self, tCARD):
        """
        根据定义的Rule来绑定对应的信号槽
        """
           
        #添加变量组合控制逻辑
        if hasattr(tCARD,'RuleVariableStatus'):
            for iR in tCARD.RuleVariableStatus:#[]
                tCWidget  = tCARD.findChild(TB, "comboBox_%s"%iR['ControlVar'])
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
