# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'E:\Projects\PyDatcomLab\PyDatcomLab\GUIs\PlaneConfiguration\ASYFLP.ui'
#
# Created by: PyQt5 UI code generator 5.9.1
#
# WARNING! All changes made in this file will be lost!

from PyQt5 import QtCore, QtGui, QtWidgets

from PyQt5.QtWebEngineWidgets import QWebEngineView

from PyDatcomLab.Core.datcomDefine import Dimension , groupDefine

import logging

class DatcomBaseUI(object):
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
                
                #判断类型
                if tWidget.VariableList[tVarName]['TYPE'] == 'Array':
                    #对于表格类型不在这里创建信息
                    groupName = tWidget.VariableList[tVarName]['Group']
                    if groupName in tableCache.keys() :
                        tableCache[groupName].append(tVarName)
                    else:
                        tableCache[groupName] =[tVarName]
                    continue
                else:
                    #判断是否需要Check
                    tLabelItem = None
                    if 'CheckState' in tWidget.VariableList[tVarName].keys():
                        #存在可选项
                        tLabelItem = QtWidgets.QCheckBox(CARD)
                        tLabelItem.setObjectName("checkBox_%s"%(tVarName))
                        tLabelItem.setCheckState(tWidget.VariableList[tVarName]['CheckState'])
                    else: #没有选项卡
                        tLabelItem = QtWidgets.QLabel(CARD)
                        tLabelItem.setObjectName("label_%s"%(tVarName))
                    #给Label赋值
                    if 'Label' in tWidget.VariableList[tVarName].keys():
                        tLabelItem.setText(tWidget.VariableList[tVarName]['Label'])
                    else:
                        tLabelItem.setText(tVarName)                        
                    aLayout.addWidget(tLabelItem)  
                    self.__dict__[tLabelItem.objectName()] =  tLabelItem #向类注册
                    #增加sprizer
                    aSpacerItem = QtWidgets.QSpacerItem(10, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
                    aLayout.addItem(aSpacerItem)  
                    
                    #根据不同的类型添加对应的项目
                    tVarWidget = None
                    if tWidget.VariableList[tVarName]['TYPE'] == 'INT':
                        tVarWidget = QtWidgets.QLineEdit(CARD)
                        tVarWidget.setObjectName("%s"%(tVarName))
                        #验证器
                        tVd = QtGui.QIntValidator(CARD)
                        tVd.setBottom(0)
                        if 'Range' in CARD.VariableList[tVarName].keys():
                            tRange = tWidget.VariableList[tVarName]['Range']
                            tVd.setRange(tRange[0], tRange[1])
                        tVarWidget.setValidator(tVd) 
                        #默认值
                        #查询默认值
                        if 'Default' in tWidget.VariableList[tVarName].keys():
                            tVarWidget.setText(str( tWidget.VariableList[tVarName]['Default'] ))
                    elif tWidget.VariableList[tVarName]['TYPE'] == 'REAL':
                        tVarWidget = QtWidgets.QLineEdit(CARD)
                        tVarWidget.setObjectName("%s"%(tVarName))
                        #验证器
                        tVd = QtGui.QDoubleValidator(CARD)
                        if 'Range' in tWidget.VariableList[tVarName].keys():
                            tRange = tWidget.VariableList[tVarName]['Range']
                            if tRange[0] not in [float('-inf'), float('inf'), float('nan')] :
                                tVd.setBottom(tRange[0])
                            if tRange[1] not in [float('-inf'), float('inf'), float('nan')] :
                                tVd.setTop(tRange[1])
                        tVarWidget.setValidator(tVd) 
                        #查询默认值
                        if 'Default' in tWidget.VariableList[tVarName].keys():
                            tVarWidget.setText(str( tWidget.VariableList[tVarName]['Default'] ))
                    elif tWidget.VariableList[tVarName]['TYPE'] == 'List':
                        tVarWidget = QtWidgets.QComboBox(CARD)
                        tVarWidget.setObjectName("comboBox_%s"%(tVarName))
                        if 'Range' in tWidget.VariableList[tVarName].keys():
                            for itIndex in tWidget.VariableList[tVarName]['Range']:
                                tVarWidget.addItem(itIndex)                        

                    #限制var的格式
                    sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
                    sizePolicy.setHorizontalStretch(0)
                    sizePolicy.setVerticalStretch(0)
                    sizePolicy.setHeightForWidth(tVarWidget.sizePolicy().hasHeightForWidth())
                    tVarWidget.setSizePolicy(sizePolicy)

                    #创建ToolTips
                    if 'ToolTips' in tWidget.VariableList[tVarName].keys():
                        tVarWidget.setToolTip(tWidget.VariableList[tVarName]['ToolTips'])
                    #添加到系统
                    aLayout.addWidget(tVarWidget)
                    #self.__dict__[tVarWidget.objectName()] =  tVarWidget #向类注册
                    
                    #创建量纲
                    if 'Dimension' in tWidget.VariableList[tVarName].keys():
                        aDimension = QtWidgets.QComboBox(CARD)
                        aDimension.setObjectName("comboBox_Dimension_%s"%(tVarName))
                        if tWidget.VariableList[tVarName]['Dimension'] in Dimension.keys():
                            for itUnit in Dimension[tWidget.VariableList[tVarName]['Dimension']]:
                                aDimension.addItem(itUnit)
                        else:
                            self.logger("尝试添加不存在的量纲%s"%(tWidget.VariableList[tVarName]['Dimension']))
                        #选择默认单位
                        if aDimension.count() >0: aDimension.setCurrentIndex(0)
                        aLayout.addWidget(aDimension)   
                    
                # 结束单值工程量创建
                    
                #
                        
  
  
                #添加项目具体输入框 END
                        
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
        if len(tableCache) >0:
            for tGroup in tableCache.keys():
                tTab = QtWidgets.QWidget(CARD)
                tTab.setObjectName("tab_%s"%(tGroup))
                tHorizontalLayout = QtWidgets.QHBoxLayout(tTab)
                tHorizontalLayout.setObjectName("horizontalLayout_%s"%tGroup)
                tTabTable = QtWidgets.QTableWidget(tTab)
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
                if hasattr(CARD,'groupDefine'):
                    if tGroup in CARD.groupDefine.keys():    
                        if 'ToolTips' in CARD.groupDefine[tGroup].keys():
                            tTab.setToolTip(CARD.groupDefine[tGroup]['ToolTips'])
                        if 'ShowName' in CARD.groupDefine[tGroup].keys():
                            tabWidget_right.addTab(tTab, CARD.groupDefine[tGroup]['ShowName']) 
                else:
                    tabWidget_right.addTab(tTab, tGroup)               
                
        #创建说明文档结构
        self.tab_Help = QWebEngineView()
        self.tab_Help.load(QtCore.QUrl("https://github.com/darkspring2015/PyDatcomLab/blob/master/wiki/%E5%8F%82%E6%95%B0%E8%AF%B4%E6%98%8E%E6%96%87%E4%BB%B6.md"))
        tabWidget_right.addTab(self.tab_Help, "说明文档")
        
        self.horizontalLayout_CARD.addWidget(tabWidget_right)                
        
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
    
    ui = DatcomBaseUI()
    ui.setupUi(card)
    if hasattr(ui,'NALT'):
        print(ui.NALT)
    if card.findChild(QtWidgets.QLineEdit,'NALT'):
        print(card.findChild(QtWidgets.QLineEdit,'NALT'))
    card.show()
    sys.exit(app.exec_())
