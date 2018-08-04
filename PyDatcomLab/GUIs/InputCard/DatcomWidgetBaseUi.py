# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'E:\Projects\PyDatcomLab\PyDatcomLab\GUIs\PlaneConfiguration\ASYFLP.ui'
#
# Created by: PyQt5 UI code generator 5.9.1
#
# WARNING! All changes made in this file will be lost!

from PyQt5 import QtCore, QtWidgets

from PyQt5.QtWebEngineWidgets import QWebEngineView
#from PyDatcomLab.Core.datcomDefine import groupDefine #,  Dimension
from PyDatcomLab.GUIs.InputCard.DatcomInputTable import DatcomInputTable  as TB
from PyDatcomLab.GUIs.InputCard.DatcomInputComboChooser import DatcomInputComboChooser
from PyDatcomLab.GUIs.InputCard import DatcomInputSingle as SInput,  DatcomInputList as LInput
#from PyDatcomLab.Core.DictionaryLoader import  defaultDatcomDefinition as DDefine
#from PyDatcomLab.Core.DictionaryLoader import   DTdictionary

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
        CARD.setObjectName(CARD.Namelist)
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
        #计算输入控件的分组信息
        tGroupVars , tNormalLifts, tRuleLifts , tLiftGroupNum = CARD._getInputWidgetClassification()
        tAllTabKey = list(set(list(tNormalLifts.keys())+list(tRuleLifts.keys()))) #获得所有的分组信息
        if tLiftGroupNum == 1:
            #如果一个分组可以容纳
            self.Widget_lift = QtWidgets.QGroupBox(self.splitter_H1)
            self.Widget_lift.setTitle("")
            self.Widget_lift.setObjectName("groupBox_lift")
            tLiftLayout = QtWidgets.QVBoxLayout(self.Widget_lift)
            tLiftLayout.setObjectName("LiftLayout%d"%(1))
            #指向顶级控件
            #tGroupWidget = self.Widget_lift
            #添加tNormalLifts
            for iT in tNormalLifts:
                #遍历iT
                for iV in tNormalLifts[iT]:
                    #遍历所有的变量
                    self._addWidgetToLayout(CARD,tLiftLayout,  iV)
            #添加tRuleLifts
            for iT in tRuleLifts:
                #遍历iT
                for iV in tRuleLifts[iT]:
                    #遍历所有的变量
                    self._addWidgetToLayout(CARD,tLiftLayout,  iV)

            #创建下方的空间分割器
            spacerItem_LiftBottom = QtWidgets.QSpacerItem(20, 40, QtWidgets.QSizePolicy.Minimum, QtWidgets.QSizePolicy.Expanding)
            tLiftLayout.addItem(spacerItem_LiftBottom)
        else:
            #需要多个分组的情况
            self.Widget_lift = QtWidgets.QTabWidget(self.splitter_H1)
            self.Widget_lift.setTabPosition(QtWidgets.QTabWidget.West) #设置左侧分页
            #添加tNormalLifts
            for iT in tNormalLifts:
                tIndex =-1
                for tTabId in range(0, self.Widget_lift.tabBar().count()):
                    tData = self.Widget_lift.tabBar().tabData(tTabId)
                    if iT == tData['Key']:
                        tIndex = tTabId
                        break
                #如果没有Tab，则新建Tab
                if tIndex <= -1:
                    #新建Tab
                    tWidget = QtWidgets.QWidget(self)
                    tLiftLayout = QtWidgets.QVBoxLayout(tWidget)
                    tLiftLayout.setObjectName("LiftLayout_%s"%(iT))
                    tIndex = self.Widget_lift.addTab(tWidget, iT)
                    self.Widget_lift.tabBar().setTabData(tIndex, {'Key':iT, 'VariableList':tNormalLifts[iT]})

                #存在有效索引
                tWidget = self.Widget_lift.widget(tIndex)
                if tWidget is None:
                    raise UserWarning("setupUi异常，无法获得对应的Widget")
                #获取布局
                tLiftLayout = tWidget.layout()
                if tLiftLayout is None:
                    raise UserWarning("setupUi异常，无法获得Widget对应的Layout")
                #执行添加
                for iV in tNormalLifts[iT]:
                    #遍历所有的变量
                    self._addWidgetToLayout(CARD,tLiftLayout,  iV)

            #添加tRuleLifts
            for iT in tRuleLifts:
                tIndex =-1
                for tTabId in range(0, self.Widget_lift.tabBar().count()):
                    tData = self.Widget_lift.tabBar().tabData(tTabId)
                    if iT == tData['Key']:
                        tIndex = tTabId
                        break
                #如果没有Tab，则新建Tab
                if tIndex <= -1:
                    #新建Tab
                    tWidget = QtWidgets.QWidget(self)
                    tLiftLayout = QtWidgets.QVBoxLayout(tWidget)
                    tLiftLayout.setObjectName("LiftLayout_%s"%(iT))
                    tIndex = self.Widget_lift.addTab(tWidget, iT)
                    self.Widget_lift.tabBar().setTabData(tIndex, {'Key':iT, 'VariableList':tNormalLifts[iT]})

                #存在有效索引
                tWidget = self.Widget_lift.widget(tIndex)
                if tWidget is None:
                    raise UserWarning("setupUi异常，无法获得对应的Widget")
                #获取布局
                tLiftLayout = tWidget.layout()
                if tLiftLayout is None:
                    raise UserWarning("setupUi异常，无法获得Widget对应的Layout")
                #执行添加
                for iV in tRuleLifts[iT]:
                    #遍历所有的变量
                    self._addWidgetToLayout(CARD,tLiftLayout,  iV)

            #为所有的分组添加分割器
            for iT in tAllTabKey:
                tIndex = -1
                for tTabId in range(0, self.Widget_lift.tabBar().count()):
                    tData = self.Widget_lift.tabBar().tabData(tTabId)
                    if iT == tData['Key']:
                        tIndex = tTabId
                        break
                #如果没有Tab，则新建Tab
                if tIndex <= -1:
                    continue
                #负责识别
                #存在有效索引
                tWidget = self.Widget_lift.widget(tIndex)
                if tWidget is None:
                    self.logger.error("setupUi异常，无法获得对应的Widget")
                    continue
                #获取布局
                tLiftLayout = tWidget.layout()
                if tLiftLayout is None:
                    self.logger.error("setupUi异常，无法获得Widget对应的Layout")
                    continue
                #创建下方的空间分割器
                spacerItem_LiftBottom = QtWidgets.QSpacerItem(20, 40, QtWidgets.QSizePolicy.Minimum, QtWidgets.QSizePolicy.Expanding)
                tLiftLayout.addItem(spacerItem_LiftBottom)

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
        for tGroup in tGroupVars.keys():
            #创建表单  iNamelist, iGroup,  parent=None, iDefine = DDefine , iModel =None
            tTabTable = TB(iNamelist = CARD.Namelist, iGroup = tGroup , iDefine = CARD.dtDefine, parent = CARD, iModel =CARD.dtModel)
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
        CARD._connectSlots() #绑定自定义的信号槽关系
        QtCore.QMetaObject.connectSlotsByName(CARD)

    def _addWidgetToLayout(self, CARD,tLayout,  iVarName):
        """
        创建tUrl对应的控件并添加到Layout

        """
        tUrl = '%s/%s'%(CARD.Namelist, iVarName)
        tVarDefine = CARD.dtDefine.getVariableDefineByUrl(tUrl)

        if 'TYPE' in tVarDefine.keys() and tVarDefine['TYPE'] in ['INT', 'REAL'] :
            # 开始单值工程量创建
            tVarWidget = SInput.DatcomInputSingle(tUrl, parent=self,
                        iModel =CARD.dtModel , iDefinition = CARD.dtDefine )
            tVarWidget.setObjectName(iVarName)
            #限制var的格式
            sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
            sizePolicy.setHorizontalStretch(0)
            sizePolicy.setVerticalStretch(0)
            sizePolicy.setHeightForWidth(tVarWidget.sizePolicy().hasHeightForWidth())
            tVarWidget.setSizePolicy(sizePolicy)
            tLayout.addWidget(tVarWidget)
        if 'TYPE' in tVarDefine.keys() and tVarDefine['TYPE'] in ['List'] :
            # 结束单值工程量创建
            tVarWidget = LInput.DatcomInputList(tUrl, parent = self ,  iModel =CARD.dtModel  , iDefinition = CARD.dtDefine)
            tVarWidget.setObjectName(iVarName)
            #限制var的格式
            sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
            sizePolicy.setHorizontalStretch(0)
            sizePolicy.setVerticalStretch(0)
            sizePolicy.setHeightForWidth(tVarWidget.sizePolicy().hasHeightForWidth())
            tVarWidget.setSizePolicy(sizePolicy)
            tLayout.addWidget(tVarWidget)
        if 'TYPE' not in tVarDefine.keys():
            if 'RuleIndexToCombo' in tVarDefine.keys():
                tWidget = DatcomInputComboChooser(tUrl,
                               parent=CARD, iDefinition = CARD.dtDefine)
                tWidget.setObjectName('Chooser_%s'%iVarName)
                tLayout.addWidget(tWidget)
                #连接变量变化信号到本Widget的转发信号
                #tWidget.varComboChanged.connect(CARD.Singal_RuleIndexToCombo)
                #连接当前转发的表格变化信号
                #CARD.Singal_varComboChangedFromTable.connect(tWidget.on_Singal_varComboChangedFromTable)
        else:
            self.logger.warning("错误的变量分组信息")




    def _connectSlotsByRule(self, tCARD):
        """
        根据定义的Rule来绑定对应的信号槽
        """




        #添加变量组合控制逻辑
        if hasattr(tCARD,'RuleVariableStatus'):
            for iR in tCARD.RuleVariableStatus:#[]
                tControlVar = iR['ControlVar'] #获得控制变量的名称
                if tControlVar not in self.VariableList.keys() :
                    self.logger.warning("emit_Singal_RuleVariableStatus() 尝试使用不在Datcom定义中的变量%s，忽略!"%tControlVar)
                    tCWidget  = tCARD.findChild(QtWidgets.QWidget, "comboBox_%s"%(tControlVar))
                    tCWidget.currentIndexChanged.connect(tCARD.on_RuleVariableStatus_cuurentIndexChanged)
                else:
                    tCWidget  = tCARD.findChild(QtWidgets.QWidget, "%s"%(tControlVar))
                    if tCWidget is None :
                        self.logger.error("变量%s对应的控件不存在"%(iR['ControlVar']))
                        continue
                    #变量num->Table
                    tCWidget.currentIndexChanged.connect(tCARD.on_RuleVariableStatus_dt_triggered)

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
                tCWidget.editingFinished.connect(tTbWidget.on_Singal_RuleNumToCount)
                #表格长度到-NUM
                tTbWidget.Signal_rowCountChanged.connect(tCWidget.on_Signal_rowCountChanged)

        #添加NMACH对表格长度的控制信号
        tGroupByNMACH = tCARD.dtDefine.getRuleNMACHLinkTable(tCARD.Namelist)
        for iG in tGroupByNMACH:
            tTabTable = tCARD.findChild(QtWidgets.QWidget,"tableWidget_%s"%iG)
            if tTabTable is not None:
                #如果该表格需要外部NMACH触发,则转发Signal
                tCARD.Singal_NMACHChanged.connect(tTabTable.on_Singal_NMACHChanged)

        #联结发送NMACH信号的控件
        if tCARD.Namelist == 'FLTCON':
            tWidget = tCARD.findChild(QtWidgets.QWidget,"NMACH")
            if tWidget is None:
                self.logger.error("无法找到FLTCON/NMACH所对应的控件")
            else:
                tWidget.Signal_NMACHChanged.connect(tCARD.Singal_NMACHChanged)

        #添加self.RuleVariableCorrelation规则的相关逻辑
        for iMv in self.RuleVariableCorrelationMasterVList:
            tWidget = tCARD.findChild(QtWidgets.QWidget,"%s"%iMv)
            if tWidget is not None:
                #链接主变量的编辑信号
                tWidget.editingFinished.connect(tCARD.on_MasterVariable_editingFinished)
        for iCv in self.RuleVariableCorrelationConditionVList:
            tWidget = tCARD.findChild(QtWidgets.QWidget,"%s"%iCv) #此处认为必然是Datcom变量
            if tWidget is not None:
                #链接条件变量的选择信号
                tWidget.currentIndexChanged.connect(tCARD.on_ConditionVariable_currentIndexChanged)


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

    sys.exit(app.exec_())
