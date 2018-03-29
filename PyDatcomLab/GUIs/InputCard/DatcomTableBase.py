"""
Module implementing DatcomTableBase.
可以作为所有CARD的基类，提供DatcomTableBase操作
"""
from PyQt5 import QtWidgets
from PyQt5.QtCore import  Qt, pyqtSlot, QPoint , QMetaObject, pyqtSignal
from PyQt5.QtWidgets import QAction , QTableWidgetItem, QTableWidget, QMenu
from PyQt5.QtGui import  QIcon, QPixmap#,QDoubleValidator, QIntValidator, QValidator
from PyDatcomLab.Core.DictionaryLoader import  defaultDatcomDefinition as DDefine
import logging
from PyDatcomLab.Core import dcModel
from PyDatcomLab.Core import datcomDimension as dtDimension
from PyDatcomLab.Core.datcomDimension import Dimension
from PyDatcomLab.GUIs.PlaneConfiguration import card_ico_rc, card_rc_rc
from PyDatcomLab.GUIs.InputCard.DatcomInputDelegate import DatcomInputContinuousDelegate as CDelegate


class DatcomTableBase(QTableWidget):
    """
    class DatcomCARD 是提供CARD录入的基础类
    """
    Signal_rowCountChanged      = pyqtSignal(str,int)      #向外部通知表格长度发生了变化
    Singal_RuleNumToCount       = pyqtSignal(int)          #用来接收外部的表格长度变化信号
    Singal_variableComboChanged = pyqtSignal(str , str)    #向外部通知表格中激活的列组合关系发生变化  <self.vUrl,"[]">

    
    def __init__(self, iNameList, iGroup , iDefine = DDefine, parent = None ):
        """
        初始化所有必需的操作
        """    
        super(DatcomTableBase, self).__init__(parent)
        #创建日志
        self.logger = logging.getLogger(r'Datcomlogger') 
        #验证输入
 
        #界面参数
        self.curPos = QPoint(0, 0)
        #读取配置文件
        self.setDefinition( iNameList, iGroup, iDefine )
        self.setDelegate()
        #指定Header的内容菜单
        #self.horizontalHeader().customContextMenuRequested.connect(self.OnHeaderCustomContextMenuRequested)
        self.horizontalHeader().sectionClicked.connect(self.ForSectionClicked)
        
        #附加初始化过程
        self.InitializeContextMenu() #配置内容菜单
        self.setContextMenuPolicy(Qt.CustomContextMenu)
        self.customContextMenuRequested.connect(self.on_customContextMenuRequested)
        
        #绑定执行逻辑 
        self.Singal_RuleNumToCount.connect(self.on_Singal_RuleNumToCount)
        self.itemChanged.connect(self.onItemChanged)
  
        
        #再次执行绑定
        QMetaObject.connectSlotsByName(self)
    
    def InitializeContextMenu(self):
        """
        右键菜单的初始化代码        
        """
        #New Row
        self.actionAddRow = QAction(self)
        icon = QIcon()
        icon.addPixmap(QPixmap(":/cardIco/rc_card/icos/AddedIcon.ico"), QIcon.Normal, QIcon.Off)
        self.actionAddRow.setIcon(icon)
        self.actionAddRow.setObjectName("actionAddRow")
        self.actionAddRow.setText( "增加行")
        self.actionAddRow.setToolTip( "新增行")
        #Delete Row
        self.actionDeleteRow = QAction(self)
        self.actionDeleteRow.setText("删除行")
        self.actionDeleteRow.setToolTip( "删除一行")
        icon1 = QIcon()
        icon1.addPixmap(QPixmap(":/cardIco/rc_card/icos/DeletedIcon.ico"), QIcon.Normal, QIcon.Off)
        self.actionDeleteRow.setIcon(icon1)
        self.actionDeleteRow.setObjectName("actionDeleteRow")  
        #Add All Row
        self.actionAddRowToMax = QAction(self)
        self.actionAddRowToMax.setText("添加所有行")
        self.actionAddRowToMax.setToolTip( "添加到最大行数")
        icon1 = QIcon()
        icon1.addPixmap(QPixmap(":/cardIco/rc_card/icos/AddedIcon.ico"), QIcon.Normal, QIcon.Off)
        self.actionAddRowToMax.setIcon(icon1)
        self.actionAddRowToMax.setObjectName("actionAddRowToMax")     
        #Delete all Row
        self.actionClearRows = QAction(self)
        self.actionClearRows.setText("删除所有行")
        self.actionClearRows.setToolTip( "删除所有行")
        icon1 = QIcon()
        icon1.addPixmap(QPixmap(":/cardIco/rc_card/icos/DeletedIcon.ico"), QIcon.Normal, QIcon.Off)
        self.actionClearRows.setIcon(icon1)
        self.actionClearRows.setObjectName("actionClearRows")  
        
        #创建菜单
        self.popMenu = QMenu(self)
        #定义
        self.popMenu.addAction(self.actionAddRow)
        self.popMenu.addAction(self.actionDeleteRow)
        self.popMenu.addAction(self.actionAddRowToMax)
        self.popMenu.addAction(self.actionClearRows)

     
    def setDefinition(self, tNameList, tGroup, tDefine ):
        """
        利用tDefine定义的信息，初始化表格信息
        tNameList 指向当前表格承担的Namelist的名称
        tGroup指向表格容纳的所有列
        """
        #判断定义有效性
        if tDefine is None or tNameList is None or tGroup is None: return  
        self.DDefine   = tDefine    
        self.GroupName = tGroup      #对应的变量组的名称  
        self.Namelist  = tNameList   #对应NameList的名称
        self.vUrl      = '%s/%s'%(tNameList,tGroup )
        self.varsDf   = {}
        self.groupDf  = {}
        self.varsDfList = []  #顺序保存的所有变量的定义，用以关联表头
        self.maxCount = 20
        self.minCount = 0
        self.CountVar = None    #表格行数对应的变量名
        self.ComboVar = None    #表格列组合对应的附加变量名
        self.ComboRule = None   #表格列组合对应的规则
        self.ComboVarUrl = None #表格列组合对应的附加变量的Url
    
        #分析组定义
        tVariableDf = self.DDefine.getGroupVarsByName(tNameList, tGroup) #对应数组的定义   
        #分析组定义
        if len(tVariableDf) == 0 :
            self.logger.error("不包含%s对应的定义信息"%(tNameList))
            return
        tGroupDfSet = self.DDefine.getCARDAddtionalInformation(tNameList, 'GroupDefine' )
        if len(tGroupDfSet) == 0 or tGroup not in tGroupDfSet.keys():
            self.logger.error("不包含%s的组信息定义%s对应的定义信息"%(tNameList, tGroup))
            return
        #保存定义
        self.varsDf   = tVariableDf
        #分析定义表头
        self.varsDfList = []
        for iv in tVariableDf.keys():
            self.varsDfList.append(tVariableDf[iv])
        self.groupDf  = tGroupDfSet[tGroup]
        #设置表头
        self.InitializeHeader()
        #分析表格行数限制
        self.maxCount  = self.DDefine.getGroupLimitByName(tNameList, tGroup)[1]
        self.minCount  = self.DDefine.getGroupLimitByName(tNameList, tGroup)[0]
        #分析表格行数控制变量的结果
        tCountVar       = self.DDefine.gettRuleNumToCountByGroup(tNameList, tGroup)
        if tCountVar is not None : 
            self.CountVar = tCountVar
        self.CountVarUrl = '%s/%s'%(self.Namelist, self.CountVar)
        #分析表头协同变量结果
        tComboVar       = self.DDefine.getRuleIndexToComboByGroup(tNameList, tGroup)
        if tComboVar is not None and  len(tComboVar) > 0: 
            self.ComboVar  = tComboVar['Index']
            self.ComboRule = tComboVar['HowTo']
            self.ComboVarUrl = '%s/%s'%(self.Namelist, self.ComboVar)
            

    def InitializeHeader(self):
        """
        根据定义初始化表头，添加表格的列消隐关系
        将表格附加定义信息添加到HorizontalHeader中
        """
        if self.varsDfList == []:return
        self.setColumnCount(len(self.varsDfList))
        #执行表头初始化
        for iC in range(0, len(self.varsDfList)) :

            tConfig = self.varsDfList[iC]
            if "DisplayName" in tConfig.keys():
                tDisplay = tConfig["DisplayName" ]
            else:
                tDisplay = tConfig["VarName" ]
            tHItem = QTableWidgetItem(tDisplay)
            if 'Dimension' in  tConfig.keys():
                tMU = dtDimension.getMainUnitByDimension(tConfig['Dimension'])  
                tConfig['CurrentUnit'] = tMU
                tDisplay = '%s %s'%(tDisplay,tMU )   
            
            tHItem.setData(Qt.DisplayRole, tDisplay)
            tHItem.setData(Qt.UserRole, tConfig)
            self.setHorizontalHeaderItem(iC,tHItem )
            
        
 
 
    def setDelegate(self):
        """
        为各列设置代理
        """
        for iC in range(0, self.columnCount()):            
            tUrl = '%s/%s'%(self.Namelist, self.varsDfList[iC]['VarName'])
            self.setItemDelegateForColumn(iC, CDelegate(tUrl, parent = self, tDDefine = self.DDefine ) )
       
        
    def setDtModelData(self, tModel):
        """
        从tModel加载数据
        执行setDtModelData将导致所有数据包括表头被重置
        """
        if tModel is None or type(tModel) != dcModel.dcModel:
            return 
        #清除所有数据
        self.clear()
        self.InitializeHeader()
        #分析写入数据
        for iC  in range(0, len(self.varsDfList)):
            iV = self.varsDfList[iC]
            tDataVar = tModel.getNamelistVar(self.Namelist,iV['VarName'])            
            if tDataVar is None:
                #不存在数据则隐藏对应的列
                self.setColumnHidden(iC, True)
                self.logger.info("%s的列%s没有数据，不显示"%(self.vUrl, self.horizontalHeaderItem(iC).text()))
                continue
            #执行表头坐标同步
            tDimension = ''
            if 'Dimension' in self.varsDfList[iC]:
                tDimension = self.varsDfList[iC]['Dimension'] 
            tUnit = ''
            if 'Unit' in tDataVar.keys():
                tUnit = tDataVar['Unit']
                self.setHorizontalHeaderUnit(iC,tUnit )
            else:
                tUnit = dtDimension.getMainUnitByDimension(tDimension)
                #self.logger.info("数据格式异常，缺少单位信息")
            #定义本列的魔板
            tDataTemplate  = {'Dimension':tDimension, 'Unit':tUnit, 'Value':None}
                

            #执行数据写入
            tData = tDataVar['Value']
            if self.rowCount() != len(tData): 
                self.logger.info("%s加载数据过程中表格长度%d与数据长度%d不同，修改表格长度"%(self.vUrl,self.rowCount(), len(tData) ))
                self.setRowCount(len(tData))
            if len(tData) in range(self.minCount, self.maxCount):                   
                for iR in range(0, len(tData)):
                    tItem  = QTableWidgetItem(str(tData[iR]))
                    tDataUserRole = tDataTemplate.copy()
                    tDataUserRole['Unit']  = tUnit
                    tDataUserRole['Value'] =  tData[iR]
                    tItem.setData( Qt.UserRole,tDataUserRole )
                    #tItem.setData(Qt.DisplayRole,str(tData[iR]) )
                    self.setItem(iR, iC, tItem)
                self.setColumnHidden(iC, False)
            else:
                self.logger.error("加载表格%s数据越界：%d min：%d max：%d"%(self.GroupName,len(tData), 
                               self.minCount, self.maxCount ))
        #发送行变更消息
        self.Signal_rowCountChanged.emit(self.CountVarUrl , self.rowCount())         #向外通知数据加载后的长度
        self.Singal_variableComboChanged.emit(self.vUrl, str(self.getColumnCombo())) #向外通知数据列的组合关系发生变换



    def getColumnCombo(self):
        """
        返回当前采用的变量组合关系
        """
        tShowColumnList = []
        for iC in range(0, self.columnCount()):
            if not self.isColumnHidden(iC):
                tShowColumnList.append(self.varsDfList[iC]['VarName'])
        return tShowColumnList
        
    def getDtModelData(self, tModel):
        """
        设置tModel中的数据
        考虑两个因素：1变量是否可见，2变量类型
        
        """
        for iC in range(0, len(self.varsDfList)):
            #遍历所有变量的定义
            iV = self.varsDfList[iC]
            if self.isColumnHidden(iC):
                #True is Hidden Delete the Variable from the Model
                tModel.setNamelist( self.Namelist , iV['VarName'], None)
            else:
                #False : warite the data
                tVarlist = []
                if 'SubType' in iV.keys() and iV['SubType'] == 'BOOL':
                    for iR in range(0, self.rowCount()):
                        tText = self.item(iR, iC).text()
                        if tText == '.FALSE.':
                            tVarlist.append(False)
                        elif tText == '.TRUE.':
                            tVarlist.append(True)
                        else:
                            self.logger.error("输入数据%s不合法"%tText)
                else:
                    #值校验认为由控件已经完成了
                    #单位换算认为已经由控件换算完成了
                    for iR in range(0, self.rowCount()):
                        tData = self.item(iR, iC).data(Qt.UserRole)
                        #这里应该执行一次统一的坐标变换
                        if tData is None or tData['Value'] is None:
                            self.logger.error("输入数据%s不合法 R：%d，C：%d"%(self.item(iR, iC).text(), iR, iC))
                        else:                            
                            tVarlist.append(tData['Value']) 
                #此处传递了CurrentUnit给Model
                
                if 'CurrentUnit' in self.horizontalHeaderItem(iC).data(Qt.UserRole).keys():
                    tUnit = self.horizontalHeaderItem(iC).data(Qt.UserRole)['CurrentUnit']
                else:
                    tUnit = ''
                tModel.setNamelist( self.Namelist , iV['VarName'],{'Index':1, 'Value':tVarlist, 'Unit':tUnit} )
        #读取数据完成
       
       
    def on_Singal_RuleIndexToCombo(self, senderUrl,  choisedKey):
        """
        将tIndex对应的列隐藏
        这里应统一使用vCombo的Range中的值作为索引
        
        @param senderUrl 发送者的Url用来识别此消息是否需要被处理 对应
        @type QWidget
        """        
        if  self.ComboVarUrl  != senderUrl  :
            return
        tKey = choisedKey
        if self.ComboRule is None or tKey not in self.ComboRule.keys():
            self.logger.info("尝试不存在的组合关系！%s"%tKey)
            return 
        #分析处理
        tVarCombo = self.ComboRule[tKey]   
        for iC in range(0, self.columnCount()):
            #遍历关闭可见性
            tVarName = self.varsDfList[iC]['VarName']
            if tVarName in tVarCombo:
                self.setColumnHidden(iC, False)
            else:
                self.setColumnHidden(iC, True)
        #print("row:%d,col:%d"%(self.rowCount(), self.columnCount()))
    
    @pyqtSlot(str, str)     
    def on_TbLength_editingFinished(self, vUrl, vCount):
        """
        响应表格长度变化事件
        也可以用来设置表格长度
        """
        tUrl = "%s/%s"%(self.Namelist, self.CountVar)
        if tUrl == vUrl:
            try:
                tNum = int(vCount)
                self.on_Singal_RuleNumToCount(tNum)
            except Exception as e1:
                self.logger.error("变量：%s 的值：%s ，无法转换为Int"%(vUrl, vCount))

        
    @pyqtSlot(int)    
    def on_Singal_RuleNumToCount(self, tNum):
        """
        """
        if tNum >= self.minCount and tNum <= self.maxCount  and  tNum >= self.rowCount():
            self.setRowCount(tNum)
        else:
            self.logger.error("无法将表格的行数设置为%d,当前%d"%(tNum,self.rowCount() ))
            self.Signal_rowCountChanged.emit(self.CountVarUrl, self.rowCount())

        
        
    @pyqtSlot()
    def on_actionAddRow_triggered(self):
        """
        增加新行的代码.
        """
        # TODO: not implemented yet
        #添加行
        aItem = self.indexAt(self.curPos) #认为是表格 ，否则会异常
        rowIndex = 0
        if aItem.row() == -1 :
            #没有命中
            rowIndex = self.rowCount()
        else:
            rowIndex = aItem.row()

        if self.rowCount() <self.maxCount:
            self.insertRow(rowIndex)
        else:
            self.logger.info("%s已经达到最大行数不能添加"%self.objectName())
            
        #self.curN.setText(str(self.rowCount()))
        self.Signal_rowCountChanged.emit(self.CountVarUrl, self.rowCount())

    @pyqtSlot()
    def on_actionAddRowToMax_triggered(self):
        """
        增加到最大行的代码.
        """
        #添加行
        if self.rowCount() < self.maxCount:
            self.setRowCount(self.maxCount)

        self.Signal_rowCountChanged.emit(self.CountVarUrl, self.rowCount())        

    
    @pyqtSlot()
    def on_actionDeleteRow_triggered(self):
        """
        删除行的代码.
        """
        # TODO: not implemented yet
        aItem = self.indexAt(self.curPos)
        if  aItem.row() >= 0 :            
            self.removeRow(aItem.row())
        else:
            self.logger.info("没有命中任何行")
        
        self.Signal_rowCountChanged.emit(self.CountVarUrl, self.rowCount())

    @pyqtSlot()
    def on_actionClearRows_triggered(self):
        """
        删除行的代码.
        """
        # TODO: not implemented yet
        #self.clear()
        self.setRowCount(self.minCount)        
        self.Signal_rowCountChanged.emit(self.CountVarUrl, self.rowCount())         
        
            
    @pyqtSlot(QPoint)
    def on_customContextMenuRequested(self, pos):
        """
        Slot documentation goes here.
        
        @param pos DESCRIPTION
        @type QPoint
        """   
        self.curPos = pos        
        posG = self.mapToGlobal(pos)
        self.popMenu.exec(posG)
       


   
    def ForSectionClicked(self, vIndex):
        """
        设置表头的内容菜单.
        
        @param pos DESCRIPTION
        @type QPoint
        """   
        
        tHItem = self.horizontalHeaderItem(vIndex)
        tConfig = tHItem.data(Qt.UserRole)
        tDisplay = tHItem.data(Qt.DisplayRole)
        #执行表头的单位制变换操作
        if 'Dimension' in tConfig.keys() and tConfig['Dimension'] in Dimension.keys() \
        and 'CurrentUnit' in tConfig.keys():
            #获取当前单位
            tCUnit = tConfig['CurrentUnit']
            tUList = dtDimension.getUnitListByDimension(tConfig['Dimension'])
            tCUnitIndex = tUList.index(tCUnit)
            tNextUnit = tUList[tCUnitIndex +1] if tCUnitIndex < len(tUList) -1 else tUList[0]
            tDisplay = tDisplay.split()[0] + ' ' + tNextUnit
            tConfig['CurrentUnit'] = tNextUnit
            #更新表头的显示效果
            tHItem.setData(Qt.DisplayRole, tDisplay )
            tHItem.setData(Qt.UserRole, tConfig )
            #更新表格的数据
            self.unitChanged(vIndex, tNextUnit)
    
    def setHorizontalHeaderUnit(self, vIndex, newUnit):
        """
        设置表头的单位属性，以同步数据设置操作
        """
        if newUnit is None or newUnit == '': return 
        #获取数据
        tHItem = self.horizontalHeaderItem(vIndex)
        tConfig = tHItem.data(Qt.UserRole)
        if 'Dimension' not in tConfig.keys() :return 
        tUList = dtDimension.getUnitListByDimension(tConfig['Dimension'])
        
        if tUList is None or newUnit not in  tUList:
            self.logger.error("想要设置的单位：%s 并不在当前的Dimension:%s定义内"%(newUnit, tConfig['Dimension']))
            return 
        tConfig['CurrentUnit'] = newUnit
        #更新表头的显示效果
        tHItem.setData(Qt.UserRole, tConfig )
        tDisplay = tHItem.data(Qt.DisplayRole).split()[0] + ' ' + newUnit
        tHItem.setData(Qt.DisplayRole, tDisplay )

    
    def unitChanged(self, column, tNewUnit):
        """
        将某一列数据的单位进行变换
        """
        if column < 0 : return
        #遍历所有的行
        for iR in range(0,  self.rowCount()):
            tItem = self.item(iR, column)
            if tItem is None:
                continue
            #执行坐标变换
            tUserData = tItem.data(Qt.UserRole)
            if tUserData['Unit'] == tNewUnit:
                continue
            tNewValue  = dtDimension.unitTransformation(tUserData,tNewUnit )
            if tNewValue is not None:
                tItem.setData(Qt.UserRole, tNewValue)
                tItem.setData(Qt.DisplayRole, str(tNewValue['Value']))
    
    @pyqtSlot(QTableWidgetItem)
    def onItemChanged(self, item):
        """
        void QTableWidget::itemChanged(QTableWidgetItem *item)
This signal is emitted whenever the data of item has changed.
        在此处协调代理自行变换单位的情况
        """
        #判断列结论
        tConfig = self.horizontalHeaderItem(item.column()).data(Qt.UserRole)
        if 'Dimension' not in tConfig.keys() or 'CurrentUnit'  not in tConfig.keys():
            return 
        #判断值结论
        tItemData = item.data(Qt.UserRole)
        if tItemData is None or 'Dimension' not in tItemData.keys()  \
                    or tItemData['Dimension'] == ''  or\
                    'Unit' not in tItemData.keys() or \
                    tItemData['Unit'] == '':
            return 
        if tItemData['Unit']  != tConfig['CurrentUnit']:
            tNewItemData = dtDimension.unitTransformation(tItemData, tConfig['CurrentUnit'])
            item.setData(Qt.UserRole, tNewItemData)
            item.setData(Qt.DisplayRole, str(tNewItemData['Value']))
        
        
        

        
        

