"""
Module implementing DatcomTableBase.
可以作为所有CARD的基类，提供DatcomTableBase操作
"""
from PyQt5.QtCore import  Qt, pyqtSlot, QPoint , QMetaObject, pyqtSignal
from PyQt5.QtWidgets import QAction , QTableWidgetItem, QTableWidget, QMenu
from PyQt5.QtGui import  QIcon, QPixmap#,QDoubleValidator, QIntValidator, QValidator
from PyDatcomLab.Core.DictionaryLoader import  defaultDatcomDefinition as DDefine
import logging
from PyDatcomLab.Core import dcModel
from PyDatcomLab.GUIs.PlaneConfiguration import card_ico_rc, card_rc_rc

class DatcomTableBase(QTableWidget):
    """
    class DatcomCARD 是提供CARD录入的基础类
    """
    Signal_rowCountChanged = pyqtSignal(str,int)      #处理变量组个的同步问题
    Singal_RuleNumToCount  = pyqtSignal(int)          #用来接收外部的表格长度变化信号

    
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
        
        
        #附加初始化过程
        self.InitializeContextMenu() #配置内容菜单
        self.setContextMenuPolicy(Qt.CustomContextMenu)
        self.customContextMenuRequested.connect(self.on_customContextMenuRequested)
        
        #绑定执行逻辑 
        self.Singal_RuleNumToCount.connect(self.on_Singal_RuleNumToCount)
        
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
        self.varsDf   = {}
        self.groupDf  = {}
        self.varsDfList = []  #顺序保存的所有变量的定义，用以关联表头
        self.maxCount = 20
        self.minCount = 0
        self.CountVar = None  #表格行数对应的变量名
        self.ComboVar = None  #表格列组合对应的附加变量名
        self.ComboRule = None #表格列组合对应的规则
    
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

    def InitializeHeader(self):
        """
        根据定义初始化表头，添加表格的列消隐关系
        """
        if self.varsDfList == []:return
        self.setColumnCount(len(self.varsDfList))
        tHeader = []
        for iV in self.varsDfList:
            if "DisplayName" in iV.keys():
                tHeader.append(iV['DisplayName'])
            else:
                tHeader.append(iV['VarName'])   
        self.setHorizontalHeaderLabels(tHeader) 
        
    def setDtModelData(self, tModel):
        """
        从tModel加载数据
        """
        if tModel is None or type(tModel) != dcModel.dcModel:
            return 
        self.clear()
        self.InitializeHeader()
        for iC  in range(0, len(self.varsDfList)):
            iV = self.varsDfList[iC]
            tDataVar = tModel.getNamelistVar(self.Namelist,iV['VarName'])            
            if tDataVar is None: continue
            tData = tDataVar['Value']
            if self.rowCount() < len(tData): self.setRowCount(len(tData))
            if len(tData) in range(self.minCount, self.maxCount):
                for iR in range(0, len(tData)):
                    self.setItem(iR, iC, QTableWidgetItem(str(tData[iR])))
            else:
                self.logger.error("加载表格%s数据越界：%d min：%d max：%d"%(self.GroupName,len(tData), 
                               self.minCount, self.maxCount ))
        #发送行变更消息
        self.Signal_rowCountChanged.emit(self.CountVarUrl , self.rowCount())
            
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
                        tText = self.item(iR, iC).text()
                        if tText == '':
                            self.logger.error("输入数据%s不合法 R：%d，C：%d"%(tText, iR, iC))
                        else:                            
                            tVarlist.append(float(tText))   
                tModel.setNamelist( self.Namelist , iV['VarName'],{'Index':1, 'Value':tVarlist} )
        #读取数据完成
       
    def on_Singal_RuleIndexToCombo(self, nmlst, vCombo,  tIndex):
        """
        将tIndex对应的列隐藏
        这里应统一使用vCombo的Range中的值作为索引
        """        
        if self.ComboRule is None or self.Namelist  != nmlst or self.ComboVar != vCombo :
            return
        tKey = '%d.0'%(tIndex + 1)
        if tKey not in self.ComboRule.keys():
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
       
        
 
        
