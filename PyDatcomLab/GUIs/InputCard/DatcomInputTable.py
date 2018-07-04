"""
Module implementing DatcomTableInput
该类提供一组Array类型的数据的输入接口
界面采用QTableWidget类承载
主要接口包括：

"""
from PyQt5 import QtWidgets, QtCore, QtGui
from PyQt5.QtCore import  Qt, pyqtSlot, QPoint , QMetaObject, pyqtSignal
from PyQt5.QtWidgets import QAction , QTableWidgetItem, QMenu, QWidget
from PyQt5.QtGui import  QIcon, QPixmap#,QDoubleValidator, QIntValidator, QValidator
import logging
#项目导入项
from PyDatcomLab.Core.DictionaryLoader import  defaultDatcomDefinition as DDefine
#from PyDatcomLab.Core import dcModel
from PyDatcomLab.Core import  datcomModel as dcModel
from PyDatcomLab.Core import datcomDimension as dtDimension
from PyDatcomLab.Core.datcomDimension import Dimension
from PyDatcomLab.Core import datcomTools as tools
from PyDatcomLab.GUIs.InputCard.DatcomInputDelegate import DatcomInputContinuousDelegate as CDelegate
from PyDatcomLab.GUIs  import PyDatcomLab_rc

class DatcomInputTable(QWidget):
    """
    用于输入Datcom中的Array类型的参数.其中将增加一些特殊的转换逻辑
    """
    currentIndexChanged            = pyqtSignal(str , str)    #将编辑结构发送出去 (Url,index在Range中的具体值）
    Signal_rowCountChanged       = pyqtSignal(str,int)      #向外部通知表格长度发生了变化
    Singal_RuleNumToCount         = pyqtSignal(int)           #用来接收外部的表格长度变化信号
    Singal_variableComboChanged = pyqtSignal(str , str)    #向外部通知表格中激活的列组合关系发生变化  <self.vUrl,"[]">
    #Singal_NMACHChanged           = pyqtSignal(int)          #用来接收NMACH的变化的信号
    
    def __init__(self, iNameList, iGroup,  parent=None, iDefine = DDefine ):
        """
        Constructor
        
        @param parent reference to the parent widget
        @type QWidget
        """
        super(DatcomInputTable, self).__init__( parent = parent)        
        #创建日志
        self.logger = logging.getLogger(r'Datcomlogger')        
        #配置分析
        if iDefine is  None or iNameList is None or iGroup is None:
            self.logger.error("没有有效的配置文件，无法初始化")
            return        
        #读取配置文件
        self.setDefinition( iNameList, iGroup, iDefine )
        #初始化界面
        self.setupUi(self)
        #self.InitializeUILogic()        
        #界面参数
        self.curPos = QPoint(0, 0)

        #设置表头
        self.InitializeHeader()
        self.setDelegate()  #按列设置代理
        #指定Header的内容菜单
        #self.horizontalHeader().customContextMenuRequested.connect(self.OnHeaderCustomContextMenuRequested)
        self.table.horizontalHeader().sectionClicked.connect(self.ForSectionClicked)
        
        #附加初始化过程
        self.InitializeContextMenu() #配置内容菜单
        self.table.setContextMenuPolicy(Qt.CustomContextMenu)
        self.table.customContextMenuRequested.connect(self.on_customContextMenuRequested)
        
        #绑定执行逻辑 
        self.Singal_RuleNumToCount.connect(self.on_Singal_RuleNumToCount)
        self.table.itemChanged.connect(self.onItemChanged) 
        self.table.cellChanged.connect(self.on_cellChanged)
        #self.Singal_NMACHChanged.connect(self.on_Singal_NMACHChanged)
        
        #再次执行绑定
        QMetaObject.connectSlotsByName(self)

    def setDefinition(self, tNameList, tGroup, tDefine ):
        """
        利用tDefine定义的信息，初始化表格信息
        tNameList 指向当前表格承担的Namelist的名称
        tGroup指向表格容纳的所有列
        """
        #判断定义有效性
        if tDefine is None or tNameList is None or tGroup is None:
            self.logger.error("setDefinition()无法分析定义，定义对象缺失！")
            return  
        self.dtDefine   = tDefine    
        self.GroupName = tGroup      #对应的变量组的名称  
        self.Namelist  = tNameList   #对应NameList的名称
        self.vUrl      = '%s/%s'%(tNameList,tGroup )
        self.varsDf   = {}      #所有变量的定义 dict形式
        self.groupDf  = {}
        self.varsDfList = []  #顺序保存的所有变量的定义，用以关联表头
        self.maxCount = 20
        self.minCount = 0
        self.CountVar = None    #表格行数对应的变量名
        self.ComboVar = None    #表格列组合对应的附加变量名
        self.ComboRule = None   #表格列组合对应的规则
        self.ComboVarUrl = None #表格列组合对应的附加变量的Url
    
        #分析组定义
        tVariableDf = self.dtDefine.getGroupVarsByName(tNameList, tGroup) #组内所有变量的定义   
        #分析组定义
        if len(tVariableDf) == 0 :
            self.logger.error("不包含%s对应的定义信息"%(self.vUrl ))
            return
        tGroupDfSet = self.dtDefine.getCARDAddtionalInformation(tNameList, 'GroupDefine' )
        if len(tGroupDfSet) == 0 or self.GroupName  not in tGroupDfSet.keys():
            self.logger.error("不包含%s的组信息定义%s对应的定义信息"%(tNameList, tGroup))
            return
        #保存定义
        self.varsDf   = tVariableDf
        #分析定义表头
        self.varsDfList = []
        for iv in tVariableDf.keys():
            self.varsDfList.append(tVariableDf[iv])
        self.groupDf  = tGroupDfSet[tGroup]

        #分析表格行数限制
        self.maxCount  = self.dtDefine.getGroupLimitByName(tNameList, tGroup)[1]
        self.minCount  = self.dtDefine.getGroupLimitByName(tNameList, tGroup)[0]
        #分析表格行数控制变量的结果
        tCountVar       = self.dtDefine.gettRuleNumToCountByGroup(tNameList, tGroup)
        if tCountVar is not None : 
            self.CountVar = tCountVar
        self.CountVarUrl = '%s/%s'%(self.Namelist, self.CountVar)
        #分析表头协同变量结果
        tComboVar       = self.dtDefine.getRuleIndexToComboByGroup(tNameList, tGroup)
        if tComboVar is not None and  len(tComboVar) > 0: 
            self.ComboVar  = tComboVar['Index']
            self.ComboRule = tComboVar['HowTo']
            self.ComboVarUrl = '%s/%s'%(self.Namelist, self.ComboVar)
        #分析是否关联到NMACH限制因素
        self.isLinkNMACH = self.dtDefine.isLinkToNMACH(tNameList, tGroup)
        

        
    def setupUi(self, Form):
        """
        配置界面元素
        """        
        Form.setObjectName(self.GroupName )
        self.verticalLayout = QtWidgets.QVBoxLayout(Form)
        self.verticalLayout.setContentsMargins(1, 1, 1, 1)
        self.verticalLayout.setSpacing(2)
        self.verticalLayout.setObjectName("TopLayout")
       #create the tablewidget
        self.table = QtWidgets.QTableWidget( self)
        self.table.setObjectName("tableWidget_%s"%self.GroupName)
        #设置表格的大小策略
        sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Expanding, QtWidgets.QSizePolicy.Expanding)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.table.sizePolicy().hasHeightForWidth())
        self.table.setSizePolicy(sizePolicy)
        self.table.setMaximumSize(QtCore.QSize(16777215, 16777215))        
        self.verticalLayout.addWidget(self.table)
        #执行其他逻辑
        self.retranslateUi(Form)        
        QtCore.QMetaObject.connectSlotsByName(Form)
        
    def retranslateUi(self, Form):
        #_translate = QtCore.QCoreApplication.translate
        pass

    
    def InitializeContextMenu(self):
        """
        右键菜单的初始化代码        
        """
        #New Row
        self.actionAddRow = QAction(self)
        icon = QIcon()
        icon.addPixmap(QPixmap(":/InputCard/images/InputCard/addLine.ico"), QIcon.Normal, QIcon.Off)
        self.actionAddRow.setIcon(icon)
        self.actionAddRow.setObjectName("actionAddRow")
        self.actionAddRow.setText( "增加行")
        self.actionAddRow.setToolTip( "新增行")
        #Delete Row
        self.actionDeleteRow = QAction(self)
        self.actionDeleteRow.setText("删除行")
        self.actionDeleteRow.setToolTip( "删除一行")
        icon1 = QIcon()
        icon1.addPixmap(QPixmap(":/InputCard/images/InputCard/deleteLine.ico"), QIcon.Normal, QIcon.Off)
        self.actionDeleteRow.setIcon(icon1)
        self.actionDeleteRow.setObjectName("actionDeleteRow")  
        #Add All Row
        self.actionAddRowToMax = QAction(self)
        self.actionAddRowToMax.setText("添加所有行")
        self.actionAddRowToMax.setToolTip( "添加到最大行数")
        icon1 = QIcon()
        icon1.addPixmap(QPixmap(":/InputCard/images/InputCard/addLine.ico"), QIcon.Normal, QIcon.Off)
        self.actionAddRowToMax.setIcon(icon1)
        self.actionAddRowToMax.setObjectName("actionAddRowToMax")     
        #Delete all Row
        self.actionClearRows = QAction(self)
        self.actionClearRows.setText("删除所有行")
        self.actionClearRows.setToolTip( "删除所有行")
        icon1 = QIcon()
        icon1.addPixmap(QPixmap(":/InputCard/images/InputCard/deleteLine.ico"), QIcon.Normal, QIcon.Off)
        self.actionClearRows.setIcon(icon1)
        self.actionClearRows.setObjectName("actionClearRows")  
        
        #创建菜单
        self.popMenu = QMenu(self.table)
        #定义
        self.popMenu.addAction(self.actionAddRow)
        self.popMenu.addAction(self.actionDeleteRow)
        self.popMenu.addAction(self.actionAddRowToMax)
        self.popMenu.addAction(self.actionClearRows)

     

    def InitializeHeader(self):
        """
        根据定义初始化表头，添加表格的列消隐关系
        将表格附加定义信息添加到HorizontalHeader中
        """
        if self.varsDfList == []:return
        self.table.setColumnCount(len(self.varsDfList))
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
            self.table.setHorizontalHeaderItem(iC,tHItem )        
            
    def setDelegate(self):
        """
        为各列设置代理
        """
        for iC in range(0, self.table.columnCount()):            
            tUrl = '%s/%s'%(self.Namelist, self.varsDfList[iC]['VarName'])
            self.table.setItemDelegateForColumn(iC, CDelegate(tUrl, parent = self, tDDefine = self.dtDefine ) )
       
    def clear(self):
        """
        clear the table context
        """
        self.table.clear()
        
    def loadData(self, iModel):
        """
        将iModel定义的数据加载到控件以方便的编辑 iModel-> self
        函数行为:
        1. 根据Namelist 和 Group的值从iModel中读取对应的信息
        2. 加载所有Group中指定的Variable，从AddtionalInformation设定显示规则
        3. 调整对应变量的单位
        4. 如果输入值不合法进行背景标红提示
        """
        
        if iModel is None or type(iModel) != dcModel.dcModel:
            self.logger.info("尝试传递非dcModel对象给DatcomInputTable的loadData函数，Type：%s"%str(type(iModel)))
            return 
        #清除所有数据
        self.clear()
        self.InitializeHeader()   
        #分析写入数据
        for iC  in range(0, len(self.varsDfList)):
            iV       = self.varsDfList[iC]   #这是所有的定义
            tUrl     = '%s/%s'%(iV['NameList'], iV['VarName'])
            tDataVar = iModel.getVariableByUrl(tUrl)            
            if tDataVar is None:
                #不存在数据则隐藏对应的列
                self.table.setColumnHidden(iC, True)
                #self.logger.info("%s的列%s没有数据，不显示"%(self.vUrl, self.table.horizontalHeaderItem(iC).text()))
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
            if tData is None :continue
            if self.table.rowCount() != len(tData):                 
                #这里应该是比较大的那个值
                if 'Limit' in iV.keys() :
                    if len(tData) < iV['Limit'][0]:
                        self.logger.info("%s加载数据过程中,数据长度%d小于下限限制%d，修改表格长度为下限"%(self.vUrl,len(tData), iV['Limit'][0] ))        
                        self.table.setRowCount( iV['Limit'][0])
                    if len(tData) > iV['Limit'][1]:
                        self.logger.error("%s加载数据过程中,数据长度%d大于上限限制%d，修改表格长度为上限"%(self.vUrl,len(tData), iV['Limit'][1] ))        
                        self.table.setRowCount( iV['Limit'][1])
            if len(tData) in range(self.minCount, self.maxCount):     
                for iR in range(0, len(tData)):
                    tItem  = QTableWidgetItem(str(tData[iR]))
                    tDataUserRole = tDataTemplate.copy()
                    tDataUserRole['Unit']  = tUnit
                    tDataUserRole['Value'] =  tData[iR]
                    tItem.setData( Qt.UserRole,tDataUserRole )
                    #tItem.setData(Qt.DisplayRole,str(tData[iR]) )
                    self.table.setItem(iR, iC, tItem)
                self.table.setColumnHidden(iC, False)
            else:
                self.logger.error("加载表格%s数据越界：%d min：%d max：%d"%(self.GroupName,len(tData), 
                               self.minCount, self.maxCount ))
        #发送行变更消息
        self.Signal_rowCountChanged.emit(self.CountVarUrl , self.table.rowCount())         #向外通知数据加载后的长度
        self.Singal_variableComboChanged.emit(self.vUrl, str(self.getColumnCombo())) #向外通知数据列的组合关系发生变换

    def saveData(self, iModel):
        """
        将控件的编辑结果保存到iModel中 self->iModel
        函数行为:
        1. 如果列被隐藏,则将Value= None 的变量值写入到iModel
        2. 变量的基础模板从DDefine中获得
        3. Datom中Array类型包括两种 REAL和 String['.TRUE.','.FALSE.'],所以需要区别对待
        """

        for iC in range(0, len(self.varsDfList)):
            #遍历所有变量的定义
            iV = self.varsDfList[iC]
            tUrl = '%s/%s'%(self.Namelist , iV['VarName'])  #
            #分析
            if self.table.isColumnHidden(iC):
                #True is Hidden Delete the Variable from the Model
                tVar = self.dtDefine.getVariableTemplateByUrl(tUrl)
                tVar['Value'] = None
                iModel.setVariable( tVar)
            else:
                #False : warite the data
                tVarlist = []
                for iR in range(0, self.table.rowCount()): 
                    #如果没有输入值则利用定义的默认值
                    if self.table.item(iR, iC) is None:  
                        self.logger.error("表格%s在R：%d，C：%d的输入不应为空"%(self.vUrl, iR, iC))
                        tData = self.dtDefine.getVariableTemplateByUrl(tUrl, True)  #查询Array的子项的默认值
                        #self.table.item(iR, iC).setBackground(QtGui.QBrush(QtGui.QColor(255, 0, 0)))
                    else:
                        #如果输入了则使用输入值                    
                        if 'SubType' in iV.keys() and iV['SubType'] == 'BOOL':
                            tText = self.table.item(iR, iC).text()
                            if tText == '.FALSE.':
                                tVarlist.append(False)
                            elif tText == '.TRUE.':
                                tVarlist.append(True)
                            else:
                                self.logger.error("DatcomInputTable.saveData()输入数据%s不合法"%tText)
                        else:
                            #值校验认为由控件已经完成了
                            #单位换算认为已经由控件换算完成了
                            tData = self.table.item(iR, iC).data(Qt.UserRole)
                        
                    #将结果写入到序列
                    if tData is None or tData['Value'] is None:
                        self.logger.error("DatcomInputTable.saveData()输入数据%s不合法 R：%d，C：%d"%(self.table.item(iR, iC).text(), iR, iC))
                    else:                            
                        tVarlist.append(tData['Value']) 
                #此处传递了CurrentUnit给Model
                
                if 'CurrentUnit' in self.table.horizontalHeaderItem(iC).data(Qt.UserRole).keys():
                    tUnit = self.table.horizontalHeaderItem(iC).data(Qt.UserRole)['CurrentUnit']
                else:
                    tUnit = ''
                tVar = self.dtDefine.getVariableTemplateByUrl(tUrl)
                tVar['Unit']  = tUnit
                tVar['Value'] = tVarlist
                iModel.setVariable( tVar )
        #回写iModel完成

    def getColumnCombo(self):
        """
        返回当前采用的变量组合关系
        """
        tShowColumnList = []
        for iC in range(0, self.table.columnCount()):
            if not self.table.isColumnHidden(iC):
                tShowColumnList.append(self.varsDfList[iC]['VarName'])
        return tShowColumnList      
        
    @pyqtSlot(int, int)    
    def on_cellChanged(self, row,  column ):  
        """
        响应cellChanged，验证新值是否合法
        """
        if row <0 or column <0:
            return
        #获得变量定义
        tUrl = '%s/%s'%(self.Namelist ,self.varsDfList[column]['VarName'])  
        tVarDf = self.dtDefine.getVariableDefineByUrl(tUrl)
        #tVarTmp = self.dtDefine.getVariableTemplateByUrl(tUrl, True)
        tItem = self.table.item( row,  column)
        if tItem is None :
            #本身数据是不存在
            return
            
            
        if 'SubType' in tVarDf.keys() and  tVarDf['SubType'] in ['BOOL', 'LIST']:
            #如果是离散值的验证
            pass
        else:
            tVd = QtGui.QDoubleValidator()
            tVd.setObjectName('DoubleValidator')
            #tVd.setNotation(QtGui.QDoubleValidator.StandardNotation) #否则无法限制
            if 'Range' in tVarDf.keys():
                tRange = tVarDf['Range']
                if  tools.isNotNanInf(tRange[0]) :
                    tVd.setBottom(tRange[0])                        
                if  tools.isNotNanInf(tRange[1]):
                    tVd.setTop(tRange[1])                    
            #分析占位符
            if 'Decimals' in tVarDf.keys()and \
                QtGui.QIntValidator().validate(tVarDf['Decimals'], 0)[0] == QtGui.QValidator.Acceptable:
                    tVd.setDecimals(int(tVarDf['Decimals']))  
            if tVd.validate(str(tItem.data(Qt.UserRole)['Value']), 0)[0] != QtGui.QValidator.Acceptable:
                tItem.setBackground((QtGui.QBrush(QtGui.QColor(255, 0, 0))))
            else:
                tItem.setBackground((QtGui.QBrush(QtGui.QColor(25, 255, 0))))
    
  
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
        for iC in range(0, self.table.columnCount()):
            #遍历关闭可见性
            tVarName = self.varsDfList[iC]['VarName']
            if tVarName in tVarCombo:
                self.table.setColumnHidden(iC, False)
            else:
                self.table.setColumnHidden(iC, True)
        #print("row:%d,col:%d"%(self.table.rowCount(), self.table.columnCount()))
    
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
                self.logger.error("变量：%s 的值：%s ，无法转换为Int"%(vUrl, vCount +e1))

        
    @pyqtSlot(int)    
    def on_Singal_RuleNumToCount(self, tNum):
        """
        """
        if tNum >= self.minCount and tNum <= self.maxCount  and  tNum >= self.table.rowCount():
            self.table.setRowCount(tNum)
        else:
            self.logger.error("无法将表格的行数设置为%d,当前%d"%(tNum,self.table.rowCount() ))
            self.Signal_rowCountChanged.emit(self.CountVarUrl, self.table.rowCount())
            
    @pyqtSlot(int)  
    def on_Singal_NMACHChanged(self, iNMCAH):
        """
        根据数据定义初始化表格
        1.如果关联了MACH、限制等参数，执行表格规模调整至约束条件
        2.定义了变量的初始值，赋值
        3.
        """
        #分析是否关联到NMACH限制因素
        self.isLinkNMACH = self.dtDefine.isLinkToNMACH(self.Namelist, self.GroupName)      
        if self.isLinkNMACH:
            #表格关联了NMACH
            self.minCount  = iNMCAH
            self.maxCount = iNMCAH
            if self.table.rowCount() < iNMCAH:
                for iR in range(self.table.rowCount(), iNMCAH):
                    self.table.insertRow(iR)
            elif self.table.rowCount() < iNMCAH:
                for iR in range(iNMCAH, self.table.rowCount()):
                    self.table.removeRow(iR)      
                self.logger.warning("on_Singal_NMACHChanged() 根据NMACH值删除了表格最后的几行数据！")
                    
        
    @pyqtSlot()
    def on_actionAddRow_triggered(self):
        """
        增加新行的代码.
        """
        # TODO: not implemented yet
        aItem = self.table.indexAt(self.curPos) #认为是表格 ，否则会异常
        rowIndex = 0
        if aItem.row() == -1 :
            #没有命中
            rowIndex = self.table.rowCount()
        else:
            rowIndex = aItem.row()

        if self.table.rowCount() <self.maxCount:
            self.table.insertRow(rowIndex)
        else:
            self.logger.info("%s已经达到最大行数不能添加"%self.objectName())
            
        #self.curN.setText(str(self.rowCount()))
        self.Signal_rowCountChanged.emit(self.CountVarUrl, self.table.rowCount())

    @pyqtSlot()
    def on_actionAddRowToMax_triggered(self):
        """
        增加到最大行的代码.
        """
        #添加行
        if self.table.rowCount() < self.maxCount:
            self.table.setRowCount(self.maxCount)

        self.Signal_rowCountChanged.emit(self.CountVarUrl, self.table.rowCount())        

    
    @pyqtSlot()
    def on_actionDeleteRow_triggered(self):
        """
        删除行的代码.
        """
        # TODO: not implemented yet
        aItem = self.table.indexAt(self.curPos)
        if  aItem.row() >=0 :            
            self.table.removeRow(aItem.row())
        else:
            self.logger.info("没有命中任何行")
        
        self.Signal_rowCountChanged.emit(self.CountVarUrl, self.table.rowCount())

    @pyqtSlot()
    def on_actionClearRows_triggered(self):
        """
        删除行的代码.
        """
        # TODO: not implemented yet
        #self.clear()
        self.table.setRowCount(self.minCount)        
        self.Signal_rowCountChanged.emit(self.CountVarUrl, self.table.rowCount())         
        
            
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
        
        tHItem = self.table.horizontalHeaderItem(vIndex)
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
        tHItem = self.table.horizontalHeaderItem(vIndex)
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
        for iR in range(0,  self.table.rowCount()):
            tItem = self.table.item(iR, column)
            if tItem is None:
                continue
            #执行坐标变换
            tUserData = tItem.data(Qt.UserRole)
            if tUserData['Unit'] == tNewUnit:
                continue
            tNewValue  = dtDimension.unitTransformation(tUserData,tNewUnit )
            if tNewValue is not None:
                tItem.table.setData(Qt.UserRole, tNewValue)
                tItem.table.setData(Qt.DisplayRole, str(tNewValue['Value']))
    
    @pyqtSlot(QTableWidgetItem)
    def onItemChanged(self, item):
        """
        void QTableWidget::itemChanged(QTableWidgetItem *item)
This signal is emitted whenever the data of item has changed.
        在此处协调代理自行变换单位的情况
        """
        #做值判断
        if item.data(Qt.DisplayRole) is not None and item.data(Qt.UserRole) is None:
            #tDataTemplate  = {'Dimension':tDimension, 'Unit':tUnit, 'Value':None}
            pass
        
        #判断列结论
        tConfig = self.table.horizontalHeaderItem(item.column()).data(Qt.UserRole)
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
        
        
        
if __name__ == "__main__":
    import sys, os
    app = QtWidgets.QApplication(sys.argv)
    tMain = QtWidgets.QWidget()  
    LiftLayout = QtWidgets.QVBoxLayout()
    #LiftLayout.setContentsMargins(5, 0, 0, 5)
    tMain.setLayout(LiftLayout)
    tTable = DatcomInputTable( 'FLTCON', 'Speed_Atmospheric',  parent=tMain, iDefine = DDefine )
    LiftLayout.addWidget(tTable) 
    tMPath = os.path.join(os.path.expanduser('~'), r'.PyDatcomLab\extras\PyDatcomProjects\1\case2.xml')
    tModel = dcModel.dcModel(tMPath)
    tTable.loadData(tModel)    
    tMain.show()
    tTable.saveData(tModel)
    tModel.save(tMPath)
    sys.exit(app.exec_())
        
        

