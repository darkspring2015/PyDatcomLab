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
from PyDatcomLab.Core.datcomModel import    dcModel
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
    #Singal_RuleNumToCount         = pyqtSignal(int)           #用来接收外部的表格长度变化信号
    Singal_variableComboChanged = pyqtSignal(str , str)    #向外部通知表格中激活的列组合关系发生变化  <self.vUrl,"[]">
    #Singal_NMACHChanged           = pyqtSignal(int)          #用来接收NMACH的变化的信号
    
    def __init__(self, iNamelist, iGroup,  parent=None, iDefine = DDefine , iModel =None):
        """
        Constructor
        
        @param parent reference to the parent widget
        @type QWidget
        """
        super(DatcomInputTable, self).__init__( parent = parent)        
        #创建日志
        self.logger = logging.getLogger(r'Datcomlogger')        
        #配置分析
        if iDefine is  None or iNamelist is None or iGroup is None:
            self.logger.error("没有有效的配置文件，无法初始化")
            return        
        if iModel is None:
            iModel = dcModel()
        self.dtModel        = iModel
        #读取配置文件
        self.setDefinition( iNamelist, iGroup, iDefine )
        #初始化界面
        self.setupUi(self)
        #self.InitializeUILogic()        
        #界面参数
        self.curPos = QPoint(0, 0)
        self.isTransaction  = False   #用来标记是否处于事务处理状态，如果是True则临时禁用itemChanged中的dtModel的刷新
        #设置表头
        self.InitializeHeader()
        self.setDelegate()  #按列设置代理
        #指定Header的内容菜单
        #self.horizontalHeader().customContextMenuRequested.connect(self.OnHeaderCustomContextMenuRequested)
        self.table.horizontalHeader().sectionClicked.connect(self.on_SectionClicked)
        
        #附加初始化过程
        self.InitializeContextMenu() #配置内容菜单
        self.table.setContextMenuPolicy(Qt.CustomContextMenu)
        self.table.customContextMenuRequested.connect(self.on_customContextMenuRequested)
        
        #绑定执行逻辑 
        #self.Singal_RuleNumToCount.connect(self.on_Singal_RuleNumToCount)
        self.table.itemChanged.connect(self.onItemChanged) 
        self.table.cellChanged.connect(self.on_cellChanged)
        #self.Singal_NMACHChanged.connect(self.on_Singal_NMACHChanged)
        
        #再次执行绑定
        QMetaObject.connectSlotsByName(self)
        
        #执行表格内容的初始化逻辑
        self.InitializeTableSize()
        
                #为数据模型调用初始化函数
        if self.dtModel is not None :
            if type(self.dtModel) == dcModel:
                self.setModel(self.dtModel)
            else:
                self.logger.warning("初始化DatcomInputSingle时传入的dtModel的类型为%s，应该为%s"%(str(type(self.dtModel)), str(type(dcModel))))
                self.dtModel = dcModel()
              
        #联结部分的slot
        self.installEventFilter(self)

    def setDefinition(self, iNamelist, tGroup, tDefine ):
        """
        利用tDefine定义的信息，初始化表格信息
        iNamelist 指向当前表格承担的Namelist的名称
        tGroup指向表格容纳的所有列
        """
        #判断定义有效性
        if tDefine is None or iNamelist is None or tGroup is None:
            self.logger.error("setDefinition()无法分析定义，定义对象缺失！")
            return  
        self.dtDefine   = tDefine    
        self.GroupName = tGroup      #对应的变量组的名称  
        self.Namelist  = iNamelist    #对应NameList的名称
        self.vUrl      = '%s/%s'%(iNamelist,tGroup )
        self.varsDf   = {}               #所有变量的定义 dict形式
        self.groupDf  = {}              #组定义
        self.varsDfList = []             #顺序保存的所有变量的定义，用以关联表头
        self.maxCount = 20            #最大行数限制
        self.minCount = 0              #最小行数限制
        self.CountVar = None        #表格行数对应的变量名
        self.ComboVar = None       #表格列组合对应的附加变量名
        self.ComboRule = None      #表格列组合对应的规则
        self.ComboVarUrl = None    #表格列组合对应的附加变量的Url
        
         
        #分析组内变量的定义
        self.varsDf   = self.dtDefine.getGroupVarsByName(self.Namelist, self.GroupName) #组内所有变量的定义  
        if len(self.varsDf  ) == 0 :
            self.logger.error("DatcomInputTable：不包含%s对应的定义信息"%(self.vUrl ))
            return
        #分析组定义
        tGroupDfSet = self.dtDefine.getCARDAddtionalInformation(iNamelist, 'GroupDefine' )
        if len(tGroupDfSet) == 0 or self.GroupName  not in tGroupDfSet.keys():
            self.logger.error("不包含%s的组信息定义%s对应的定义信息"%(iNamelist, tGroup))
            return
        self.groupDf  = tGroupDfSet[tGroup]
        #保存定义
        #生成组变量定义的列表形式，为所有操作提供定义
        self.varsDfList = []
        for iv in self.varsDf.keys():
            self.varsDfList.append(self.varsDf[iv])
        #分析表格各列的默认值
        self.varDefaultList = []
        for iVar in self.varsDfList:
            tUrl = '%s/%s'%(iVar['NameList'], iVar['VarName'])
            tD =  self.dtDefine.getVariableTemplateByUrl(tUrl, isSubType= True)
            if tD is None : 
                self.logger.error("Datcom中没有对应变量的定义！")
            self.varDefaultList.append(tD)

        #分析表格行数限制
        self.minCount, self.maxCount  = self.dtDefine.getGroupLimitByName(iNamelist, tGroup)
        #分析是否关联到NMACH限制因素
        self.isLinkNMACH = self.dtDefine.isLinkToNMACH(iNamelist, tGroup)
        if self.isLinkNMACH:
            self.CountVar = 'NMACH'
            self.CountVarUrl = '%s/%s'%(self.Namelist, self.CountVar)
        else:
            #分析表格行数控制变量的结果
            tCountVar       = self.dtDefine.gettRuleNumToCountByGroup(iNamelist, tGroup)
            if tCountVar is not None : 
                self.CountVar = tCountVar
            self.CountVarUrl = '%s/%s'%(self.Namelist, self.CountVar)
        #分析表头协同变量结果
        tComboVar       = self.dtDefine.getRuleIndexToComboByGroup(iNamelist, tGroup)
        if tComboVar is not None and  len(tComboVar) > 0: 
            self.ComboVar  = tComboVar['Index']
            self.ComboRule = tComboVar['HowTo']
            self.ComboVarUrl = '%s/%s'%(self.Namelist, self.ComboVar)


        
    def setupUi(self, Form):
        """
        配置界面元素，创建软件界面
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
        #Delete all Row
        self.actionPasteColumn = QAction(self)
        self.actionPasteColumn.setText("粘贴一列")
        self.actionPasteColumn.setToolTip( "粘贴一列")
        icon1 = QIcon()
        icon1.addPixmap(QPixmap(":/InputCard/images/InputCard/editPaste.png"), QIcon.Normal, QIcon.Off)
        self.actionPasteColumn.setIcon(icon1)
        self.actionPasteColumn.setObjectName("actionPasteColumn") 
        self.actionPasteColumn.setShortcuts(QtGui.QKeySequence.Paste)
        
        #clipboard = QtGui.QGuiApplication.clipboard()
        
        #创建菜单
        self.popMenu = QMenu(self.table)
        #定义
        self.popMenu.addAction(self.actionAddRow)
        self.popMenu.addAction(self.actionDeleteRow)
        self.popMenu.addAction(self.actionAddRowToMax)
        self.popMenu.addAction(self.actionClearRows)
        self.popMenu.addSeparator()
        self.popMenu.addAction(self.actionPasteColumn)
        
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
         
    def InitializeTableSize(self):
        """
        初始化表格的形状
        主要功能：
        1.识别表格的行数限制，将表格的规模调整好最小行数
        2.给添加的行赋默认值
        """
        #分析定义
        self.table.setRowCount(self.minCount)
        for iR in range(0, self.minCount):
            for iC in range(0, self.table.columnCount()):
                tD = self.varDefaultList[iC]
                tText = str(tD.get('Value', ''))
                tItem = QtWidgets.QTableWidgetItem(tText)
                tItem.setData(Qt.UserRole, tD)
                self.table.setItem(iR, iC, tItem)
        #表格各列的默认值self.varDefaultList 
            
    def setDelegate(self):
        """
        为各列设置代理
        """
        for iC in range(0, self.table.columnCount()):            
            tUrl = '%s/%s'%(self.Namelist, self.varsDfList[iC]['VarName'])
            self.table.setItemDelegateForColumn(iC, CDelegate(tUrl, parent = self, iDefine = self.dtDefine ) )
       
    def clear(self):
        """
        clear the table context
        """
        self.table.clear()
        
    #下面开始的是控件的读写逻辑部分
    # 

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
            self.logger.warning("DatcomInputSingle.setModel()传入的dtModel的类型为%s，应该为%s"%(str(type(self.dtModel)), str(type(dcModel))))        
    
    def getModel(self):
        """
        返回模型的Model
        不推荐使用该方法        
        """
        return self.dtModel 

    def _loadData(self):
        """
        将self.dtModel定义的数据加载到控件以方便的编辑 iModel-> self
        函数行为:
        1. 根据Namelist 和 Group的值从iModel中读取对应的信息
        2. 加载所有Group中指定的Variable，从AddtionalInformation设定显示规则
        3. 调整对应变量的单位
        4. 如果输入值不合法进行背景标红提示
        """
        #参数验证
        if self.dtModel is None or type(self.dtModel) != dcModel:
            self.logger.info("尝试传递非dcModel对象给DatcomInputTable的loadData函数，Type：%s"%str(type(self.dtModel)))
            return 
        #清除所有数据
        self.clear()
        self.InitializeHeader()   
        self.InitializeTableSize()  #重新赋初值
        #h获得行数限制
        if self.isLinkNMACH :
            tTableRows = self.dtModel.getVariableByUrl('FLTCON/NMACH')['Value']
        else:
            tTableRows = self.dtModel.getVariableByUrl(self.CountVarUrl)['Value']
        #分析写入数据
        for iC  in range(0, len(self.varsDfList)):
            #分析并获取定义
            iV       = self.varsDfList[iC]   #这是所有的定义
            tUrl     = '%s/%s'%(iV['NameList'], iV['VarName'])
            tDataVar = self.dtModel.getVariableByUrl(tUrl)      
            #定义本列的模板
            tElementTemplate  = self.dtDefine.getVariableTemplateByUrl(tUrl, True).copy() #{'Dimension':tDimension, 'Unit':tUnit, 'Value':None}     
            tDataTemplate      = self.dtDefine.getVariableTemplateByUrl(tUrl)            
            if tDataVar is None :
                #不存在数据则隐藏对应的列
                self.table.setColumnHidden(iC, True)
                #被隐藏列默认的值
                tDataVar = tDataTemplate.copy()
                tDataVar.update({'Value': [tElementTemplate['Value']] * tTableRows})
                tDataVar.update({'InUsed':'False'})
                #但是需要添加必要
                #continue  
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

            #执行数据写入
            tData = tDataVar['Value']
            if tData is None :continue
            #判断表格长度
            if self.isLinkNMACH :
                #如果是链接到NMACH的内容
                tNMACH = self.dtModel.getVariableByUrl('FLTCON/NMACH')
                if tNMACH is None:
                    self.logger.error("无法数据")
                else:
                    if self.table.rowCount() !=tNMACH['Value']:   
                        tNeedRows = tNMACH['Value']
                        self.minCount  = tNeedRows
                        self.maxCount = tNeedRows
                        self.table.setRowCount( tNeedRows)
            else:
                    if self.table.rowCount() != len(tData):                 
                        #这里应该是比较大的那个值
                        tNeedRows = len(tData)
                        if 'Limit' in iV.keys() :
                            if len(tData) < iV['Limit'][0]:      
                                tNeedRows = iV['Limit'][0]
                            if len(tData) > iV['Limit'][1]:
                                tNeedRows = iV['Limit'][1]
                        #修改表格长度
                        self.table.setRowCount( tNeedRows)
                       
            #开始表格的赋值操作                    
            if len(tData) >= self.minCount and len(tData) <=  self.maxCount:     
                for iR in range(0, len(tData)):
                    tItem  = QTableWidgetItem(str(tData[iR]))
                    tDataUserRole = tDataTemplate.copy()
                    tDataUserRole['Unit']  = tUnit
                    tDataUserRole['Value'] =  tData[iR]
                    tItem.setData( Qt.UserRole,tDataUserRole )
                    #tItem.setData(Qt.DisplayRole,str(tData[iR]) )
                    self.table.setItem(iR, iC, tItem)
                #判断是否在使用
                if tDataVar['InUsed'] == 'True':
                    self.table.setColumnHidden(iC, False)     
                else:     
                    self.table.setColumnHidden(iC, True)    
            else:
                self.logger.warning("加载表格%s数据长度错误：%d ，需要min：%d max：%d"%(self.GroupName,len(tData), 
                               self.minCount, self.maxCount ))

        #发送行变更消息
        self.Signal_rowCountChanged.emit(self.CountVarUrl , self.table.rowCount())         #向外通知数据加载后的长度
        self.Singal_variableComboChanged.emit(self.vUrl, str(self.getColumnCombo()))     #向外通知数据列的组合关系发生变换
            
    def setItemData(self, iUrl, iRow, iVar):
        """
        从外部设置具体某列的具体值
        """
        if 'Url'not in iVar or iVar['Url'] not in self.varsDf or iRow > self.varsDf[iUrl]['Limit'][1] or\
        iRow < 0 or  iVar is None:
            self.logger.warning("setItemData()的参数无效！%s,C:%d,var:%s"%(iUrl, iRow, iVar))
            return 
        #开始设置逻辑
        #扩张表格规模
        if iRow > self.table.rowCount():            
            self._resizeTableToCount(iRow)
        #开始分析数据并添加
        tCIndex = self._getIndexByUrl(iUrl)
        tUData = self.table.item(iRow, tCIndex).data(Qt.UserRole)
        #检查iVar
        if tUData['Url'] != iUrl:
            self.logger.error("错误的数据关系，现有Url：%s,写入Url")
            return
        #转换坐标
        tCurrentUnit = self.getHorizontalHeaderUnit(tCIndex)
        tNewData = dtDimension.unitTransformation(tUData,tCurrentUnit )
        self.table.item(iRow, tCIndex).setData(Qt.UserRole,tNewData)
        self.table.item(iRow, tCIndex).setData(Qt.DisplayRole,tNewData['Value'])
        #写入到数据模型
        self.dtModel.setVariablebyArrayIndex(tNewData, tCIndex)
        
    def _getIndexByUrl(self, iUrl):
        """
        获得iUrl对应的列号
        """
        tIndex = -1
        for iC in range(0, len(self.varsDfList)):
            if self.varsDfList[iC]['Url'] == iUrl:
                tIndex = iC
        return tIndex


    def _flushData(self ):
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
                tVar['InUsed'] = 'False'  #设置标志位
                self.dtModel.setVariable( tVar)
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
                tVar['Unit']    = tUnit
                tVar['Value']  = tVarlist
                tVar['InUsed'] = 'True'
                self.dtModel.setVariable( tVar )
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

    def _getCurrentValueInModel(self, iUrl):
        """
        内部函数，从self.dtModel中获得当前值
        如果model中没有当前变量，则使用datcomDefine中的默认值
        返回值是获得变量值
        如果使用模板值，则设置标记位"InUsed"为False
        """
        tV = self.dtModel.getVariableByUrl(iUrl)  #获取模型中的具体数值
        if tV is None:
            tV = self.dtDefine.getVariableTemplateByUrl(iUrl)
            tV.update({"InUsed":'False'})        
        return tV

    def _UpdateUsedFlags(self, iColumn,  isUsed = 'True'):
        """
        更新变量的值
        """
        if iColumn < 0 or iColumn >= len(self.varsDfList):
            self.logger.warning("索引越界！")
            return
        tUrl = '%s/%s'%(self.varsDfList[iColumn]['NameList'], self.varsDfList[iColumn]['VarName'] )       
        #获取模型值
        tV = self._getCurrentValueInModel(tUrl)  #获取模型中的具体数值
        #更新标志
        tV.update({'InUsed':isUsed})
        #回写到数据模型
        self.dtModel.setVariable( tV)       
        

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
                self._UpdateUsedFlags(iC, 'True')
            else:
                self.table.setColumnHidden(iC, True)
                self._UpdateUsedFlags(iC, 'False')
        #print("row:%d,col:%d"%(self.table.rowCount(), self.table.columnCount()))
    
    @pyqtSlot(str, str)     
    def on_Singal_RuleNumToCount(self, vUrl, vCount): 
        """
        响应表格长度变化事件
        也可以用来设置表格长度
        """
        tUrl = "%s/%s"%(self.Namelist, self.CountVar)
        if tUrl == vUrl:
            try:
                tNum = int(vCount)
                self._resizeTableToCount(tNum) 
            except Exception as e1:
                self.logger.error("变量：%s 的值：%s ，无法转换为Int"%(vUrl, vCount +e1))

    def _resizeTableToCount(self, iNum):
        """
        扩张表格规模到iNum
        将会刷新到Model
        """
        if iNum >= self.minCount and iNum <= self.maxCount  and  iNum > self.table.rowCount():
            tNCount = self.table.rowCount()
            #开始事务
            self.isTransaction = True
            self.table.setRowCount(iNum)
            for iR in range(tNCount, iNum):
                self._setRowWithDefault(iR)
            #结束事务
            self.isTransaction = False
            #刷新数据到Model
            self._flushData()
            self.Signal_rowCountChanged.emit(self.CountVarUrl, self.table.rowCount())     
        else:
            if iNum != self.table.rowCount() :
                self.logger.error("无法将表格的行数设置为%d,当前%d"%(iNum,self.table.rowCount() ))
               
            
    def _setRowWithDefault(self, tNum):
        """
        在表格的tNum行插入新行，并赋初值  ,但是不触发到Model的同步      
        """
        if tNum < 0 :return 
        #self.table.insertRow(tNum)
        for iC in range(0, self.table.columnCount()):
            tD = self.varDefaultList[iC].copy()
            tD['Unit']  = self.getHorizontalHeaderUnit(iC)  #此处没有执行坐标变换操作
            tText = str(tD.get('Value', ''))
            tItem = QtWidgets.QTableWidgetItem(tText)
            tItem.setData(Qt.UserRole, tD)
            self.table.setItem(tNum, iC, tItem)       
            
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
                self.on_Singal_RuleNumToCount(self.vUrl, str(iNMCAH))
            if self.table.rowCount() > iNMCAH:
                self.isTransaction = True
                for iR in range(iNMCAH, self.table.rowCount()):
                    self.table.removeRow(iR)     
                    #这里将触发大量的读写 
                self.isTransaction = False
                self.logger.warning("on_Singal_NMACHChanged() 根据NMACH值删除了表格最后的几行数据！")
            #就刷新到Model
            self._flushData()
        
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
        #开始事务处理
        self.isTransaction = True  
        if self.table.rowCount() <self.maxCount:            
            self.table.insertRow(rowIndex)
            self._setRowWithDefault(rowIndex)
        else:
            self.logger.info("%s已经达到最大行数不能添加"%self.objectName())
        #结束事务处理
        self.isTransaction = False     
        #刷新到Model
        self._flushData()       
        #向外发送结果
        self.Signal_rowCountChanged.emit(self.CountVarUrl, self.table.rowCount())

    @pyqtSlot()
    def on_actionAddRowToMax_triggered(self):
        """
        增加到最大行的代码.
        """
        #开始事务处理
        self.isTransaction = True  
        #添加行
        if self.table.rowCount() < self.maxCount:
            self.table.setRowCount(self.maxCount)
            for iR in range(self.table.rowCount(), self.maxCount):
                self._setRowWithDefault(iR)           
        #结束事务处理
        self.isTransaction = False     
        #刷新到Model
        self._flushData()       
        #向外发送结果
        self.Signal_rowCountChanged.emit(self.CountVarUrl, self.table.rowCount())        

    
    @pyqtSlot()
    def on_actionDeleteRow_triggered(self):
        """
        删除行的代码.
        """
        #开始事务处理
        self.isTransaction = True  
        #删除操作
        aItem = self.table.indexAt(self.curPos)
        if  aItem.row() >=0 :            
            self.table.removeRow(aItem.row())
        else:
            self.logger.info("没有命中任何行")
         #结束事务处理
        self.isTransaction = False     
        #刷新到Model
        self._flushData()       
        #向外发送结果       
        self.Signal_rowCountChanged.emit(self.CountVarUrl, self.table.rowCount())

    @pyqtSlot()
    def on_actionClearRows_triggered(self):
        """
        删除行的代码.
        """
        #开始事务处理
        self.isTransaction = True  
        #删除所有的行
        self.table.setRowCount(self.minCount)    
         #结束事务处理
        self.isTransaction = False     
        #刷新到Model
        self._flushData()       
        #向外发送结果   
        self.Signal_rowCountChanged.emit(self.CountVarUrl, self.table.rowCount())         


    @pyqtSlot()
    def on_actionPasteColumn_triggered(self):
        """
        删除行的代码.
        """
        #开始事务处理
        self.isTransaction = True  
        #获得粘贴板中的数据
        clipboard = QtGui.QGuiApplication.clipboard()
        tStr = clipboard.text()
        if tStr is not None and tStr !='':
            tDigits = tStr.split(',')
            tDList = []
            tDigitsOld = []
            for iD in range(0, len(tDigits)):
                try:
                    if tDigits[iD].strip() == '':                        
                        continue
                    tStrT = tDigits[iD].replace("\r", "").replace("\n", "").strip()  
                    if tStrT is not None or tStrT !='':    
                        tDigitsOld.append(tStrT)                    
                        tDList.append( float(tStrT))
                except Exception as e:
                    self.logger.info("无法转换的数据：%s！"%iD)
            #尝试写入到结果
            nRow = self.table.currentRow()
            nCol  = self.table.currentColumn()
            if nRow >-1 and nCol >-1:
                button = QtWidgets.QMessageBox.question(self, r"粘贴数据",
                                       "将粘贴%d个数据：\n%s\n到:R:%d,C:%d列"%(len(tDigitsOld),'\n'.join(tDigitsOld), nRow,nCol ),
                                       QtWidgets.QMessageBox.Yes | QtWidgets.QMessageBox.No)
                if button == QtWidgets.QMessageBox.Yes:    
                    #遍历添加数据
                    for iR in range(0, len(tDList)):
                        tItem = self.table.item(nRow + iR , nCol)
                        #如果是空Item，则使用默认值进行推定
                        if tItem is None :
                            tItem =self._getInitializeItem(nCol)
                            self.table.setItem(nRow + iR , nCol, tItem )
                        #写入数据
                        tItemData = tItem.data(Qt.UserRole)
                        tItemData.update({'Value':tDList[iR]})
                        tItem.setData(Qt.UserRole,tItemData )
                        tItem.setData(Qt.DisplayRole,str(tDList[iR]) )
        #结束事务处理
        self.isTransaction = False     
        #刷新到Model
        self._flushData()       
        #向外发送结果   
        #self.Signal_rowCountChanged.emit(self.CountVarUrl, self.table.rowCount())            
        
    def _getInitializeItem(self, iCol):
        """
        获得列iCol的默认Item
        需要根据当前的单位值进行转换
        """
        #分析并获取定义
        iV       = self.varsDfList[iCol]   #这是所有的定义
        tUrl     = '%s/%s'%(iV['NameList'], iV['VarName'])   
        tElementTemplate  = self.dtDefine.getVariableTemplateByUrl(tUrl, True).copy()   
        #获取背景信息
        tConfig = self.table.horizontalHeaderItem(iCol).data(Qt.UserRole)
        #执行表头的单位制变换操作
        if 'Dimension' in tConfig.keys() and tConfig['Dimension'] in Dimension.keys() \
                and 'CurrentUnit' in tConfig.keys():
            if tElementTemplate['Unit'] != tConfig['CurrentUnit']:
                tElementTemplate  = dtDimension.unitTransformation(tElementTemplate,tConfig['CurrentUnit'] )
        #赋值
        tItem = QtWidgets.QTableWidgetItem(tElementTemplate['Value'])
        tItem.setData(Qt.UserRole, tElementTemplate)
        return tItem
        
        
        
         
         
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
        
    @pyqtSlot(int)
    def on_SectionClicked(self, vIndex):
        """
        表头被点击的响应函数        
        @param vIndex 表头的logicIndex
        @type int
        """   
        #获取背景信息
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
            #开始事务处理
            self.isTransaction = True              
            self._unitChanged(vIndex, tNextUnit)
            #结束事务处理
            self.isTransaction = False     
            #刷新到Model
            self._flushData()  
    
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
        #更新表格的数据
        #开始事务处理
        self.isTransaction = True              
        self._unitChanged(vIndex, newUnit)
        #结束事务处理
        self.isTransaction = False     
        #刷新到Model
        self._flushData()  
        
    def getHorizontalHeaderUnit(self, vIndex):
        """
        获得表头的当前单位
        """
        tHItem = self.table.horizontalHeaderItem(vIndex)
        if tHItem is None : 
            self.logger.warning("尝试获得没有意义的索引%d的单位"%vIndex)
            return None
        tConfig = tHItem.data(Qt.UserRole)
        if 'Dimension' not in tConfig.keys() :
            return ''
        else:
            return tConfig['CurrentUnit']         
        
    
    def _unitChanged(self, column, tNewUnit):
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
        if self.isTransaction : return
        #获取定义
        tConfig = self.table.horizontalHeaderItem(item.column()).data(Qt.UserRole)
        #做值判断,修复错误
        if item.data(Qt.DisplayRole) is not None and item.data(Qt.UserRole) is None:
            tDf = self.dtDefine.getVariableTemplateByUrl(tConfig["Url"], True)
            tDf.update({'Value':item.data(Qt.DisplayRole), 'Unit':tConfig['CurrentUnit']})
            item.setData(Qt.UserRole, tDf)
            self.logger.warning("onItemChanged（）修复了之错误")
        
        #判断列结论        
        if 'Dimension' in tConfig.keys() and 'CurrentUnit'   in tConfig.keys():
            #判断值结论
            tItemData = item.data(Qt.UserRole)
            #判断值得有效性
            if tItemData is not None and   'Unit'  in tItemData.keys() and  tItemData['Unit'] == '':                
                if tItemData['Unit']  != tConfig['CurrentUnit']:
                    tNewItemData = dtDimension.unitTransformation(tItemData, tConfig['CurrentUnit'])
                    item.setData(Qt.UserRole, tNewItemData)
                    item.setData(Qt.DisplayRole, str(tNewItemData['Value']))
            else:
                self.logger.warning("onItemChanged()检查出错！") 
            
        #逐项值更新表格数据
        #写入到数据模型
        if self.isTransaction  == False :
            self.dtModel.setVariablebyArrayIndex(item.data(Qt.UserRole), item.row())
  
        
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
    tModel = dcModel(tMPath)
    tTable.setModel(tModel)    
    tMain.show()
    tModel.save(tMPath)
    sys.exit(app.exec_())
        
        

