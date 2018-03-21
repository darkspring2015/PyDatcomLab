# -*- coding: utf-8 -*-

"""
Module implementing DatcomCARD.
可以作为所有CARD的基类，提供程式化的输入输出操作
"""
from PyQt5.QtCore import  Qt
from PyQt5.QtWidgets import QTableWidgetItem,  QLineEdit, QComboBox, QTableWidget, QCheckBox#, QMessageBox
from PyQt5.QtGui import QDoubleValidator, QIntValidator, QValidator


from PyDatcomLab.Core import dcModel #, datcomDefine as DD#
#from PyDatcomLab.Core import datcomDefine  as DF
import logging

class DatcomCARDLogicBase(object):
    """
    class DatcomCARD 是提供CARD录入的基础类
    """
    
    def __init__(self,  tWidget):
        """
        初始化所有必需的操作
        """    
        #创建日志
        self.logger = logging.getLogger(r'Datcomlogger') 
        #验证输入
        if tWidget is None:
            self.logger.error("输入的Widget对象不能为空")
            return

        self.Widget       = tWidget                   #界面对象 
        self.model        = dcModel.dcModel()
        
        
 
        
    def InitUi(self):
        """
        该函数必须在SetupUi和给self.Widget.NameList,self.Widget.VariableList 赋值之后才能使用
        """
        #配置界面 
        for varName in self.Widget.VariableList.keys():   
            #给REAL类型绑定验证器            
            if self.Widget.VariableList[varName]['TYPE'] == 'REAL':
                #给控件设置属性
                tWidget = self.Widget.findChild(QLineEdit,varName)
                if tWidget is None:
                    self.logger.error("访问的变量：%s 不在本窗体"%varName)
                else:
                    tVd = QDoubleValidator(tWidget)
                    if 'Range' in self.Widget.VariableList[varName].keys():
                        tRange = self.Widget.VariableList[varName]['Range']
                        if tRange[0] not in [float('-inf'), float('inf'), float('nan')] :
                            tVd.setBottom(tRange[0])
                        if tRange[1] not in [float('-inf'), float('inf'), float('nan')] :
                            tVd.setTop(tRange[1])
                    tWidget.setValidator(tVd)  
                    
            #给INT类型绑定验证器        
            elif self.Widget.VariableList[varName]['TYPE'] == 'INT':
                #给控件设置属性
                tWidget = self.Widget.findChild(QLineEdit,varName)
                if tWidget is None:
                    self.logger.error("访问的变量：%s 不在本窗体"%varName)
                else:
                    tVd = QIntValidator(tWidget)
                    tVd.setBottom(0)
                    if 'Range' in self.Widget.VariableList[varName].keys():
                        tRange = self.Widget.VariableList[varName]['Range']
                        tVd.setRange(tRange[0], tRange[1])
                    tWidget.setValidator(tVd) 

            
                    
    def InitDoc(self):
        """
        分析并初始化后台数据
        """
        
        #开始参数配置过程
        tableCache ={}
        #自动化循环赋值
        for varName in self.Widget.VariableList.keys():
            tVar = self.model.getNamelistVar(self.Widget.NameList,varName)            
            if self.Widget.VariableList[varName]['TYPE'] == 'REAL':
                #查询默认值
                if 'Default' in self.Widget.VariableList[varName].keys():
                    tVar = self.Widget.VariableList[varName]['Default'] if tVar is None else float(tVar)
                else:
                    tVar = 0 if tVar is None else float(tVar)
                #给控件赋值
                tWidget = self.Widget.findChild(QLineEdit,varName)
                if tWidget is None:
                    self.logger.error("访问的变量：%s 不在本窗体"%varName)
                else:
                    tWidget.setText(str(tVar))
                
            elif self.Widget.VariableList[varName]['TYPE'] == 'INT':
                #查询默认值
                if 'Default' in self.Widget.VariableList[varName].keys():
                    tVar = self.Widget.VariableList[varName]['Default'] if tVar is None else int(float(tVar))
                else:
                    tVar = 0 if tVar is None else int(float(tVar))
                #给控件赋值
                tWidget = self.Widget.findChild(QLineEdit,varName)
                if tWidget is None:
                    self.logger.error("访问的变量：%s 不在本窗体"%varName)
                else:
                    tWidget.setText(str(tVar))

            elif self.Widget.VariableList[varName]['TYPE'] == 'List':  
                #查询默认值
                tVarIndex =0
                if 'Default' in self.Widget.VariableList[varName].keys():
                    if tVar is None:
                        tVarIndex = self.Widget.VariableList[varName]['Range'].index(self.Widget.VariableList[varName]['Default'])  
                    else :
                        tVarIndex = self.Widget.VariableList[varName]['Range'].index(str(tVar) )    
                else:
                    tVar = 0 if tVar is None else int(float(tVar))                    
                #给控件赋值
                tWidget = self.Widget.findChild(QComboBox,'comboBox_'+varName)
                if tWidget is None:
                    self.logger.error("访问的变量：%s 不在本窗体"%varName)
                else:
                    tWidget.setCurrentIndex(tVarIndex)
            elif self.Widget.VariableList[varName]['TYPE'] == 'Array': 
                """对于Array类型需要专门的进行分析"""
                #'SLOPE':{'TYPE':'Array' , 'Limit':[6, 6]  , 'Group':'SLOPE'}  ,
                groupName = self.Widget.VariableList[varName]['Group']
                if groupName in tableCache.keys() :
                    tableCache[groupName].append(varName)
                else:
                    tableCache[groupName] =[varName]
            else:
                self.logger.info("访问的变量：%s 具有无法识别的类型 %s"%(varName,self.Widget.VariableList[varName]['TYPE'] ))
        #自动化循环赋值
        
        #对于表格类型的数据进行赋值
        for iTb in tableCache.keys():
            tWidget = self.Widget.findChild(QTableWidget,'tableWidget_'+iTb)
            if tWidget is None:
                self.logger.error("访问的控件：tableWidget_%s 不在本窗体"%iTb)
                continue
            #Widget存在
            tWidget.clearContents() #清除内容
            #tWidget.clear() #清除内容和表头
            #tWidget.setColumnCount(len(tableCache[iTb]))
            #tWidget.setHorizontalHeaderLabels( tableCache[iTb])   #绘制表头
            
            #遍历所有的遍历写入到表格中
            for itC in range(len(tableCache[iTb])):
                varName = tableCache[iTb][itC]
                #获得变量的限制属性
                if 'Limit' in self.Widget.VariableList[varName].keys():
                    limitVar = self.Widget.VariableList[varName]['Limit']
                else:
                    limitVar = None
                #获得变量元素的默认值
                if 'Default' in  self.Widget.VariableList[varName].keys():
                    defualtVar = self.Widget.VariableList[varName]['Default']
                else:
                    defualtVar = None
                #查询变量的值
                tVar = self.model.getNamelistVar(self.Widget.NameList,varName)
                if tVar is None: #没有定义该变量                    
                    if iTb == 'Lift': #对特殊表格进行处理
                        tNMACH = self.model.getNamelistVar('FLTCON','NMACH')
                        if tNMACH is None : 
                            self.logger.error("升力参数表单依赖与NMACH 但是没有定义")
                        else:
                            limitVar = [int(float(tNMACH)), int(float(tNMACH))]
                    #其他表格进行通用处理  
                    self.logger.info("数据卡：%s 不在数据表单中,扩大表格至必须的规模"%varName)
                    if  not limitVar is  None:
                        if limitVar[0] > tWidget.rowCount(): #将表格的行数限制到最小行数以上
                            for ii in range(tWidget.rowCount(), limitVar[0]):
                                tWidget.insertRow(ii)
                        if limitVar[1] < tWidget.rowCount(): #将表格的行数限制到最大行数以下
                            NRow = tWidget.rowCount()
                            for ii in range(limitVar[0],NRow):
                                tWidget.removeRow(tWidget.rowCount())
                        if not defualtVar is None: #如果指定了默认值，则使用默认值进行填充
                            for itR in range(tWidget.rowCount()):
                                tWidget.setItem(itR, itC, QTableWidgetItem(str(defualtVar)))
                    else: #限制不可用
                        pass
                else :#定义了该变量
                    tValueList = tVar['Value']
                    if  not limitVar is  None:   #考虑表格最小规模限制
                        if limitVar[0] > tWidget.rowCount(): #将表格的行数限制到最小行数以上                            
                            for ii in range(tWidget.rowCount(), limitVar[0]):
                                tWidget.insertRow(ii)
                            if not defualtVar is None: #如果指定了默认值，则使用默认值进行填充
                                for itR in range(0, tWidget.rowCount()):
                                    tWidget.setItem(itR, itC, QTableWidgetItem(str(defualtVar)))
                        elif limitVar[1] < tWidget.rowCount():
                            self.logger.error("现有表格长度大于了限制的最大表格大小：%d ： %d!执行删除"%(tWidget.rowCount(), limitVar[1]))
                            for ii in range( tWidget.rowCount(), limitVar[1]):
                                tWidget.removeRow(ii)
                    #写入数据到表格
                    if len(tValueList) not in range(limitVar[0], limitVar[1] +1):
                        self.logger.error("输入数据行数%d不在限制范围之内[%d,%d]"%(
                                        len(tValueList), limitVar[0], limitVar[1]))
                    else:
                        if tWidget.rowCount()< len(tValueList):
                            for ii in range(tWidget.rowCount(), len(tValueList)):
                                tWidget.insertRow(ii)
                    #写入数据    
                    for itR in range(len(tValueList)): 
                        tWidget.setItem(itR, itC,QTableWidgetItem(str(tValueList[itR])))
                                
                        
            #遍历所有的遍历写入到表格中
        #遍历所有表格控件
        
    def setModel(self, tModel):
        """
        初始化本节点的xml描述文档
        """
        
        #修改后台的数据
        if tModel is None or type(tModel) is not dcModel.dcModel:
            self.logger.error('传递的参数不是合格的类型：%s'%type(tModel) )
            tModel = dcModel.dcModel('J6', '常规布局')        
        self.model = tModel  
      
        #执行参数配置过程        
        self.InitDoc()       

        
    def getModel(self):
        """
        将界面的内容刷新到变量model
        """
 
        #自动化循环赋值
        for varName in self.Widget.VariableList.keys():
            #分类型开展写入         
            if self.Widget.VariableList[varName]['TYPE'] == 'REAL':
                #查询默认值
                tWidget = self.Widget.findChild(QLineEdit,varName)
                if tWidget is None :
                    self.logger.error('没有改空间%s'%varName)
                    continue
                tCheckWidget = self.Widget.findChild(QCheckBox,'checkBox_'+varName)
                if not tCheckWidget is None: #存在Check
                    if tCheckWidget.checkState() == Qt.Checked:
                        self.model.setNamelist( self.Widget.NameList , varName, float(tWidget.text()))
                    else:
                        self.model.setNamelist( self.Widget.NameList , varName, None)
                else: #必须参数
                    self.model.setNamelist( self.Widget.NameList , varName, float(tWidget.text()))
                #end REAL 
            elif self.Widget.VariableList[varName]['TYPE'] == 'INT':
                #查询默认值
                tWidget = self.Widget.findChild(QLineEdit,varName)
                if tWidget is None :
                    self.logger.error('没有改空间%s'%varName)
                    continue
                tCheckWidget = self.Widget.findChild(QCheckBox,'checkBox_'+varName)
                if not tCheckWidget is None: #存在Check
                    if tCheckWidget.checkState() == Qt.Checked:
                        self.model.setNamelist( self.Widget.NameList , varName, '%s.0'%tWidget.text())
                    else:
                        self.model.setNamelist( self.Widget.NameList , varName, None)
                else: #必须参数
                    self.model.setNamelist( self.Widget.NameList , varName, '%s.0'%tWidget.text())

            elif self.Widget.VariableList[varName]['TYPE'] == 'List':  
                #查询默认值
                tWidget = self.Widget.findChild(QComboBox,'comboBox_'+varName)
                if tWidget is None :
                    self.logger.error('没有改空间%s'%varName)
                    continue
                tCheckWidget = self.Widget.findChild(QCheckBox,'checkBox_'+varName)
                tRange = self.Widget.VariableList[varName]['Range']
                if not tCheckWidget is None: #存在Check
                    if tCheckWidget.checkState() == Qt.Checked:
                        self.model.setNamelist( self.Widget.NameList , varName,  tRange[tWidget.currentIndex() ])
                    else:
                        self.model.setNamelist( self.Widget.NameList , varName, None)
                else: #必须参数
                    self.model.setNamelist( self.Widget.NameList , varName,  tRange[tWidget.currentIndex() ])
                #LIST
            elif self.Widget.VariableList[varName]['TYPE'] == 'Array': 
                """对于Array类型需要专门的进行分析"""
                #'SLOPE':{'TYPE':'Array' , 'Limit':[6, 6]  , 'Group':'SLOPE'}  , 
                      #检测参数组合 Lift
                groupName = self.Widget.VariableList[varName]['Group']
                tTableWidget = self.Widget.findChild(QTableWidget, 'tableWidget_'+groupName)
                if tTableWidget is None :
                    self.logger.info("无法找到对应的Widget ：tableWidget_%s "%groupName) 
                    continue
                tCheckWidget = self.Widget.findChild(QCheckBox,'checkBox_'+varName)
                if not tCheckWidget is None : #存在Check 并且没有选中 则删除数据
                    if tCheckWidget.checkState() == Qt.Unchecked:
                        self.model.setNamelist( self.Widget.NameList , varName, None)
                        continue               
                #执行分析 
                itC = self.getColumnIndex(tTableWidget, varName)
                if itC == -1:
                    self.logger.error("在tableWidget_%s无法找到%s对应的列 "%(groupName,varName ))
                    self.model.setNamelist( self.Widget.NameList , varName, None)
                    continue 
                   
                tVder = QDoubleValidator(self.Widget)
                tValList =[]
                for itR in range(tTableWidget.rowCount()):
                    tItem = tTableWidget.item(itR, itC)
                    if tItem is None : 
                        self.logger.info("%s 在%d,%d没有有效值"%(varName, itR, itC))
                        continue
                    #如果不为None
                    st = tVder.validate(tItem.text(), 0)[0]
                    if st == QValidator.Acceptable:
                        tValList.append(float(tItem.text()))
                    else:
                        self.logger.error("%s 在%d,%d没有有效值"%(tItem.text(), itR, itC))
                #查看结果
                if len(tValList)  ==  tTableWidget.rowCount() and len(tValList) != 0: #有数据则加入到集合
                    self.model.setNamelist( self.Widget.NameList , varName, 
                    {'Index':1, 'Value':tValList})
                else : #没有数据则清除
                    self.model.setNamelist( 'FLTCON' , varName, None)  
            else:
                self.logger.info("访问的变量：%s 具有无法识别的类型 %s"%(varName,self.WGSCHRList[varName]['TYPE'] ))
        #自动化循环赋值        
        return self.model
    
    def on_Singal_RuleIndexToCombo(self, tIndex, tName):
        """
        执行变量组合类的随动触发逻辑
        tIndex是改变后的Index
        tName是触发改变的Combo控件Name
        """
        tWidget = self.Widget.findChild(QComboBox,tName)
        if tWidget is None:
            self.logger.error("消息传递机制异常！")
            return
        if not hasattr(self.Widget,'RuleIndexToCombo'):
            return 
        #执行变量组合控制逻辑
        #print(tWidget.currentText())
        tControlVarName = tName.replace('comboBox_', '')  
        tGroupName = ''
        tHowTos ={}
        for iR in  self.Widget.RuleIndexToCombo: #[]
            if tControlVarName != iR['Index']:
                continue
            tGroupName = iR['Group']
            tHowTos = iR['HowTo']
        if tGroupName == '' or tHowTos == {}:
            return 
        #获得表格对象
        tTableWidget = self.Widget.findChild(QTableWidget,"tableWidget_%s"%tGroupName)
        if tTableWidget is None: return 
        #检查是否在子规则库中
        tkey = '%d.0'%(tIndex+1)
        if tkey  in tHowTos.keys():
            tHeaderRule = tHowTos[tkey]
            tNowHeader = []
            for itC in range(tTableWidget.columnCount()):
                tNowHeader.append(tTableWidget.horizontalHeaderItem(itC).text())
            #判断标题是否相等
            if tHeaderRule != tNowHeader:
                #表头不一致的情况下进行分析
                if tTableWidget.rowCount() ==0 or tTableWidget.columnCount() ==0:
                    #空表的情况下
                    tTableWidget.clear()
                    tTableWidget.setColumnCount(len(tHeaderRule))
                    tTableWidget.setHorizontalHeaderLabels(tHeaderRule)
                    self.logger.info("刷新了%s表格"%('tableWidget_%s'%tGroupName)) 
                else:
                    #获取现有数据
                    oldData = {}
                    oldRowCount = tTableWidget.rowCount()                        
                    for itC in range(len(tHeaderRule)):
                        tOldItc = self.getColumnIndex( tTableWidget, tHeaderRule[itC])
                        if tOldItc >= 0: #存在有效的数据则移动表格数据
                            tList =[]
                            for itR in range(tTableWidget.rowCount()):
                                tItem = tTableWidget.item(itR, tOldItc)
                                if tItem is None:
                                    tList.append(None)
                                else:
                                    tList.append(tItem.text())
                            tTableWidget.removeColumn(tOldItc)
                            oldData[itC] = tList
                        else:
                            oldData[itC] = []
                    #写入现有数据
                    tTableWidget.clear()
                    tTableWidget.setColumnCount(len(tHeaderRule))
                    tTableWidget.setRowCount(oldRowCount)
                    tTableWidget.setHorizontalHeaderLabels(tHeaderRule)
                    for itC in range(len(tHeaderRule)):
                        for itR in range(len(oldData[itC])):
                            tTableWidget.setItem(itR, itC, QTableWidgetItem(oldData[itC][itR]))
                    #
                    self.logger.info("重构了%s表格"%('tableWidget_%s'%tGroupName)) 
        else:
            self.logger.error("无法理解的规则模式%d"%tIndex)         
    
    def on_Singal_CheckboxStateChanged(self, tIndex, tName):
        """
        响应CheckBox的StateChanged
        """
        tWidget = self.Widget.findChild(QCheckBox,tName)
        if tWidget is None:
            self.logger.error("消息传递机制异常！%s不存在"%tName)
            return
        tVarName = tName.replace('checkBox_', '')
        if not hasattr(self.Widget,'VariableList') or tVarName not in self.Widget.VariableList.keys():
            return 
        #执行变量组合控制逻辑
        tVarDefine = self.Widget.VariableList[tVarName]
        #查找变量的输入控件
        tInputWidget = None
        if tVarDefine['TYPE'] in ['INT' , 'REAL']:
            tInputWidget   = self.Widget.findChild(QLineEdit, tVarName)
        elif  tVarDefine['TYPE'] in ['List'] :
            tInputWidget   = self.Widget.findChild(QComboBox, 'comboBox_%s'%tVarName)                
        elif  tVarDefine['TYPE'] in ['Array'] :
            groupName = tVarDefine['Group']
            tInputWidget   = self.Widget.findChild(QTableWidget, 'tableWidget_%s'%groupName)
        if tInputWidget is None: 
            self.logger.error("不存在目标变量%s对应的输入变量"%tVarName)
        #查找变量的单位控件
        tUnitWidget = None
        if 'Dimension' in  tVarDefine.keys():
            tUnitWidget   = self.Widget.findChild(QComboBox, "comboBox_Dimension_%s"%tVarName)
        
        #执行状态切换
        if tWidget.checkState() == Qt.Checked:
            if tInputWidget : tInputWidget.setEnabled(True)
            if tUnitWidget  : tUnitWidget.setEnabled(True)
        else:
            if tInputWidget : tInputWidget.setEnabled(False)
            if tUnitWidget  : tUnitWidget.setEnabled(False)
    
        
    
    def on_Singal_TbLength_editingFinished(self, tName ):
        """
        处理表格长度控制变量值变化时的基本逻辑
        """
        if not hasattr(self.Widget,'RuleNumToCount')  : 
            return
        tGroupName  = None
        for iR in self.Widget.RuleNumToCount: #[]
            if 'Num' in iR.keys() and tName == iR['Num']:
                tGroupName = iR['Group']
        if tGroupName is None : return 
                
        #通用数据与表格行数的逻辑
        tCVar = self.Widget.findChild(QLineEdit, tName)
        if tCVar is None:
            self.logger.error("当前CARD：%s并不存在%s的变量"%(self.Widget.objectName(),tName ))
            return 
        #存在 Num
        tNumVar = int(float(tCVar.text()))

        tTableWidget = self.Widget.findChild(QTableWidget, 'tableWidget_%s'%tGroupName)
        if tTableWidget is None:
            self.logger.error("当前CARD：%s并不存在%s的表单"%(self.Widget.objectName(),tGroupName ))
            return 
            
        #存在 Table
        #执行 Num to Table row的逻辑            
        if tNumVar >tTableWidget.rowCount(): #扩增表格行数
            for itR in range(tTableWidget.rowCount(), tNumVar):
                tTableWidget.insertRow(itR)
        elif  tNumVar < tTableWidget.rowCount(): #不能表格行数
            self.logger.info("CARD： %s尝试直接缩减%s表格行数到现有行数%d之下，重置为%s"%(self.Widget.objectName(), 
                                    tGroupName,tTableWidget.rowCount() , tName))            
            tCVar.setText(str(tTableWidget.rowCount()))
        #结束表格逻辑协同操作        
        
        

    def on_Singal_CommonUIChanged(self, tName):
        """
        其他通用UI逻辑处理
        """
        self.UILogic()
        
    def on_Singal_RuleVariableStatus(self, tName):
        """
        处理Singal_RuleVariableStatus的逻辑
        """
        
        if hasattr(self.Widget,'RuleVariableStatus'):
            #定义了控制量则扫描控制量
            for itRule in self.Widget.RuleVariableStatus:  
                tContrelWidget = self.Widget.findChild(QComboBox, 'comboBox_%s'%(itRule['ControlVar']))
                if tContrelWidget is None :
                    self.logger.error("无法找到%s对应的控件"%(itRule['ControlVar']))
                    continue
                #存在控制变量
                tControlValue = '%d.0'%(tContrelWidget.currentIndex()+1)
                if not tControlValue in itRule['HowTo'].keys():
                    #确保可以获得对应的值
                    self.logger.error("索引值%s不在规则定义的列表中"%(tControlValue))
                    continue
                tEnableList    = itRule['HowTo'][tControlValue]['Enabled']
                tDisEnableList = itRule['HowTo'][tControlValue]['Disabled']
                #遍历进行禁用和启用
                for itM in tEnableList:
                    #临时处理表格类型
                    if self.Widget.VariableList[itM]['TYPE'] == 'Array':
                        continue
                    tWidget, tWidgetCheckBox = self.getWidgetByName(self.Widget, itM)
                    if tWidget is None:
                        continue
                    #存在widget
                    if not tWidgetCheckBox  is None:
                        tWidgetCheckBox.setCheckState(Qt.Checked)
                    tWidget.setEnabled(True)
                for itM in tDisEnableList:
                    if self.Widget.VariableList[itM]['TYPE'] == 'Array':
                        continue
                    tWidget, tWidgetCheckBox = self.getWidgetByName(self.Widget, itM)
                    if tWidget is None:
                        continue
                    #存在widget
                    if not tWidgetCheckBox  is None:
                        tWidgetCheckBox.setCheckState(Qt.Unchecked)
                    tWidget.setEnabled(False)
                #结束遍历状态修改
            #结束所有规则的应用
        #结束状态选择量控制其他状态量的过程
    def on_Singal_NMACHLinkTable(self, tName):
        """
        执行与NMACH进行联动的逻辑
        """
        #表格行数联动        
        #NPTS的联动        
        #跨越Widget获得NMACH的参数 FLTCON中获得NMACH VINF的数量指定
        if hasattr(self.Widget,'NMACHLinkTable'):            
            tNMACH = self.model.getNamelistVar('FLTCON', 'NMACH')
            if  not tNMACH is None:
                tNMACH = int(float(tNMACH))
                for iTab in self.Widget.NMACHLinkTable:
                    tWidget   = self.Widget.findChild(QTableWidget, 'tableWidget_%s'%iTab)
                    if tWidget is None: 
                        self.logger.error("不存在目标变量%s"%iTab)
                        continue
                    if tNMACH > tWidget.rowCount():
                        for itR in range(tWidget.rowCount(), tNMACH):
                            tWidget.insertRow(itR)
                    elif tNMACH < tWidget.rowCount():
                        self.logger.error("此处录入的行数%d不等与FLTCON定义的行数%d"%(tNMACH,tWidget.rowCount() ))

        
        
    def UILogic(self):
        """
        提供可以标准化的UI自动化部分的代码
        """
        #暂时没有其他可以做的了
    
    def getColumnIndex(self, tTable, tHeader):
        """
        搜索表头的函数
        """
        tIndex =-1
        for itC in range(0, tTable.columnCount()):
            if tTable.horizontalHeaderItem(itC).text() == tHeader:
                tIndex = itC 
                break
        return tIndex 
    
    def getVariableFromWidget(self,tContainer, tVarName):
        """
        寻找tContainer中包含tVarName对应的控件的值
        没有定义
        """
        VariableList    = self.Widget.VariableList
        WidgetContainer = tContainer
        #判断变量是否在定义列表中
        if not tVarName in VariableList.keys():
            self.logger.error("尝试获取未在Widget中定义的变量:%s - %s"%(tContainer.NameList, tVarName))
            return None
            
        #根据变量特点获取变量的值
        if VariableList[tVarName]['TYPE'] == 'REAL':
            tWidget = WidgetContainer.findChild(QLineEdit, tVarName)
            if tWidget is  None:
                return None
            else:
                if tWidget.text() == '':return None
                return float(tWidget.text())
        elif VariableList[tVarName]['TYPE'] == 'INT':
            tWidget = WidgetContainer.findChild(QLineEdit, tVarName)
            if tWidget is  None:
                return None
            else:
                if tWidget.text() == '':
                    return None
                return int(float((tWidget.text())))                
        elif VariableList[tVarName]['TYPE'] == 'List':
            tWidget = WidgetContainer.findChild(QComboBox, 'comboBox_%s'%tVarName)
            if tWidget is  None:
                return None
            else:
                return tWidget.currentIndex()
        elif VariableList[tVarName]['TYPE'] == 'Array':
            groupName = VariableList[tVarName]['Group'] 
            tWidget = WidgetContainer.findChild(QTableWidget, 'tableWidget_%s'%groupName)
            if tWidget is  None:
                return None
            else:
                itC = self.getColumnIndex( tWidget, tVarName)
                if itC == -1:
                    return None
                #读取表格数据
                res =[]
                for itR in range(tWidget.rowCount()):
                    tItem = tWidget.item(itR, itC)
                    if tItem is None:continue
                    if tItem.text() == '':continue
                    res.append(float(tItem.text()))
                #分析结果
                return res
        else:
            self.logger.error("尝试读取未定义类型%s的变量%s的值"%(VariableList[tVarName]['TYPE'],tVarName ))
            return None

    def getWidgetByName(self, tContainer, tVarName):
        """
        返回tContainer中包含的名为tVarName的控件和对应的checkState控件
        """
        if tContainer is None: return None, None
        if tVarName not in  tContainer.VariableList.keys():
            self.logger.error("尝试获取未知的变量%s"%tVarName)
            return None, None
        if tContainer.VariableList[tVarName]['TYPE'] == 'INT' :
            tWidgetName = tVarName
            tWidgetType = QLineEdit
        elif  tContainer.VariableList[tVarName]['TYPE'] == 'REAL' :
            tWidgetName = tVarName
            tWidgetType = QLineEdit
        elif  tContainer.VariableList[tVarName]['TYPE'] == 'List' :
            tWidgetName = 'comboBox_%s'%tVarName
            tWidgetType = QComboBox
        elif tContainer.VariableList[tVarName]['TYPE'] == 'Array' :
            groupName = tContainer.VariableList[tVarName]['Group'] 
            tWidgetName = 'tableWidget_%s'%groupName
            tWidgetType = QTableWidget
        else :
            self.logger.error("尝试获取未知类型%s的变量%s"%(tContainer.VariableList[tVarName]['TYPE'], tVarName))
            return None, None
        
        #查询
        tWidget         = tContainer.findChild(tWidgetType,tWidgetName )        
        tWidgetCheckBox = tContainer.findChild(tWidgetType,"checkBox_%s"%tVarName )
        #tips 这里不判断table的组件的check组是否存在
        
        if tWidget is None :
            self.logger.error("%s中不包含名称为%s的widget"%(tContainer.objectName(), tWidgetName))
            return None, None
        return tWidget, tWidgetCheckBox
            
