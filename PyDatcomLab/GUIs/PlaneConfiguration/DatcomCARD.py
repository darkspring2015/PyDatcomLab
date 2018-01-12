# -*- coding: utf-8 -*-

"""
Module implementing DatcomCARD.
可以作为所有CARD的基类，提供程式化的输入输出操作
"""
from PyQt5.QtCore import  Qt
from PyQt5.QtWidgets import QTableWidgetItem,  QLineEdit, QComboBox, QTableWidget, QCheckBox
from PyQt5.QtGui import QDoubleValidator, QIntValidator, QValidator


from PyDatcomLab.Core import dcModel #, datcomDefine as DD#
#from PyDatcomLab.Core import datcomDefine  as DF
import logging

class DatcomCARD(object):
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
                self.logger.error("访问的控件：%s 不在本窗体"%varName)
                continue
            #Widget存在
            #tWidget.clearContents() #清除内容
            tWidget.clear() #清除内容和表头
            tWidget.setColumnCount(len(tableCache[iTb]))
            tWidget.setHorizontalHeaderLabels( tableCache[iTb])   #绘制表头
            
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
                    if len(tValueList) not in range(limitVar[0], limitVar[1]):
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
    
    def UILogic(self):
        """
        提供可以标准化的UI自动化部分的代码
        """

        #判断CheckBox的逻辑
        for varName in self.Widget.VariableList.keys():
            #判断checkbox是否存在
            tWidgetCB = self.Widget.findChild(QCheckBox, 'checkBox_%s'%varName)
            if self.Widget.VariableList[varName]['TYPE'] == 'INT' or self.Widget.VariableList[varName]['TYPE'] == 'REAL':
                tWidget   = self.Widget.findChild(QLineEdit, varName)
                if tWidget is None: 
                    self.logger.error("不存在目标变量%s"%varName)
                    continue
            elif  self.Widget.VariableList[varName]['TYPE'] == 'List':
                tWidget   = self.Widget.findChild(QComboBox, 'comboBox_%s'%varName)
                if tWidget is None: 
                    self.logger.error("不存在目标变量%s"%varName)
                    continue
            elif  self.Widget.VariableList[varName]['TYPE'] == 'Array':
                groupName = self.Widget.VariableList[varName]['Group']
                tWidget   = self.Widget.findChild(QTableWidget, 'tableWidget_%s'%groupName)
                if tWidget is None: 
                    self.logger.error("不存在目标变量%s"%varName)
                    continue
            #执行状态切换
            if not tWidgetCB is None:
                if tWidgetCB.checkState() == Qt.Checked:
                    tWidget.setEnabled(True)
                else:
                    tWidget.setEnabled(False)
                    
        #表格行数联动
        
        #NPTS的联动
        
        #跨越Widget获得NMACH的参数 FLTCON中获得NMACH VINF的数量指定
        
        tNMACH = self.model.getNamelistVar('FLTCON', 'NMACH')
        if  not tNMACH is None:
            tNMACH = int(float(tNMACH))
            for iTab in self.Widget.NMACHLinkTable:
                tWidget   = self.Widget.findChild(QTableWidget, 'tableWidget_%s'%iTab)
                if tWidget is None: 
                    self.logger.error("不存在目标变量%s"%varName)
                    continue
                if tNMACH > tWidget.rowCount():
                    for itR in range(tWidget.rowCount(), tNMACH):
                        tWidget.insertRow(itR)
                elif tNMACH < tWidget.rowCount():
                    self.logger.error("此处录入的行数%d不等与FLTCON定义的行数%d"%(tNMACH,tWidget.rowCount() ))
    
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
    
