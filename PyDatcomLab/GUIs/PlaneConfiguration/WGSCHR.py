# -*- coding: utf-8 -*-

"""
Module implementing WGSCHR.
"""

from PyQt5.QtCore import pyqtSlot, Qt, QPoint
from PyQt5.QtWidgets import QWidget, QMenu, QTableWidgetItem, QMessageBox, QLineEdit, QComboBox, QTableWidget
from PyQt5.QtWidgets import QAction, QCheckBox
from PyQt5.QtGui import QDoubleValidator, QIntValidator, QIcon, QPixmap, QValidator

from PyDatcomLab.Core import dcModel
import logging

from Ui_WGSCHR import Ui_WGSCHR


class WGSCHR(QWidget, Ui_WGSCHR):
    """
    Class documentation goes here.
    NACA
    PINF
    """
    #def __init__(self, parent=None, config = {}):
    def __init__(self, parent=None, tModel = None):
        """
        Constructor
        
        @param parent reference to the parent widget
        @type QWidget
        """
        super(WGSCHR, self).__init__(parent)
        self.setupUi(self)
        
        #创建日志
        self.logger = logging.getLogger(r'Datcomlogger')
        
        #修改后台的数据
#        tModel = config['model'] if 'model' in config.keys() else None
#        if tModel is None:
#            tModel = dcModel.dcModel('J6', '常规布局')         
#        self.model = tModel 

        #修改后台的数据
        if tModel is None:
            tModel = dcModel.dcModel('J6', '常规布局')         
        self.model = tModel  
        #定义核心数据
        self.NameList = 'WGSCHR'
        self.VariableList = {
                'TOVC':{'TYPE':'REAL'  ,'Range':[0, float('inf') ] }, 
                #'TOVC':{'TYPE':'REAL'  ,'Range':[0, 100000000 ] }, 
                'DELTAY':{'TYPE':'REAL'}, 
                'XOVC':{'TYPE':'REAL'}, 
                'CLI':{'TYPE':'REAL'},
                'ALPHAI':{'TYPE':'REAL'},
                'CMO':{'TYPE':'REAL'},    
                'LERI':{'TYPE':'REAL'}, 
                'LERO':{'TYPE':'REAL'},
                'TOVCO':{'TYPE':'REAL'}, 
                'XOVCO':{'TYPE':'REAL'}, 
                'CMOT':{'TYPE':'REAL'}, 
                'CLMAXL':{'TYPE':'REAL'},  
                'CLAMO':{'TYPE':'REAL'}, 
                'TCEFF':{'TYPE':'REAL'}, 
                'KSHARP':{'TYPE':'REAL'}, 
                'ARCL':{'TYPE':'REAL'}, 
                'YCM':{'TYPE':'REAL'}, 
                'CLD':{'TYPE':'REAL', 'Default':0.0}, 
                'NPTS':{'TYPE':'INT'  , 'Range':[0, 50]}, 
                'CAMBER':{'TYPE':'List','Range':['.TRUE.', '.FALSE.']   , 'Default':'.TRUE.'}, 
                'DWASH':{'TYPE':'List', 'Range':['1.0'   , '2.0', '3.0'], 'Default':'1.0'}, 
                'TYPEIN':{'TYPE':'List','Range':['1.0'   , '2.0']       , 'Default':'1.0'}, 
                'SLOPE':{'TYPE':'Array' , 'Limit':[6, 6]  , 'Group':'SLOPE'}  , 
                'CLALPA':{'TYPE':'Array', 'Limit':[0, 20] , 'Group':'Lift'}, 
                'CLMAX':{'TYPE':'Array',  'Limit':[0, 20] , 'Group':'Lift'}, 
                'XAC':{'TYPE':'Array',    'Limit':[0, 20] , 'Group':'Lift'}, 
                'XCORD':{'TYPE':'Array',  'Limit':[0, 50] , 'Group':'AirfoilSection'},
                'YUPPER':{'TYPE':'Array', 'Limit':[0, 50] , 'Group':'AirfoilSection'},
                'YLOWER':{'TYPE':'Array', 'Limit':[0, 50] , 'Group':'AirfoilSection'},
                'MEAN':{'TYPE':'Array',   'Limit':[0, 50] , 'Group':'AirfoilSection'},
                'THICK':{'TYPE':'Array',  'Limit':[0, 50] , 'Group':'AirfoilSection'},   
        }
        #配置界面 
        for varName in self.VariableList.keys():            
            if self.VariableList[varName]['TYPE'] == 'REAL':
                #给控件设置属性
                tWidget = self.findChild(QLineEdit,varName)
                if tWidget is None:
                    self.logger.error("访问的变量：%s 不在本窗体"%varName)
                else:
                    tVd = QDoubleValidator(tWidget)
                    if 'Range' in self.VariableList[varName].keys():
                        tRange = self.VariableList[varName]['Range']
                        if tRange[0] not in [float('-inf'), float('inf'), float('nan')] :
                            tVd.setBottom(tRange[0])
                        if tRange[1] not in [float('-inf'), float('inf'), float('nan')] :
                            tVd.setTop(tRange[1])
                    tWidget.setValidator(tVd)  
                    
            elif self.VariableList[varName]['TYPE'] == 'INT':
                #给控件设置属性
                tWidget = self.findChild(QLineEdit,varName)
                if tWidget is None:
                    self.logger.error("访问的变量：%s 不在本窗体"%varName)
                else:
                    tVd = QIntValidator(tWidget)
                    tVd.setBottom(0)
                    if 'Range' in self.VariableList[varName].keys():
                        tRange = self.VariableList[varName]['Range']
                        tVd.setRange(tRange[0], tRange[1])
                    tWidget.setValidator(tVd) 

        #设置表格功能
        self.tableWidget_AirfoilSection.setContextMenuPolicy(Qt.CustomContextMenu)
        self.tableWidget_Lift.setContextMenuPolicy(Qt.CustomContextMenu)
        self.tableWidget_SLOPE.setContextMenuPolicy(Qt.CustomContextMenu)

        
        #界面参数-表格逻辑
        self.curPos = QPoint(0, 0)
        self.curWidget = None
        self.curN = None
        self.popMenu = None
        
        
        #初始化数据和内容
        self.InitDoc()   
        self.UILogic()  
        
    def InitDoc(self):
        """
        分析并初始化后台数据
        """
        
        #开始参数配置过程
        tableCache ={}
        #自动化循环赋值
        for varName in self.VariableList.keys():
            tVar = self.model.getNamelistVar('WGSCHR',varName)            
            if self.VariableList[varName]['TYPE'] == 'REAL':
                #查询默认值
                if 'Default' in self.VariableList[varName].keys():
                    tVar = self.VariableList[varName]['Default'] if tVar is None else float(tVar)
                else:
                    tVar = 0 if tVar is None else float(tVar)
                #给控件赋值
                tWidget = self.findChild(QLineEdit,varName)
                if tWidget is None:
                    self.logger.error("访问的变量：%s 不在本窗体"%varName)
                else:
                    tWidget.setText(str(tVar))
                
            elif self.VariableList[varName]['TYPE'] == 'INT':
                #查询默认值
                if 'Default' in self.VariableList[varName].keys():
                    tVar = self.VariableList[varName]['Default'] if tVar is None else int(float(tVar))
                else:
                    tVar = 0 if tVar is None else int(float(tVar))
                #给控件赋值
                tWidget = self.findChild(QLineEdit,varName)
                if tWidget is None:
                    self.logger.error("访问的变量：%s 不在本窗体"%varName)
                else:
                    tWidget.setText(str(tVar))

            elif self.VariableList[varName]['TYPE'] == 'List':  
                #查询默认值
                tVarIndex =0
                if 'Default' in self.VariableList[varName].keys():
                    if tVar is None:
                        tVarIndex = self.VariableList[varName]['Range'].index(self.VariableList[varName]['Default'])  
                    else :
                        tVarIndex = self.VariableList[varName]['Range'].index(tVar)     
                else:
                    tVar = 0 if tVar is None else int(float(tVar))                    
                #给控件赋值
                tWidget = self.findChild(QComboBox,'comboBox_'+varName)
                if tWidget is None:
                    self.logger.error("访问的变量：%s 不在本窗体"%varName)
                else:
                    tWidget.setCurrentIndex(tVarIndex)
            elif self.VariableList[varName]['TYPE'] == 'Array': 
                """对于Array类型需要专门的进行分析"""
                #'SLOPE':{'TYPE':'Array' , 'Limit':[6, 6]  , 'Group':'SLOPE'}  ,
                groupName = self.VariableList[varName]['Group']
                if groupName in tableCache.keys() :
                    tableCache[groupName].append(varName)
                else:
                    tableCache[groupName] =[varName]
            else:
                self.logger.info("访问的变量：%s 具有无法识别的类型 %s"%(varName,self.VariableList[varName]['TYPE'] ))
        #自动化循环赋值
        
        #对于表格类型的数据进行赋值
        for iTb in tableCache.keys():
            tWidget = self.findChild(QTableWidget,'tableWidget_'+iTb)
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
                if 'Limit' in self.VariableList[varName].keys():
                    limitVar = self.VariableList[varName]['Limit']
                else:
                    limitVar = None
                #获得变量元素的默认值
                if 'Default' in  self.VariableList[varName].keys():
                    defualtVar = self.VariableList[varName]['Default']
                else:
                    defualtVar = None
                #查询变量的值
                tVar = self.model.getNamelistVar('WGSCHR',varName)
                if tVar is None: #没有定义该变量                    
                    if iTb == 'Lift': #对特殊表格进行处理
                        tNMACH = self.model.getNamelistVar('FLTCON','NMACH')
                        if tNMACH is None : 
                            self.logger.error("升力参数表单依赖与NMACH 但是没有定义")
                        else:
                            limitVar = [int(tNMACH), int(tNMACH)]
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
                    for itR in range(len(tValueList)): #写入数据
                        tWidget.setItem(itR, itC,QTableWidgetItem(str(tValueList[itR])))
                    if  not limitVar is  None:   #考虑表格最小规模限制
                        if limitVar[0] > tWidget.rowCount(): #将表格的行数限制到最小行数以上                            
                            for ii in range(tWidget.rowCount(), limitVar[0]):
                                tWidget.insertRow(ii)
                            if not defualtVar is None: #如果指定了默认值，则使用默认值进行填充
                                for itR in range(len(tValueList), tWidget.rowCount()):
                                    tWidget.setItem(itR, itC, QTableWidgetItem(str(defualtVar)))
                        
            #遍历所有的遍历写入到表格中
        #遍历所有表格控件
 

    def setModel(self, tModel):
        """
        初始化本节点的xml描述文档
        """
        
        self.Model = tModel        
        #执行参数配置过程        
        self.InitDoc()        
        self.UILogic()
        
    def getDoc(self):
        """
        将界面的内容刷新到变量model
        """
        
        #执行界面刷新


        #自动化循环赋值
        for varName in self.VariableList.keys():
            #分类型开展写入         
            if self.VariableList[varName]['TYPE'] == 'REAL':
                #查询默认值
                tWidget = self.findChild(QLineEdit,varName)
                if tWidget is None :
                    self.logger.error('没有改空间%s'%varName)
                    continue
                tCheckWidget = self.findChild(QCheckBox,'checkBox_'+varName)
                if not tCheckWidget is None: #存在Check
                    if tCheckWidget.checkState() == Qt.Checked:
                        self.model.setNamelist( self.NameList , varName, float(tWidget.text()))
                    else:
                        self.model.setNamelist( self.NameList , varName, None)
                else: #必须参数
                    self.model.setNamelist( self.NameList , varName, float(tWidget.text()))
                #end REAL 
            elif self.VariableList[varName]['TYPE'] == 'INT':
                #查询默认值
                tWidget = self.findChild(QLineEdit,varName)
                if tWidget is None :
                    self.logger.error('没有改空间%s'%varName)
                    continue
                tCheckWidget = self.findChild(QCheckBox,'checkBox_'+varName)
                if not tCheckWidget is None: #存在Check
                    if tCheckWidget.checkState() == Qt.Checked:
                        self.model.setNamelist( self.NameList , varName, '%s.0'%tWidget.text())
                    else:
                        self.model.setNamelist( self.NameList , varName, None)
                else: #必须参数
                    self.model.setNamelist( self.NameList , varName, '%s.0'%tWidget.text())

            elif self.VariableList[varName]['TYPE'] == 'List':  
                #查询默认值
                tWidget = self.findChild(QComboBox,'comboBox_'+varName)
                if tWidget is None :
                    self.logger.error('没有改空间%s'%varName)
                    continue
                tCheckWidget = self.findChild(QCheckBox,'checkBox_'+varName)
                tRange = self.VariableList[varName]['Range']
                if not tCheckWidget is None: #存在Check
                    if tCheckWidget.checkState() == Qt.Checked:
                        self.model.setNamelist( self.NameList , varName, tRange[tWidget.currentIndex() ])
                    else:
                        self.model.setNamelist( self.NameList , varName, None)
                else: #必须参数
                    self.model.setNamelist( self.NameList , varName, tRange[tWidget.currentIndex() ])
                #LIST
            elif self.VariableList[varName]['TYPE'] == 'Array': 
                """对于Array类型需要专门的进行分析"""
                #'SLOPE':{'TYPE':'Array' , 'Limit':[6, 6]  , 'Group':'SLOPE'}  , 
                      #检测参数组合 Lift
                groupName = self.VariableList[varName]['Group']
                tTableWidget = self.findChild(QTableWidget, 'tableWidget_'+groupName)
                if tTableWidget is None :
                    self.logger.info("无法找到对应的Widget ：tableWidget_%s "%groupName) 
                    continue
                tCheckWidget = self.findChild(QCheckBox,'checkBox_'+varName)
                if not tCheckWidget is None : #存在Check 并且没有选中 则删除数据
                    if tCheckWidget.checkState() == Qt.Unchecked:
                        self.model.setNamelist( self.NameList , varName, None)
                        continue               
                #执行分析 
                itC = self.getColumnIndex(tTableWidget, varName)
                if itC == -1:
                    self.logger.error("在tableWidget_%s无法找到%s对应的列 "%(groupName,varName ))
                    self.model.setNamelist( self.NameList , varName, None)
                    continue 
                   
                tVder = QDoubleValidator(self)
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
                    self.model.setNamelist( self.NameList , varName, 
                    {'Index':1, 'Value':tValList})
                else : #没有数据则清除
                    self.model.setNamelist( 'FLTCON' , varName, None)  
            else:
                self.logger.info("访问的变量：%s 具有无法识别的类型 %s"%(varName,self.VariableList[varName]['TYPE'] ))
        #自动化循环赋值        
        

        return self.model
        
    def getColumnIndex(self, tTable, tHeader):
        """"""
        tIndex =-1
        for itC in range(0, tTable.columnCount()):
            if tTable.horizontalHeaderItem(itC).text() == tHeader:
                tIndex = itC 
                break
        return tIndex
                
            

    def UILogic(self):
        """
        在此刷新UI，需要根据不同的情况执行判断
        """
        
        #NPTS的联动
        
        #跨越Widget获得NMACH的参数 FLTCON中获得NMACH VINF的数量指定
        tNMACH = self.model.getNamelistVar('FLTCON', 'NMACH')
        if  not tNMACH is None:
            tNMACH = int(tNMACH)
            if tNMACH > self.tableWidget_Lift.rowCount():
                for itR in range(self.tableWidget_Lift.rowCount(), tNMACH):
                    self.tableWidget_Lift.insertRow(itR)
            elif tNMACH < self.tableWidget_Lift.rowCount():
                self.logger.error("此处录入的行数%d不等与FLTCON定义的行数%d"%(tNMACH,self.tableWidget_Lift.rowCount() ))
        #SLOPE KSHARP
        if self.checkBox_SLOPE.checkState() == Qt.Unchecked:
            self.tableWidget_SLOPE.setEnabled(False)
            self.KSHARP.setEnabled(False)
        else: 
            self.tableWidget_SLOPE.setEnabled(True)
            self.KSHARP.setEnabled(True)
        
        
    @pyqtSlot(str, int)    
    def on_NMACH_changed(self, command , index):
        """
        同步FLTCON中的行增加操作
        """
        
        #协同
        tNMACH = int(float(self.model.getNamelistVar('FLTCON', 'NMACH')))
        if tNMACH is None:
            self.logger.error("数据结构异常")
            return
        #分步协同处理
        
        if command == 'Add':
            self.tableWidget_Lift.insertRow(index)
        elif command == 'Delete':
            self.tableWidget_Lift.removeRow(index)
        elif command == 'Resize':
            self.tableWidget_Lift.clearContents()
            self.tableWidget_Lift.setColumnCount(index)
            
            
    @pyqtSlot(str, bool) 
    def on_NACA_changed(self, command , tState):
        """
        选择了NACA选项卡
        """
        if tState:
            self.checkBox_SLOPE.setCheckstate(True)
            self.KSHARP.setEnabled(True)
        else:
            self.checkBox_SLOPE.setCheckstate(False)
            self.KSHARP.setEnabled(False)
        
        self.UILogic()
        
    
    @pyqtSlot(int)
    def on_comboBox_CAMBER_currentIndexChanged(self, index):
        """
        Slot documentation goes here.
        
        @param index DESCRIPTION
        @type int
        """
        # TODO: not implemented yet
        self.UILogic()  
    
    @pyqtSlot(int)
    def on_comboBox_DWASH_currentIndexChanged(self, index):
        """
        Slot documentation goes here.
        
        @param index DESCRIPTION
        @type int
        """
        # TODO: not implemented yet
        self.UILogic()  
    
    @pyqtSlot(int)
    def on_checkBox_SLOPE_stateChanged(self, p0):
        """
        Slot documentation goes here.
        
        @param p0 DESCRIPTION
        @type int
        """
        # TODO: not implemented yet
        
        self.UILogic()  
    
    @pyqtSlot(QPoint)
    def on_tableWidget_SLOPE_customContextMenuRequested(self, pos):
        """
        Slot documentation goes here.
        
        @param pos DESCRIPTION
        @type QPoint
        """
        # TODO: not implemented yet
        self.curPos = pos
        self.curWidget = self.tableWidget_SLOPE        
        posG = self.curWidget.mapToGlobal(pos)
        self.popMenu = QMenu(self.curWidget)
        #self.popMenu.addAction(self.actionAddRow)
        #self.popMenu.addAction(self.actionDeleteRow)
        self.curWidget.setContextMenuPolicy(Qt.CustomContextMenu)
        self.curN = None
        
        self.popMenu.exec(posG)
    
    @pyqtSlot(QPoint)
    def on_tableWidget_Lift_customContextMenuRequested(self, pos):
        """
        Slot documentation goes here.
        
        @param pos DESCRIPTION
        @type QPoint
        """
        # TODO: not implemented yet
        self.curPos = pos
        self.curWidget = self.tableWidget_Lift        
        posG = self.curWidget.mapToGlobal(pos)
        self.popMenu = QMenu(self.curWidget)
        #self.popMenu.addAction(self.actionAddRow)
        #self.popMenu.addAction(self.actionDeleteRow)        
        tAct = QAction(self)
        icon = QIcon()
        icon.addPixmap(QPixmap(":/cardIco/rc_card/icos/AddedIcon.ico"), QIcon.Normal, QIcon.Off)
        tAct.setIcon(icon)
        tAct.setObjectName("tipsAction")
        tAct.setText('表格行数须等于FLTCON中NMACH或VINF，请修改算例')
        self.popMenu.addAction(tAct)
        self.curWidget.setContextMenuPolicy(Qt.CustomContextMenu)
        self.curN = None
        
        self.popMenu.exec(posG)
    
    @pyqtSlot(int)
    def on_comboBox_TYPEIN_currentIndexChanged(self, index):
        """
        Slot documentation goes here.
        
        @param index DESCRIPTION
        @type int
        """
        # TODO: not implemented yet
        self.UILogic() 
    
    @pyqtSlot(QPoint)
    def on_tableWidget_AirfoilSection_customContextMenuRequested(self, pos):
        """
        Slot documentation goes here.
        
        @param pos DESCRIPTION
        @type QPoint
        """
        # TODO: not implemented yet
        self.curPos = pos
        self.curWidget = self.tableWidget_AirfoilSection        
        posG = self.curWidget.mapToGlobal(pos)
        self.popMenu = QMenu(self.curWidget)
        self.popMenu.addAction(self.actionAddRow)
        self.popMenu.addAction(self.actionDeleteRow)
        self.curWidget.setContextMenuPolicy(Qt.CustomContextMenu)
        self.curN = self.NPTS
        
        self.popMenu.exec(posG)
    
    @pyqtSlot()
    def on_actionAddRow_triggered(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        #添加行
        aItem = self.curWidget.indexAt(self.curPos) #认为是表格 ，否则会异常
        rowIndex = 0
        if aItem.row() == -1 :
            #没有命中
            rowIndex = self.curWidget.rowCount()
        else:
            rowIndex = aItem.row()
        
        if self.curWidget.objectName() == 'tableWidget_SLOPE' :
            tLimit = 6    
        if self.curWidget.objectName() == 'tableWidget_AirfoilSection':
            tLimit = 50
        if self.curWidget.objectName() == 'tableWidget_Lift':
            tLimit = 20
        

        if self.curWidget.rowCount() < tLimit:
            self.curWidget.insertRow(rowIndex)
        else:
            self.logger.info("%s已经达到最大行数不能添加"%self.curWidget.objectName())
        if not self.curN is None: 
            self.curN.setText(str(self.curWidget.rowCount()))
        
        self.UILogic()  
    
    @pyqtSlot()
    def on_actionDeleteRow_triggered(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        aItem = self.curWidget.indexAt(self.curPos)
        if  aItem.row() >= 0 :            
            self.curWidget.removeRow(aItem.row())
        else:
            self.logger.info("没有命中任何行")
            
        if not self.curN is None:
            self.curN.setText(str(self.curWidget.rowCount()))

        self.UILogic()  
    
    @pyqtSlot()
    def on_NPTS_editingFinished(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        
        nowRows = self.tableWidget_AirfoilSection.rowCount()
        tNx = int(self.NPTS.text())
        if nowRows < tNx: 
            #if 当前行数少，考虑增加
            for itR in range(nowRows, tNx):
                self.tableWidget_AirfoilSection.insertRow(itR)
        if nowRows == tNx:
            pass
        if nowRows > tNx:
            self.NPTS.setText(str(nowRows))
            strInfo = "尝试的NPTS小于现有数据行数，请手动从表格中删除对应行"
            QMessageBox.information(self, "提示" , strInfo)  
            self.logger.info(strInfo)    
            
        
        self.UILogic()  
