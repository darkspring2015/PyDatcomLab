# -*- coding: utf-8 -*-

"""
Module implementing FLTCON.
"""

from PyQt5.QtCore import  QPoint, QMetaObject, pyqtSignal#, pyqtSlot,
from PyQt5.QtWidgets import QWidget #,QMessageBox


#from PyDatcomLab.Core import dcModel
from PyDatcomLab.Core import datcomModel as dcModel
from PyDatcomLab.Core.DictionaryLoader import  defaultDatcomDefinition as DDefine  
#from PyDatcomLab.GUIs.InputCard import DatcomCARDLogicBase as DCLogic  
from PyDatcomLab.GUIs.InputCard.DatcomWidgetBaseUi import DatcomWidgetBaseUi 
import logging



class DatcomWidgetBase(QWidget, DatcomWidgetBaseUi):

    """
    Datcom 输入选项卡的基础类.
    DatcomWidgetBase的实例是QWidget对象，将承载一个Datcom的选项卡
    基本约束：
    1.必须指定iNamelist；
    2.如果未给出iModel，将内部创建一个datcomModel对象的实例，并添加默认的变量集合
    3.可以忽略iDefine输入
    """
    
    #定义各个Widget之间进行参数同步的信号
    Singal_InitializeUI    = pyqtSignal(int)                        #用来提供额外的界面初始化逻辑，响应信号对界面进行额外的初始化
    Singal_RuleIndexToCombo              = pyqtSignal(str,str)      #处理变量组合的选择发生变换时，由ComboCHoose触发 <sender.vUrl,Howto-ChosedKey>
    Singal_CheckboxStateChanged          = pyqtSignal(int,str)      #处理checkbox的同步问题
    Singal_TableCountEditingFinished     = pyqtSignal(str, int)     #处理表格长度控制变量的触发逻辑 str Url int :count
    Singal_CommonUIChanged               = pyqtSignal(str)          #通用的输入状态变化规则
    Singal_RuleVariableStatus            = pyqtSignal(str)          #通用的RuleVariableStatus状态变化规则 
    Singal_TBRowCountChanged             = pyqtSignal(int, str)     #用于通知表格的行数发生了变化    
    Singal_varComboChangedFromTable      = pyqtSignal(str , str)    #向外部通知表格中激活的列组合关系发生变化 <sender.vUrl,"[]">
    
    def __init__(self, iNamelist, parent=None , iModel = None, iDefine = DDefine ):
        """
        Constructor
        @param iDefine  DTdictionary的实例，存储Datcom的定义
        @type DTdictionary   
        @param iModel  datcomModel的实例，用以加载或者存储数据
        @type datcomModel        
        @param iNamelist widget将呈现的Namelist的名称
        @type str        
        @param parent reference to the parent widget
        @type QWidget
        """
        super(DatcomWidgetBase, self).__init__(parent)
        #self.setupUi(self)
        
        #创建日志
        self.logger = logging.getLogger(r'Datcomlogger')
        #获得Datcom的界面定义
        if iNamelist is None or iNamelist == '':
            self.logger.error("未指定Namelist信息：%s!"%str(iNamelist))
            iNamelist = 'FLTCON'
        self.NameList            = iNamelist
        self.dtDefine                 = iDefine
        self.NamelistAttriabute    = self.dtDefine.getNamelistAttributeByName(self.NameList)
        self.VariableList             = self.dtDefine.getNamelistDefineByName(self.NameList) 
        self.NMACHLinkTable       = self.dtDefine.getCARDAddtionalInformation(self.NameList, 'NMACHLinkTable') 
        self.RuleNumToCount      = self.dtDefine.getCARDAddtionalInformation(self.NameList, 'RuleNumToCount')        
        self.RuleIndexToCombo   = self.dtDefine.getCARDAddtionalInformation(self.NameList, 'RuleIndexToCombo')  
        self.GroupDefine            = self.dtDefine.getCARDAddtionalInformation(self.NameList, 'GroupDefine')  
        self.RuleVariableStatus   = self.dtDefine.getCARDAddtionalInformation(self.NameList, 'RuleVariableStatus')
        self.HelpUrl                   = self.dtDefine.getCARDHelpDoc(self.NameList)
        self.HashVaribles         = {}
        
        
        #配置完成后再调用界面初始化
        self.setupUi(self)
        
        #修改后台的数据，如果tModel is None 将创建空的datcomModel对象       
        #将附加指定 InitDoc，InitLogic
        self.setModel( iModel)        
        #绑定处理逻辑

        #界面参数
        self.curPos = QPoint(0, 0)
        self.curWidget = None
        self.curN = None
        self.popMenu = None
        
        #再次执行绑定
        QMetaObject.connectSlotsByName(self)
        
        
        #执行附加的界面初始化操作
        self.InitializeUI()
        #刷新界面
        self.UILogic()  
        

        
    def emit_Singal_RuleVariableStatus(self ):
        """
        用来触发新的信号函数,转发Singal_RuleVariableStatus
        """
        tName = self.sender().objectName()
        self.Singal_RuleVariableStatus.emit( tName)
        
        
    def setModel(self, tModel):
        """
        初始化本节点的xml描述文档
        """
        #修改后台的数据
        if tModel is None or type(tModel) is not dcModel.dcModel:
            self.logger.error('传递的参数不是合格的类型：%s'%type(tModel) )
            #tModel = dcModel.dcModel('J6', '常规布局')     
            tModel = dcModel.dcModel()   
        #检查是否包含    
        if self.NameList not in tModel.getNamelistCollection():
            tModel.addNamelist(self.NameList)
        self.model = tModel  
        
        #执行参数配置过程        
        self.InitDoc()  
        
        #触发界面协调
        self.UILogic()
        
    def InitDoc(self):
        """
        分析并初始化后台数据
        """        

        #自动化循环赋值
        for varName in self.VariableList.keys():                      
            if self.VariableList[varName]['TYPE'] in ['REAL', 'INT', 'List'] :
                #查询默认值
                tWidget = self.findChild(QWidget,varName)
                if tWidget is None:
                    self.logger.error("访问的变量：%s 不在本窗体"%varName)
                else:
                    tWidget.loadData(self.model)
        #自动化循环赋值
        
        #对于表格类型的数据进行赋值
        for iTb in self.dtDefine.getGroupDefine(self.NameList):
            tWidget = self.findChild(QWidget,'tableWidget_'+iTb)
            if tWidget is None:
                self.logger.error("访问的控件：tableWidget_%s 不在本窗体"%iTb)
                continue
            tWidget.loadData(self.model)
            #遍历所有的遍历写入到表格中
        #遍历所有表格控件
      
    def getDoc(self):
        """
        将界面的内容刷新到变量model
        """
        #自动化循环赋值
        for varName in self.VariableList.keys():                      
            if self.VariableList[varName]['TYPE'] in ['REAL', 'INT', 'List'] :
                #查询默认值
                tWidget = self.findChild(QWidget,varName)
                if tWidget is None:
                    self.logger.error("访问的变量：%s 不在本窗体"%varName)
                else:
                    tWidget.saveData(self.model)
        #自动化循环赋值
        
        #对于表格类型的数据进行赋值
        for iTb in self.dtDefine.getGroupDefine(self.NameList):
            tWidget = self.findChild(QWidget,'tableWidget_'+iTb)
            if tWidget is None:
                self.logger.error("访问的控件：tableWidget_%s 不在本窗体"%iTb)
                continue
            tWidget.saveData(self.model)
            #遍历所有的遍历写入到表格中
        #遍历所有表格控件
        #执行界面刷新
        return self.model      
        
        
    def UILogic(self):
        """
        在此刷新UI，需要根据不同的情况执行判断
        """       
        #self.DatcomCARD.UILogic()
        
    
    def InitializeUI(self):
        """
        此函数执行UI的初始化逻辑，确保各个组件定义的初始状态时可靠地
        此函数应当在SetupUI和connectSlot之后被调用
        函数最后将触发 Singal_InitializeUI 用来做非公用的的初始化操作
        """
        
        #创建附加控件 如果定义筛选变量组的控件
        if hasattr(self,'RuleIndexToCombo'):
            #逐条创建附加筛选逻辑
            for tCombo in self.RuleIndexToCombo:
                if  tCombo is None or tCombo == {} or not 'Index'  in tCombo.keys():
                    continue
                #创建一个水平布局器
                tComboWidget = self.findChild(QWidget, 'Chooser_%s'%tCombo['Index'])
                if tComboWidget is not None :
                    tComboWidget.sendCurrentIndex()

                

    
if __name__ == "__main__":
    import sys
    from PyQt5.QtWidgets import QApplication
    app = QApplication(sys.argv)
    #tModel = dcModel.dcModel(r'E:\Projects\PyDatcomLab\extras\PyDatcomProjects\1\abcd2.dcxml')
    tModel = dcModel.dcModel()
    #tModel.loadXML()
    #card = DatcomWidgetBase(tCARD = 'FLTCON', tModel = tModel)  
    #card = DatcomWidgetBase(tCARD = 'OPTINS', tModel = tModel)  
    card = DatcomWidgetBase(iNamelist = 'SYNTHS', iModel = tModel)  
    #card = DatcomWidgetBase(tCARD = 'BODY', tModel = tModel) 
    card.dt_setSizes(400, 600)
    #ui = DatcomBaseUI()
    #ui.setupUi(card)
    card.show()
    sys.exit(app.exec_())
    
