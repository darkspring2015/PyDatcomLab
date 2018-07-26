# -*- coding: utf-8 -*-

"""
Module implementing FLTCON.
"""

from PyQt5.QtCore import  QPoint, QMetaObject, pyqtSignal#, pyqtSlot,
from PyQt5.QtWidgets import QWidget #,QMessageBox
from PyQt5 import  QtWidgets ,QtCore

#from PyDatcomLab.Core import dcModel
from PyDatcomLab.Core import datcomModel as dcModel
#from PyDatcomLab.Core.DictionaryLoader import  defaultDatcomDefinition as DDefine  
from PyDatcomLab.Core.DictionaryLoader import   DTdictionary 
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
    Singal_InitializeUI                            = pyqtSignal(int)            #用来提供额外的界面初始化逻辑，响应信号对界面进行额外的初始化
    Singal_RuleIndexToCombo                 = pyqtSignal(str,str)       #处理变量组合的选择发生变换时，由ComboCHoose触发 <sender.vUrl,Howto-ChosedKey>
    Singal_CheckboxStateChanged          = pyqtSignal(int,str)       #处理checkbox的同步问题
    Singal_TableCountEditingFinished       = pyqtSignal(str, int)      #处理表格长度控制变量的触发逻辑 str Url int :count
    Singal_CommonUIChanged                = pyqtSignal(str)            #通用的输入状态变化规则
    #Singal_RuleVariableStatus                = pyqtSignal(str, str)       #通用的RuleVariableStatus状态变化规则  (ControlVarUrl,key)
    Singal_TBRowCountChanged             = pyqtSignal(int, str)      #用于通知表格的行数发生了变化    
    Singal_varComboChangedFromTable   = pyqtSignal(str , str)     #向外部通知表格中激活的列组合关系发生变化 <sender.vUrl,"[]">
    Singal_NMACHChanged                    = pyqtSignal(int)            #用来接收NMACH的变化的信号
    
    def __init__(self, iNamelist, parent=None , iModel = None, iDefine = DTdictionary.defaultConfig ):
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
            return 
            #iNamelist = 'FLTCON'
        self.Namelist                = iNamelist
        self.dtDefine                 = iDefine
        self.NamelistAttriabute    = self.dtDefine.getNamelistAttributeByName(self.Namelist)
        self.VariableList             = self.dtDefine.getNamelistDefineByName(self.Namelist) 
        self.NMACHLinkTable       = self.dtDefine.getCARDAddtionalInformation(self.Namelist, 'NMACHLinkTable') 
        self.RuleNumToCount      = self.dtDefine.getCARDAddtionalInformation(self.Namelist, 'RuleNumToCount')        
        self.RuleIndexToCombo   = self.dtDefine.getCARDAddtionalInformation(self.Namelist, 'RuleIndexToCombo')  
        self.GroupDefine            = self.dtDefine.getCARDAddtionalInformation(self.Namelist, 'GroupDefine')  
        self.RuleVariableStatus   = self.dtDefine.getCARDAddtionalInformation(self.Namelist, 'RuleVariableStatus')
        #规则self.RuleVariableCorrelation 
        self.RuleVariableCorrelation = self.dtDefine.getCARDAddtionalInformation(self.Namelist, 'RuleVariableCorrelation')
        self.RuleVariableCorrelationMasterVList = []
        self.RuleVariableCorrelationConditionVList = []
        if self.RuleVariableCorrelation is not None:
            for iR in self.RuleVariableCorrelation :
                self.RuleVariableCorrelationMasterVList.append(iR['MatserVariable'])
                self.RuleVariableCorrelationConditionVList.append(iR['ConditionVariable'])      
        #
        self.HelpUrl                   = self.dtDefine.getCARDHelpDoc(self.Namelist)
        #界面参数
        self.curPos = QPoint(0, 0)
        self.curWidget = None
        self.curN = None
        self.popMenu = None       
        
        #定义控件dtModel
        if iModel is None or type(iModel) is not dcModel.dcModel:
            self.logger.error('传递的参数不是合格的类型：%s'%type(iModel) )  
            self.dtModel = dcModel.dcModel()      
        else:
            self.dtModel = iModel
        
        #配置完成后再调用界面初始化
        self.setupUi(self) 
        
        #重新执行数据绑定！  
        #将附加指定 _InitDoc，InitLogic
        self.setModel( iModel)       

        #绑定处理逻辑
        QMetaObject.connectSlotsByName(self)
        
        #执行附加的界面初始化操作
        self.InitializeUI()
        #刷新界面
        self.UILogic()  

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
        if self.Namelist not in tModel.getNamelistCollection():
            tModel.addNamelist(self.Namelist)
        self.dtModel = tModel  
        
        #执行参数配置过程        
        self._InitDoc()  
        
        #触发界面协调
        self.UILogic()
        
    def _InitDoc(self):
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
                    #如果控件没有激活则跳过添加，逻辑应当在控件内部实现
                    tWidget.setModel(self.dtModel)
        #自动化循环赋值
        
        #对于表格类型的数据进行赋值
        for iTb in self.dtDefine.getGroupDefine(self.Namelist):
            tWidget = self.findChild(QWidget,'tableWidget_'+iTb)
            if tWidget is None:
                self.logger.error("访问的控件：tableWidget_%s 不在本窗体"%iTb)
                continue
            tWidget.setModel(self.dtModel)
            #遍历所有的遍历写入到表格中
        #遍历所有表格控件
      
    def getModel(self):
        """
        返回模型的Model
        不推荐使用该方法   
        """
#        #自动化循环赋值
#        for varName in self.VariableList.keys():                      
#            if self.VariableList[varName]['TYPE'] in ['REAL', 'INT', 'List'] :
#                #查询默认值
#                tWidget = self.findChild(QWidget,varName)
#                if tWidget is None:
#                    self.logger.error("访问的变量：%s 不在本窗体"%varName)
#                else:
#                    tWidget.saveData(self.dtModel)
#        #自动化循环赋值
#        
#        #对于表格类型的数据进行赋值
#        for iTb in self.dtDefine.getGroupDefine(self.Namelist):
#            tWidget = self.findChild(QWidget,'tableWidget_'+iTb)
#            if tWidget is None:
#                self.logger.error("访问的控件：tableWidget_%s 不在本窗体"%iTb)
#                continue
#            tWidget.saveData(self.dtModel)
#            #遍历所有的遍历写入到表格中
#        #遍历所有表格控件
#        #执行界面刷新
        return self.dtModel    


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
                    
        #RuleVariableStatus
        for iR in self.RuleVariableStatus:
            tCVName = iR['ControlVar']
            tWidget = self.findChild(QtWidgets.QWidget, tCVName)
            if tWidget is None:
                continue
            else:
                tIndex = tWidget.getCurrentKey()  #认为不许是DatcomInputList对象
                self.UILogic_RuleVariableStatus(tCVName, tIndex) 
                
        
    @QtCore.pyqtSlot(str, str ) 
    def on_RuleVariableStatus_dt_triggered(self , iUrl, iKey):
        """
        用来处理Datcon变量的list控件消息
        """
        tControlVar = iUrl.split('/')[-1]
        #tControlVar = self.sender().objectName().split('_')[-1]      
        #触发UI逻辑
        self.UILogic_RuleVariableStatus(tControlVar, iKey)        
        
        
    @QtCore.pyqtSlot(int)  
    def on_RuleVariableStatus_cuurentIndexChanged(self , index):
        """
        对RuleVariableStatus进行逻辑处理
        假设：
        1.函数依赖控件名称获得变量名，认为comboBox_VAR  或者VAR两种形式之一
        """ 
        tControlWidget =  self.sender()
        tControlVar = self.sender().objectName().split('_')[-1]             
        if tControlWidget is None :
                self.logger.error("无法找到%s对应的控件"%(tControlVar))
                return
        #存在控制变量
        tCVarRange = self.dtDefine.getVariableDefineByName(self.Namelist, tControlVar).get('Range', [])
        if tCVarRange is None or len(tCVarRange) ==0:
            self.logger.error("不存在%s对应的Range定义，忽略余下逻辑"%(tControlVar))
            return                         
        tControlValue = tCVarRange[index] #从Range中获得对应的定义值
        #触发UI逻辑
        self.UILogic_RuleVariableStatus(tControlVar, tControlValue)
 
    def UILogic_RuleVariableStatus(self, iControlVar, iKey):
        """
        由变量iControlVar的不同值控制的其他变量和控件的可用和不可用逻辑
        """
        if self.RuleVariableStatus  is None or len(self.RuleVariableStatus ) ==0:
            self.logger.error("异常触发RuleVariableStatus规则")
            return
        #获得信息

        #定义了控制量则扫描控制量
        for itRule in self.RuleVariableStatus:  
            if itRule['ControlVar']  != iControlVar:
                continue
            #执行设置逻辑            
            if  iKey not in itRule['HowTo'].keys():
                #确保可以获得对应的值
                self.logger.error("%s索引值%s不在规则定义的列表中"%(iControlVar, iKey ))
                continue
            tEnableList     = itRule['HowTo'][iKey]['Enabled']
            tDisEnableList = itRule['HowTo'][iKey]['Disabled']
            #遍历进行禁用和启用
            for itM in tEnableList:
                #临时处理表格类型
                if self.VariableList[itM]['TYPE'] == 'Array':
                    #此处交由其他规则处理，这里只处理单独的控件
                    continue
                tWidget = self.findChild(QtWidgets.QWidget, '%s'%(itM))  #认为所有的Datcom变量的控件名称均无前缀
                if tWidget is None:
                    continue
                tWidget.on_EnabledStatusChanged(QtCore.Qt.Checked)
            for itM in tDisEnableList:
                #临时处理表格类型
                if self.VariableList[itM]['TYPE'] == 'Array':
                    #此处交由其他规则处理，这里只处理单独的控件
                    continue
                tWidget = self.findChild(QtWidgets.QWidget, '%s'%(itM))  #认为所有的Datcom变量的控件名称均无前缀
                if tWidget is None:
                    continue
                tWidget.on_EnabledStatusChanged(QtCore.Qt.Unchecked)
            #结束遍历状态修改
            
    @QtCore.pyqtSlot(str, str)  #标示和值
    def on_MasterVariable_editingFinished(self, iUrl, iText):
        """
        响应RuleVariableCorrelation的主变量的editingFinished信号
        iText为编辑的结果
        """
        tMVarName = self.sender().objectName()
        self.UILogic_RuleVariableCorrelation(tMVarName, iText) #偷懒不进行类型转换
        
        
    @QtCore.pyqtSlot(str, str)  #标示和值    
    def on_ConditionVariable_currentIndexChanged(self, key):
        """
        响应RuleVariableCorrelation的条件变量的currentIndexChanged信号
        iText为编辑的结果
        """
        tCVarName = self.sender().objectName()
        #判断名称
        if self.RuleVariableCorrelation is  None:
            return
        tMvs = []
        #获得条件变量到主变量的查询关系，可能1对多
        for iR in self.RuleVariableCorrelation :
            if iR['ConditionVariable'] == tCVarName:
                tMvs.append(iR['MatserVariable'])
            
        #发送所有的信号
        for iM in tMvs:
            tWidget = self.findChild(QtWidgets.QWidget, '%s'%(iM))  #认为所有的Datcom变量的控件名称均无前缀                
            if tWidget is None:
                continue
            iValue = tWidget.getDataByVariable()    
            self.UILogic_RuleVariableCorrelation(iM,iValue['Value'] )       
            
    def UILogic_RuleVariableCorrelation(self, iMVarName, iVar ):
        """
        响应主变量变化导致从变量变化的逻辑
        iText是主变量的新值 str 类型
        主要功能:
        1.实现定义文件中的RuleVariableCorrelation规则
        资料：
      <RuleVariableCorrelation dcType="Rule">
        <Rule MatserVariable="NMACH" ConditionVariable="LOOP" CorrelatedVariables="['NALT']">
          <HowTo key="1.0" RelationalExpr="['{MVar}']" />
          <HowTo key="2.0" RelationalExpr="['{SVar}']" />
          <HowTo key="3.0" RelationalExpr="['{SVar}']" />
        </Rule>
        RuleVariableCorrelation规则：
        1. MatserVariable 主变量  ，ConditionVariable条件变量，CorrelatedVariables随动变量
        2. Howto为一条具体的规则，key指向条件变量的取值，RelationalExpr是CorrelatedVariables随动变量的取值表达式
        3. RelationalExpr由 CorrelatedVariable =eval("".format(MVar=MatserVariable,SVar=CorrelatedVariable))执行解析逻辑
        """
        #tMVarName = self.sender().objectName()
        #iMVarName = tMVarName
        
        if iMVarName not in self.VariableList.keys():
            self.logger.error("RuleVariableCorrelation()： 推定的变量名%s 不存于Datcom的定义！"%iMVarName)
            return
        #检查规则
        tRuleSet = self.dtDefine.getCARDAddtionalInformation(self.Namelist, 'RuleVariableCorrelation')
        if tRuleSet is None or len(tRuleSet) ==0:
            return 
        for iR in tRuleSet:
            if 'MatserVariable' not in iR.keys() or iR['MatserVariable'] != iMVarName or \
            'ConditionVariable' not in  iR.keys() or iR['ConditionVariable'] not in self.VariableList or\
            'CorrelatedVariables' not in iR.keys():
                continue
            #开始分析逻辑
            tCdWidget = self.findChild(QtWidgets.QWidget, '%s'%(iR['ConditionVariable']))  #认为所有的Datcom变量的控件名称均无前缀
            if tCdWidget is None :
                self.logger.error("RuleVariableCorrelation()： 无法获得条件变量：%s 的控件！"%tCdWidget)
                return 
            tCdVar = tCdWidget.getCurrentKey()  #获得主键 ，认为值
            for iSV in iR['CorrelatedVariables']:
                tSWidget = self.findChild(QtWidgets.QWidget, '%s'%(iSV))  #认为所有的Datcom变量的控件名称均无前缀                
                if tSWidget is None:
                    continue
                iSValue = tSWidget.getDataByVariable()
                iSVarValue = ''
                if iSValue is not None  and 'Value' in iSValue.keys() and iSValue['Value'] is not None:
                    iSVarValue = iSValue ['Value']
                else:
                    #如果无法获得有效值
                    self.logger.warning("UILogic_RuleVariableCorrelation 无法获得有效的当前值")
                    #continue
                for iH in iR['HowTo'].keys():
                    if iH == tCdVar:
                        #执行该条指令
                        try:
                            tSVIndex = iR['CorrelatedVariables'].index(iSV)
                            nCorrelatedVariable = eval(iR['HowTo'][iH]['RelationalExpr'][tSVIndex].format(MVar=iVar, SVar=iSVarValue))
                            iSValue.update({'Value':nCorrelatedVariable})
                            tSWidget.setDataByVariable(iSValue)  #
                        except Exception as e:
                            self.logger.error("RuleVariableCorrelation规则应用出错！%s"%e)
 
    
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
    
