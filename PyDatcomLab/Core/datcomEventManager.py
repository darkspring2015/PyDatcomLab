# -*- coding: utf-8 -*-
# 正处于设计开发阶段


from PyQt5 import QtCore
class datcomEvent(QtCore.QEvent):
    """
    自定义的Datcom事件系统
    
    """
    dtTypeID = QtCore.QEvent.registerEventType()
    def __init__(self, type = dtTypeID):
        """
        构造函数
        
        """
        super(datcomEvent, self).__init__(type)
        self.eventLabel = ''                #规则的名称[ 'NMACHLinkTable'  ,'RuleNumToCount','RuleIndexToCombo',
        self.controlVariables = {}         #存储引起变换的变量的名称和值，某些规则可能是多触发的，因此使用dict类型 {'FLTCON/NMACH':'1'}


        
    
class datcomEventWarpper(QtCore.QObject):
    """
    datcomModel中使用的，作为注册中心使用

    """
    def __init__(self):
        """
        注册中心的模型
        receiver           | 注册者的实例 |event的接收者
        eventLabel       | 事件标签      |需要监控的事件类别  
        controlVariable  | 事件的参数和参数值 |控制变量和值        
        """
        super(datcomEventWarpper, self).__init__()
        #注册中心
        #单个模板 {‘receiver':None,'eventLable':'','controlVariables’:[]}
        self.registerCenter = []
        
    def registerObject(self, receiver, eventLabel, controlVariables):
        """
        向注册中心注册一个事件接收
        @param receiver reference to the widget to receive the event
        @type QObject
        @param eventLabel 事件标签 
        @type str
        @param controlVariables 触发事件的变量 
        @type str
        注意事项：应当在对象销毁的地方显式调用反注册函数
        """
        if controlVariables is None or type(controlVariables) != list or\
                eventLabel is None or eventLabel =='' or \
                receiver is None:
            self.logger.warning("调用注册函数的参数无效！")
            return
        #开始注册过程
        
    
    def isRegistered(self, iDict):
        """
        检查对象iDict是否在仓库中已经注册了，包含当前注册返回True，没有返回False        
        """
        


    def simluateSendEvent(self, eventLabel, eventStr):
        """
        """
        if eventLabel in self.doc:
            for iR in self.doc[eventLabel]['Receiver']:
                tEvent = datcomEvent()
                tEvent.Url = eventLabel
                tEvent.Value = {eventLabel:eventStr}
                tApp = QtCore.QCoreApplication.instance()
                tApp.notify(iR, tEvent)
