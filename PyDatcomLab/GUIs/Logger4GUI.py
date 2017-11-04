# -*- coding: utf-8 -*-

"""
    为项目封装的日志系统，再MainWindows中创建这个对象
    文件记录和标准流记录由Application来完成
"""

from PyQt5.QtCore import  pyqtSignal,  QObject
import logging
from logging.handlers import MemoryHandler

class myMemoryHandler(MemoryHandler, QObject):
    """
    封装MemoryHandler，利用slot-singal机制重定向到UI界面，在Flush中调用界面的回调函数
    注意：请不要在回调函数中调用logger，将导致死锁！
    SLot 必须接受一个参数 value：str
    """
    logSingal = pyqtSignal(str)  #自定义信号，发送日志
    
    def __init__(self, loggerSlot, tCapacity =1, tFlushLevel = logging.info):  
        """
        tCapacity : 多少条刷新一次,默认1：立马刷新
        """
        QObject.__init__(self)   
        logging.handlers.MemoryHandler.__init__(self, capacity=tCapacity,flushLevel=tFlushLevel, target=None)   
        self.loggerSlot = loggerSlot
        if self.loggerSlot :
            self.logSingal.connect(self.loggerSlot)
        
    def flush(self):  
        """ 
        只要调用flush就把数据刷新到text中
        """  
        if self.buffer!=[] and len(self.buffer) >= self.capacity:  
            content=""  
            for record in self.buffer:  
                content+= self.format(record)
            self.logSingal.emit(content)
            self.buffer = [] 
    



#设置函数
def getLogger(logSlot, 
                        logName = r'Datcomlogger', #被记录到的日志分类
                        logLevel = logging.INFO ,   #记录级别
                        formatStr = r'%(asctime)s - %(name)s - %(levelname)s - %(message)s'):
    """
    回调函数中不要调用该logger
    """   
    
    # 创建一个logger
    logger = logging.getLogger(logName)
    logger.setLevel(logLevel)    
    # 检查回调函数是否存在
    if logSlot is None : 
        logger.error('传递的回调函数（slot）无效')  
        return logger
    # 创建一个logger.handler
    formatter = logging.Formatter(formatStr)   
    ch =myMemoryHandler(logSlot, tCapacity =1, tFlushLevel = logLevel)
    ch.setLevel(logLevel)    
    # 定义handler的输出格式
    ch.setFormatter(formatter)    
    # 给logger添加handler
    logger.addHandler(ch)
    # 记录一条日志
    logger.info('完成Slot日志系统的初始化,%s'%logSlot.__name__)  
    return logger, ch


