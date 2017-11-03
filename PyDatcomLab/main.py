#！
#PyDatcom的主程序
import os, sys
from PyQt5 import QtCore, QtWidgets


#确保相对的导入能够起到作用，需要导入相对路径
sys.path.append(os.path.abspath(os.path.join('.', 'PyDatcomLab', 'GUIs')))
sys.path.append(os.path.abspath(os.path.join('.', 'PyDatcomLab', 'Core')))

#导入主要的窗体
from PyDatcomLab.GUIs.MainWindow import DatcomMainWindow

import logging #导入日志系统

def InitLogger(logFile=r'test.log'):

    # 创建一个logger
    logger = logging.getLogger('Datcomlogger')
    logger.setLevel(logging.DEBUG)
    
    # 创建一个handler，用于写入日志文件
    fh = logging.FileHandler(logFile)
    fh.setLevel(logging.DEBUG)
    
    # 再创建一个handler，用于输出到控制台
    ch = logging.StreamHandler()
    ch.setLevel(logging.DEBUG)
    
    # 定义handler的输出格式
    formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')
    fh.setFormatter(formatter)
    ch.setFormatter(formatter)
    
    # 给logger添加handler
    logger.addHandler(fh)
    logger.addHandler(ch)    
    # 记录一条日志
    logger.info('完成日志系统的初始化')  
    return logger

#执行主要脚本
dirStr = os.path.join(os.path.abspath(sys.path[0]), r'extras','PyDatcomProjects', '1')
logger = InitLogger(os.path.abspath(os.path.join(dirStr, 'DatcomLog.log')))

app = QtWidgets.QApplication(sys.argv)
mainWin = DatcomMainWindow()
mainWin.logger = logger
logger.info("启动了日志系统")
mainWin.show()
sys.exit(app.exec_())





