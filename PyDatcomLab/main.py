#!/usr/bin/env python
#encoding: utf-8

"""
#PyDatcom的主程序
>>>main.py --prjDir=""
"""

import os, sys, time
from PyQt5 import QtWidgets
import logging #导入日志系统

def InitLogger(tlogFile = r'datcomlog.log', tlogName = r'Datcomlogger', tlogLevel = logging.INFO ):
    """
    """   

    # 创建一个logger
    logger = logging.getLogger(tlogName)
    logger.setLevel(tlogLevel)
    
    # 创建一个handler，用于写入日志文件  
    fh = logging.FileHandler(tlogFile)
    fh.setLevel(tlogLevel)
    
    # 再创建一个handler，用于输出到控制台
    ch = logging.StreamHandler()
    ch.setLevel(tlogLevel)
    
    # 定义handler的输出格式
    formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')
    fh.setFormatter(formatter)
    ch.setFormatter(formatter)
    
    # 给logger添加handler
    logger.addHandler(fh)
    logger.addHandler(ch)    
    # 记录一条日志
    logger.info('完成文件日志系统的初始化')  
    logger.info('完成标准流日志系统的初始化')  
    return logger



#执行主要脚本
import argparse
parser= argparse.ArgumentParser()
parser.add_argument('--ProjectDir', help='项目目录')
parser.add_argument('--log', help='日志文件')
parser.add_argument('--logLevel', help='日志等级 info,error,warning')

argments = parser.parse_args()


#确保相对的导入能够起到作用，需要导入相对路径
mainPath = os.path.split(os.path.realpath(__file__))[0]
sys.path.append(os.path.abspath(os.path.join(mainPath, '..')))
sys.path.append(os.path.abspath(os.path.join(mainPath, 'GUIs')))
sys.path.append(os.path.abspath(os.path.join(mainPath, 'GUIs', 'PlaneConfiguration')))
sys.path.append(os.path.abspath(os.path.join(mainPath, 'GUIs', 'components')))
sys.path.append(os.path.abspath(os.path.join(mainPath, 'Core')))



#建立项目文件夹 ~\.PyDatcomLab
configPath = os.path.join(os.path.expanduser('~'), '.PyDatcom')
dirList = {'PyDatcomLab':os.path.join(os.path.expanduser('~'), '.PyDatcom') , 
           'LogDirectory':os.path.join(os.path.expanduser('~'), '.PyDatcom', 'log') 
}
for tDir in dirList.keys():
    if  not os.path.exists(dirList[tDir]):
        os.makedirs(dirList[tDir])

#配置日志系统  
logFileName = time.strftime('%Y-%m-%d_%H-%M-%S',time.localtime(time.time()))
logfile = os.path.abspath(os.path.join(dirList['LogDirectory'], logFileName))
#if not os.path.exists(logfile):
#    os.mknod(logfile) #创建空文件
    
logChannel = r'Datcomlogger'
logLevel = logging.INFO
logger = InitLogger(tlogFile =logfile, tlogName = logChannel , tlogLevel = logLevel)


#导入主要的窗体
from PyDatcomLab.GUIs.MainWindow import DatcomMainWindow
#进入系统的主循环
app = QtWidgets.QApplication(sys.argv)

#MainWindow
mainWin = DatcomMainWindow()
mainWin.logger = logger
logger.info("启动了DatcomMainWindow")
mainWin.show()

sys.exit(app.exec_())





