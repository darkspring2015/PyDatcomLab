#!/usr/bin/env python
#encoding: utf-8

"""
#PyDatcom的主程序
>>>main.py --prjDir=""
"""

import os, sys, time,shutil
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

def main():
    """
    PyDatcomLab项目的主函数，执行配置逻辑并启动窗口
    """

    #执行主要脚本
    import argparse
    parser= argparse.ArgumentParser(description='UASF Datcom程序的前端界面程序')
    parser.add_argument('--PyDatcomWorkspace', action='store', dest='PyDatcomWorkspace',
                                 default='~/.PyDatcomLab/',
                                 help='项目目录')
    parser.add_argument('--logDir', action='store', dest='logDir',
                                 default='~/.PyDatcomLab/log/',
                                 help='日志文件')
    parser.add_argument('--MathJaxPath', action='store', dest='MathJaxPath',
                                 default='./3rdparty/MathJax/',
                                 help='MathJaxPath库的路径')
#    parser.add_argument('--logLevel', action='append_const', dest='logLevel',
#                            const='INFO',        default=[],
#                            help='日志等级 info,error,warning')
    parser.add_argument('--isDevelop ', action="store_true", default=False,  dest='isDevelop',
                                  help='是否开发模式 开发模式使用代码树中的Datcom定义文件，在使用模式下使用~/.PyDatcomLab下的配置文件')

    argments = parser.parse_args()
    #配置整个项目的基础配置
    PyDatcomLabProperties ={}
    #设置开发标志,不传递参数时使用False，即非开发模式
    if hasattr(argments, 'isDevelop'):
        PyDatcomLabProperties.update( {'isDevelop':argments.isDevelop ,})
    else:
        PyDatcomLabProperties.update( {'isDevelop':False ,})

    #确保相对的导入能够起到作用，需要导入相对路径
    mainPath = os.path.split(os.path.realpath(__file__))[0]
    #在开发模式下添加所有的路径，在安装模式下不使用
    if PyDatcomLabProperties['isDevelop'] :
        sys.path.append(os.path.abspath(os.path.join(mainPath, '..')))
        sys.path.append(os.path.abspath(os.path.join(mainPath, 'GUIs')))
        sys.path.append(os.path.abspath(os.path.join(mainPath, 'GUIs', 'PlaneConfiguration')))
        sys.path.append(os.path.abspath(os.path.join(mainPath, 'GUIs', 'components')))
        sys.path.append(os.path.abspath(os.path.join(mainPath, 'GUIs', 'tools')))
        sys.path.append(os.path.abspath(os.path.join(mainPath, 'GUIs', 'tools', 'XMLEditer')))
        sys.path.append(os.path.abspath(os.path.join(mainPath, 'GUIs', 'tools', 'HelperSystem')))
        sys.path.append(os.path.abspath(os.path.join(mainPath, 'Core')))
    else:
        #即使是发布状态，仍然是需要部分导入的，是对资源文件的集中引用导致的bug
        #添加路径，可以避免PyQt5生成的界面代码找不到资源文件
        sys.path.append(os.path.abspath(os.path.join(mainPath, 'GUIs')))  #PyDatcomLab_rc
        sys.path.append(os.path.abspath(os.path.join(mainPath, 'GUIs', 'tools', 'HelperSystem'))) #resource_rc
        sys.path.append(os.path.abspath(os.path.join(mainPath, 'GUIs', 'tools', 'XMLEditer')))     #tools_rc
    #保存包位置
    PyDatcomLabProperties.update( {'DatcomModuleDirectory':mainPath , })


    #建立项目文件夹 ~\.PyDatcomLab
    if hasattr(argments, 'PyDatcomWorkspace') and os.path.exists( argments.PyDatcomWorkspace):
        tDefaultDir = argments.PyDatcomWorkspace
    else:
        tDefaultDir = os.path.join(os.path.expanduser('~'), '.PyDatcomLab')
    tDirlist =  {
               'PyDatcomWorkspace':os.path.join(tDefaultDir) ,
               'LogDirectory':os.path.join(tDefaultDir, 'log') ,
               'SQliteDB':os.path.join(tDefaultDir, 'DB') ,
               'ConfigDirectory':os.path.join(tDefaultDir, 'config') ,
               'WikiDirectory':os.path.join(tDefaultDir, 'wiki'),
               #'extras':os.path.join(configPath, 'extras')
    }
    for tDir in tDirlist.keys():
        if  not os.path.exists(tDirlist[tDir]):
            os.makedirs(tDirlist[tDir])
    #将项目目录配置更新到配置列表
    PyDatcomLabProperties.update(tDirlist)

    #配置日志系统
    #设置日志目录
    if hasattr(argments, 'logDir') and os.path.exists( argments.logDir):
        PyDatcomLabProperties.update({'LogDirectory':argments.logDir , })
    #设置日志级别
    logFileName = time.strftime('%Y-%m-%d_%H-%M-%S',time.localtime(time.time()))
    logfile = os.path.abspath(os.path.join(PyDatcomLabProperties['LogDirectory'], logFileName))
    logChannel = r'Datcomlogger'
    logLevel = logging.INFO
    try:
        logger = InitLogger(tlogFile =logfile, tlogName = logChannel , tlogLevel = logLevel)
        PyDatcomLabProperties.update({'LogFile':logfile})
    except  Exception as e:
        print("无法创建日志系统！%s"%e)

    #创建配置文件目录
    try:
        tDefaultDefinePath = os.path.join(mainPath, 'config','datcomDefinitionDefault.xml')
        #复制默认的配置文件到工作目录，可能会掩盖部分的错误
        bPath = os.path.join(PyDatcomLabProperties['ConfigDirectory'], 'datcomDefine.xml')
        #开始复制基本配置文件
        if not os.path.exists(bPath):
            shutil.copyfile(tDefaultDefinePath,bPath)
        #判断是否直接修改安装包的默认数据
        if 'isDevelop' in PyDatcomLabProperties and PyDatcomLabProperties['isDevelop']:
            #用于开发目的时，临时启用
            bPath = tDefaultDefinePath
        else:
            #应用模式,使用用户的配置
            bPath = os.path.join(PyDatcomLabProperties['ConfigDirectory'], 'datcomDefine.xml')
        #更新配置
        PyDatcomLabProperties.update({'DatcomDefineFile':bPath})
    except Exception as e:
        logger.error("复制配置文件异常! %s"%(e))

    #修改默认的对象类
    from PyDatcomLab.Core.DictionaryLoader import  DTdictionary
    DTdictionary.defaultConfig = DTdictionary(PyDatcomLabProperties['DatcomDefineFile'])

    #创建并使用PyDatcomLab的配置文件，提供模型库等信息



    #创建Wiki目录，用来存储产生的doc Html资源
    try:
        bPath = PyDatcomLabProperties['WikiDirectory']
        sPath = os.path.abspath(os.path.join(mainPath, 'docs', 'PyDatcomLab-Help'))
        #创建wiki目录但是没有复制，直接使用代码中的wiki
        if not os.path.exists(bPath):
#            os.makedirs(bPath)
            shutil.copytree(sPath,bPath)
        elif not os.listdir(bPath):
            os.rmdir(bPath)
            shutil.copytree(sPath,bPath)
        #更新doc路径配置
        if 'isDevelop' in PyDatcomLabProperties and PyDatcomLabProperties['isDevelop']:
            PyDatcomLabProperties.update({'docDirectory':sPath})
        else:
            PyDatcomLabProperties.update({'docDirectory':bPath})
    except Exception as e:
        logger.error("复制Wiki文件系统异常：%s"%e)

    #复制附加资源 copy extras to ~/.PyDatcomLab for tests
    try:
        bPath = os.path.join(PyDatcomLabProperties['PyDatcomWorkspace'], 'extras')
        sPath = os.path.abspath(os.path.join(mainPath, 'extras'))
        if not os.path.exists(bPath):
            shutil.copytree(sPath,bPath)
        PyDatcomLabProperties.update({'ProjectDirectory':os.path.join(bPath, 'PyDatcomProjects') })
    except  Exception as e:
        logger.error("复制Extras资源出错：%s"%(e))

    #创建基础数据库
    #import sqlite3
    #dbFile = os.path.join(dirList['SQliteDB'], 'datcom.db')
    #if not os.path.exists(dbFile):

    #定义MathJax附加库的路径
    tMathJax = os.path.realpath(os.path.join(PyDatcomLabProperties['DatcomModuleDirectory'],  '3rdparty', 'MathJax', 'MathJax.js'))
    #推定传入参数类型
    if hasattr(argments, 'MathJaxPath') :
        if os.path.isabs(argments.MathJaxPath) and os.path.exists(argments.MathJaxPath):
            tMathJax = os.path.realpath(os.path.join(argments.MathJaxPath,  'MathJax.js'))
        if not os.path.isabs(argments.MathJaxPath):
            tMP = os.path.realpath(os.path.join(PyDatcomLabProperties['DatcomModuleDirectory'], argments.MathJaxPath, 'MathJax.js'))
            if os.path.exists(tMP):
                tMathJax = tMP
    #更新MathJax库路径定义
    if os.path.exists(tMathJax):
        PyDatcomLabProperties.update({'MathJaxPath':tMathJax})
        PyDatcomLabProperties.update({'MathJaxOnLinePath':r'http://cdn.mathjax.org/mathjax/latest/MathJax.js'})
    else:
        try:
            t3rdRoot = os.path.join(PyDatcomLabProperties['DatcomModuleDirectory'],  '3rdparty')
            tTar = os.path.join(t3rdRoot, 'MathJax.zip')
            if os.path.exists(tTar):
                logger.info("开始解压MathJax，请耐心等待！")
                shutil.unpack_archive(tTar,  os.path.join(t3rdRoot, 'MathJax'))
                logger.info("MathJax解压完毕！")
            else:
                logger.error("不存在MathJax的压缩包！")
        except Exception as e:
            logger.error("无法解压MathJax！%s"%e)

    #导入主要的窗体
    from PyDatcomLab.GUIs.MainWindow import DatcomMainWindow
    #进入系统的主循环
    app = QtWidgets.QApplication(sys.argv)
    #获得当前系统风格
    #QtWidgets.QApplication.setStyle(QtWidgets.QStyleFactory.create("Fusion"))
    #启动 Datcm的MainWindow   ，更新全局配置给MainWindows
    mainWin = DatcomMainWindow( iProperties = PyDatcomLabProperties)
    mainWin.logger = logger
    logger.info("启动了DatcomMainWindow")
    mainWin.show()
    #退出应用
    sys.exit(app.exec_())

if __name__ == '__main__':
    main()



