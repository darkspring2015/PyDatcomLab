# -*- coding: utf-8 -*-

"""
Module implementing PyMarkDownHelper.
"""

from PyQt5.QtCore import pyqtSlot
from PyQt5.QtWidgets import QMainWindow
from PyQt5 import QtWidgets, QtWebEngineWidgets ,  QtCore, Qt
#
import os ,logging#,  codecs
#
from MarkdownEngine import  markdownEngine

from Ui_HelperBrowses import Ui_PyMarkDownHelper


class PyMarkDownHelper(QMainWindow, Ui_PyMarkDownHelper):
    """
    Class documentation goes here.
    帮助系统
    """
    def __init__(self, parent=None,  iProperties ={}):
        """
        Constructor 帮助系统
        
        @param parent reference to the parent widget
        @type QWidget
        @param iProperties reference to 配置信息
        @type dict        
        """
        super(PyMarkDownHelper, self).__init__(parent)
        #日志
        self.logger = logging.getLogger(r'Datcomlogger')        
        #定义配置参数
        self.Properties = {'extFilter': ["*.md"], 
                                'docDirectory':os.path.join(os.path.expanduser('~'), '.PyDatcomLab', 'wiki')
        }
        if iProperties is not None and type(iProperties) is dict:
            self.Properties.update(iProperties)            
        #数据模型
        self.helperDirectory =QtWidgets.QFileSystemModel()
        self.namefilter =  self.Properties['extFilter']
        self.helperDirectory.setNameFilters(self.namefilter)
        self.helperDirectory.setNameFilterDisables(False)       
        
        #设置界面
        self.setupUi(self)        
        #self.setWindowFlags(Qt.Qt.CustomizeWindowHint) 
        #吟唱
        self.hideToolBar(False)
        
        #添加基本的Page        
        self.tabWidget_right.clear()
        self.pageSet = {0:self.addWebBrowseToTab()}
        #UI参数
        self.baseSplitterSize = [400, 600]
        self.baseStretchFactor = [1, 4]
        #执行分裂期附加配置
        self.splitter.setStretchFactor(0, self.baseStretchFactor[0])
        self.splitter.setStretchFactor(1, self.baseStretchFactor[1])
        self.splitter.setSizes(self.baseSplitterSize)
        
        #设置引擎
        self.docEngine = markdownEngine(self.Properties)

        #Test 初始化
        testPath = os.path.realpath(self.Properties['docDirectory'])
        self.helperDirectory.setRootPath(testPath)
        self.treeView_Helper.setModel(self.helperDirectory)
        self.treeView_Helper.setRootIndex(self.helperDirectory.index(testPath))
        #修改左侧Tree
        self.treeView_Helper.setColumnHidden(1, True)
        self.treeView_Helper.setColumnHidden(2, True)
        self.treeView_Helper.setColumnHidden(3, True)
        

      
    def hideToolBar(self, iStutas =True):
        """
        隐藏工具栏
        """
        self.toolBar.setVisible(iStutas)
    
    def addWebBrowseToTab(self,iLable = '',  index =-1, iUrl= r'https://github.com/darkspring2015/PyDatcomLab/blob/2.0-cleanall/wiki/PyDatcomLab%E4%BD%BF%E7%94%A8%E8%AF%B4%E6%98%8E%E6%96%87%E4%BB%B6.md'):
        """
        在Index位置添加一个QtWebEngineWidgets
        """
        #添加
        tWeb = QtWebEngineWidgets.QWebEngineView()
        tWeb.load(QtCore.QUrl(iUrl))
        if iLable =='':
            iLable = '结果'
        if index < 0 or  index >= self.tabWidget_right.count():
            self.tabWidget_right.addTab(tWeb, iLable)
        else:
            self.tabWidget_right.insertTab(index, tWeb, iLable)        
            

    def resizeEvent(self, event):
        """
        """
        all = sum(self.baseStretchFactor)
        if all != 0:
            tStretchSize = []
            for iS in self.baseStretchFactor:
                tStretchSize.append(self.width() * iS/ all)
            self.splitter.setSizes(tStretchSize)

        
    
    @pyqtSlot(str)
    def on_lineEdit_search_textEdited(self, p0):
        """
        Slot documentation goes here.
        
        @param p0 DESCRIPTION
        @type str
        """
        tStr = self.lineEdit_search.text()
        self.logger.info("%s"%tStr)
        #QtWidgets.QMessageBox.information(self,"aaa", "%s"%tStr)

    
    @pyqtSlot()
    def on_actionOpenDirectory_triggered(self):
        """
        Slot documentation goes here.
        """
        #打开文件选择对话框
        tDir = QtWidgets.QFileDialog.getExistingDirectory(self,"打开帮助文件目录", ''
                                    , QtWidgets.QFileDialog.DontUseNativeDialog) 
        if not os.path.exists(tDir):
            self.logger.error("Try 打开模型目录%s 不存在！"%tDir)
            return 
        self.helperDirectory.setRootPath(tDir)
        #定义给目录树
        self.treeView_Helper.setModel(self.helperDirectory)
        self.treeView_Helper.setRootIndex(self.helperDirectory.index(tDir))

        
    
    @pyqtSlot()
    def on_actionExit_triggered(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        self.close()
    
    @pyqtSlot(QtCore.QModelIndex)
    def on_treeView_Helper_clicked(self, index):
        """
        Slot documentation goes here.
        
        @param index DESCRIPTION
        @type QModelIndex
        """
        tPath = self.helperDirectory.filePath(index)
        if self.docEngine  is None:
            self.docEngine  = markdownEngine(self.Properties)
        try:
            tOutPath , tHtml = self.docEngine .md2Html(tPath)
            #更新右侧显示
            tWidget = self.tabWidget_right.widget(0)
            if tWidget is not None:
                tWidget.load(QtCore.QUrl.fromLocalFile(tOutPath))
            #显示
            self.logger.info("%s"%tOutPath)
            
        except Exception as e:
            self.logger.warning(e)
