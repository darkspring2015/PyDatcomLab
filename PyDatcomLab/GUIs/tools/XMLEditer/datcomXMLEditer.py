#！
"""
实现Datcom定制化的XMLEditer.
"""

#from PyQt5.QtCore import pyqtSlot, pyqtSignal ,  Qt #
#from PyQt5.QtWidgets import  QTreeWidgetItem, QFileDialog, QMessageBox,  QHeaderView, QMainWindow, QApplication
#from PyDatcomLab.Core.datcomTools import xml_Indent as indent

from PyQt5 import QtCore ,QtWidgets , QtGui

#from xml.etree import ElementTree  as ET

import logging  #, os


from PyDatcomLab.GUIs.tools.XMLEditer.XMLEditer import XMLEditer




class datcomXMLEditer(XMLEditer):
    """
    XMLEditer是编辑XML、Datocm基础配置的软件
    """
    #Command_ReloadDtDefine_triggered = pyqtSignal(str)  #用来传递重载DatcomDefine的命令，str指向新配置文件的路径
    
    def __init__(self, parent=None):
        """
        Constructor  Datcom定义的XMLEditer的构造函数
        
        @param parent reference to the parent widget
        @type QWidget
        """
        super(datcomXMLEditer, self).__init__(parent)
        #日志系统        
        self.logger = logging.getLogger(r'Datcomlogger')

        #设置右键菜单系统
        self.curPos = QtCore.QPoint()  #存储右键菜单时的坐标
        self.actionCollection = {}        #存储所有的右键菜单项  key:{'Action':aAction,}
        self.treeWidget_xml.setContextMenuPolicy(QtCore.Qt.CustomContextMenu)    
        self.treeWidget_xml.customContextMenuRequested.connect(self.on_TreeWidget_xml_customContextMenuRequested)
        #添加Action
        self._InitializeContextMenu()
        
    
    def _InitializeContextMenu(self):
        """
        定义所需要的右键菜单按钮
        """
        #添加属性节点
        tAction = QtWidgets.QAction(self)
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap(self.icoList['attrib']), QtGui.QIcon.Normal,QtGui.QIcon.Off)
        tAction.setIcon(icon)
        tAction.setObjectName("actionAddProperty")
        tAction.setText( "增加属性节点")
        tAction.setToolTip( "增加属性节点")
        self.actionCollection.update({'actionAddProperty':{'Action':tAction}}  ) 
        
        #删除属性节点
        tAction = QtWidgets.QAction(self)
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap(self.icoList['attrib']), QtGui.QIcon.Normal,QtGui.QIcon.Off)
        tAction.setIcon(icon)
        tAction.setObjectName("actionDeleteProperty")
        tAction.setText( "删除属性")
        tAction.setToolTip( "删除属性")
        self.actionCollection.update({'actionDeleteProperty':{'Action':tAction}}  ) 


        
    @QtCore.pyqtSlot(QtCore.QPoint)
    def on_TreeWidget_xml_customContextMenuRequested(self, pos):
        """
        右键内容菜单选项
        
        @param pos 点击的位置
        @type QPoint
        """
        self.curPos = pos
        self.curWidget = self.sender()        
        posG = self.curWidget.mapToGlobal(pos)
        self.popMenu = QtWidgets.QMenu(self.curWidget)
        #循环添加所有的Action
        for iA in self.actionCollection:
            self.popMenu.addAction(self.actionCollection[iA]['Action'])
        #设置并显示右键菜单
        self.curWidget.setContextMenuPolicy(QtCore.Qt.CustomContextMenu)
        self.popMenu.exec(posG)
        
    @QtCore.pyqtSlot()
    def on_actionAddProperty_triggered(self):
        """
        增加新属性的代码.
        """
        self.logger.info("新增属性")

        
    @QtCore.pyqtSlot()
    def on_actionDeleteProperty_triggered(self):
        """
        删除属性的代码.
        """
        self.logger.info("删除属性")  
