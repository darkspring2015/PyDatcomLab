# -*- coding: utf-8 -*-

"""
Module implementing DatcomMainWindow.
"""

from PyQt5.QtCore import pyqtSlot
from PyQt5.QtWidgets import QMainWindow, QLabel

from PyQt5 import  QtCore,  QtWidgets, QtGui

from .Ui_MainWindow import Ui_MainWindow
from PyDatcomLab.GUIs  import   logForm



class DatcomMainWindow(QMainWindow, Ui_MainWindow):
    """
    Class documentation goes here.
    """
    def __init__(self, parent=None):
        """
        Constructor
        
        @param parent reference to the parent widget
        @type QWidget
        """
        super(DatcomMainWindow, self).__init__(parent)
        self.setupUi(self)
        
        #配置MainWindow的Dock系统信息
        self.setDockNestingEnabled(True)
        
        #创建数据
        self.InitProjectsData()
        
        #初始化存储Dock配置信息的位置
        self.docksConfig={}  #key 是dock的name，内容是{}，包括
#        self.docksConfig['DefaulLeft'] = self.dockWidget_Left
#        self.docksConfig['DefaulRight'] = self.dockWidget_Right
#        self.docksConfig['DefaulBottom'] = self.dockWidget_Bottom
        
        #添加日志系统
        self.logForm = logForm.logForm()
        self.logger = self.logForm.getLogger()
        #添加日志窗口到下方dack中
        #self.singal
        self.on_actionLogWindow_triggered()
        self.on_actionProjectManager_triggered()
        

     
  
    def InitProjectsData(self):
        """    
        """  
        self.ProjectsModel = QtGui.QStandardItemModel(3, 3)
        self.ProjectsModel.setHeaderData(0,QtCore.Qt.Horizontal,u"项目名称") 
        self.ProjectsModel.setHeaderData(1,QtCore.Qt.Horizontal,u"路径") 
        self.ProjectsModel.setHeaderData(2,QtCore.Qt.Horizontal,u"说明")

    
    @pyqtSlot()
    def on_actionAbout_triggered(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        print('Test!')
        #raise NotImplementedError
    
    @pyqtSlot()
    def on_actionBrowse_triggered(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        raise NotImplementedError
    
    @pyqtSlot()
    def on_actionCopyModel_triggered(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        raise NotImplementedError
    
    @pyqtSlot()
    def on_actionCopyCASE_triggered(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        raise NotImplementedError
    
    @pyqtSlot()
    def on_actionImportModel_triggered(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        raise NotImplementedError
    
    @pyqtSlot()
    def on_actionDocs_triggered(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        raise NotImplementedError
    
    @pyqtSlot(str)
    def on_statusBar_windowTitleChanged(self, title):
        """
        Slot documentation goes here.
        
        @param title DESCRIPTION
        @type str
        """
        # TODO: not implemented yet
        raise NotImplementedError
    
    @pyqtSlot()
    def on_actionNewProject_triggered(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        label = QLabel( 'Action : 创建项目!')
        self.statusBar.addWidget(label)
        #raise NotImplementedError
    
    @pyqtSlot()
    def on_actionLogWindow_triggered(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        if '日志窗口'  not in self.docksConfig.keys():
            #创建日志窗口并添加
            self.dock_log = QtWidgets.QDockWidget('日志窗口', self)  
            self.dock_log.setAllowedAreas(QtCore.Qt.LeftDockWidgetArea|QtCore.Qt.RightDockWidgetArea|QtCore.Qt.BottomDockWidgetArea)          
            self.dock_log.setWidget(self.logForm)  
            self.addDockWidget(QtCore.Qt.BottomDockWidgetArea, self.dock_log)  
            self.docksConfig['日志窗口'] = self.dock_log  
        else:
            if self.docksConfig['日志窗口'].isHidden() :
                self.docksConfig['日志窗口'].show()
            else:
                self.docksConfig['日志窗口'].hide()
    
    @pyqtSlot()
    def on_actionProjectManager_triggered(self):
        """
        Slot documentation goes here.
        """
        from PyDatcomLab.GUIs import ProjectsManager
        # TODO: not implemented yet
        if '项目管理器'  not in self.docksConfig.keys():
            #create ProjectsManager
            prjMgr= ProjectsManager.ProjectsManager()
            #prjMgr.logger = self.logger
            actions = [self.actionNewProject , 
                       self.actionOpenProject, 
                       self.actionSaveProject, 
                       ]
            prjMgr.BindingAction(actions)
            #绑定数据 
            if self.ProjectsModel is None:
                self.InitProjectsData()
            prjMgr.BindingModel(self.ProjectsModel)
            
            #创建日志窗口并添加
            self.dock_Prjmanager = QtWidgets.QDockWidget('项目管理器', self)  
            self.dock_Prjmanager.setAllowedAreas(QtCore.Qt.LeftDockWidgetArea|QtCore.Qt.RightDockWidgetArea|QtCore.Qt.BottomDockWidgetArea)          
            self.dock_Prjmanager.setWidget(prjMgr)  
            self.addDockWidget(QtCore.Qt.LeftDockWidgetArea, self.dock_Prjmanager)  
            self.docksConfig['项目管理器'] = self.dock_Prjmanager

            
            
        else:
            if self.docksConfig['项目管理器'].isHidden() :
                self.docksConfig['项目管理器'].show()
            else:
                self.docksConfig['项目管理器'].hide()
    
    @pyqtSlot()
    def on_actionOpenProject_triggered(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        raise NotImplementedError
