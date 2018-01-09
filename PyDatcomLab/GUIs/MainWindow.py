# -*- coding: utf-8 -*-

"""
Module implementing DatcomMainWindow.
"""

import os
from PyQt5.QtCore import pyqtSlot
from PyQt5.QtWidgets import QMainWindow, QLabel, QMessageBox, QFileDialog
from PyQt5 import  QtCore,  QtWidgets
from PyQt5.QtGui import  QStandardItem, QStandardItemModel

from .Ui_MainWindow import Ui_MainWindow
#from PyDatcomLab.GUIs  import   

from PyDatcomLab.GUIs.components import BrowseModels , NewModel, logForm
from PyDatcomLab.GUIs.components import ProjectsManager
from PyDatcomLab.GUIs.components import ImageTips
from PyDatcomLab.GUIs.components import PlaneConfiguration, AddProject
from PyDatcomLab.Core import projectManager as PM
from PyDatcomLab.Core import  datcomRunner  



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
        
        #初始化配置信息
        self.prjPath = os.path.abspath(r"E:\Projects\PyDatcomLab\extras\PyDatcomProjects\1")
        self.exePath = os.path.join(os.path.dirname(__file__), r'..\Bin\datcom.exe')
        #配置MainWindow的Dock系统信息
        self.setDockNestingEnabled(True)
        
        #创建数据
        self.InitProjectsData()
        self.InitModelData()
        self.currentModelPath = os.getcwd()
        self.currentCASE = None
        
        #创建UI逻辑
        self.centralWidgetUsed = False
        self.nowPlaneConfiguration = None
        
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
        self.on_actionModelPreview_triggered()         #模型预览窗口
        
        #连接信号和槽
        self.docksConfig["模型预览窗口"].widget().emit_ModelSelected.connect(self.currentModelChanged)
        

        #全屏显示
        self.showMaximized()
        #self.showFullScreen()
     
  
    def InitProjectsData(self):
        """
        初始化项目信息的存储空间
        """  
        self.ProjectsModel = QStandardItemModel(0, 3)
        self.ProjectsModel.setHeaderData(0,QtCore.Qt.Horizontal,u"项目名称") 
        self.ProjectsModel.setHeaderData(1,QtCore.Qt.Horizontal,u"路径") 
        self.ProjectsModel.setHeaderData(2,QtCore.Qt.Horizontal,u"说明")
    
    def InitModelData(self):
        """
        初始化模型信息的存储空间
        """
        self.ModelStorge = QStandardItemModel(0, 3)
        self.ModelStorge.setHeaderData(0,QtCore.Qt.Horizontal,u"模型名称") 
        self.ModelStorge.setHeaderData(1,QtCore.Qt.Horizontal,u"路径") 
        self.ModelStorge.setHeaderData(2,QtCore.Qt.Horizontal,u"说明")

    
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
        浏览模型
        """
        # TODO: not implemented yet
        
        #遍历目录，获得所有的模型
        self.ModelBrowseDlg = BrowseModels.DlgBrowseModels()
        #self.ModelBrowseDlg.setModal(True)
        self.ModelBrowseDlg.show()
        #raise NotImplementedError
    
    @pyqtSlot()
    def on_actionCopyModel_triggered(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        
        #获得当前模型的信息
        
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

        #创建目录结构        
        aDlg = AddProject.AddProject()
        aDlg.show()
        prj = aDlg.getData()
        
        self.PM = PM.projectManager()
        prjPath = self.PM.newProject( prj['Path'] , prj['ProjectName'],
                    prj['ProjectName'],prj['Describe'])
        
        self.ProjectsModel.insertRow(self.ProjectsModel.rowCount(), 
                            [QStandardItem( prj['ProjectName']), 
                            QStandardItem(prjPath), 
                            QStandardItem(prj['Describe'])] 
                            )
                            
        self.docksConfig['项目管理器'].widget().BindingModel(self.ProjectsModel)

      
    
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
        #模式对话框
        button = QFileDialog.getOpenFileName(self,"打开项目文件", 
                    "~/", "Datcom Project Files (*.dcxml *.xml )")
        if os.path.exists(button):
            self.ProjectsModel
        
    
    @pyqtSlot()
    def on_actionImageTips_triggered(self):
        """
        Slot documentation goes here.
        """

        # TODO: not implemented yet
        if '可视化提示'  not in self.docksConfig.keys():
            #create ProjectsManager
            imgTips = ImageTips.ImageTips()
            #创建日志窗口并添加
            self.dock_ImageTips = QtWidgets.QDockWidget('可视化提示', self)             
            self.dock_ImageTips.setAllowedAreas(QtCore.Qt.LeftDockWidgetArea|QtCore.Qt.RightDockWidgetArea|QtCore.Qt.BottomDockWidgetArea)          
            self.dock_ImageTips.setWidget(imgTips)  
            self.addDockWidget(QtCore.Qt.RightDockWidgetArea, self.dock_ImageTips)  
            self.dock_ImageTips.setFloating(True)
            self.dock_ImageTips.resize(QtCore.QSize(300, 600))
            self.docksConfig['可视化提示'] = self.dock_ImageTips
            

            
            
        else:
            if self.docksConfig['可视化提示'].isHidden() :
                self.docksConfig['可视化提示'].show()
            else:
                self.docksConfig['可视化提示'].hide()
    
    @pyqtSlot()
    def on_actionNewModel_triggered(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        
        self.NewModel = NewModel.NewModelDlg()
        self.NewModel.show()
        
        #raise NotImplementedError
    
    @pyqtSlot()
    def on_actionModelPreview_triggered(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        if '模型预览窗口'  not in self.docksConfig.keys():
            #create ProjectsManager
            browseMod = BrowseModels.DlgBrowseModels()
            #创建日志窗口并添加
            self.dock_BrowseModels = QtWidgets.QDockWidget('模型预览窗口', self)             
            self.dock_BrowseModels.setAllowedAreas(QtCore.Qt.LeftDockWidgetArea|QtCore.Qt.RightDockWidgetArea|QtCore.Qt.BottomDockWidgetArea)          
            self.dock_BrowseModels.setWidget(browseMod)  
            self.addDockWidget(QtCore.Qt.RightDockWidgetArea, self.dock_BrowseModels)  
            #self.dock_BrowseModels.setFloating(True)
            self.dock_BrowseModels.resize(QtCore.QSize(200, 600))
            self.docksConfig['模型预览窗口'] = self.dock_BrowseModels
            
            #设置初始化目录
            browseMod.setPreviewDirectory(self.prjPath)
            
        else:
            if self.docksConfig['模型预览窗口'].isHidden() :
                self.docksConfig['模型预览窗口'].show()
            else:
                self.docksConfig['模型预览窗口'].hide()
                
    def currentModelChanged(self, modelPath):
        """
        更改当前模型是触发
        """
        self.logger.info("当前模型为%s"%modelPath)
        
        #保存当前的模型
        if  self.nowPlaneConfiguration is not None and self.centralWidgetUsed :
            #当前模型存在,则提示是否写入到XML
            button = QMessageBox.question(self, r"切换模型",
                                   r"保存当前的Model吗?",
                                   QMessageBox.Yes | QMessageBox.No)
            if button == QMessageBox.Yes:
                tDoc = self.nowPlaneConfiguration.getDoc()
                tDoc.writeToXML(self.currentModelPath)
                self.logger.info(r"保存模型到：%s"%self.currentModelPath)
        #加载新模型
        self.nowPlaneConfiguration = PlaneConfiguration.PlaneConfiguration(modelpath = modelPath)
        self.setCentralWidget(self.nowPlaneConfiguration)
        self.currentModelPath = modelPath
        self.centralWidgetUsed = True
    
    @pyqtSlot()
    def on_actionExit_triggered(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        button = QMessageBox.question(self, r"退出程序",
                                   r"确认退出程序?",
                                   QMessageBox.Yes | QMessageBox.No)
        if button == QMessageBox.Yes:
            self.close()
        #raise NotImplementedError
    
    @pyqtSlot()
    def on_actionRunning_triggered(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        
        #获得当前的CASE
        tPC = self.centralWidget()
        if tPC == 0:
            QMessageBox.information(self, '提示', '没有打开任何模型！')
            return
        dcM = tPC.getDoc()
        #创建运行目录
        import tempfile
        tmpPath= tempfile.mkdtemp(suffix='',prefix='Datcom')
        tFile = os.path.join(tmpPath, 'ex.inp')
        dcM.writeToDatcomInput(tFile)
        
        #执行计算
        aRunner = datcomRunner.runner(problemDir=tmpPath,
                            execDatcomPath=self.exePath)
        strRes = aRunner.runningPopen(exePath= self.exePath,
                            problemFile=tFile,    tcwd =tmpPath  )
        if strRes == "成功执行":
            self.logger.info("完成了当前算例的计算。算例目录：%s"%tmpPath)
        else:
            self.logger.info("当前算例的计算失败。算例目录：%s"%tmpPath)

        #弹出计算结果
        os.system("explorer.exe %s" % tmpPath) 
    
    @pyqtSlot()
    def on_actionSaveModel_triggered(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        #保存模型
            #当前模型存在,则提示是否写入到XML
        button = QMessageBox.question(self, r"保存模型",
                               r"保存当前的Model吗?",
                               QMessageBox.Yes | QMessageBox.No)
        if button == QMessageBox.Yes:
            tDoc = self.nowPlaneConfiguration.getDoc()
            tDoc.writeToXML(self.currentModelPath)
            self.logger.info(r"保存模型到：%s"%self.currentModelPath)        
