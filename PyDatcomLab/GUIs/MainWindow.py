# -*- coding: utf-8 -*-

"""
Module implementing DatcomMainWindow.
"""

import os
import platform  #判断操作系统
from PyQt5.QtCore import pyqtSlot
from PyQt5.QtWidgets import QMainWindow, QLabel, QMessageBox, QFileDialog
from PyQt5 import  QtCore,  QtWidgets
from PyQt5.QtGui import  QStandardItem, QStandardItemModel

from Ui_MainWindow import Ui_MainWindow
#from PyDatcomLab.GUIs  import   

from PyDatcomLab.GUIs.components import *
from PyDatcomLab.GUIs.InputCard import *
from PyDatcomLab.Core import projectManager as PM
from PyDatcomLab.Core import  datcomRunner  
from PyDatcomLab.Core.DictionaryLoader import  DTdictionary, defaultDatcomDefinition as DDefine
from PyDatcomLab.Core.PyDatcomConfigLoader import  PyDatcomLabConfig as dtConfig
from PyDatcomLab.GUIs.tools.XMLEditer import XMLEditer 
from PyDatcomLab.GUIs.tools.HelperSystem.HelperBrowses import PyMarkDownHelper



class DatcomMainWindow(QMainWindow, Ui_MainWindow):
    """
    Class documentation goes here.
    """
    def __init__(self, parent=None, iProperties ={}):
        """
        Constructor
        
        @param parent reference to the parent widget
        @type QWidget
        """
        super(DatcomMainWindow, self).__init__(parent)
        self.setupUi(self)
        #添加日志系统
        #添加日志系统
        self.logForm = logForm.logForm()
        self.logger = self.logForm.getLogger()
        
        #初始化配置信息
        tDefaultDir = os.path.join(os.path.expanduser('~'), '.PyDatcomLab')
        sysstr = platform.system()
        if(sysstr =="Windows"):
            tExe = os.path.join(os.path.dirname(__file__), r'..\Bin\datcom.exe')
        elif(sysstr == "Linux"):
            tExe = os.path.join(os.path.dirname(__file__), r'..\Bin\datcom.lnx')
        elif(sysstr == "Mac"): 
            tExe = os.path.join(os.path.dirname(__file__), r'..\Bin\datcom.mac')
        else:
            self.logger.error("DatcomMainWindow()判断操作系统%s，不受支持"%sysstr)
            
        #定义属性
        self.Properties ={'PyDatcomWorkspace':tDefaultDir, 
                                'ConfigFile':os.path.join(tDefaultDir, 'config', 'PyDatcomLabConfig.xml'), 
                                'PrjectDirectory':os.path.join(tDefaultDir,  'extras', 'PyDatcomProjects'),      
                                'DatcomExecute': tExe
                                }
        self.Properties.update(iProperties)
        #定义基本的项目信息存储
        self.PyDatcomLabConfig = dtConfig(self.Properties['ConfigFile'])  #加载配置到Main
        #定义DatcomDefine的实体
        if 'DatcomDefineFile' in self.Properties and os.path.isfile(self.Properties['DatcomDefineFile']):
            self.dtDefine = DTdictionary(self.Properties['DatcomDefineFile'])
        else:
            self.dtDefine = DDefine
        #配置MainWindow的Dock系统信息
        self.setDockNestingEnabled(True)
        
        #创建数据
        self.InitProjectsData()
        self.InitModelData()
        self.currentModelPath = os.getcwd()
        self.currentCASE = None
        
        #创建UI逻辑
        self.centralWidgetUsed = False

        
        #初始化存储Dock配置信息的位置
        self.docksConfig={}  #key 是dock的name，内容是{}，包括
#        self.docksConfig['DefaulLeft'] = self.dockWidget_Left
#        self.docksConfig['DefaulRight'] = self.dockWidget_Right
#        self.docksConfig['DefaulBottom'] = self.dockWidget_Bottom
        
        #内部窗口
        self.XMLEditer = None
        self.dtXMLEditer = None
        self.docHelper = None

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
        响应浏览模型action
        """
        # TODO: not implemented yet
        
        #遍历目录，获得所有的模型
        self.ModelBrowseDlg = BrowseModels.BrowseModels()
        #self.ModelBrowseDlg.setModal(True)
        self.ModelBrowseDlg.show()
        #raise NotImplementedError
    
    @pyqtSlot()
    def on_actionCopyModel_triggered(self):
        """
        复制模型
        """
        tSourceModel = ""
        tModelBrowse = self.docksConfig['模型预览窗口'].widget()
        if tModelBrowse :
            tPath = tModelBrowse.getCurrentModelPath()
            if os.path.exists(tPath):
                tSourceModel = tPath
      
        #获得当前模型的信息
        if not os.path.exists(tSourceModel):
            tSourceModel = r"~/"
        button, fType = QFileDialog.getOpenFileName(self,"选择源模型的配置文件", 
                    tSourceModel, "Datcom Model Files (*.dcxml *.xml )")
        if os.path.exists(button):
            self.PreViewdlg = ModelPreview.ModelPreview()
            self.PreViewdlg.loadModel(button)
            self.PreViewdlg.isCopy = True
            self.PreViewdlg.show()
            
    
    @pyqtSlot()
    def on_actionCopyCASE_triggered(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        #raise NotImplementedError
    
    @pyqtSlot()
    def on_actionImportModel_triggered(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        #raise NotImplementedError
    
    @pyqtSlot()
    def on_actionDocs_triggered(self):
        """
        打开帮助文件系统
        """
        if self.docHelper is None:
            self.docHelper = PyMarkDownHelper(self, self.Properties)
        self.docHelper.show()
        

    
    @pyqtSlot(str)
    def on_statusBar_windowTitleChanged(self, title):
        """
        Slot documentation goes here.
        
        @param title DESCRIPTION
        @type str
        """
        # TODO: not implemented yet
        #raise NotImplementedError
    
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
        aDlg.exec()
        prj = aDlg.getData()
        
        self.PM = PM.projectManager()
        prjPath = self.PM.CreateProject( prj['Path'] , prj['ProjectName'],
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
            prjMgr.logger = self.logger
            actions = [self.actionNewProject , 
                       self.actionOpenProject, 
                       self.actionSaveProject, 
                       ]
            prjMgr.BindingAction(actions)
            #绑定数据 
            if self.ProjectsModel is None:
                self.InitProjectsData()
            prjMgr.BindingModel(self.ProjectsModel)
            prjMgr.hiddenToolBar(True)
            
            #创建日志窗口并添加
            self.dock_Prjmanager = QtWidgets.QDockWidget('项目管理器', self)  
            self.dock_Prjmanager.setAllowedAreas(QtCore.Qt.LeftDockWidgetArea|QtCore.Qt.RightDockWidgetArea|QtCore.Qt.BottomDockWidgetArea)          
            self.dock_Prjmanager.setWidget(prjMgr)  
            #linger
#            sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Fixed)
#            sizePolicy.setHorizontalStretch(0)
#            sizePolicy.setVerticalStretch(0)
#            sizePolicy.setHeightForWidth(self.dock_Prjmanager.sizePolicy().hasHeightForWidth())  
            #linger end
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
        button, p = QFileDialog.getOpenFileName(self,"打开项目文件", 
                    os.path.expanduser("~/.PyDatcom/"), "Datcom Project Files (*.dcxml *.dcprj *.xml ) | all (*.*)")
        if os.path.exists(button):
            fn, ext = os.path.splitext(os.path.basename(button))
            self.ProjectsModel.appendRow([QStandardItem(fn), QStandardItem(button), QStandardItem('')])
            self.docksConfig['项目管理器'].widget().addProject(button)
        
    
    @pyqtSlot()
    def on_actionImageTips_triggered(self):
        """
        Slot documentation goes here.
        """

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
        模型浏览窗口的触发条件
        """
        # TODO: not implemented yet
        if '模型预览窗口'  not in self.docksConfig.keys():
            #create ProjectsManager
            browseMod = BrowseModels.BrowseModels( iConfig = self.PyDatcomLabConfig )  #创建时给系统复制
            #创建日志窗口并添加
            self.dock_BrowseModels = QtWidgets.QDockWidget('模型预览窗口', self)             
            self.dock_BrowseModels.setAllowedAreas(QtCore.Qt.LeftDockWidgetArea|QtCore.Qt.RightDockWidgetArea|QtCore.Qt.BottomDockWidgetArea)          
            self.dock_BrowseModels.setWidget(browseMod)  
            self.addDockWidget(QtCore.Qt.RightDockWidgetArea, self.dock_BrowseModels)  
            #self.dock_BrowseModels.setFloating(True)
            self.dock_BrowseModels.resize(QtCore.QSize(200, 600))
            self.docksConfig['模型预览窗口'] = self.dock_BrowseModels            
            #设置初始化目录
            #browseMod.setPreviewDirectory(self.Properties['ProjectDirectory'])
            
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
        if  self.currentCASE is not None and self.centralWidgetUsed :
            #当前模型存在,则提示是否写入到XML
            button = QMessageBox.question(self, r"切换模型",
                                   r"保存当前的Model吗?",
                                   QMessageBox.Yes | QMessageBox.No)
            if button == QMessageBox.Yes:
                try:
                    self.currentCASE.writeToXML(self.currentModelPath)
                except Exception as e:    
                    self.logger.error(r"保存模型异常：%s"%e)
                #输入日志
                self.logger.info(r"保存模型到：%s"%self.currentModelPath)
        #加载新模型
        #self.currentCASE = PlaneConfiguration.PlaneConfiguration(modelpath = modelPath)
        self.currentCASE = DatcomCASEEditer.DatcomCASEEditer(iModelpath = modelPath,  iDefine =self.dtDefine)        
        self.setCentralWidget(self.currentCASE)
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
       
        #获得当前的CASE
        tPC = self.centralWidget()
        if tPC is None or type(tPC ) not in [DatcomCASEEditer.DatcomCASEEditer]:
            QMessageBox.information(self, '提示', '没有打开任何模型！')
            return
        #创建运行目录
        import tempfile
        tmpPath= tempfile.mkdtemp(suffix='',prefix='Datcom')
        tFile = os.path.join(tmpPath, 'ex.inp')
        try:
            dcM = tPC.getModel()
            dcM.buildDatcomInputFile(tFile)
        except Exception as e:
            QMessageBox.information(self, '创建Datcom输入文件失败', '%s'%e)
            return
        
        #执行计算
        aRunner = datcomRunner.runner(problemDir=tmpPath,
                            execDatcomPath=self.Properties['DatcomExecute'])
        strRes = aRunner.runningPopen(exePath= self.Properties['DatcomExecute'],
                            problemFile=tFile,    tcwd =tmpPath  )
        tTiltie = ''
        if strRes[0] in [0, '0'] : 
            #返回码正确
            tTiltie = '完成了当前算例的计算'
            tReport = "完成算例!\n算例目录：\n{0}\n输出：\n{1}\n错误:\n{2}".format(tmpPath,strRes[1], strRes[2] )
            self.logger.info("完成了当前算例的计算。算例目录：%s"%tmpPath)
        else:
            tTiltie = '完成了当前算例的计算,但未成功'
            tReport = "未成功完成算例!\n返回码：{3}\n算例目录：\n{0} \n输出信息：\n{1} \n错误信息:\n {2} ".format(tmpPath,strRes[1], strRes[2] ,
                                                            strRes[0] + " : " +aRunner.getReturnCodeDescribe(strRes[0]))
            self.logger.info("当前算例的计算失败。算例目录：%s"%tmpPath)     
        #报告结果
        QMessageBox.information(self, tTiltie, '%s'%tReport)        
        
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
            tDoc = self.currentCASE.getDoc()
            #tDoc.writeToXML(self.currentModelPath)
            tDoc.save(self.currentModelPath)
            self.logger.info(r"保存模型到：%s"%self.currentModelPath)        
    
    @pyqtSlot()
    def on_actionXMLEditer_triggered(self):
        """
        Slot documentation goes here.
        """
        # TODO: 打开XML编辑器
        self.XMLEditer = XMLEditer.XMLEditer()
        self.XMLEditer.show()        
    
    @pyqtSlot()
    def on_actionDtcomDefineEditer_triggered(self):
        """
        Slot 打开CASE界面编辑器.
        """
        # TODO: not implemented yet
        if self.dtXMLEditer is None:
            self.dtXMLEditer = XMLEditer.XMLEditer()
            self.dtXMLEditer.Load(self.Properties['DatcomDefineFile'])
            self.dtXMLEditer.Command_ReloadDtDefine_triggered.connect(self.on_ReloadCaseUI)
        self.dtXMLEditer.expandToDepth(1)
        self.dtXMLEditer.show()   
        
    def on_ReloadCaseUI(self, iDefinePath):
        """
        Slot 重载中央的CASE界面
        """
        #重新加载datcomDefine配置文件到软件
        if os.path.isfile(iDefinePath):
            try:
                if self.dtDefine is None:
                    self.dtDefine = DTdictionary(iDefinePath)
                    self.Properties['DatcomDefineFile'] = iDefinePath
                else :
                    self.dtDefine.loadDictionory(iDefinePath)
                    #self.dtDefine = DTdictionary(iDefinePath)
                    self.Properties['DatcomDefineFile'] = iDefinePath
            except Exception as e:
                self.logger.warning("重新加载DatcomDefineFile出错：%s ！"%(iDefinePath))
                return
        else:
            self.logger.warning("输入不是有效的配置文件路径：%s ！"%(iDefinePath))
            return
            
        #刷新中心控件
        #保存当前的模型
        if  self.currentCASE is not None and self.centralWidgetUsed :
            #当前模型存在,则提示是否写入到XML
            button = QMessageBox.question(self, r"重新加载当前模型",
                                   r"重新加载当前模型，使Datcom定义生效？\n\t注意：操作将导致当前模型被写入到文件！",
                                   QMessageBox.Yes | QMessageBox.No)
            if button == QMessageBox.Yes:                    
                try:
                    #保存当前模型
                    self.currentCASE.writeToXML()
                    #加载新模型
                    self.currentCASE = DatcomCASEEditer.DatcomCASEEditer(iModelpath = self.currentModelPath,  iDefine =self.dtDefine)  
                    self.setCentralWidget(self.currentCASE)
                    self.currentModelPath =  self.currentModelPath
                    self.centralWidgetUsed = True                       
                except Exception as e:
                    self.logger.warning("重新加载模型出错 ！")
        else: 
            if os.path.isfile( self.currentModelPath):
                try:
                    #加载新模型
                    self.currentCASE = DatcomCASEEditer.DatcomCASEEditer(iModelpath = self.currentModelPath,  iDefine =self.dtDefine)  
                    self.setCentralWidget(self.currentCASE)
                    self.currentModelPath =  self.currentModelPath
                    self.centralWidgetUsed = True                       
                except Exception as e:
                    self.logger.warning("重新加载模型出错 ！")                

    @pyqtSlot()
    def on_actionUpdateCASEUIConfig_triggered(self):
        """
        Slot 更新Datcom Define，并重载中央的CASE界面.
        """
        if self.dtXMLEditer is None: return 
        self.on_ReloadCaseUI(self.Properties['DatcomDefineFile'])
