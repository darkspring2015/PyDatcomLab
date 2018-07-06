
# -*- coding: utf-8 -*-

"""
Module implementing DatcomCASEEditer.

"""
import os 
import logging
#Qt  
from PyQt5.QtCore import pyqtSlot, pyqtSignal
from PyQt5.QtWidgets import QDialog, QWidget
from PyQt5 import QtWidgets, QtGui, QtCore

#PyDatcomLab
from PyDatcomLab.Core import datcomModel as dcModel
from PyDatcomLab.Core.DictionaryLoader import  defaultDatcomDefinition as DDefine  , DTdictionary as dtDefinition
#from PyDatcomLab.Core.datcomCaseConstraint import  datcomCaseConstraint as dcConstrait
from PyDatcomLab.GUIs.InputCard.DatcomCASEEditerUi import DatcomCASEEditerUi
from PyDatcomLab.GUIs.InputCard.DatcomWidgetBase import DatcomWidgetBase


class DatcomCASEEditer(QDialog, DatcomCASEEditerUi):
    """
    Class documentation goes here.
    """
    # TODO: Define the action for the program
    Singal_NMACHChanged                    = pyqtSignal(int)          #用来接收NMACH的变化的信号
    
    def __init__(self, parent=None, modelpath = None, iDefine = DDefine):
        """
        Constructor
        
        @param parent reference to the parent widget
        @type QWidget
        """
        super(DatcomCASEEditer, self).__init__(parent)

        #初始化日志系统
        self.logger = logging.getLogger(r'Datcomlogger')
        #创建内部数据结构        
        if iDefine is None or type(iDefine) != dtDefinition:
            self.dtDefine = DDefine
        else:
            self.dtDefine = iDefine
            
        self.dcModelPath =  modelpath
        if os.path.isfile(modelpath):
            self.dcModel = dcModel.dcModel(modelpath, self.dtDefine)             
        else:
            self.dcModel = dcModel.dcModel(dtDefine = self.dtDefine) 
        
        #内部数据
        self.lastIndex  = -1    
        self.namelistSet = {}
        #初始化界面
        self.setupUi(self)        
        #添加页码
        self.Initialize()        
        #连接各个页面之间的信号
       
        
    def Initialize(self):
        """
        初始化所有的page页
        """
        #定义Action
        self.defineActions()
        #给TabWidget的标签栏定义菜单
        self.tabWidget_Configuration.tabBar().setContextMenuPolicy(QtCore.Qt.CustomContextMenu)
        self.tabWidget_Configuration.tabBar().customContextMenuRequested.connect(self.on_tabBar_customContextMenuRequested)
        #link slot and signal 对Action执行绑定
        QtCore.QMetaObject.connectSlotsByName(self)
        
    def loadTabs(self):
        """
        删除当前所有数据并重新从Model中加载，可能造成数据丢失
        """
        self.tabWidget_Configuration.clear()  
        for mdName in self.dcModel.getNamelistCollection():
            self.addTab(mdName)
            

    def addTab(self, iNamelist):
        """
        添加一个Tab承载iNamelist 
        """
        dF                =  self.dtDefine.getNamelistDefineByName(iNamelist)
        dFNmlstAtrrib  =  self.dtDefine.getNamelistAttributeByName(iNamelist)
        if dF == {} :
            self.logger.error("不存在%s对应的选项卡定义"%iNamelist)
            return    
        if 'DisplayName' not in dFNmlstAtrrib.keys():
            dFNmlstAtrrib["DisplayName"] = iNamelist
        if iNamelist in self.namelistSet and self.namelistSet[iNamelist]['Widget'] is not None:
            aW = self.namelistSet[iNamelist]['Widget']
        else:
            aW = DatcomWidgetBase(iNamelist = iNamelist , iModel =  self.dcModel, parent = self)     
        #aW.setObjectName(iNamelist)
        tIndex = self.tabWidget_Configuration.addTab( aW, dFNmlstAtrrib['DisplayName'])   
        self.namelistSet.update({iNamelist:{'Widget':aW, 'isRemove':False}})
        #设置不能关闭
        if iNamelist in self.dtDefine.getBasicNamelistCollection():
            self.tabWidget_Configuration.tabBar().setTabButton(tIndex, QtWidgets.QTabBar.RightSide, None)
            
        #设置NMACH信号
        if iNamelist == 'FLTCON':
            aW.Singal_NMACHChanged.connect(self.Singal_NMACHChanged)
        else:
            self.Singal_NMACHChanged.connect(aW.Singal_NMACHChanged)
            
        return tIndex
            
    
    def defineActions(self):
        """
        设置窗口的Action
        """
        # TODO addNamelist deleteNamelist save load saveas
        #addNamelist
        self.actionAddNamelist = QtWidgets.QAction(self)
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap(":/InputCard/images/InputCard/addLine.ico"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionAddNamelist.setIcon(icon)
        self.actionAddNamelist.setObjectName("actionAddNamelist")
        self.actionAddNamelist.setText( "增加选项卡")
        self.actionAddNamelist.setToolTip( "增加选项卡")
        #deleteNamelist
        self.actiondeleteNamelist = QtWidgets.QAction(self)
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap(":/InputCard/images/InputCard/deleteLine.ico"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actiondeleteNamelist.setIcon(icon)
        self.actiondeleteNamelist.setObjectName("actionDeleteNamelist")
        self.actiondeleteNamelist.setText( "删除选项卡")
        self.actiondeleteNamelist.setToolTip( "删除选项卡")
        #save
        self.actionSave = QtWidgets.QAction(self)
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap(":/mainwindow/images/mainwindow/modelsave.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionSave.setIcon(icon)
        self.actionSave.setObjectName("actionSave")
        self.actionSave.setText( "保存数据")
        self.actionSave.setToolTip( "保存数据") 
        #saveas
        self.actionSaveas = QtWidgets.QAction(self)
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap(":/mainwindow/images/mainwindow/modelsave.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionSaveas.setIcon(icon)
        self.actionSaveas.setObjectName("actionSaveas")
        self.actionSaveas.setText( "数据另存为")
        self.actionSaveas.setToolTip( "数据另存为") 
        #buildDatcomInput
        self.actionBuildDatcomInput = QtWidgets.QAction(self)
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap(":/mainwindow/images/mainwindow/case-run.png"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.actionBuildDatcomInput.setIcon(icon)
        self.actionBuildDatcomInput.setObjectName("actionBuildDatcomInput")
        self.actionBuildDatcomInput.setText( "运行算例")
        self.actionBuildDatcomInput.setToolTip( "运行算例") 
        #创建菜单
        self.popMenu = QtWidgets.QMenu(self.tabWidget_Configuration)
        #定义
        self.popMenu.addAction(self.actionAddNamelist)
        self.popMenu.addAction(self.actiondeleteNamelist)
        self.popMenu.addAction(self.actionSave)
        self.popMenu.addAction(self.actionSaveas)


    @pyqtSlot()
    def on_actionSave_triggered(self):
        """
        响应actionSave的槽函数
        """
        button = QtWidgets.QMessageBox.question(self, r"保存模型",
                               r"保存当前的Model吗?",
                               QtWidgets.QMessageBox.Yes | QtWidgets.QMessageBox.No)
        if button == QtWidgets.QMessageBox.Yes:    
            try:
                for iI in range(0, self.tabWidget_Configuration.count()):
                    tWd = self.tabWidget_Configuration.widget(iI)
                    if tWd is not None and type(tWd) is DatcomWidgetBase:
                        tWd.getDoc()
                self.writeToXML(self.dcModelPath)
                self.logger.info("保存模型到%s"%self.dcModelPath)
            except Exception as e:
                self.logger.error("保存模型时出错：%s!"%repr(e))
                
            
    @pyqtSlot()
    def on_actionSaveas_triggered(self):
        """
        响应actionSaveas的槽函数
        """
        self.logger.info("save model!")
        try:
            self.writeToXML(self.dcModelPath)
        except Exception as e:
            self.logger.error("保存模型时出错：%s!"%e.message)   
            
    @pyqtSlot()
    def on_actionDeleteNamelist_triggered(self):
        """
        响应actionSaveas的槽函数
        """
        # TODO     实现
        pass
        
    @pyqtSlot()
    def on_actionAddNamelist_triggered(self):
        """
        响应actionSaveas的槽函数
        """
        # TODO     实现
        self.on_tabWidget_Configuration_tabBarDoubleClicked(-1)        
            
    @pyqtSlot(QtCore.QPoint)
    def on_tabBar_customContextMenuRequested(self, pos):
        """
        Slot documentation goes here.
        
        @param pos DESCRIPTION
        @type Qt.QPoint
        """   
        self.curPos = pos        
        posG = self.mapToGlobal(pos)
        self.popMenu.exec(posG)

            
    @pyqtSlot(int)
    def on_tabWidget_Configuration_tabCloseRequested(self, index):
        """
        响应tabBar栏的关闭行为
        1. 当为-1时，创建新的选项卡
        """
        #判断是否是最小配置中的选项卡
       
        tW = self.tabWidget_Configuration.widget(index)
        if tW is None:return
        tNamelist = tW.NameList
        if tNamelist not in self.dtDefine.getBasicNamelistCollection():
            self.tabWidget_Configuration.removeTab(index)
            self.namelistSet.update({tNamelist:{'Widget':tW, 'isRemove':True}}) 
            self.dcModel.deleteNamelist(tNamelist)
        else:
            self.logger.error("用户在尝试关闭一个不允许关闭的Tab，这是一个Bug")
    
    def on_addNamelist(self, iNamelist):
        """
        响应添加Namelist的操作 
        iNamelist是要添加的Namelist的名称
        """
        if iNamelist is None or iNamelist=="":
            return
        tHaveAdd = self.dcModel.getNamelistCollection().keys()
        if iNamelist in tHaveAdd:
            tIndex = self.indexOfText(iNamelist)
            if tIndex > 0: #如果已存在则切换
                self.tabWidget_Configuration.setCurrentIndex(tIndex)
            else:
                self.logger.error("数据异常：Model和Tab对不上")
        else:
            self.dcModel.addNamelist(iNamelist)
            self.addTab(iNamelist)
 
    def indexOfText(self, iLable):
        """
        查找第一个符合(iLable)要求的Tab
        """
        for iI in range(0, self.tabWidget_Configuration.count()):
            tW = self.tabWidget_Configuration.widget(iI)
            if tW.Namelist == iLable:
                return iI
        return -1
            
    @pyqtSlot(int)
    def on_tabWidget_Configuration_tabBarDoubleClicked(self, index):
        """
        响应tabBar栏的双击行为
        1. 当为-1时，创建新的选项卡
        """
        self.logger.info("on_tabWidget_Configuration_tabBarDoubleClicked %d"%index)
        if index <0: #index == -1
            tWd = QtWidgets.QDialog(self)
            tWd.setObjectName("ChooseNamelist")
            tWd.resize(200, 40)
            tWd.setSizeGripEnabled(True)
            tWd.verticalLayout = QtWidgets.QVBoxLayout(tWd)
            tWd.verticalLayout.setObjectName("verticalLayout")
            spacerItem = QtWidgets.QSpacerItem(20, 40, QtWidgets.QSizePolicy.Minimum, QtWidgets.QSizePolicy.Expanding)
            tWd.verticalLayout.addItem(spacerItem)
            tWd.horizontalLayout = QtWidgets.QHBoxLayout()
            tWd.horizontalLayout.setObjectName("horizontalLayout")
            tWd.label = QtWidgets.QLabel(tWd)
            tWd.label.setObjectName("label")
            tWd.horizontalLayout.addWidget(tWd.label)
            spacerItem1 = QtWidgets.QSpacerItem(40, 20, QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Minimum)
            tWd.horizontalLayout.addItem(spacerItem1)
            tWd.comboBox_Namelist = QtWidgets.QComboBox(    tWd)
            sizePolicy = QtWidgets.QSizePolicy(QtWidgets.QSizePolicy.Expanding, QtWidgets.QSizePolicy.Fixed)
            sizePolicy.setHorizontalStretch(0)
            sizePolicy.setVerticalStretch(0)
            sizePolicy.setHeightForWidth(tWd.comboBox_Namelist.sizePolicy().hasHeightForWidth())
            tWd.comboBox_Namelist.setSizePolicy(sizePolicy)
            tWd.comboBox_Namelist.setObjectName("comboBox_Namelist")
            tWd.horizontalLayout.addWidget(    tWd.comboBox_Namelist)
            tWd.verticalLayout.addLayout(tWd.horizontalLayout)
            spacerItem2 = QtWidgets.QSpacerItem(20, 40, QtWidgets.QSizePolicy.Minimum, QtWidgets.QSizePolicy.Expanding)
            tWd.verticalLayout.addItem(spacerItem2)
            
            #添加Namelist到list
            tAllNamelist = self.dtDefine.getNamelistCollection()
            tHaveAdd = self.dcModel.getNamelistCollection().keys()
            for iC in tAllNamelist :
                if iC not in tHaveAdd:
                    tWd.comboBox_Namelist.addItem(iC)
            
            #运行界面获得结果
            tWd.exec()
            tChoose = tWd.comboBox_Namelist.currentText()
            if tChoose  is not None:
                self.on_addNamelist(tChoose)
        else: #双击Tab本身 。忽略
            pass
                
 
    
    @pyqtSlot(int)
    def on_tabWidget_Configuration_currentChanged(self, index):
        """
        Slot documentation goes here.
        
        @param index DESCRIPTION
        @type int
        """
        # TODO: not implemented yet
        #切换数据表单时将数据写入到dcModel中
        if self.lastIndex != -1:
            tW =self.tabWidget_Configuration.widget(self.lastIndex)
            if not tW is None :
                self.dcModel = tW.getDoc()
        #刷新
        self.lastIndex = index
        if not self.tabWidget_Configuration.widget(index) is None:
            if not type(self.tabWidget_Configuration.widget(index)) is QWidget:
                self.tabWidget_Configuration.widget(index).setModel(self.dcModel)

    def getDoc(self):
        """
        刷新Doc并返回
        """
        tabCount = self.tabWidget_Configuration.count()
        if tabCount > 0:
            for num in range(0,tabCount):
                self.tabWidget_Configuration.widget(num).getDoc()

                
        return self.dcModel
    
    def writeToXML(self, tFile = None):
        """
        将结果写入到XML文件中
        """
        if tFile is None: tFile = self.dcModelPath
        
        #判断dcModelPath
        if not os.path.isfile(tFile):
            self.logger.error("输入的XML文件不存在%s"%tFile)
            #raise  Exception("输入的XML文件不存在%s"%tFile)
            return
        
        #写入到XML
        self.dcModel.save(tFile)
        
        

if __name__ == "__main__":
    import sys
    bPath = os.path.join(os.path.expanduser('~'), '.PyDatcomLab', 'extras', 'PyDatcomProjects', '1')
    sPath  = os.path.join(bPath,    r'case2.xml')
    obPath  = os.path.join(bPath,  r'case3.xml')
    dtPath  = os.path.join(bPath,   r'case3.inp')
    #sPath  = r'E:\Projects\PyDatcomLab\extras\PyDatcomProjects\1\case2.xml'
    #obPath = r'E:\Projects\PyDatcomLab\extras\PyDatcomProjects\1\case3.xml'
    #dtPath = r'E:\Projects\PyDatcomLab\extras\PyDatcomProjects\1\case3.inp'
    app = QtWidgets.QApplication(sys.argv)
    Dialog = DatcomCASEEditer(modelpath = sPath)
    Dialog.show()
    sys.exit(app.exec_())
    
