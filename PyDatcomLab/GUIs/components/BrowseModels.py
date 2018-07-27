# -*- coding: utf-8 -*-

"""
Module implementing DlgBrowseModels.
"""

from PyQt5.QtCore import pyqtSlot, pyqtSignal, QSize, QPoint, Qt
from PyQt5.QtWidgets import QDialog, QFileDialog, QMenu, QTableWidgetItem
from PyQt5.QtGui import QIcon, QPixmap
import logging
import os
from xml.etree import ElementTree  as ET

from PyDatcomLab.GUIs.components.NewModel import NewModelDlg 
from PyDatcomLab.GUIs.components.ModelPreview import ModelPreview as Mp
from PyDatcomLab.Core.PyDatcomConfigLoader import defaultConfig as dtConfig
#导入界面
from .Ui_BrowseModels import Ui_BrowseModel


class BrowseModels(QDialog, Ui_BrowseModel):
    """
    模型浏览器部分的内容
    """
    
    emit_ModelSelected = pyqtSignal(object)
    
    def __init__(self, parent=None, iConfig = dtConfig):
        """
        Constructor
        
        @param parent reference to the parent widget
        @type QWidget
        """
        super(BrowseModels, self).__init__(parent)
        self.setupUi(self)
        #self.splitter.setStretchFactor(1, 4)
        #日志
        self.logger = logging.getLogger(r'Datcomlogger')
        #加载datcom的配置文件
        if iConfig is None: 
            self.dtConfig = dtConfig
        else:
            self.dtConfig = iConfig                       
        #设置模型信息
        self.extList = ['.xml', '.dcXML', '.dcxml']
        self.libraryKeyWord = 'ModelLibrary'
        self.rootTag  = self.dtConfig.getLibraryRootTag(self.libraryKeyWord)  #获取库文件的根节点的tag
        self.MEKeys = list(dtConfig.getLibraryElementTemplate(self.libraryKeyWord ))
        #装载表头
        if self.MEKeys is None:
            self.logger.error("尝试读取ProjectLibrary并不存在定义！")
        else:
            self.ModelSet.setColumnCount(len(self.MEKeys))
            self.ModelSet.setHorizontalHeaderLabels(self.MEKeys)    
            self.ModelSet.horizontalHeaderItem(1).setTextAlignment(Qt.AlignRight)        
            self.ModelSet.setContextMenuPolicy(Qt.CustomContextMenu)    
        self.lastEditingRow = -1
        #刷新模型数据
        self.dataSet = None
        self._resetModel()
        #界面参数
        self.curPos = QPoint(0, 0)
        self.curWidget = None
        self.popMenu = None
        
        #初始化界面
        

    def getCurrentModelPath(self):
        """
        返回当前的模型的路径，没有对应行返回None
        如果找不到对应列，返回None
        其他返回对应的Path值
        """
        tRow = self.ModelSet.currentRow()
        tPathKey = self.dtConfig.getPathKeyByLibraryName(self.libraryKeyWord)
        if tRow <0  or tPathKey is None:
            return None
        else:
            #获得对应的key          
            tCIndex =  self.MEKeys.index(tPathKey)
            if tCIndex >= 0:
                return self.ModelSet.item(tRow, 1).text()
            else:
                return None
        
    @pyqtSlot()
    def on_pushButton_ChoiseDir_clicked(self):
        """
        点击选择目录按钮的响应函数
        """

        tDir = QFileDialog.getExistingDirectory(self,"打开模型目录", ''
                                    , QFileDialog.DontUseNativeDialog) 
        if not os.path.exists(tDir):
            self.logger.error("Try 打开模型目录%s 不存在！"%tDir)
            return 
            
        self.logger.info("Try 打开模型目录")            
        #保证当前目录在选择的范围内
        tIndex = self.comboBox_Dirs.findText(tDir, Qt.MatchExactly | Qt.MatchCaseSensitive)
        if tIndex == -1:
            self.comboBox_Dirs.addItem(tDir, None) 
            self.comboBox_Dirs.setCurrentIndex(self.comboBox_Dirs.count() -1)
        elif tIndex != self.comboBox_Dirs.currentIndex():
            self.comboBox_Dirs.setCurrentIndex(tIndex)
        else:
            self.AddModels(self.modelDir)
        
    def AddModels(self, tDir):
        """
        批量添加目录下的模型
        @para dir 模型所在目录
        """
        #保证当前目录在选择的范围内
        tIndex = self.comboBox_Dirs.findText(tDir, Qt.MatchExactly | Qt.MatchCaseSensitive)
        if tIndex == -1:
            self.comboBox_Dirs.addItem(tDir, None) 
            self.comboBox_Dirs.setCurrentIndex(self.comboBox_Dirs.count() -1)
        elif tIndex != self.comboBox_Dirs.currentIndex():
            self.comboBox_Dirs.setCurrentIndex(tIndex)
        
        #清空当前表格
        for aFile in os.listdir(tDir):
            # os.path.splitext():分离文件名与扩展名
            if os.path.splitext(aFile)[1] in self.extList :
                self.AddModel(tDir, aFile)


    def AddModel(self,tDir,  tFile):
        """
        添加一个模型
        """
        #检查文件系统
        fpath = os.path.join(tDir, tFile)
        if not os.path.isfile(fpath):
            return            #    
        fN, extN = os.path.splitext(tFile)
        if extN not in self.extList:
            return 
        #检查内容是否符合XML格式要求
        tCName = None
        try:            
            tRoot = ET.parse(fpath).getroot()
            if tRoot.tag != self.rootTag:
                self.logger.info("AddModels()测试模型文件的的根节点Tag，期望%s，实际：%s！忽略该文件"%(self.rootTag, tRoot.tag))
                return   
            else:
                tCName = tRoot.attrib.get('CName', None) #获得CASE Name，使用先验知识
        except Exception as e:
            self.logger.error("AddModels()测试模型文件过程中发生异常，%s ：%s"%(repr(e), str(e)))
        #添加一行 
        tTemplate =  self.dtConfig.getLibraryElementTemplate(self.libraryKeyWord)
        tTemplate.update({'ModelName':tCName,
        'path':fpath, 
        })
        if self.dtConfig.addItemToLibrary(self.libraryKeyWord, tTemplate) == '成功添加':
            #刷新模型
            self._resetModel()
            self.ModelSet.setCurrentCell(self.ModelSet.rowCount() -1, 0)   

        
    def _resetModel(self):
        """
        根据config的配置重置QTableWidget的模型数据
        """
        if self.dtConfig is None : return 
        self.dataSet = self.dtConfig.getLibrary(self.libraryKeyWord)
        self.ModelSet.clearContents()
        self.ModelSet.setRowCount(0)
        for iT in self.dataSet :
            tRowCount = self.ModelSet.rowCount() 
            self.ModelSet.insertRow(tRowCount)
            pix1 = QPixmap(r":/card/rc_card/亚音速常规布局.jpg")                
            self.ModelSet.setItem(tRowCount, 0, QTableWidgetItem(QIcon(pix1.scaled(QSize(100,100))),iT.get('ModelName', '')))            
            self.ModelSet.setItem(tRowCount, 1, QTableWidgetItem(iT.get('path', '.') ))
            self.ModelSet.item(tRowCount, 1).setFlags(Qt.NoItemFlags|Qt.ItemIsEnabled)
            #self.ModelSet.item(tRowCount, 1).setFlags(Qt.NoItemFlags|Qt.ItemIsSelectable|Qt.ItemIsEnabled)
            #self.ModelSet.setItem(tRowCount, 2, QTableWidgetItem(''))        
         

    
    @pyqtSlot(QPoint)
    def on_ModelSet_customContextMenuRequested(self, pos):
        """
        Slot documentation goes here.
        
        @param pos DESCRIPTION
        @type QPoint
        """
        self.curPos = pos
        self.curWidget = self.ModelSet        
        posG = self.curWidget.mapToGlobal(pos)
        self.popMenu = QMenu(self.curWidget)
        self.popMenu.addAction(self.actionNewModel)
        self.popMenu.addAction(self.actionAddModel)
        self.popMenu.addAction(self.actionRemoveModel)
        self.popMenu.addAction(self.actionPreviewModel)
        self.curWidget.setContextMenuPolicy(Qt.CustomContextMenu)
        self.popMenu.exec(posG)
    
    @pyqtSlot()
    def on_actionNewModel_triggered(self):
        """
        Slot documentation goes here.
        """
        dlg         = NewModelDlg()
        dlg.exec() 
        if dlg.result() == QDialog.Accepted:
            mPath = dlg.getModelPath()
            self.AddModel(os.path.dirname(mPath), os.path.basename(mPath))
    
    @pyqtSlot()
    def on_actionAddModel_triggered(self):
        """
        Slot documentation goes here.
        """
        baseDir = os.path.expanduser('~')
        if os.path.exists(self.comboBox_Dirs.currentText() ): 
            baseDir = self.comboBox_Dirs.currentText()
        #打开文件选择对话框
        tFiles = QFileDialog.getOpenFileNames(self,"选择模型文件", 
                    baseDir, 
                    "Datcom Project Files (*.dcxml *.xml )")
        for iF in tFiles[0]:
            if os.path.exists(iF):
                fN, extN = os.path.splitext(os.path.basename(iF))
                self.AddModel(os.path.dirname(iF), os.path.basename(iF))
 
        #切换模型,认为AddModel将数据添加到最后一行
        #self.on_ModelSet_itemDoubleClicked( self.ModelSet.item(self.ModelSet.rowCount() - 1, 0))
        
    @pyqtSlot()
    def on_actionRemoveModel_triggered(self):
        """
        从列表移除模型，但不会删除模型
        """        
        aItem = self.curWidget.indexAt(self.curPos)
        if  aItem.row() >= 0 :  
            tTemplate =  self.dtConfig.getLibraryElementTemplate(self.libraryKeyWord)
            tTemplate.update({'ModelName':self.curWidget.item(aItem.row(), 0).text(),
            'path':self.curWidget.item(aItem.row(), 1).text(),         })
            self.dtConfig.delItemFromLibrary(self.libraryKeyWord, tTemplate)          
            self.curWidget.removeRow(aItem.row())
        else:
            self.logger.info("没有命中任何行")

    
    @pyqtSlot(int, int, int, int)
    def on_ModelSet_currentCellChanged(self, currentRow, currentColumn, previousRow, previousColumn):
        """
        Slot documentation goes here.
        
        @param currentRow DESCRIPTION
        @type int
        @param currentColumn DESCRIPTION
        @type int
        @param previousRow DESCRIPTION
        @type int
        @param previousColumn DESCRIPTION
        @type int
        """

#        #发送模型信号
#        
#        if previousRow != currentRow:
#            if not self.ModelSet.itemAt(currentRow, 1) is None :
#                self.emit_ModelSelected.emit( self.ModelSet.item(currentRow, 1).text())

    #void QTableWidget::cellChanged(int row, int column)
    @pyqtSlot(int, int)
    def on_ModelSet_cellChanged(self, iRow, iColumn):
        """
        cellChanged的槽函数，用来刷新编辑属性
        因为在系统中设置了不可编辑路径，所以只编辑名称
        """
        if iColumn == 0: 
            #ModelName列
            self.dataSet[iRow].update({'ModelName':self.ModelSet.item(iRow, 0).text(),     })
            self.dtConfig.setLibrary(self.libraryKeyWord, self.dataSet)  
    
    @pyqtSlot(int)
    def on_comboBox_Dirs_currentIndexChanged(self, index):
        """
        重新加载当前目录的模型
        
        @param index DESCRIPTION
        @type int
        """
        # 在此处重新加载当前目录的模型
        pass

    
    @pyqtSlot()
    def on_actionPreviewModel_triggered(self):
        """
        打开模型预览窗口.
        """
        # 打开模型预览窗口
        aItem = self.curWidget.indexAt(self.curPos)
        if  aItem.row() < 0 :   
            self.logger.info("没有命中任何行") 
            return         
        tPath = self.ModelSet.item(aItem.row(), 1).text()
        self.PreViewdlg = Mp()
        self.PreViewdlg.loadModel(tPath)
        self.isDcModel = True
        self.PreViewdlg.show()
            
        
    
    @pyqtSlot(QTableWidgetItem)
    def on_ModelSet_itemDoubleClicked(self, item):
        """
        在双击项目是加载到中央.
        
        @param item DESCRIPTION
        @type QTableWidgetItem
        """
        if self.lastEditingRow != item.row():
            if not self.ModelSet.itemAt(item.row(), 1) is None :
                self.lastEditingRow = item.row()
                self.emit_ModelSelected.emit( self.ModelSet.item(item.row(), 1).text())
