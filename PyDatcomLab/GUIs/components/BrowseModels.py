# -*- coding: utf-8 -*-

"""
Module implementing DlgBrowseModels.
"""

from PyQt5.QtCore import pyqtSlot, pyqtSignal, QSize, QPoint, Qt
from PyQt5.QtWidgets import QDialog, QFileDialog, QMenu, QTableWidgetItem
from PyQt5.QtGui import QIcon, QPixmap

from Ui_BrowseModels import Ui_BrowseModel
import logging
import os

from PyDatcomLab.GUIs.components.NewModel import NewModelDlg 
from PyDatcomLab.GUIs.components.ModelPreview import ModelPreview as Mp

#import PyDatcomLab.GUIs.PlaneConfiguration.card_rc_rc

class BrowseModels(QDialog, Ui_BrowseModel):
    """
    Class documentation goes here.
    """
    
    emit_ModelSelected = pyqtSignal(object)
    
    def __init__(self, parent=None):
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
        self.extList = ['.xml', '.dcXML', '.dcxml']
        
        #self 
        self.ModelSet.setColumnCount(3)
        self.ModelSet.setHorizontalHeaderLabels(['名称', '路径', '说明'])    
        self.ModelSet.horizontalHeaderItem(1).setTextAlignment(Qt.AlignRight)        
        self.ModelSet.setContextMenuPolicy(Qt.CustomContextMenu)    
        self.lastEditingRow = -1
        #界面参数
        self.curPos = QPoint(0, 0)
        self.curWidget = None
        self.popMenu = None
        
        #初始化界面
        

    def getCurrentModelPath(self):
        """
        返回当前的模型
        """
        tRow = self.ModelSet.currentRow()
        if tRow <0 :
            return ''
        else:
            return self.ModelSet.item(tRow, 1).text()
        
    @pyqtSlot()
    def on_pushButton_ChoiseDir_clicked(self):
        """
        Slot documentation goes here.
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
        self.ModelSet.clearContents()
        self.ModelSet.setRowCount(0)
        # print f_list
        for aFile in os.listdir(tDir):
            # os.path.splitext():分离文件名与扩展名
            if os.path.splitext(aFile)[1] in self.extList :
                #self.ModelSet.setRowCount(self.ModelSet.rowCount()+1)
                self.AddModel(tDir, aFile)
                

    def AddModel(self,tDir,  tFile):
        """
        添加一个模型
        """
        fpath = os.path.join(tDir, tFile)
        if not os.path.isfile(fpath):
            return            #    
        fN, extN = os.path.splitext(tFile)
        if extN not in self.extList:
            return 
        #添加一行    
        tRow = self.ModelSet.rowCount() 
        self.ModelSet.insertRow(tRow)
        pix1 = QPixmap(r":/card/rc_card/亚音速常规布局.jpg")                
        self.ModelSet.setItem(tRow, 0, QTableWidgetItem(QIcon(pix1.scaled(QSize(100,100))),fN))
        self.ModelSet.setItem(tRow, 1, QTableWidgetItem(os.path.abspath(fpath)))
        self.ModelSet.setItem(tRow, 2, QTableWidgetItem(''))

    def setPreviewDirectory(self, prjDir):
        """
        设置预览窗口
        """
        if not os.path.exists(prjDir):
            self.logger.error(r'尝试浏览的目录%s 不存在！'%prjDir)
            return
        
        self.AddModels(prjDir)
        #self.textEdit_Dir.setText(prjDir)
    
    @pyqtSlot(QPoint)
    def on_ModelSet_customContextMenuRequested(self, pos):
        """
        Slot documentation goes here.
        
        @param pos DESCRIPTION
        @type QPoint
        """
        # TODO: not implemented yet
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
        # TODO: not implemented yet
        dlg = NewModelDlg()
        dlg.exec()
        mPath = dlg.getModelPath()        
        self.AddModel(os.path.dirname(mPath), os.path.basename(mPath))
    
    @pyqtSlot()
    def on_actionAddModel_triggered(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        baseDir = r"~/"
        if os.path.exists(self.comboBox_Dirs.currentText() ): 
            baseDir = self.comboBox_Dirs.currentText()
        #打开文件选择对话框
        aFile, ext = QFileDialog.getOpenFileName(self,"选择模型文件", 
                    baseDir, 
                    "Datcom Project Files (*.dcxml *.xml )")
        if os.path.exists(aFile):
            fN, extN = os.path.splitext(os.path.basename(aFile))
            tRow = self.ModelSet.rowCount() 
            self.ModelSet.insertRow(tRow)
            pix1 = QPixmap(r":/card/rc_card/亚音速常规布局.jpg")                
            self.ModelSet.setItem(tRow, 0, QTableWidgetItem(QIcon(pix1.scaled(QSize(100,100))),fN))
            self.ModelSet.setItem(tRow, 1, QTableWidgetItem(os.path.abspath(aFile)))
            self.ModelSet.setItem(tRow, 2, QTableWidgetItem(''))
            
        #切换模型
        self.on_ModelSet_itemDoubleClicked( self.ModelSet.item(tRow, 0))
        
    @pyqtSlot()
    def on_actionRemoveModel_triggered(self):
        """
        从列表移除模型，但不会删除模型
        """
        
        aItem = self.curWidget.indexAt(self.curPos)
        if  aItem.row() >= 0 :            
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
    
    @pyqtSlot(int)
    def on_comboBox_Dirs_currentIndexChanged(self, index):
        """
        重新加载当前目录的模型
        
        @param index DESCRIPTION
        @type int
        """
        # 在此处重新加载当前目录的模型
        tDir = self.comboBox_Dirs.itemText(index)
        if os.path.exists(tDir):
            self.setPreviewDirectory(tDir)
    
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
                self.emit_ModelSelected.emit( self.ModelSet.item(item.row(), 1).text())
