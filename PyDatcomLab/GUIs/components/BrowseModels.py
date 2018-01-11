# -*- coding: utf-8 -*-

"""
Module implementing DlgBrowseModels.
"""

from PyQt5.QtCore import pyqtSlot, pyqtSignal, QSize, QPoint, Qt
from PyQt5.QtWidgets import QDialog, QFileDialog, QMenu, QTableWidgetItem
from PyQt5.QtGui import QIcon, QPixmap

from .Ui_BrowseModels import Ui_Dialog
import logging
import os

from PyDatcomLab.GUIs.components.NewModel import NewModelDlg 

#import PyDatcomLab.GUIs.PlaneConfiguration.card_rc_rc

class DlgBrowseModels(QDialog, Ui_Dialog):
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
        super(DlgBrowseModels, self).__init__(parent)
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
        #界面参数
        self.curPos = QPoint(0, 0)
        self.curWidget = None
        self.popMenu = None
        
        #初始化界面
        

        
        
    @pyqtSlot()
    def on_pushButton_ChoiseDir_clicked(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        self.modelDir = QFileDialog.getExistingDirectory(self,"打开模型目录", ''
                                    , QFileDialog.DontUseNativeDialog) 
        self.logger.info("Try 打开模型目录")
        if self.modelDir is None:
            self.logger.error("无效的目录")
            return
            
        self.textEdit_Dir.setText(self.modelDir)
        
        self.AddModels(self.modelDir)
        
    def AddModels(self, dir):
        """
        """

        # print f_list
        for aFile in os.listdir(dir):
            # os.path.splitext():分离文件名与扩展名
            if os.path.splitext(aFile)[1] in self.extList :
                tItems =  self.ModelSet.findItems(aFile, Qt.MatchExactly)
                if len(tItems) >0:
                    continue
                self.AddModel(dir, aFile)
                

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
        self.textEdit_Dir.setText(prjDir)
    
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
        self.curWidget.setContextMenuPolicy(Qt.CustomContextMenu)
        self.popMenu.exec(posG)
    
    @pyqtSlot()
    def on_actionNewModel_triggered(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        dlg = NewModelDlg()
        dlg.show()
        mPath = dlg.getModelPath()        
        self.AddModel(os.path.dirname(mPath), os.path.basename(mPath))
    
    @pyqtSlot()
    def on_actionAddModel_triggered(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        baseDir = r"~/"
        if os.path.exists(self.textEdit_Dir.toPlainText() ): 
            baseDir = self.textEdit_Dir.toPlainText()
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
            


    
    @pyqtSlot()
    def on_actionRemoveModel_triggered(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
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
        # TODO: not implemented yet
        #发送模型信号
        
        if previousRow != currentRow:
            if not self.ModelSet.itemAt(currentRow, 1) is None :
                self.emit_ModelSelected.emit( self.ModelSet.item(currentRow, 1).text())
