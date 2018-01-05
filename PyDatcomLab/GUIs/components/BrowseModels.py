# -*- coding: utf-8 -*-

"""
Module implementing DlgBrowseModels.
"""

from PyQt5.QtCore import pyqtSlot, pyqtSignal, QSize
from PyQt5.QtWidgets import QDialog, QFileDialog, QListView, QListWidgetItem
from PyQt5.QtGui import QIcon, QPixmap

from .Ui_BrowseModels import Ui_Dialog
import logging
import os

import PyDatcomLab.GUIs.PlaneConfiguration.card_rc_rc

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
        self.listWidget_Models.setViewMode(QListView.IconMode)
        self.listWidget_Models.setIconSize(QSize(100,100));
        self.logger = logging.getLogger(r'Datcomlogger')
    
    @pyqtSlot()
    def on_listWidget_Models_itemSelectionChanged(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        self.logger.info(r"点击了模型")

        fN = os.path.join(self.textEdit_Dir.toPlainText(),self.listWidget_Models.currentItem().text() )
        self.emit_ModelSelected.emit(fN)
        
        
    @pyqtSlot()
    def on_pushButton_ChoiseDir_clicked(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        self.modelDir = QFileDialog.getExistingDirectory(self,"打开模型目录", '.')
        self.logger.info("Try 打开模型目录")
        if self.modelDir is None:
            self.logger.error("无效的目录")
            return
            
        self.textEdit_Dir.setText(self.modelDir)
        
        self.AddModels(self.modelDir)
        
    def AddModels(self, dir):
        """
        """

        f_list = os.listdir(dir)
        # print f_list
        for i in f_list:
            # os.path.splitext():分离文件名与扩展名
            if os.path.splitext(i)[1] == '.xml':
                dirName, fileName = os.path.split(i)
                pix1 = QPixmap(r":/card/rc_card/亚音速常规布局.jpg");
                #pix1 = QPixmap(r"E:\Projects\PyDatcomLab\PyDatcomLab\GUIs\PlaneConfiguration\rc_card/亚音速常规布局.jpg");
                it = QListWidgetItem(QIcon(pix1.scaled(QSize(100,100))),fileName)
                it = self.listWidget_Models.addItem(it);
    
    def setPreviewDirectory(self, prjDir):
        """
        设置预览窗口
        """
        if not os.path.exists(prjDir):
            self.logger.error(r'尝试浏览的目录%s 不存在！'%prjDir)
            return
        
        self.AddModels(prjDir)
        self.textEdit_Dir.setText(prjDir)
        


        
        
