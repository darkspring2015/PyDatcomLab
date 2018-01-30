# -*- coding: utf-8 -*-

"""
Module implementing reDatcomparser.
"""

from PyQt5.QtCore import pyqtSlot, Qt
from PyQt5.QtWidgets import QMainWindow, QApplication, QFileDialog, QTextEdit
from PyQt5.QtGui import QColor, QTextFormat, QTextCursor, QTextCharFormat
import sys, os, re
import importlib

#sys.path.append(os.path.abspath(os.path.join(r'.', 'PyDatcomLab', 'Core')))
#from PyDatcomLab.Core import parser as P

from Ui_datcomParsermain import Ui_reDatcomparser


class reDatcomparser(QMainWindow, Ui_reDatcomparser):
    """
    Class documentation goes here.
    """
    def __init__(self, parent=None):
        """
        Constructor
        
        @param parent reference to the parent widget
        @type QWidget
        """
        super(reDatcomparser, self).__init__(parent)
        self.setupUi(self)
        
        #初始化
        self.lineEdit_Modul.setText(r'PyDatcomLab.Core.parser')
        self.module = None
        self.on_pushButton_load_clicked()
        
    
    @pyqtSlot()
    def on_lineEdit_Modul_editingFinished(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet

    
    @pyqtSlot()
    def on_pushButton_load_clicked(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        tMstr = self.lineEdit_Modul.text()

        try:
            if self.module is None :
                self.module = importlib.import_module(tMstr)
            else:
                self.module = importlib.reload(self.module)
            #self.module = __import__(tMstr) 
        except :
            print("加载模块失败：%s ！"%tMstr)
            return
        #加载所有re
        if self.module is None : return 
        tClass = self.module.DatcomParser()
        self.comboBox_re.clear()
        for attr in dir(tClass):
            # 加载re_前缀的属性
            if attr[0:3] == 're_':
                self.comboBox_re.addItem(attr, eval('tClass.%s'%attr))
            if attr[0:2] == 't_':
                # 获取导入obj方法。
                class_attr_obj = getattr(tClass, attr)
                if hasattr(class_attr_obj, '__call__'):
                # 执行函数
                    self.comboBox_re.addItem(attr, class_attr_obj.__doc__)
                else:
                    self.comboBox_re.addItem(attr, eval('tClass.%s'%attr))
                
            
    @pyqtSlot(int)
    def on_comboBox_re_currentIndexChanged(self, index):
        """
        Slot documentation goes here.
        
        @param index DESCRIPTION
        @type int
        """
        # TODO: not implemented yet        
        if self.comboBox_re.itemData(index) is None:
            self.plainTextEdit_reg.setPlainText('')
        else:
            self.plainTextEdit_reg.setPlainText(self.comboBox_re.itemData(index))
    
    @pyqtSlot()
    def on_pushButton_click_clicked(self):
        """
        Slot documentation goes here.
        加载备选文件
        """
        # TODO: not implemented yet
        aFile, ext = QFileDialog.getOpenFileName(self,"选择文件", 
                    None, 
                    "Datcom Files (*.dcxml *.xml *.inp *.* )")
        if os.path.exists(aFile):
            fN, extN = os.path.splitext(os.path.basename(aFile))
            with open(aFile) as f:
                file_data = f.read()
            self.plainTextEdit_input.setPlainText(file_data)   
            
    @pyqtSlot()
    def on_actionRun_triggered(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        tText = self.plainTextEdit_input.toPlainText()
        self.plainTextEdit_input.setPlainText(tText)
        tRe = self.plainTextEdit_reg.toPlainText()
        if tText == "" or tRe == "":
            return         
        #执行分析        
        regex = re.compile(tRe, re.I)
        extraSelections =[]
      
        tDoc = self.plainTextEdit_input.document()
        lColors = [Qt.red, Qt.blue, Qt.cyan, Qt.yellow, Qt.green,Qt.magenta, 
                   Qt.darkRed,Qt.darkBlue, Qt.darkCyan, Qt.darkYellow,  Qt.darkGreen, Qt.darkMagenta, 
                   #Qt.gray, Qt.darkGray, Qt.lightGray, 
                   ]
        
        for iM in regex.finditer(tText):
            #迭代访问所有的匹配
            """"""
            i=0

            for iG in iM.regs:
                if iG[0] == -1 or iG[1]==-1:
                    continue
                #移动光标

                #selection = QTextEdit.ExtraSelection()
                cur = QTextCursor(tDoc)
                if(cur.hasSelection()):
                    cur.clearSelection()
                cur.beginEditBlock()
                color_format = QTextCharFormat(cur.charFormat())
                color_format.setForeground(lColors[i%12])
                i+=1
                cur.setPosition(iG[0])
                #self.plainTextEdit_input.setTextCursor(cur)
                cur.setPosition(iG[1], QTextCursor.KeepAnchor)                   
                cur.mergeCharFormat(color_format)
                cur.endEditBlock()

#                lineColor = QColor(Qt.yellow).lighter(160)
#                selection.format.setProperty(QTextFormat.FullWidthSelection, True)
#                selection.format.setBackground(lineColor)  
#                #selection.format.
#         
#                extraSelections.append(selection)
            
        #self.plainTextEdit_input.setExtraSelections(extraSelections)  #设置高亮
        
        

    
    @pyqtSlot()
    def on_pushButton_Run_clicked(self):
        """
        Slot documentation goes here.
        """
        # TODO: not implemented yet
        self.on_actionRun_triggered()
        
        
if __name__ == "__main__":
   # import sys
    app = QApplication(sys.argv)
    reDatcomparser = reDatcomparser()
    reDatcomparser.show()
    sys.exit(app.exec_())
