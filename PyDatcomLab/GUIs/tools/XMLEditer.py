# -*- coding: utf-8 -*-

"""
Module implementing XMLEditer.
"""

from PyQt5.QtCore import pyqtSlot,  Qt 
from PyQt5.QtWidgets import  QTreeWidgetItem, QFileDialog, QMessageBox,  QHeaderView, QMainWindow, QApplication

from PyQt5.QtGui import QIcon

from xml.etree import ElementTree  as ET

import logging, os


from Ui_XMLEditer import Ui_XMLEditer

editingModeDef = ['ValueOnly', 'Readonly', 'All']

class XMLEditer(QMainWindow, Ui_XMLEditer):
    """
    Class documentation goes here.
    """
    def __init__(self, parent=None):
        """
        Constructor
        
        @param parent reference to the parent widget
        @type QWidget
        """
        super(XMLEditer, self).__init__(parent)
        self.setupUi(self)
        
        #日志系统        
        self.logger = logging.getLogger(r'Datcomlogger')
        self.sourceXML  = ""
        self.objectXML  = ""
        #初始化相关配置
        self.treeWidget_xml.header().setSectionResizeMode(QHeaderView.ResizeToContents )
        self.treeWidget_xml.header().setStretchLastSection( True)
        #
        self.EditingMode = 'All' # ['ValueOnly', 'Readonly', 'All']
        self.isSetCheckstate = True
        self.itemFlags = Qt.ItemIsEnabled | Qt.ItemIsSelectable | Qt.ItemIsEditable\
                        |Qt.ItemIsAutoTristate|Qt.ItemIsUserCheckable 
        self.icoList = {
        'root':':/nodes/res/qscintilla.png', 
        'middle':':/nodes/res/defaultIcon.png', 
        'node':':/nodes/res/drawFill.png', 
        'attrib':':/nodes/res/comment.png'
        }
        self.ExpandingDepth = 2

    def setEditingMode(self, tMode):
        """
        设置对应编辑模式
        """
        
        if tMode == 'Readonly':
            self.isSetCheckstate = False
            self.itemFlags = Qt.ItemIsEnabled | Qt.ItemIsSelectable
        elif tMode == 'ValueOnly':
            self.isSetCheckstate = False
            self.itemFlags = Qt.ItemIsEnabled | Qt.ItemIsSelectable | Qt.ItemIsEditable
        elif tMode == 'All':
            self.isSetCheckstate = True
            self.itemFlags = Qt.ItemIsEnabled | Qt.ItemIsSelectable | Qt.ItemIsEditable\
                        |Qt.ItemIsAutoTristate|Qt.ItemIsUserCheckable 
        if self.EditingMode !=  tMode:
            self.on_actionReload_triggered()
            self.EditingMode =  tMode
                        
    def listDom(self, docElem, pItem):
        """ 
        遍历添加到docElem到pItem节点
        """
        #添加本节点和属性
        if pItem:
            tItem = QTreeWidgetItem(pItem)
        else:
            tItem = QTreeWidgetItem(self.treeWidget_xml)
            tItem.setIcon(0, QIcon(self.icoList['root']))
        #写入当前节点的名称
        tItem.setText(0, docElem.tag)  
        if self.isSetCheckstate:  tItem.setCheckState(0, Qt.Checked)
        tItem.setFlags(self.itemFlags )     
        #写入节点的属性信息
        if  docElem.attrib :
            tAttrItem = QTreeWidgetItem(tItem)
            tAttrItem.setText(0, '属性')
            tAttrItem.setIcon(0, QIcon(self.icoList['attrib']))
            if self.isSetCheckstate: tAttrItem.setCheckState(0, Qt.Checked)
            tAttrItem.setFlags(self.itemFlags )
            for iAttr in docElem.attrib:
                subAttr = QTreeWidgetItem(tAttrItem)
                subAttr.setText(0, iAttr)
                subAttr.setText(1, docElem.attrib[iAttr]) 
                subAttr.setIcon(0, QIcon(self.icoList['attrib']))
                #subAttr.setFlags(self.itemFlags )
                if self.isSetCheckstate: subAttr.setCheckState(0, Qt.Checked)   

        #判断是否是叶节点            
        if len(docElem.getchildren()) == 0:
            tItem.setText(1, docElem.text) 
            tItem.setIcon(0, QIcon(self.icoList['node']))
        #判断是否有子节点
        else:
            tItem.setIcon(0, QIcon(self.icoList['middle']))
            for iChild in docElem.getchildren():            
                self.listDom(iChild, tItem)      
    #END listDom
    
    def recursiveTreeToXML(self, item, etElem):
        """
        通过遍历的方法将QTreeWidget中的信息抽取到XML中
        @param item 待分析的Tree节点
        @type QTreeWidgetItem
        
        @param etElem 父节点XML
        @type ET.ETElement
        """
        if item is None:
            self.logger.error("输入的Item无效！")
            return None
        if etElem is None:
            etElem = ET.Element(item.text(0))
            
        #处理当前树节点的信息 
        tTreeWidget = self.treeWidget_xml
        if tTreeWidget.itemAbove(item) is None: #判断其为根节点
            #这是根节点 
            etElem.tag = item.text(0)
            for iD in range(0, item.childCount()):
                if not self.isSetCheckstate or item.child(iD).checkState(0) != Qt.Unchecked:
                    self.recursiveTreeToXML(item.child(iD), etElem)
            return etElem
            
        #进入非根节点处理过程
        #处理属性节点        
        if item.text(0) == "属性":
            for iD in range(0, item.childCount()):
                if self.isSetCheckstate and item.child(iD).checkState(0) == Qt.Unchecked:
                    continue
                else:
                    etElem.attrib[item.child(iD).text(0)] = item.child(iD).text(1)
        #遍历所有子节点
        else:
            if item.childCount() <= 0: #没有子节点的情况 value
                #y叶节点
                if not self.isSetCheckstate or item.checkState(0) != Qt.Unchecked:
                    aElem = ET.SubElement(etElem, item.text(0))
                    aElem.text = '' if item.text(1) is None else item.text(1)
        
            elif item.childCount() > 0:
                #处理非属性节点
                if not self.isSetCheckstate or item.checkState(0) != Qt.Unchecked:
                    subElem = ET.SubElement(etElem,item.text(0) )
                    if item.text(1)!= "":
                        subElem.text = item.text(1).strip()
                    for iD in range(0, item.childCount()):
                        self.recursiveTreeToXML(item.child(iD), subElem)
                        
        return  etElem
        
    
    @pyqtSlot(QTreeWidgetItem, int)
    def on_treeWidget_xml_itemChanged(self, item, column):
        """
        Slot documentation goes here.
        
        @param item DESCRIPTION
        @type QTreeWidgetItem
        @param column DESCRIPTION
        @type int
        """
    def loadXML(self, tFile):
        """
        将tFile对应的XML文件加载到树结构
        """
        if not os.path.exists(tFile):
            self.logger.error("文件：%s 不存在！"%tFile)
            return
        #加载模型
        root = None 
        try:
            root = ET.parse(self.sourceXML).getroot()
        except:
            self.logger.error("加载XMl：%s 失败！"%self.sourceXML)
            
        if root is None:return
        #清理控件
        self.treeWidget_xml.clear()        
        #递归添加所有元素
        self.listDom(root, None)        
        #展开控件
        self.treeWidget_xml.expandToDepth(self.ExpandingDepth)
        
    @pyqtSlot()
    def on_actionLoadXML_triggered(self):
        """
        Slot documentation goes here.
        """
        #打开文件选择对话框
        fN , fType = QFileDialog.getOpenFileName(self, "XML文件路径",'',
                            "Datcom Model Files (*.dcxml *.xml )" )
        if not os.path.exists (fN):
            self.logger.error("文件：%s 不存在！"%fN)
            return
        #配置信息
        self.sourceXML = fN
        self.loadXML( fN)
    
    @pyqtSlot()
    def on_actionSave_XML_triggered(self):
        """
        Slot documentation goes here.
        """
        from PyDatcomLab.Core.tools import xml_Indent as indent
        if self.objectXML == "" :
            self.objectXML = self.sourceXML

        for id in range(0, self.treeWidget_xml.topLevelItemCount()):
            topItem = self.treeWidget_xml.topLevelItem(id)
            resXMl = self.recursiveTreeToXML(topItem, None)                        
            indent(resXMl, 0)
            if self.treeWidget_xml.topLevelItemCount() > 1:
                tDir,fN = os.path.split(self.objectXML)
                tFName,tExt = os.path.splitext(fN)
                tPath = os.path.join(tDir, '%s-%d.%s'%(tFName, id,tExt ) )
            else:
                tPath = self.objectXML
            ET.ElementTree(resXMl).write(tPath, encoding="UTF-8" )
            QMessageBox.information(self,'修改XML文件', '已经将修改写入到模型文件：%s'%tPath)

            
            
    @pyqtSlot()
    def on_actionSave_as_XML_triggered(self):
        """
        Slot documentation goes here.
        """
        tObjpath, fType = QFileDialog.getSaveFileName(self, '选择新的文件名',self.sourceXML,
                        "Datcom Model Files (*.dcxml *.xml )" )
        if tObjpath :
            self.objectXML = tObjpath
            self.on_actionSave_XML_triggered()
        else:
            self.logger.info("没有选择到合适的文件")
            

    
    @pyqtSlot()
    def on_actionUnload_triggered(self):
        """
        Slot documentation goes here.
        """
        self.treeWidget_xml.clear()

    
    @pyqtSlot()
    def on_actionReload_triggered(self):
        """
        Slot documentation goes here.
        """
        if os.path.exists(self.sourceXML):
            self.loadXML(self.sourceXML)
            
    @pyqtSlot()
    def on_actionReadOnlyMode_triggered(self):
        """
        Slot documentation goes here.
        """

        self.setEditingMode('Readonly')
        self.on_actionReload_triggered()
    
    @pyqtSlot()
    def on_actionValueOnlyMode_triggered(self):
        """
        Slot documentation goes here.
        """
        self.setEditingMode('ValueOnly')
        self.on_actionReload_triggered()
        
    @pyqtSlot()
    def on_actionAllMode_triggered(self):
        """
        Slot documentation goes here.
        """
        self.setEditingMode('All')
        self.on_actionReload_triggered()
        
if __name__ == "__main__":
    import sys
    app = QApplication(sys.argv)
    tWidget = XMLEditer()
    tWidget.show()
    sys.exit(app.exec_())
