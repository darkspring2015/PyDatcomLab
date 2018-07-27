# -*- coding: utf-8 -*-

"""
Module implementing XMLEditer.
"""

from PyQt5.QtCore import pyqtSlot, pyqtSignal ,  Qt 
from PyQt5.QtWidgets import  QTreeWidgetItem, QFileDialog, QMessageBox,  QHeaderView, QMainWindow, QApplication
from PyDatcomLab.Core.datcomTools import xml_Indent as indent

from PyQt5.QtGui import QIcon

from xml.etree import ElementTree  as ET

import logging, os


from .Ui_XMLEditer import Ui_XMLEditer

editingModeDef = ['ValueOnly', 'Readonly', 'All']


class XMLEditer(QMainWindow, Ui_XMLEditer):
    """
    XMLEditer是编辑XML、Datocm基础配置的软件
    """
    Command_ReloadDtDefine_triggered = pyqtSignal(str)  #用来传递重载DatcomDefine的命令，str指向新配置文件的路径
    
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
        #初始化相关配置
        self.treeWidget_xml.header().setSectionResizeMode(QHeaderView.ResizeToContents )
        self.treeWidget_xml.header().setStretchLastSection( True)
        self.treeWidget_xml.setUniformRowHeights(True)  #可以极大加速Tree的加载时间 http://blog.csdn.net/rabinsong/article/details/8452946
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
        
    def Load(self, iPath):
        """
        从外部加载iPath的文件用于显示和编辑
        主要功能：
        1.将会iPath保存到路径
        """
        if os.path.isfile(iPath):
            try:
                self._loadXML(iPath)
                self.sourceXML = iPath
            except Exception as e:
                self.logger.error("加载XML文件过程出错：%s"%e)
                
    def _loadXML(self, tFile):
        """
        将tFile对应的XML文件加载到树结构
        """
        if not os.path.exists(tFile):
            self.logger.error("文件：%s 不存在！"%tFile)
            return
        #加载模型
        root = None 
        try:
            root = ET.parse(tFile).getroot()
        except:
            self.logger.error("加载XMl：%s 失败！"%self.sourceXML)
            
        if root is None:return
        #清理控件
        self.treeWidget_xml.clear()        
        #递归添加所有元素
        self.listDom(root, None)        
        #展开控件
        self.treeWidget_xml.expandToDepth(self.ExpandingDepth)
        
    def expandToDepth(self, iDepth):
        """
        从外部设置展开深度 iDepth为展开深度 [0,inf]
        """
        if iDepth >= 0:
            self.ExpandingDepth  = iDepth
            self.treeWidget_xml.expandToDepth(self.ExpandingDepth)
            

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
        docElem 是ET表示的XML ElementTree
        pItem 是QTreeWidget的实例
        """
        #添加本节点和属性
        if pItem is not None:
            #在self.treeWidget_xml的pItem节点下创建节点 
            tItem = QTreeWidgetItem(pItem)
        else:
            tItem = QTreeWidgetItem(self.treeWidget_xml)
            tItem.setIcon(0, QIcon(self.icoList['root']))
        #写入当前节点的名称
        tItem.setText(0, docElem.tag)  
        if self.isSetCheckstate:  tItem.setCheckState(0, Qt.Checked)
        tItem.setFlags(self.itemFlags )     
        #写入节点的属性信息
        if  len(docElem.attrib)  >0:
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
        if len(list(docElem)) == 0:
            tItem.setText(1, docElem.text) 
            tItem.setIcon(0, QIcon(self.icoList['node']))
        #判断是否有子节点
        else:
            tItem.setIcon(0, QIcon(self.icoList['middle']))
            for iChild in list(docElem):            
                self.listDom(iChild, tItem)      
    #END listDom
    
    def  saveTo(self, iPath):
        """
        保存当前文档到iPath
        """
        try:
            tDir,fN = os.path.split(iPath)
            tFName,tExt = os.path.splitext(fN)
            for id in range(0, self.treeWidget_xml.topLevelItemCount()):
                topItem = self.treeWidget_xml.topLevelItem(id)
                resXMl = self.recursiveTreeToXML(topItem, None)                        
                indent(resXMl, 0)
                if self.treeWidget_xml.topLevelItemCount() > 1:      
                    tPath = os.path.join(tDir, '%s-%d.%s'%(tFName, id,tExt ) )
                else:
                    tPath = iPath
                ET.ElementTree(resXMl).write(tPath, encoding="UTF-8" )
        except Exception as e:
            self.logger.warning("尝试写入到%s失败:%s！"%(iPath, e))
            return False
        return True
    
    
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
        #if  self.treeWidget_xml.itemAbove(item) is None: #判断其为根节点
        if  item.parent() is None: #判断其为根节点        
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
        self._loadXML( fN)
    
    @pyqtSlot()
    def on_actionSave_XML_triggered(self):
        """
        保存当前文档
        """
        if self.saveTo(self.sourceXML ):        
            QMessageBox.information(self,'修改XML文件', '已经将修改写入到模型文件：%s'%(self.sourceXML ))
        else:
            QMessageBox.warning(self,'修改XML文件失败', '请检查日志系统，写入到模型文件：%s'%(self.sourceXML ))
    
    @pyqtSlot()
    def on_actionSave_as_XML_triggered(self):
        """
        Slot 另存为 的槽函数.
        函数行为：
        1.询问保存路径，保存文件
        2.保存成功将切换当前文档路径
        """
        tObjpath, fType = QFileDialog.getSaveFileName(self, '选择新的文件名',self.sourceXML,
                        "Datcom Model Files (*.dcxml *.xml )" )
        if tObjpath :            
            try:
                self.saveTo(tObjpath)
                self.sourceXML  = tObjpath
                QMessageBox.information(self,'文件另存为', '已经将修改写入到模型文件：%s'%(self.sourceXML   ))
            except Exception as e:
                self.logger.warning("保存文件出错")   
                QMessageBox.warning(self,'文件另存为失败', '请检查日志系统，写入到模型文件：%s'%(self.sourceXML ))             
        else:
            self.logger.info("没有选择到合适的文件")
 
    
    @pyqtSlot()
    def on_actionUnload_triggered(self):
        """
        Slot documentation 卸载文档.
        """
        self.treeWidget_xml.clear()


    
    @pyqtSlot()
    def on_actionReload_triggered(self):
        """
        Slot documentation goes here.
        """
        if os.path.exists(self.sourceXML):
            self._loadXML(self.sourceXML)
            
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
        
    @pyqtSlot()
    def on_actionReLoadCaseUI_triggered(self):
        """
        Slot 重新加载PyDatcomLab的CASE系统的界面.
        """
        #触发外部命令
        self.Command_ReloadDtDefine_triggered.emit(self.sourceXML)
        
    
        
        
if __name__ == "__main__":
    import sys
    app = QApplication(sys.argv)
    tWidget = XMLEditer()
    tWidget.show()
    sys.exit(app.exec_())
    

