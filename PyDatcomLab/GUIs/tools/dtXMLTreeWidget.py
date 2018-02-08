# -*- coding: utf-8 -*-

"""
Module implementing dtXMLTreeWidget. 
子类化QTreeWidget提供对应的封装
"""

from PyQt5.QtCore import  Qt, QRect, QSize
from PyQt5.QtWidgets import QTreeWidget, QTreeWidgetItem, QHeaderView, QApplication, QStyledItemDelegate
from PyQt5.QtWidgets import QTableWidget, QTableWidgetItem
from PyQt5.QtGui import QIcon
from xml.etree import ElementTree  as ET


import logging
import os
#from time import ctime


#定义XML的修饰函数
def xml_Indent(elem, level=0):
    """
    修饰xml，使其符合人类阅读习惯
    """
    i = "\n" + level*"  "
    if len(elem):
        if not elem.text or not elem.text.strip():
            elem.text = i + "  "
        if not elem.tail or not elem.tail.strip():
            elem.tail = i
        for elem in elem:
            xml_Indent(elem, level+1)
        if not elem.tail or not elem.tail.strip():
            elem.tail = i
    else:
        if level and (not elem.tail or not elem.tail.strip()):
            elem.tail = i

class dtXMLTreeWidget(QTreeWidget):  
    """
    自定义实现的XML模型，用作模型视图
    """
    def __init__(self, parent,  tXMLFile):
        """
        初始化函数
        """
        super(dtXMLTreeWidget, self).__init__(parent)
        #日志系统        
        self.logger = logging.getLogger(r'Datcomlogger')  
        #内部常量
        self.doc = ET.ElementTree().getroot()
        self.itemFlags = Qt.ItemIsEnabled | Qt.ItemIsSelectable | Qt.ItemIsEditable\
                        |Qt.ItemIsAutoTristate|Qt.ItemIsUserCheckable
        self.defaultCheckstate = Qt.Checked
        self.icoDict ={
        'Default':':/img/images/content.png',  #默认图标
        'Leaf':':/img/images/content.png',     #默认图标
        'Top':':/img/images/content.png',      #默认图标
        'Node':':/img/images/content.png',     #默认图标
        }
        #设置数据
        self.setXMLData(tXMLFile)
    
    
    def setXMLData(self,   tXML):
        """
        将XML文件添加到表中模型
        """
        #self.beginResetModel()
        
        if type(tXML)== str :
            if os.path.isfile(tXML):
                self.doc = ET.parse(tXML).getroot()
                if self.doc is None:
                    self.logger.error("解析文件%s 失败!")    
            else:
                self.logger.error("输入参数错误，文件不存在！%s"%tXML)                
        elif type(tXML)  == ET.Element :
            self.doc = tXML
        elif type(tXML)  is ET.ElementTree :            
            self.doc = tXML.getroot()          
        #装载数据
        self.setModelByXML()    
        #设置表头风格
        self.setColumnCount(3)
        self.setHeaderLabels(['参数', '值', '属性'])
        self.header().setSectionResizeMode(QHeaderView.ResizeToContents )
        self.header().setStretchLastSection( True)
        self.setUniformRowHeights(True) #极大加速模型的加载过程
        #
        #展开数据
        self.expandAll()
        
    def setModelByXML(self):
        """
        向标准模型中添加对应的数据
        """
        #将XML Tree转化为表中模型
        if self.doc is None or type(self.doc) != ET.Element:
            return 
        self.clear()
        rItem = self.invisibleRootItem()
        self.walk(self.doc, rItem)
        
    def setXMLHeader(self, tHeader = ['节点', '值', '属性']):
        """
        配置对应的文件头
        """
        self.setHorizontalHeaderLabels(tHeader)
        
        
    def walk(self, node, pItem):
        """
        @param node ET.Element节点
        @type ET.Element
        @param pItem QModelItem 父节点的索引
        @type ET.Element
        
        parentItem = self.model.invisibleRootItem()
        """
        if node is None or pItem is None:
            return 
        #插入本级
        tTItem = QTreeWidgetItem()
        tTItem.setText(0, node.tag)
        tTItem.setText(1, node.text)
        if len(node.attrib)>0:
            tTItem.setText(2, str(node.attrib)) 
        tTItem.setCheckState(0, self.defaultCheckstate)
        if node == self.doc:
            tTItem.setIcon(0, QIcon(self.icoDict['Top']))
        elif len(node) <= 0:
            tTItem.setIcon(0, QIcon(self.icoDict['Leaf']))
        else: 
            tTItem.setIcon(0, QIcon(self.icoDict['Node']))            
        tTItem.setFlags(self.itemFlags )  
        
        pItem.addChild(tTItem)        
        for iN in list(node):
            self.walk(iN, tTItem)
        
    def getXMLFromModel(self):
        """
        将模型数据串行化到XML实现
        """
        tXML = ET.ElementTree().getroot()
        self.walkTree(self.invisibleRootItem(), tXML)
        self.doc = tXML
        return tXML

    def walkTree(self, item, pElem):
        """
        遍历模型创建xml
        """
        if item is None or pElem is None:
            return 
        if item.columnCount < 3:
            self.logger.error("结构异常")
        #获得数据结构
        for iR in range(0, item.childCount()):
            tTag  = item.child(iR).text(0)
            tText = item.child(iR).text(1)
            tAtt  = {}             
            try:
                tAtt = dict(item.child(iR).text(2))
            except Exception as e:
                self.logger.error("转换属性到字典类型失败！%s"%(item.child(iR).text(2)))

            #分析完毕开始写入内容
            tElem = ET.SubElement(pElem, tTag, tAtt)
            if tText is not None and tText != "":
                tElem.text = tText
            for iCH in range(0, item.child(iR).childCount()):
                self.walkTree(item.child(iR).child(iCH), tElem)
                
    def writeToXML(self, tFile):
        """
        将结果数据写回到磁盘
        """
        tXML = self.getXMLFromModel()
        xml_Indent(tXML, 0)
        try:
            ET.ElementTree(tXML).write(tFile, encoding="UTF-8" )
        except Exception as e:
            self.logger.error("写入到%s失败，message : %s"%(tFile, e.message))
        finally:
            return tXML
        
        
def printTrace(tStr = ''): 
    """
    """
    from time import ctime  
    print("[%s] called :%s"%( ctime(), tStr))

class dtAttributeDelegate(QStyledItemDelegate):
    """
    定义一个用来编辑的代理控件
    """
    def __init__(self, parent):
        super(dtAttributeDelegate, self).__init__(parent)
        #日志系统        
        self.logger = logging.getLogger(r'Datcomlogger')  
        self.maxHeight = 300
        self.editor = None
        
    def createEditor(self, parent, option, index):
        """        
        """
        printTrace('createEditor')
        
        tTb = QTableWidget(parent)
        tTb.setColumnCount(2)
        #tTb.setBaseSize(60, 200)
        tTb.setHorizontalHeaderLabels(['属性', '属性值'])
        tTb.horizontalHeader().setStretchLastSection(True)
        tTb.setColumnWidth(0, 100)
        tTb.setHorizontalScrollBarPolicy(Qt.ScrollBarAsNeeded)
        tTb.setVerticalScrollBarPolicy(Qt.ScrollBarAsNeeded)   
        self.editor = tTb
        return tTb
        
    def setEditorData(self,editor, index ):
        """
        """
        
        
        tAttrib = self.getAttirb(index)
        #
        editor.setRowCount(len(tAttrib))
        #editor.setColumnCount(2)  
        iT = 0
        tH =  editor.horizontalHeader().height() + editor.lineWidth()
        for iA in tAttrib.keys():
            editor.setItem(iT, 0, QTableWidgetItem(iA))
            editor.setItem(iT, 1, QTableWidgetItem(tAttrib[iA]))
            tH = tH + editor.rowHeight(iT) + editor.lineWidth()
            iT = iT +1
        #更新UI的大小让他正好满足要求
        if tH > self.maxHeight:tH = self.maxHeight
        editor.resize(editor.size().width(), tH)
        
        printTrace('setEditorData')
        
    def getAttirb(self, index):
        """
        """
        printTrace('getAttirb')
        
        item_str = index.data(Qt.DisplayRole)
        tAttrib = {}
        try:
            tAttrib = eval(item_str)
        except Exception as e:
            self.logger.error("%s 无法被转换为dict"%item_str)   
        #
        return tAttrib
            
    def updateEditorGeometry(self, editor, option, index):
        """
        #void  updateEditorGeometry(QWidget *editor,  
         const  QStyleOptionViewItem &option,  const  QModelIndex &index) const
        """       
     
        #editor.setGeometry(QRect(option.rect.topLeft(), QSize(option.rect.width(), editor.sizeHint().height())))
        printTrace('updateEditorGeometry')
        editor.setGeometry(option.rect)

        
    def setModelData(self, editor,  model, index):
        """
        void  setModelData(QWidget *editor, QAbstractItemModel *model,  
         const  QModelIndex &index)  const 
        """
        tDict = {}
        for iR in range(0,editor.rowCount() ):
            tDict[editor.item(iR, 0).text()] = editor.item(iR, 1).text()
        #回写数据
        if len(tDict) == 0:
            model.setData(index, '', Qt.EditRole)
        else:
            model.setData(index, str(tDict), Qt.EditRole)
        
        printTrace('setModelData')

    def sizeHint2(self , option, index ):
        """
QSize MyDelegate::sizeHint ( const QStyleOptionViewItem & option,  
                 const QModelIndex & index ) const
        """
        tSize = super(dtAttributeDelegate, self).sizeHint( option, index )
        #获得属性参数
        tAttrib = self.getAttirb(index)         
        editor = QTableWidget()
        editor.setRowCount(1)
        editor.setColumnCount(2)


        tH =  editor.horizontalHeader().height() + editor.lineWidth() 
        +  editor.rowHeight(0) * len(tAttrib)              

        if tH > self.maxHeight:tH = self.maxHeight
        printTrace('sizeHint : %s - %s'%(tSize.width(), tH))
        return QSize(tSize.width(), tH)
        
 
                
if __name__ == "__main__":
    import sys
    app = QApplication(sys.argv)
    tFile = r'E:\Projects\PyDatcomLab\extras\PyDatcomProjects\tests\fghgh\fghgh.dcprj'
    tWidget = dtXMLTreeWidget(None, tFile)
    tWidget.resize(500, 400)
    tDelegate = dtAttributeDelegate(tWidget)
    tWidget.setItemDelegateForColumn(2,tDelegate )

    tWidget.show()
    sys.exit(app.exec_())

