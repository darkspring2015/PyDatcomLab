# -*- coding: utf-8 -*-

"""
Module implementing XMLModel. 
"""
from PyQt5 import QtCore  
from PyQt5.QtGui import QStandardItem, QStandardItemModel
from xml.etree import ElementTree  as ET
#from xml.dom import minidom  as dom
#from lxml import etree as ET
import logging
import os
#from time import ctime


class XMLModel(QStandardItemModel):  
    """
    自定义实现的XML模型，用作模型视图
    """
    def __init__(self, iXML):
        """
        初始化函数
        """
        super(XMLModel, self).__init__()
        #日志系统        
        self.logger = logging.getLogger(r'Datcomlogger')     
        #内部常量
        self.doc = ET.ElementTree().getroot()
        #设置数据
        self.setXMLData(iXML)
    
    
    def setXMLData(self,   iXML):
        """
        将XML文件添加到表中模型
        """
        #self.beginResetModel()
        
        if type(iXML)== str :
            if os.path.isfile(iXML):
                self.doc = ET.parse(iXML).getroot()
                if self.doc is None:
                    self.logger.error("解析文件%s 失败!")    
            else:
                self.logger.error("setXMLData()输入参数错误，文件不存在！%s"%iXML)                
        elif type(iXML)  == ET.Element :
            self.doc = iXML
        elif type(iXML)  is ET.ElementTree :            
            self.doc = iXML.getroot()          
        #开始解析
        self.setModelByXML()    
        self.setXMLHeader()

        
    def setModelByXML(self):
        """
        向标准模型中添加对应的数据
        """
        #将XML Tree转化为表中模型
        if self.doc is None or type(self.doc) != ET.Element:
            return 
        self.clear()
        #获得QStandardTree的根元素
        rItem = self.invisibleRootItem()
        #遍历添加所有元素
        self.walk(self.doc, rItem)
        
    def setXMLHeader(self, tHeader = ['节点', '值', '属性']):
        """
        配置对应的文件头
        """
        self.setHorizontalHeaderLabels(tHeader)
        
        
    def walk(self, iNode, iTreeItem):
        """
        添加iNode到 
        @param iNode ET.Element节点
        @type ET.Element
        @param pItem QModelItem 父节点的索引
        @type QStandardItem
        
        parentItem = self.model.invisibleRootItem()
        """
        if iNode is None or iTreeItem is None:
            self.logger.warning("walk()无效的遍历迭代参数，直接返回！")
            return 
        #插入本级
        tRow = [QStandardItem(iNode.tag), QStandardItem(iNode.text)]
        #遍历迭代属性
        #items() Returns the element attributes as a sequence of (name, value) pairs. The attributes are returned in an arbitrary order.
        for iA, iV in iNode.items():
            tSubiTem = QStandardItem(iA)
            tSubiTem.setData({iA:iV}, QtCore.Qt.UserRole )
            tRow.append(tSubiTem)
        #追加到树
        iTreeItem.appendRow(tRow)        
        #遍历所有的子节点
        for iN in list(iNode):
            self.walk(iN, tRow[0])
        
    def getXMLFromModel(self):
        """
        将模型数据串行化到XML实现
        """
        #创建子节点
        tXML = ET.ElementTree().getroot()
        #调用遍历
        self.walkTree(self.invisibleRootItem(), tXML)
        #写入到doc
        self.doc = tXML
        return tXML

    def walkTree(self, iTreeItem, iNode):
        """
        遍历TreeModel模型创建xml
        @param iNode ET.Element节点
        @type ET.Element
        @param iTreeItem QModelItem 父节点的索引
        @type QStandardItem
        """
        if iTreeItem is None or iNode is None:
            self.logger.warning("walk()无效的遍历迭代参数，直接返回！")
            return 
        #列计数
        if iTreeItem.columnCount < 3:
            self.logger.error("结构异常")
        #获得数据结构
        for iR in range(0, iTreeItem.rowCount()):
            #遍历所有的内容
            tAttrib ={}
            #获得属性
            for iC in range(0, iTreeItem.columnCount()):
                if iC == 0:
                    tTag  = iTreeItem.child(iR, iC).text()
                elif iC == 1:
                    tText = iTreeItem.child(iR, iC).text()
                else:
                    #tAttriKey = iTreeItem.child(iR, iC).text()
                    tAttribElem =  iTreeItem.child(iR, iC).data(QtCore.Qt.UserRole )
                    tAttrib.update(tAttribElem)
            #属性分析完毕开始写入内容
            tElem = ET.SubElement(iNode, tTag, tAttrib)            
            if tText is not None and tText != "":
                tElem.text = tText
            #遍历子节点
            if iTreeItem.hasChildren():
                self.walkTree(iTreeItem.child(iR, 0), tElem)

        

    
