# -*- coding: utf-8 -*-

"""
Module implementing XMLModel. 
"""
#from PyQt5.QtCore import 
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
    def __init__(self, tXMLFile):
        """
        初始化函数
        """
        super(XMLModel, self).__init__()
        #日志系统        
        self.logger = logging.getLogger(r'Datcomlogger')     
        #内部常量
        self.doc = ET.ElementTree().getroot()
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
        tRow = [QStandardItem(node.tag), QStandardItem(node.text)]
        for iA, iV in node.items():
            tRow.append(QStandardItem('%s :%s'%(iA, iV)))
        
        pItem.appendRow(tRow)        
        for iN in list(node):
            self.walk(iN, tRow[0])
        
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
        for iR in range(0, item.rowCount()):
            
            tAttrib ={}
            for iC in range(0, item.columnCount()):
                if iC == 0:
                    tTag = item.child(iR, iC).text()
                elif iC == 1:
                    tText = item.child(iR, iC).text()
                else:
                    tITtext = item.child(iR, iC).text()
                    if tITtext is not None  or tITtext != "":
                        tAN, tAV = tITtext.split(':')
                        tAttrib[tAN] = tAV
            #分析完毕开始写入内容
            tElem = ET.SubElement(pElem, tTag, tAttrib)
            if tText is not None and tText != "":
                tElem.text = tText
            if item.hasChildren():
                self.walkTree(item.child(iR, 0), tElem)

        

    
