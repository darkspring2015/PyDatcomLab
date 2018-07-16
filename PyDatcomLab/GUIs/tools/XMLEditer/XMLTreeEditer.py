# -*- coding: utf-8 -*-

"""
Module implementing XMLTreeModel.

"""
from PyQt5.QtCore import QAbstractItemModel, Qt, QVariant, QModelIndex
#from xml.etree import ElementTree  as ET
from xml.dom import minidom  as dom
from lxml import etree as ET
import logging
import os
from time import ctime

DebugFlag = True

def Autolog(func):
    def timef(*s,**gs):
        if DebugFlag:
            retrunCode = func(*s,**gs)        
            #print("[%s] %s() called by %s . 结果: %s"%(ctime(),func.__name__, str(s[1:]), str(retrunCode)))
            print("[%s] %s() called, return:  %s "%( ctime(),func.__name__, str(retrunCode)))
            return retrunCode
        else:
            return func(*s,**gs)  
    return timef


class XMLTreeModel(QAbstractItemModel):  
    """
    自定义实现的XML模型，用作模型视图
    
    """
    def __init__(self, tXMLFile):
        """
        初始化函数
        """
        super(XMLTreeModel, self).__init__()
        #日志系统        
        self.logger = logging.getLogger(r'Datcomlogger')
        self.maxColumn = 3  
        #self.root = ET.Element('ROOT') #这是所有的父节点但是永远不被模型之外访问到 
        self.root = dom.Document().createElement('root')

        #设置数据
        self.setXMLDataMinidom(tXMLFile)
        #self.setXMLData(tXMLFile)
    #定义一些有用的函数
    def getXMLParent(self, tXML):
        """
        返回tXML对应的父节点
        """
        if tXML is None :
            return None 
        return tXML.getparent() #lXML支持
        
    def getXMLrow(self, tXML):
        """
        返回tXML元素在父节点中的行号
        L1 级别  根据ToplevelItems确定
        L2 级别  根据其他的确定
        错误返回 -1
        """
        if type(tXML) != ET._Element : #验证是否是LXML类
            return -1
        
        #return tXML.parentNode.childNodes.index(tXML) #LXML支持
        return self.getNodeChild(tXML.parentNode).index(tXML) #LXML支持
        
    def getXMLrowminidom(self, tXML):
        """
        返回tXML元素在父节点中的行号
        L1 级别  根据ToplevelItems确定
        L2 级别  根据其他的确定
        错误返回 -1
        """
            
        return tXML.parentNode.childNodes.index(tXML) #LXML支持
    
    #@Autolog    
    def nodeFromIndex(self, index):
        """
        从Index得到对应的元素
        """
        if index.isValid():
            return index.internalPointer()
        else:
            return self.root
    
    @Autolog
    def rowCount(self,parent ):  
        """
        基本的理解parent指定了父节点，从parent中可以获得父节点的tag和attrib，
        利用tag和attrib确定唯一的path确定其在xml树种的路径，从而定义出对应element
        在将element对应信息刷新到tree结构中
        是否需要动态添加呢？设计为需要
        """
        if parent.column() > 0:
            return 0 
        if not parent.isValid():
            pNode = self.root
        else:
            pNode = parent.internalPointer()
        #return  len(pNode)
        #return  len(pNode.childNodes)
        return   len(self.getNodeChild(pNode.parentNode))


    @Autolog    
    def columnCount(self, parent):
        """
        返回当前节点的列数
        对于包含属性的节点为2+n列，第三列起为属性字典
        [tag,text,attrib*n]
        """
        return self.maxColumn
        
    @Autolog    
    def index(self,  row, column, parent ):
        """
        建立当前数据的索引,输入查询的是上级元素，相当于表头 ，然后按行列查询对应的元素，并根据特点重点QModelIndex
        
        """
        if not self.hasIndex(row, column, parent):
            return QModelIndex()
        if not parent.isValid():
            pNode = self.root
            if row in  range(0, len(self.getNodeChild(self.root))) :
                #return self.createIndex(row, column, self.root[row] )
                #return self.createIndex(row, column, self.root.childNodes[row] )
                return self.createIndex(row, column, self.getNodeChild(self.root)[row] )
            return QModelIndex() 
        else:
            pNode = parent.internalPointer()
            if row in range(0, len(self.getNodeChild(pNode)) ):
                #return self.createIndex(row, column, pNode[row] )
                return self.createIndex(row, column, self.getNodeChild(pNode[row] ))
            return QModelIndex() 
                
    @Autolog        
    def parent(self, index):
        """
        建立反向的追述关系，这里必须使用lxml库
        #lxml.etree only
        """
        
        if not index.isValid():
            return QModelIndex()
        cNode = index.internalPointer()
        #pNode = self.getXMLParent(cNode)
        pNode = cNode.parentNode
        if pNode == self.root:
            return QModelIndex()
        #return  self.createIndex(self.getXMLrow(pNode), 0, pNode)
        return  self.createIndex(self.getXMLrowminidom(pNode), 0, pNode)
        
    def walk(self, elem, pelem):
        """
        遍历整个XML树创建index
        """
        if elem is None:
            return
        if pelem is None:
            self.createIndex(0, 0, elem)
        else:
            self.createIndex(pelem.index(elem), 0, elem)
        for ic in elem:
            self.walk(ic, elem)
 
            
    #@Autolog              
    def data(self,index,role):  
        """
        在XML树种查询对应的数据，返回对应的数据
        """
        
        if not index.isValid() :  
            return QVariant()  
            
        #获得对应的XML节点
        node = index.internalPointer()       
        if node is None:
            return QVariant()         
        
        #分析值和结果 
        if role == Qt.DisplayRole:              
            if index.column() == 0:
               #return node.tag
               return node.nodeName
            elif index.column() == 1:
                if node.nodeType == 'TEXT_NODE':
                    return ""                    
                #return node.text
                return str(node.nodeValue)
            elif index.column() >= 2:
                if node.attributes is None or len(node.attributes) == 0:
                   return QVariant()
                else:
                   return str(node.attributes[list(node.attributes.keys())[index.column()-2]])
        elif role == Qt.EditRole:
            if index.column() == 0:
                #return node.tag
               return node.nodeName
            elif index.column() == 1:
               #return node.text
               return str(node.nodeValue)
            elif index.column() == 2:
                if node.attributes is None or len(node.attributes) == 0:
                   return QVariant()
                else:
                   return str(node.attributes[list(node.attributes.keys())[index.column()-2]])
        else:
            return QVariant()
            
 
    #@Autolog  
    def headerData(self,section,orientation,role=Qt.DisplayRole):  
        """
        """
        if DebugFlag : self.logger.info("[%s] %s() called ."%( ctime(),'rowCount'))
        
        tHeader = ['节点', '值', '属性']
        if role != Qt.DisplayRole:  
            return QVariant()  
        if role == Qt.DisplayRole and orientation == Qt.Horizontal:             
            return tHeader[section]  
        elif role == Qt.DisplayRole and orientation == Qt.Vertical:
            return '%d'%section
        else:  
            pass
            
    #@Autolog      
    def flags(self,index):  
        """
        
        """
        if not index.isValid() :
            return 0
            
        return Qt.ItemIsSelectable | Qt.ItemIsEnabled | Qt.ItemIsEditable
        
    #@Autolog  
    def setData(self,index,value,role=Qt.EditRole):  
        """
        设置数据
        """
        if index.isValid() or role == Qt.EditRole:  
            node = index.internalPointer()
            if node is None: 
                self.logger.error("无法写入对应的节点")
                return 
            try:  
                if index.column() == 0 :
                    node.tag = str(value)
                elif index.column() == 1 :
                    node.text = str(value)
                elif index.column() == 2 :
                    tAttrib = dict(str(value)) 
                    for iC in tAttrib.keys():
                        node.Attrib[iC] = tAttrib[iC]
                else:
                    pass
            except Exception as e:  
                self.logger.error("%s 失败！%s,异常信息%s"%('SetData', repr(e), str(e)))  
                
            self.dataChanged.emit(index,index)  
            return True  
        return False  
        
    #@Autolog  
    def insertRows(self,position,rows,index): 
        """
        实现插入功能，index指向父节点
        """
        node = index.internalPointer
        if node is None:
            self.logger.error("无法找到Index对应的节点 .%s"%str(index))
            return 
        #开始执行插入操作 
        self.beginInsertRows(QModelIndex(),position,position + rows - 1)  
        for i in range(0, rows):  
            ET.SubElement(node, 'tag')
        self.endInsertRows()  
        return True  
        
    #@Autolog  
    def removeRows(self,position,count,index):  
        """
        实现删除节点操作，index指向父节点
        """
        node = index.internalPointer
        if node is None:
            self.logger.error("无法找到Index对应的节点 .%s"%str(index))
            return 
        #开始执行删除操作 
        self.beginRemoveRows(QModelIndex(),position,position + count - 1)  
        for row in range(0, count):  
            tSubElement = list(node)[position]
            node.remove(tSubElement) 
        self.endRemoveRows()  
        return True    
        
    #@Autolog    
    def setXMLData(self, tXML):
        """
        配置内部数据结构
        """
        #self.reset()
        self.beginResetModel()
        
        if type(tXML)== str :
            if os.path.isfile(tXML):
                self.doc = ET.parse(tXML).getroot()
                if self.doc is None:
                    self.logger.error("解析文件%s 失败!")  
                    #self.doc = ET.Element("测试")
            else:
                self.logger.error("setXMLData()输入参数错误，文件不存在！%s"%tXML)
                #self.doc = ET.Element("测试")
                    
        elif type(tXML)  == ET._Element :
            self.doc = tXML
        elif type(tXML)  is ET._ElementTree :            
            self.doc = tXML.getroot()
            
        self.root.append(self.doc) 
        self.endResetModel()    
        #self.walk(self.doc, None)
        
    #@Autolog    
    def setXMLDataMinidom(self, tXML):
        """
        配置内部数据结构
        """
        #self.reset()
        self.beginResetModel()
        self.doc = None
        if type(tXML)== str :
            if os.path.isfile(tXML):
                self.doc = dom.parse(tXML).documentElement
                if self.doc is None:
                    self.logger.error("解析文件%s 失败!")  
            else:
                self.logger.error("setXMLDataMinidom()输入参数错误，文件不存在！%s"%tXML)
                    
        elif type(tXML)  == ET._Element :
            self.doc = tXML
        elif type(tXML)  is ET._ElementTree :            
            self.doc = tXML.getroot()
            
        if self.doc is not None :self.root.appendChild(self.doc) 
        self.endResetModel()    
        #self.walk(self.doc, None)
        
    def getElementText(self, node):
        """
        
        """
    def getNodeChild(self, node):
        """
        获得节点的子节点
        提出其中的Text节点，只保留有效的节点
        """
        tcNode =[]
        if node is None : return tcNode
        for iC in node.childNodes:
            if iC.nodeType == 'ATTRIBUTE_NODE':
                continue
            if iC.nodeType == 'TEXT_NODE':
                continue
            tcNode.append(iC)
        return tcNode
            
 
        

 

    
