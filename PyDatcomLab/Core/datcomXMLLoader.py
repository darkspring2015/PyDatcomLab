#!/usr/bin/env python

"""
datcomXMLLoader 加载XML的基础类
"""
import logging
from xml.etree import ElementTree  as ET
from PyDatcomLab.Core import datcomTools as dtTool

class datcomXMLLoader(object):
    """
    datcomXMLLoader 负责加载、写入XML文件
    """

    def __init__(self, path = None):
        """
        初始化
        """
        #初始化日志系统
        self.logger = logging.getLogger(r'Datcomlogger')
        #定义各种属性
        self.Tag        = 'Root'
        tPath =  '' if path is None else path
        self.Properties = {                           
                           'Describe':'1',
                           'CreateTime':self.getNowTime(), 
                           'ModifyTime':self.getNowTime(), 
                           'modelPath':tPath, 
                           }
        self.exts       = {'ALL':'*.*', 'XML':'.xml'}
        #定义XML结构
        self.xmlDoc = self.createXMLDocBasic()  #Tree Object
        self.doc    = []                     #存储解析后的数据报文信息，格式是Url：Value            
        #执行path的分析
        
        #尝试加载文件并提示错误异常信息
        if path is not None :
            try:
                self.load(path)
            except dtTool.dtIOException as dtE:
                self.logger.error('%s: %s'%(dtE.parameter, dtE.para_value))
            except Exception as e:
                self.logger.error("加载算例文件:%s 出现异常:%s"%(path, repr(e)))
                
    
    def load(self, tFile):
        """
        加载XML文件,只提示异常不处理
        """
        root = None
        try:
            root = ET.parse(tFile).getroot()
        except Exception as e:
            #self.logger.error("加载模型文件过程中发生异常，%s ：%s"%(repr(e), str(e)))
            raise(dtTool.dtIOException('加载文件异常',"加载模型文件过程中发生异常，%s ：%s"%(repr(e), str(e))))
            
        if root is None:return 
        #根据节点设置对象信息    
        self.Properties['modelPath'] = tFile
        #解析信息
        self.xmlDoc = root
        #分析XML文件
        self.ParseXmltoDoc()

            
    def save(self, path):
        """
        将结果保存到文件
        """
        if self.doc is not None :
            self.xmlDoc = self.buildXMLFromDoc()
        root = self.xmlDoc        
        dtTool.xml_Indent(root, 0)   
        try:
            root.attrib['modelPath'] = path
            root.attrib['ModifyTime'] = self.getNowTime()
            ET.ElementTree(root).write(path, encoding="UTF-8" )
        except Exception as e:
            tError = "写入XML文件：%s 失败！Message ：%s"%(path, repr(e))
            #self.logger.error(tError)
            raise(dtTool.dtIOException('文件写入失败',tError))

    def createXMLDocBasic(self):
        """
        创建一个基本的doc,无内容
        """
        tRoot = ET.Element(self.Tag, self.Properties)        
        return tRoot
        
    #@override
    def buildXMLFromDoc(self):
        """
        根据当前的doc创建xmlDoc
        返回一个xml 的ET根元素
        """
        tRoot  = self.createXMLDocBasic() 
        #递归解释doc
        for iD in self.doc:
            self.recursiveBuildXML(iD, tRoot)
        return tRoot
        
    def recursiveBuildXML(self, tDoc, pElem):
        """
        递归执行doc写入到pElem中
        """  
        tTag   = tDoc['Tag']         
        tP = {}
        #提取相关值
        for iP in tDoc.keys():
            if iP not in ['Tag', 'eValue', 'SubNode']:
                tP[iP] = tDoc[iP] 
        tElem = ET.SubElement(pElem, tTag, tP)
        if 'eValue' in tDoc.keys() and tDoc['eValue'] is not None:
            tValue = tDoc['eValue'].strip()  
            if tValue != '':
                tElem.text = tValue
        #SubNode
        if 'SubNode' in tDoc.keys():
            for iK in tDoc['SubNode']:
                self.recursiveBuildXML(iK, tElem)
        
        
    def getNowTime(self):
        """
        获得当前的时间
        """
        import time
        return time.strftime('%Y-%m-%d %H:%M:%S',time.localtime(time.time()))  
        
    def ParseXmltoDoc(self):
        """
        将ET.Element元素转化为self.doc,提供xml到字典类型的默认转换行为
        将会刷新tag，Properties
        """
        if self.xmlDoc is None:
            return 
        #开始解析
        self.Tag = self.xmlDoc.tag   #刷新根标签
        #刷新标签
        for iP in self.xmlDoc.attrib.keys():
            self.Properties[iP] = self.xmlDoc.attrib[iP]
        #将根节点以下的内容递归刷新到self.doc
        for iT in list(self.xmlDoc):
            self.doc.append(self.recursiveParseElement(iT))
    
    def recursiveParseElement(self, elem):
        """
        执行元素的递归解释
        """
        tDoc = elem.attrib
        tDoc['Tag'] = elem.tag
        tDoc['eValue'] = elem.text
        tDoc['SubNode'] = []
        for iT in  list(elem):
            tDoc['SubNode'].append(self.recursiveParseElement(iT))   
        return tDoc
        
    def setProperties(self, pDict):
        """
        修改属性字典定义的属性
        pDict是Python的dict类型修改基本信息
        Warning : 对于超过datcom算例文件的属性信息将作为属性写入，但不承若行为
        """
        if pDict is None or type(pDict) is not dict:
            return 
        self.Properties.update(pDict)
            
    def getProperties(self):
        """
        以dict形式返回属性
        返回值为Python的dict类型
        将返回所有的属性
        """
        return self.Properties
        
        


        
        

        
        
if __name__=="__main__":
    """
    """

    sPath  = r'E:\Projects\PyDatcomLab\extras\PyDatcomProjects\1\datcomDefine2.dcxml'
    obPath = r'E:\Projects\PyDatcomLab\extras\PyDatcomProjects\1\datcomDefine3.dcxml'
    try:
        aLoader = datcomXMLLoader(sPath)
        aLoader.save(obPath)
    except dtTool.dtIOException as e:
        print(repr(e))
