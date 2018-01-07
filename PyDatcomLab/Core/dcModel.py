

from xml.etree import ElementTree  as ET
from PyDatcomLab.Core import datcomDefine as dF  

import time
import logging

class dcModel(object):
    """
    dcModel 定义一个不包含计算条件的飞机模型的类型
    """
    def __init__(self, aerocraftName ='AircraftName', configuration='常规布局'):
        """
        初始化
        """
        self.doc ={}
        self.aerocraftName = aerocraftName
        self.configuration = configuration
        self.createTime = time.strftime('%Y-%m-%d %H:%M:%S',time.localtime(time.time()))
        self.modifyTime = time.strftime('%Y-%m-%d %H:%M:%S',time.localtime(time.time()))
        
        #初始化日志系统
        self.logger = logging.getLogger(r'Datcomlogger')
        
        #定义XML结构
        self.xmlTemplete ="""\
<AerocraftInfo AircraftName='AircraftName' Configuration='常规布局' createTime='' modifyTime = '' >
</AerocraftInfo>
        """
        self.xmlDoc = ET.ElementTree(ET.XML(self.xmlTemplete))  #Tree Object

        
    def getDocXMLString(self):
        """
        返回模型的xml描述
        """
        root = self.createXMLDoc()
        if root is None: return ""
        #保存最新信息到self.xmlDoc
        self.xmlDoc._setroot(root) 
        
        return ET.tostring(root)
        
    def setDoc(self, doc):
        """
        用外部的doc设置doc
        """
        if doc is None : 
            self.logger.error("doc参数为空")
            return
        self.doc = doc
        
    def SetDocByXML(self, tXML):
        """
        根据XML的内容设置dcModel的值
        @tXML 是Element的对象
        """
        if tXML is None: return
        #修改表头
        self.aerocraftName = tXML.get("AircraftName")
        self.configuration = tXML.get("Configuration")
        self.createTime = tXML.get("createTime")
        self.modifyTime = tXML.get("modifyTime")
        #读取实例
        keyLst = dF.reserved_NAMELISTS  #定义了所有Namelist信息的list
        
        for nmlstNode in list(tXML):
            if nmlstNode.tag not in keyLst:continue #不在Namelist列表中的将被跳过
            
            #分析子节点
            for aVar in list(nmlstNode): #遍历所有的子节点
                if not aVar.get('dcType') == 'Variable':continue #如果是变量节则跳过
                tInd = aVar.get('Index')
                if tInd: #说明是数组值                    
                    #循环读取数据
                    tValue =[]
                    for itr in aVar.findall('value'):
                        tValue.append(float(itr.text))
                    #填充数据
                    self.setNamelist( nmlstNode.tag , aVar.tag, tValue, tInd)
                else: #说明是单值
                    if aVar.text in ['.TRUE.', '.FLASE.']:
                        self.setNamelist( nmlstNode.tag , aVar.tag, aVar.text)
                    else:
                        self.setNamelist( nmlstNode.tag , aVar.tag, float(aVar.text))
        #解析结束


    def loadXML(self, tFile):
        """
        从XML文件tFile中加载数据
        """
        root = ET.parse(tFile).getroot()
        if root is None:return 
        #分析XML文件
        if not root.tag == 'AerocraftInfo': 
            self.logger.error("加载XML文件：%s失败，其中不包含AerocraftInfo节"%tFile)
            return
        #根据节点设置对象信息    
        self.SetDocByXML(root)
        
        
        
    def createXMLDoc(self):
        """
        将doc写出成XML格式
        """
        #创建临时的属
        root = ET.XML(self.xmlTemplete)
        #写入模型的属性信息
        root.set('AerocraftName',self.aerocraftName )
        root.set('Configuration',self.configuration )
        root.set('createTime',self.createTime )
        root.set('modifyTime',self.modifyTime )
        
        #循环写入NameList        
        if self.doc is None:   
            self.logger.error("模型内并没有doc")
            return None
        

        keyLst = dF.reserved_NAMELISTS  #定义了所有Namelist信息的list
        
        #若干字典信息存在，则根据字典顺序下达信息
        tDoc = self.doc
        for nmlst in tDoc.keys():
            if nmlst not in keyLst:
                continue
            #如果在，则创建响应的节点
            nmlstNode = ET.SubElement(root, nmlst, {'dcType':'NAMELIST'})
            #type("123") == str
            if type(tDoc[nmlst]) is dict: #判断是否为字典类型
                for var in tDoc[nmlst].keys(): #添加遍历namelist下面的遍历
                    varNode = ET.SubElement(nmlstNode, var, {'dcType':'Variable'})
                    #如果变量时单值的直接添加
                    tVar = tDoc[nmlst][var]
                    if type(tVar)is not dict:  #如果不是dict类型说明是.True. 或者单值函数
                        varNode.text = str(tVar)
                        continue
                    varNode.set('Index',str(tVar['Index']) ) #读取Index的值                    
                    for aVal in tVar['Value']: #如果是字典类型，说明是序列值                        
                        ET.SubElement(varNode, 'value').text = str(aVal)

            else:
                self.logger.info("异常的字典结构")
        
        #ET.dump(root)
        #返回对应的XML文档
        return  root
        
    def writeToXML(self, file):
        """
        将doc的内容写入到xml文件
        """
        root = self.createXMLDoc()
        indent(root, 0)        
        ET.ElementTree(root).write(file, encoding="UTF-8" )
    

        

    def setNamelist(self, nmlst , varName, varVaule, Index =1):
        """
        配置Namelist
        nmlist 是名称 Namelist名称
        varName 是变量名
        varVaule 是变量值
        Index 是Array类型的序号，默认1
        比如：
        >>> mod = dcModel('J5','常规布局')
        >>> mod.setNamelist('SYNTHS','XCG',10)
        """
        
        keyLst = dF.reserved_NAMELISTS  #定义了所有Namelist信息的list
        
        #判断是否存在
        if nmlst not in keyLst:
            return
        
        if nmlst not in self.doc.keys():
            self.doc[nmlst] = {}
        
        nmlstObj = self.doc[nmlst]
        
        #重复赋值会冲掉   
        if varVaule is None :
            #空值则从字典中删除对应的参数
            if varName in nmlstObj:
                nmlstObj.pop(varName) #删除该变量                
        elif type(varVaule) is list:
            tD = {'Index':Index, 'Value':varVaule}
            nmlstObj[varName] = tD
        else:
            nmlstObj[varName] = varVaule
        
    def getNamelist(self, nmlst):
        """
        获得对应的NAMELIST的参数值
        是一个集合
        """
        if nmlst not in self.doc.keys():
            return None
        
        return self.doc[nmlst]
        
    def getNamelistVar(self, nmlst, varName):
        """
        获得nmlist中varName对应变量的值
        >>> mod = dcModel('J5','常规布局')
        >>> mod.setNamelist('SYNTHS','XCG',10)
        >>> mod.getNamelistVar('SYNTHS','XCG')
        ...10
        """
        keyLst = dF.reserved_NAMELISTS  #定义了所有Namelist信息的list
        
        #判断是否存在
        if nmlst not in keyLst:
            return None
        
        if nmlst not in self.doc.keys():
            return None
        
        dic = self.doc[nmlst]
        if varName not in dic.keys():
            return None
        
        return dic[varName]
        
        
def indent(elem, level=0):
    i = "\n" + level*"  "
    if len(elem):
        if not elem.text or not elem.text.strip():
            elem.text = i + "  "
        if not elem.tail or not elem.tail.strip():
            elem.tail = i
        for elem in elem:
            indent(elem, level+1)
        if not elem.tail or not elem.tail.strip():
            elem.tail = i
    else:
        if level and (not elem.tail or not elem.tail.strip()):
            elem.tail = i
            

