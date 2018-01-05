

from xml.etree import ElementTree  as ET
from PyDatcomLab.Core import datcomDefine as dF  

class dcModel(object):
    """
    dcModel 定义一个不包含计算条件的飞机模型的类型
    """
    def __init__(self, aerocraftName, configuration='常规布局'):
        """
        初始化
        """
        self.doc ={}
        self.aerocraftName = aerocraftName
        self.configuration = configuration
        
        self.xmlTemplete ="""\
    <aerocraftInfo>
    </aerocraftInfo>
        """
        
    def getDocXML(self):
        """
        返回模型的xml描述
        """
        
        return self.toXML(self.doc)
    def setDoc(self, doc):
        self.doc = doc
        
    def toXML(self, root):
        """
        将doc写出成XML格式
        """
        root = ET.XML(self.xmlTemplete)

        #写入
        ET.SubElement(root,'aerocraftName' ).text = self.aerocraftName
        ET.SubElement(root,'configuration' ).text = self.configuration
        #循环写入NameList
        
        if self.doc is None:            
            return root.tostring()
        

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
        return  ET.tostring(root)
    
    def ToDoc(self, tXML):
        """
        将XML表述转换为dict形式
        认为tXML中只包含 model的定义部分
        """
        if tXML is None:
            return {}
            
        root = None;
        if type(tXML) is str:
            root =ET.XML(tXML)
        if type(tXML) is ET.ElementTree:
            root = tXML
        #分析XML
        #尚未实现玩
        

    def setNamelist(self, nmlst , varName, varVaule, Index =1):
        """
        配置Namelist
        nmlist 是名称 Namelist名称
        varName 是变量名
        varVaule 是变量值
        Index 是Array类型的序号，默认1
        """
        
        keyLst = dF.reserved_NAMELISTS  #定义了所有Namelist信息的list
        
        #判断是否存在
        if nmlst not in keyLst:
            return
        
        if nmlst not in self.doc.keys():
            self.doc[nmlst] = {}
        
        nmlstObj = self.doc[nmlst]
        
        #重复赋值会冲掉        
        if type(varVaule) is list:
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
        
        
