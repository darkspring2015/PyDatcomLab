# -*- coding: utf-8 -*-

# datcom 定义类的加载器
#
# @editer Linger

# 尚未完成相关的开发
#
# 

from xml.etree import ElementTree  as ET
from PyDatcomLab.Core.datcomXMLLoader import datcomXMLLoader

#from PyDatcomLab.Core import datcomDimension as dtDimension

import  os
import logging

class datcomDictionary(datcomXMLLoader):
    """
    datcomDictionary 定义所有的Datcom定义，描述Namelist和variable，以及附加关系的定义
    xml文档格式约定：
    1.文档由若干的VARIABLE节组成，在xml中text字段表示值，在内存中由VALUE的值表示
    使用说明：
    1.该类型派生自datcomXMLLoader类，需要自定义解析关系，请使用xml文档约定来实现函数ParseXmltoDoc
    """
    def __init__(self, path):
        """
        初始化模型类
        如果指定了path，将加载对应的模型文件
        """
        super(datcomDictionary, self).__init__()  #使用空初始化以避免加载
        #初始化日志系统
        self.logger = logging.getLogger(r'Datcomlogger')
        #定义各种属性

        self.Properties.update({
                                #'CName':'AerocraftName CASE', 
                                })  
        self.Tag      = 'DatcomVariableDefine'   #覆盖定义         

        #定义XML结构
        self.dictAddtional = {}              #存储datcom配置之外的信息          
        #执行path的分析
        if path is None :
            path = os.path.join(os.path.expanduser('~'), r'.PyDatcomLab\config\datcomDefine.xml')
        if os.path.isfile(path):
            #尝试加载文件并提示错误异常信息
            try:
                self.load(path)
            except Exception as e:
                self.logger.error("加载算例文件:%s 出现异常:%s"%(path, e.message))
        
    #下面是必须重载的函数    
    def ParseXmltoDoc(self):
        """
        用来覆盖父类的ParseXmltoDoc方法，提供不同的解释器功能
        """
        if self.xmlDoc is None:return False
        #分析XML文件
        if not self.xmlDoc.tag == self.Tag: 
            self.logger.error("加载的文件格式有问题，Root节点的名称不是%s"%self.Tag)
            #return False
        #根据节点设置对象信息 
        self.__analysisXML(self.xmlDoc)
        if self.doc =={}: return False
        return True
        
    def __analysisXML(self, tXML):
        """
        从XML ET.Element中解析出文档结构
        tXML 根节点
        Notice: 将情况自身的数据
        """
        if tXML is None: 
            self.logger.error("xmlDoc为空，终止解析")
            return
        #属性节点赋值
        self.Properties.update(tXML.attrib) 
        #清理自身数据
        self.doc = {}        
        self.additionalDoc = {}
        
        #读取实例 第一层循环获取Namelist层次的数据，第二层循环获取 Variable层次的数据
        for iNode in list(tXML):
            #执行非Namelist节的解析
            nmlistName = iNode.tag
            self.doc[nmlistName] = {'SelfAtrrib':iNode.attrib}  #添加Namelist的定义            
            for varNode  in list(iNode):
                if varNode.get('dcType') == 'Infomation':
                    #如果是变量节则跳过
                    self.loadAddtionalInforation(varNode, nmlistName)
                    #continue 
                elif varNode.get('dcType')=='Variable':
                    tDicElememt = {}
                    #将所有字节全部解析到字典中
                    for paraNode in list(varNode):     
                        if not paraNode.text  is None:
                            tDicElememt[paraNode.tag] = paraNode.text
                    #重新识别部分项
                    try:
                        if 'Range' in tDicElememt.keys():
                            if 'TYPE' in tDicElememt.keys() and tDicElememt['TYPE'] in ['REAL', 'Array']:
                                tDicElememt['Range'] = self.parserArray(tDicElememt['Range'])
                            else:
                                tDicElememt['Range'] = eval(tDicElememt['Range'])
                        if 'DisplayRange' in tDicElememt.keys():
                            tDicElememt['DisplayRange'] = eval(tDicElememt['DisplayRange'])
                        if 'Limit' in tDicElememt.keys():
                            tDicElememt['Limit'] = self.parserArray(tDicElememt['Limit'])
                        if 'Default' in tDicElememt.keys():
                            if ('TYPE' in tDicElememt.keys() and tDicElememt['TYPE']  == 'REAL') or\
                            ('TYPE' in tDicElememt.keys()  and tDicElememt['TYPE']  == 'Array' and 'SubType' not in tDicElememt.keys() )or\
                            ('TYPE' in tDicElememt.keys()  and tDicElememt['TYPE']  == 'Array' and\
                            'SubType' in tDicElememt.keys() and tDicElememt['SubType']  == 'REAL'):
                                tDicElememt['Default'] = float(tDicElememt['Default'])
                            if ('TYPE' in tDicElememt.keys() and tDicElememt['TYPE']  == 'INT ') or\
                            ('TYPE' in tDicElememt.keys()  and tDicElememt['TYPE']  == 'Array' and\
                            'SubType' in tDicElememt.keys() and tDicElememt['SubType']  == 'INT'):
                                tDicElememt['Default'] = int(float(tDicElememt['Default']))
                    except Exception as e:
                        self.logger.error("转换类型出错，%s ：%s"%(repr(e), str(e)))
                    #将定义添加到变量的表达之中
                    self.Dictionary[nmlistName][varNode.tag] = tDicElememt
                else:
                    tDoc = self.recursiveParseElement(iNode)  #base class的方法，解析到标准实现中
                    if iNode.tag in self.additionalDoc.keys():
                        #暂时不考虑实现                    
                        self.additionalDoc[iNode.tag].append(tDoc) 
                    else:
                        self.additionalDoc[iNode.tag] = [tDoc]
        #解析结束             
    def loadAddtionalInforation(self, elemt, tNmlst):
        """
        从elemt中解析附加信息，写入到字典中
        elemt是包含附加信息的ET.Element元素
        tNmlst是CARD的名称
        """
        #以CARD/Namelist为键，保存附加定义信息 NMACHLinkTable  RuleNumToCount RuleIndexToCombo groupDefine
        if elemt is None: return
        if elemt.tag != 'AdditionalInfo':return 
        if tNmlst not in self.dictAddtional.keys():
            self.dictAddtional[tNmlst]= {}
        for iT in list(elemt):
            if iT.tag == 'NMACHLinkTable' :
                #NMACHLinkTable
                if  not iT.text is None:
                    tNTable = eval(iT.text)
                    if tNTable is None or not type(tNTable)  is list or len(tNTable) == 0:
                        continue
                    else:
                        self.dictAddtional[tNmlst]['NMACHLinkTable'] = tNTable
                    
            elif iT.tag == 'RuleNumToCount':
                #RuleNumToCount
                tRCL = []
                for iR in list(iT):
                    tRCL.append(iR.attrib) 
                if len(tRCL)>0:
                    self.dictAddtional[tNmlst]['RuleNumToCount'] = tRCL
                    
            elif iT.tag == 'RuleIndexToCombo':
                #RuleIndexToCombo
                tAllCombo = []
                for iR in list(iT):
                    #每一条Combo有 index group，howto组成
                    tAllComboDict = iR.attrib
                    tAllComboDict['HowTo'] = {}
                    for iHowto in list(iR):
                        tAllComboDict['HowTo'][iHowto.attrib['key']] = eval(iHowto.attrib['value'])
                    tAllCombo.append(tAllComboDict)
                self.dictAddtional[tNmlst]['RuleIndexToCombo'] = tAllCombo
                        
            elif iT.tag == 'GroupDefine':
                #GroupDefine
                tGroupRules = {}
                for iR in list(iT):
                    tName = iR.attrib['Name']
                    tGroupRules[tName] = iR.attrib
                self.dictAddtional[tNmlst]['GroupDefine'] = tGroupRules
            elif iT.tag == 'RuleVariableStatus':
                tAllCombo = []
                for iR in list(iT):
                    #每一条有 index group，howto组成
                    tAllComboDict = iR.attrib
                    tAllComboDict['HowTo'] = {}
                    for iHowto in list(iR):
                        tSets ={}
                        tSets['Enabled']  = eval(iHowto.attrib['Enabled'])
                        tSets['Disabled'] = eval(iHowto.attrib['Disabled'])
                        tAllComboDict['HowTo'][iHowto.attrib['key']] = tSets
                    tAllCombo.append(tAllComboDict)
                    
                self.dictAddtional[tNmlst]['RuleVariableStatus'] = tAllCombo
            elif iT.tag == 'HelpDoc':
                self.dictAddtional[tNmlst]['HelpDoc'] = iT.attrib              
                
            else:                
                self.logger.error("解析规则异常：%s"%(tNmlst + " " + iT.tag))

        
    def buildXMLFromDoc(self):
        """
        用来覆盖父类的buildXMLFromDoc方法，提供不同的解释器功能
        将Doc
        """
        #super(dcModel, self).buildXMLFromDoc()
        tRoot  = self.createXMLDocBasic() 
        
        for iV in self.doc.keys(): 
            tVar = self.doc[iV].copy()
            tElem = ET.SubElement(tRoot, iV,tVar['SelfAtrrib'])
            tVar.pop('SelfAtrrib')
            #写入变量的定义
            for iE in tVar.keys():
                tPers = {}
                if 'SelfAtrrib' in tElem[iE].keys():
                    tPers = tElem[iE]['SelfAtrrib']
                iVinN =  ET.SubElement(tElem, iE, tPers)
                
                    
                
                


            tElem = ET.SubElement(tRoot, 'VARIABLE', tVar)
            if tValue is not None:
                tElem.text = str(tValue)
            
        return tRoot
