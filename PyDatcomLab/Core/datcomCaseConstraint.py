#####################################################
#                       定义约束类                           #
#                                                           #
######################################################


import  os
import logging
import uuid
from xml.etree import ElementTree  as ET

from PyDatcomLab.Core.DictionaryLoader import  defaultDatcomDefinition as DtDefine 
from PyDatcomLab.Core.datcomXMLLoader import datcomXMLLoader
from PyDatcomLab.Core import datcomTools as dtTools
from PyDatcomLab.Core import datcomDimension as dtDimension
from PyDatcomLab.Core import datcomModel as dcModel



class datcomCaseConstraint(datcomXMLLoader):
    """
    datcomConstraint 用来加载和记录与基本计算构型相关的datcom配置规则的信息
    配置规则由特定的xml文件描述

    """
    def __init__(self,  path = None, dtDefine = DtDefine):
        """
        初始化
        """
        super(datcomCaseConstraint, self).__init__()  #使用空初始化以避免加载
        #初始化日志系统
        self.logger = logging.getLogger(r'Datcomlogger')
        #定义各种属性
        self.dtDefine = dtDefine
        #self.Properties.update({    })  
        self.Tag      = 'ConfigurationCollection'   #覆盖定义         

        #定义XML结构
        self.additionalDoc = {}              #存储datcom配置之外的信息 
        
        #执行path的分析
        if path is None :
            self.buildBasicConstraint()
        elif os.path.isfile(path):
            #尝试加载文件并提示错误异常信息
            try:
                self.load(path)
            except Exception as e:
                self.logger.error("加载算例文件:%s 出现异常:%s"%(path, e.message))
                
                
    def buildBasicConstraint(self):
        """
        从配置文件构建基础的构型约束信息

        创建一个基本的doc，类型是ET
        基础空白的模型为了可用默认添加了所有的变量和结果
        """
        #添加最低配置的CASE
        #FLTCON    
        tConfigName = self.createEmptyConfigruation()
        tBasic =  ['FLTCON', 'SYNTHS', 'BODY', 'WGPLNF', 'WGSCHR']
        tConfigName.update( {'CNAME':'ExampleConfig', 'DisplayName':'样例配置', 
        #'UUID':uuid.uuid1(), 
        'HelpUrl':'https://github.com/darkspring2015/PyDatcomLab/tree/master/wiki', 
        'Namelists':str(tBasic), 
        'Variables':{}} )
        
        self.doc[tConfigName['CNAME']] = tConfigName

    def createEmptyConfigruation(self):
        """
        创建一个空的配置节点模板
        """
        tConfigName = {'CNAME':'', 'DisplayName':'', 
        'HelpUrl':'https://github.com/darkspring2015/PyDatcomLab/tree/master/wiki', 
        'Namelists':'', 
        'Variables':{}}
        return tConfigName
        
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
        
        #读取实例
        for iNode in list(tXML):
            #执行非Namelist节的解析
            if iNode.tag == 'Configuration':
                tSet = iNode.attrib  #获得所有属性
                if 'Namelists' in tSet.keys():
                    try:
                        tSet['Namelists'] = eval(tSet['Namelists'])
                    except Exception as e:
                        self.logger.error("解析Namelists异常！%s"%(str(e)))  
                tSet['Variables'] =[]
                #解析其他节点
                for iVNode in list(iNode):                    
                    if iVNode.tag != 'VARIABLE':
                        continue                    
                    tVAttrib = iVNode.attrib #获得变量的属性
                    tDf = self.dtDefine.getVariableDefineByUrl(tVAttrib['Url'])
                    if iVNode.text != None or iVNode.text.strip() != '':
                        if tDf['TYPE'] == 'INT':
                            tVAttrib['Value'] = int(float(iVNode.text))
                        elif tDf['TYPE'] == 'REAL':
                            tVAttrib['Value'] = float(iVNode.text)
                        elif tDf['TYPE'] == 'Array':
                            tVAttrib['Value'] = eval(iVNode.text)
                        elif tDf['TYPE'] == 'List':
                            tVAttrib['Value'] = iVNode.text
                        else:
                            self.logger.error("异常类型信息：%s"%tDf['TYPE'])
                    else:
                        self.logger.error("作为配置的特殊配置参数，不提供Value值是不可合适的")
                        #强制为该参数的默认值
                        tVAttrib['Value'] = tDf.getVariableTemplateByUrl(tVAttrib['Url'])
                    #添加到子节点中
                    tSet['Variables'].append(tVAttrib)
            else:
                tDoc = self.recursiveParseElement(iNode)
                if iNode.tag in self.additionalDoc.keys():
                    #暂时不考虑实现                    
                    self.additionalDoc[iNode.tag].append(tDoc) 
                else:
                    self.additionalDoc[iNode.tag] = [tDoc]
        #解析结束             
        
    def buildXMLFromDoc(self):
        """
        用来覆盖父类的ParseXmltoDoc方法，提供不同的解释器功能
        """
        #super(dcModel, self).buildXMLFromDoc()
        tRoot  = self.createXMLDocBasic() 
        for iV in self.doc.keys(): 
            tVar = self.doc[iV].copy()
            #分析Variables
            tVariables = [] if 'Variables' not  in tVar.keys() else tVar['Variables']
            tVar.pop('Variables')
            if 'Edited' in tVar.keys() : tVar.pop('Edited') #不保存对应的字段
            tElem = ET.SubElement(tRoot, 'Configuration', tVar)
            for iVars in tVariables:
                tValue = str(iVars['Value'])
                tElemAttrib = iVars.copy()
                tElemAttrib.pop('Value')                
                #开始转换
                tNode = ET.SubElement(tElem, 'VARIABLE', tElemAttrib)
                tNode.text = tValue            
        return tRoot

    
#结束定义必须重载的读写逻辑部分    
    
    def getVariableComboByConfiguration(self, config):
        """
        返回构型的设置
        不存在对应构型返回空
        """
        if config in self.doc.keys():
            return self.doc[config]
        else:
            return None
            
    def saveAsTemplate(self, cName, tDcModel, dName ='', hUrl =''):
        """
        将某个tDcModel对象设置为配置模板
        cName    用来命名配置
        tDcModel 包含所有的参数配置
        """
        if tDcModel is None or type(tDcModel) != dcModel.dcModel or cName is None or cName =='':
            self.logger.error("保存失败！Model无效")
            return 
        if cName in self.doc.keys():
            tConfig = self.doc[cName]
        else:
            tConfig = self.self.createEmptyConfigruation()
        tCollection = tDcModel.getNamelistCollection()
        tVariables =  tDcModel.getVariableCollection()
        if dName is None or dName == '':
            dName = cName
        if hUrl is None or hUrl == '':
            hUrl = 'https://github.com/darkspring2015/PyDatcomLab/tree/master/wiki'
        if tVariables is None :
            tVariables = {}
        
        tConfig.update( {'CNAME':cName, 'DisplayName':dName, 'HelpUrl':hUrl, 
        'Namelists':str(list(tCollection.keys())), 
        'Variables':tVariables} )
        
        
