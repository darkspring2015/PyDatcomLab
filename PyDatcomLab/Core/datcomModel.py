# -*- coding: utf-8 -*-

# datcom 计算实例的doc类
#
# @editer Linger
#
# 

from xml.etree import ElementTree  as ET
from PyDatcomLab.Core.DictionaryLoader import  defaultDatcomDefinition as DtDefine  #, datcomConstraint
from PyDatcomLab.Core.datcomXMLLoader import datcomXMLLoader
#from PyDatcomLab.Core import datcomTools as dtTools
from PyDatcomLab.Core import datcomDimension as dtDimension

import  os
import logging
import uuid

class dcModel(datcomXMLLoader):
    """
    dcModel 定义用于一次计算的完整配置
    xml文档格式约定：
    1.文档由若干的VARIABLE节组成，在xml中text字段表示值，在内存中由VALUE的值表示
    2.内存中使用dict作为存储结构
    3.如果text经过eval评估是合法的数据则解释成对应float、list等信息，其他的默认为文本
    4.变量的集合是Datcom定义defaultDatcomDefinition的子集，没有的值或者组合直接从内存中删除
    5.包含DatcomInput文件的输出，单位信息包含在每一个变量的内存结构中，单位信息应当被持续化到xml文件，在解析输出时统一后由DIM命令指定
    6.dcModel模型的实例将作为控件的引用参数被直接修改  
    使用说明：
    1.该类型派生自datcomXMLLoader类，需要自定义解析关系，请使用xml文档约定来实现函数ParseXmltoDoc
    """
    def __init__(self, iPath = None, iDefine = DtDefine):
        """
        初始化模型类
        如果指定了path，将加载对应的模型文件，否则创建基础的Datcom模型，基础模型在defaultDatcomDefinition中定义
        如果指定了dtDefine的值，将使用自定义的Datcom定义        
        """
        super(dcModel, self).__init__()  #使用空初始化以避免加载
        #初始化日志系统
        self.logger = logging.getLogger(r'Datcomlogger')
        #定义各种属性
        self.dtDefine = iDefine
        self.Properties.update({
                                'CName':'AerocraftName CASE', 
                                'CASEID':'1',  
                                'Describe':'2', 
                                'AerocraftName':'AerocraftName', 
                                'Configuration':'wing-body' , 
                                'numForE':'100'
                                })  
        self.Tag      = 'CASE'   #覆盖定义         
        #self.numForE  = 100      #使用科学计数法的值
        #定义XML结构
        self.additionalDoc = {}              #存储datcom配置之外的信息          
        self.isValid =True
        #执行path的分析
        if iPath is None :
            self.__createBasicdoc()
        elif os.path.isfile(iPath):
            #尝试加载文件并提示错误异常信息
            try:
                self.load(iPath)
            except Exception as e:
                self.logger.error("加载算例文件:%s 出现异常:%s"%(iPath, e.message))
                self.isValid = False

        
    def __createBasicdoc(self):
        """
        创建一个基本的doc，类型是ET
        基础空白的模型为了可用默认添加了所有的变量和结果
        """
        #添加最低配置的CASE
        #FLTCON        
        self.Properties.update({'UUID':str(uuid.uuid1())})
        for iN in self.dtDefine.getBasicNamelistCollection():
            self.addNamelist(iN)
        #调用验证器修正错误，并给所有量赋初值
        self.validate()
    
        
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
            if iNode.tag == 'VARIABLE':
                tSet = iNode.attrib  #获得所有属性
                tUrl = tSet.get('Url', None)
                if tUrl is None or tUrl == '':
                    self.logger.error("信息结构异常！%s"%(str(tSet))) 
                tValue = None
                try:        
                    if iNode.text is not None :  #此处遇到 .TRUE.
                        if iNode.text in ['.TRUE.', '.FLASE.']:
                            tValue = iNode.text
                        else:
                            tValue = eval(iNode.text)
                except Exception as e:
                    self.logger.error("解析值结构异常！%s - %s"%(e, iNode.text ))  
                finally:
                    tSet['Value'] = tValue
                #写入结果
                self.doc[tUrl] = tSet
            else:
                tDoc = self.recursiveParseElement(iNode)  #base class的方法，解析到标准实现中
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
            #跳过不使用的模型的值
            if 'InUsed' in tVar and  tVar['InUsed'] == 'False':
                continue
            #分析变量的值
            tValue = None if 'Value' not  in tVar.keys() else tVar['Value']
            tVar.pop('Value')
            if 'Edited' in tVar.keys() : 
                tVar.pop('Edited') #不保存对应的字段
            #将属性值全部都刷新到
            tElem = ET.SubElement(tRoot, 'VARIABLE', tVar)
            if tValue is not None:
                tElem.text = str(tValue)
            
        return tRoot
    
    def getCASEUUID(self):
        """
        获得当前CASE的UUID
        如果项目不包括UUID则创建一个,并触发保存
        """
        if 'UUID' in self.Properties.keys():
            return self.Properties['UUID']
        else:
            self.Properties.update({'UUID':str(uuid.uuid1())})
            #self.save() #此处存在一定的异常风险，可能导致错误的保存了
        
    
    def getNamelistCollection(self):
        """
        返回实例包含的所有的Namelist和对应的Variaable
        返回值为dict<br />etc：{‘FLTCON’:['NMACH']}
        Notice : 返回的结果不包括值
        """
        #从 self.xmlDoc中进行分析结果
        tResult = {}        
        for iV in self.doc.keys():
            tNamelist = iV.split('/')[-2]
            tVarName  = iV.split('/')[-1]
            if tNamelist in tResult.keys():
                tResult[tNamelist].append(tVarName)
            else:
                tResult[tNamelist] = [tVarName]           
        
        return tResult
        
    def _getNamelistCollectionInUsed(self):
        """
        返回doc中实际在用的所有变量和Namelist的组合
        """
         #从 self.xmlDoc中进行分析结果
        tResult = {}        
        for iV in self.doc.keys():
            tNamelist = iV.split('/')[-2]
            tVarName  = iV.split('/')[-1]
            #如果变量在文档中并且在使用
            if ('InUsed' in self.doc[iV] and self.doc[iV]['InUsed'] =='True') or \
                'InUsed' not in self.doc[iV] :           
                if tNamelist in tResult.keys():
                    tResult[tNamelist].append(tVarName)
                else:
                    tResult[tNamelist] = [tVarName]           
        
        return tResult       
        
    def addNamelist(self, namelist, variables = []):
        """
        向实例添加一个选项卡和对应的变量
        namelist str ：控制卡的名称<br />variables [str] 变量组合 ，可选
        当添加的变量省略时，或者变量未达到基本要求时，将根据datcom定义文件进行推定补充
        """
        #从 self.dtDefine中获得约束条件
        #增加Namelist对应的基础配置和默认值信息
        

        if variables is None or len(variables)  == 0:
            tNLDf = self.dtDefine.getNamelistDefineByName(namelist)
            variables = list(tNLDf.keys())
        
        for iV in variables:
            tUrl = '%s/%s'%(namelist, iV)
            tVtp = self.dtDefine.getVariableTemplateByUrl(tUrl)
            if tVtp is None:
                self.logger.error("无法创建对应的变量的基本实例%s"%(tUrl))
                continue
            if tUrl not in self.doc:
                self.doc[tUrl] = tVtp
       
        
    def deleteNamelist(self, namelist):
        """
        从实例移除一个选项卡
        namelist str：选项卡的名称
        Notice 不承诺数据的完整性
        """
 
        if namelist not in self.dtDefine.getNamelistCollection():
            self.logger.error("NAMELIST : %s并没有在配置中定义！"%namelist)
            return 
        #遍历执行，删除Namelist对应的所有变量
        for iV in list(self.doc):       #这里只能使用list而不能使用key，因为会无法删除
            if self.doc[iV]['Namelist'] == namelist:
                self.doc.pop(iV) 
        
        
        
    def setVariable(self, iVar):
        """
        设置实例的某一个变量的值
        iVar是Python的dict类型，包括：{Url，Unit，Value}
        url包括namelist和variableName两部分组成：namelist/variableName
        函数行为：
        1.对于不存在的namelist，将导致创建对应选项卡的操作；<br />
        2.对于存在的变量，将修改值和单位等信息<br />
        3.如果变量的值为None，将从集合中删除该变量
        4.函数不校验Namelist的完整性，Namelist最低变量组合在其他函数中负责
        5.对于存在单位的变量不进行单位变换，仅保存单位制信息
        """
        #进行必要的单位变换
        #写入数据到doc中
        
        #数据校验
        tResult, tReport = self.checkMustKeys(iVar, ['Url', 'Unit', 'Value'])
        if tResult is False: 
            self.logger.error("输入的变量不合法，%s！"%tReport)
            return False
        #检查值
        tUrl = iVar['Url']
        if self.dtDefine.getVariableDefineByUrl(tUrl) == {}:
            self.logger.error("Datcom定义中并不包含%s的定义！"%tUrl)
            return False
        #执行变量写入过程
        if tUrl in self.doc.keys() and iVar['Value'] is None :
            #如果Value为None则删除该变量
            self.doc.pop(tUrl)
            return True
        elif tUrl in self.doc.keys() and iVar['Value'] is not  None:
            #如果Vlaue不为None则更新信息
            self.doc[tUrl].update(iVar)
            return True
        elif tUrl not in self.doc.keys() and iVar['Value'] is None :
            return True
        elif  tUrl not in self.doc.keys() and iVar['Value'] is not None :
            tVarTem = self.dtDefine.getVariableTemplateByUrl(tUrl)
            tVarTem.update(iVar)
            self.doc[tUrl] = tVarTem
            return True 
            
    def setVariablebyArrayIndex(self, iVar, iIndex):
        """
        设置实例的Array变量的某个值值,
        iVar是Python的dict类型，包括：{Url，Unit，Value}
        iIndex是Int类型，是对应元素的位置
        url包括namelist和variableName两部分组成：namelist/variableName
        函数行为：
        1.对于不存在的namelist，将导致创建对应选项卡的操作；<br />
        2.对于存在的变量，将修改值和单位等信息<br />
        3.如果变量的值为None，将默认值写入到集合
        4.函数不校验Namelist的完整性，Namelist最低变量组合在其他函数中负责
        5.对于存在单位的变量进行单位变换，变换到当前model的Unit
        """
        #进行必要的单位变换
        #写入数据到doc中        
        #数据校验
        tResult, tReport = self.checkMustKeys(iVar, ['Url', 'Unit', 'Value'])
        if tResult is False: 
            self.logger.error("输入的变量不合法，%s！"%tReport)
            return False
        #检查值
        tUrl = iVar['Url']
        if self.dtDefine.getVariableDefineByUrl(tUrl) == {}:
            self.logger.error("Datcom定义中并不包含%s的定义！"%tUrl)
            return False
        #执行更新逻辑
        tDefault = self.dtDefine.getVariableTemplateByUrl( tUrl, isSubType = True)
        
        if tUrl in self.doc.keys() and iVar['Value'] is None :
            #如果Value为None则删除该变量
            self.logger.warning("设置Array变量%s的%d位的数值为None是没有意义的！"%(tUrl, iIndex))
            self._setArrayItembyIndex(tDefault, iIndex)
            #设置数据的标识位
            #self.doc[iVar['Url']].update({'InUsed':'True'})
            return False
        elif tUrl in self.doc.keys() and iVar['Value'] is not  None:
            #如果Vlaue不为None则更新信息
            self._setArrayItembyIndex(iVar, iIndex)
            return True
        elif tUrl not in self.doc.keys() and iVar['Value'] is None :
            return True
        elif  tUrl not in self.doc.keys() and iVar['Value'] is not None :
            #首先调用添加变量的函数
            namelist, varName = tUrl.split('/')
            self.addNamelist(namelist, [varName])
            self._setArrayItembyIndex(iVar, iIndex)      
            return True    
            
        return True         

    def _setArrayItembyIndex(self, iVar, iIndex):
        """
        用来设置Array变量的某个值
        函数特性:
        1.如果iIndex在iVar['Url']指定的变量现有长度之内则直接更新
        2.如果iIndex在iVar['Url']指定的允许长度之内，则扩大长度到iIndex，并添加
        3.如果iIndex超过了iVar['Url']指定允许的最大返回，则触发错误
        注意事项：
        函数不进行大量的检查，因此可能造成异常
        """
        tUrl = iVar['Url']       
        try:
            tRange        = self.dtDefine.getVariableDefineByUrl(tUrl)['Limit']
            tDefault       = self.dtDefine.getVariableTemplateByUrl( tUrl, isSubType = True)
            tNowVar      = self.getVariableByUrl(tUrl)
            tNowLength = len(tNowVar['Value']) 
            #转换坐标
            tNewInputVar = iVar
            if tNowVar['Unit'] != iVar['Unit']:
                tNewInputVar = dtDimension.unitTransformation(iVar,tNowVar['Unit'] )
            #判断长度变换
            if tNowLength> iIndex  and  tNowLength != 0:  #iIndex从零开始，-1无效，所哟len恒大于Iindex
                tNowVar['Value'][iIndex] = tNewInputVar['Value']
            if tNowLength < iIndex + 1 and iIndex > tRange[0] and iIndex < tRange[1]:
                tNowVar['Value'] = tNowVar['Value'] + [tDefault['Value']]*(iIndex + 1 - tNowLength)
                tNowVar['Value'][iIndex] = tNewInputVar['Value']
            if  iIndex > tRange[1] or  iIndex < 0:
                raise UserWarning("无效的索引")            
        except Exception as e:
            self.logger.error("设置过程出错！%s"%(e))
 

    def checkMustKeys(self, iDict, iMust=[]):
        """
        检查iDict变量是合法的dict类型
        检查规则：
        1.iDict是dict类型
        2.iMust定义的所有key都在iDict中
        
        """
        if iDict is None or not type(iDict):
            return False, '输出变量不是字典类型：%s'%str(iDict)
        if iMust == []:
            return True, ''
        tReport = []
        tResult = True
        for iKey in iMust:
            if iKey not in iDict.keys():
                tReport.append('key:%s不在字典中'%iKey)
                tResult = False
        return tResult, '\n'.join(tReport)

        
    def validateAVariable(self,tVar ):
        """
        验证单个变量是否符合定义要求        
        """
        tReport = {'status':'Intermediate', 'Report':[]}
        tLog = []
        #验证有效性        
        if tVar is None or type(tVar) != dict:
            tReport['status'] = 'Invalid'
            tReport['Report'].append("模型无效")
            return tReport
            
        #验证字段完整性
        tMust = ['VarName', 'Namelist', 'Url', 'Unit', 'Value']
        for iP in tMust:
            if iP not in tVar.keys():
                tReport['status'] = 'Invalid'
                tReport['Report'].append("模型缺少字段%s"%iP)
        if tReport['status']  == 'Invalid':
            return tReport
        #获得定义
        tUrl = tVar['Url']
        tDf = self.dtDefine.getVariableDefineByUrl(tUrl)
        #检查单位问题
        if  'Dimension' in tDf.keys() and tDf['Dimension'] not in [None, '', '/']\
        and  tVar['Unit'] not in dtDimension.getUnitListByDimension(tDf['Dimension']):
            tLog.append("变量的单位与定义的单位/量纲%s不对应"%(tDf['Dimension']))  
        
        #进行值类型
        #INT
        if tDf['TYPE'] == 'INT' :
            if type(tVar['Value']) not in [ int, float]: 
                tLog.append('变量的类型应当为INT实际是%s'%(type(tVar['Value']))) 
            elif 'Range' in tDf.keys() :
                if tVar['Value'] < tDf['Range'][0] or tVar['Value'] > tDf['Range'][1]:
                    tLog.append('变量的类型INT,值：%d,超出Range：%s '%(tVar['Value'], str(tDf['Range'])))                
        #REAL
        if tDf['TYPE'] == 'REAL' :
            if  type(tVar['Value'])  != float:
                tLog.append('变量的类型应当为REAL实际是%s'%(type(tVar['Value']))) 
            elif 'Range' in tDf.keys() :
                if tVar['Value'] < tDf['Range'][0] or tVar['Value'] > tDf['Range'][1]:
                    tLog.append('变量的类型REAL,值：%f,超出Range：%s '%(tVar['Value'], str(tDf['Range'])))             
            
        if tDf['TYPE'] == 'Array' :
            if 'InUsed' in tVar and tVar['InUsed'] == 'True':
                #默认不使用的变量是错误的
                if type(tVar['Value']) != list :
                    tLog.append('变量的类型应当为Array实际是%s'%(type(tVar['Value'])) )
                elif len(tVar['Value']) ==0:
                    tLog.append("变量%s的类型为Array，但值长度为0"%tUrl )      
                elif 'Range' in tDf.keys() :
                    for iV in tVar['Value']:
                        if ('SubType' in tDf.keys() and tDf['SubType'] in ['INT', 'REAL']) or\
                            'SubType' not  in tDf.keys() :
                            if iV < tDf['Range'][0] or iV > tDf['Range'][1]:
                                tLog.append('变量的类型Array,值：%s,超出Range：%s '%(str(iV), str(tDf['Range'])))  
                        if 'SubType' in tDf.keys() and tDf['SubType'] in ['List']:
                             if iV not in  tDf['Range']:
                                tLog.append('变量的类型Array,元素类型%s,值：%s,超出Range：%s '%(tDf['SubType'] , 
                                str(iV), str(tDf['Range'])))  
                    
        if tDf['TYPE'] == 'List' :
            if type(tVar['Value'])  != str:
                tLog.append('变量的类型应当为List 实际是%s'%(type(tVar['Value'])))
            elif 'Range' in tDf.keys() :
                if tVar['Value'] not in  tDf['Range']:
                    tLog.append('变量的类型List,值：%s,超出Range：%s '%(str(tVar['Value']), str(tDf['Range'])))                  
        #形成总报告
        if len(tLog) ==0:
            tReport = {'status':'Acceptable', 'Report':[]}
        else:
            tReport = {'status':'Invalid', 'Report':tLog}   
        return tReport
        
    def validate(self):
        """
        验证当前的配置是否是一个可以执行的配置，并返回报告信息
        返回值为当前配置的错误报告
        """
        tReport = {'status':'Acceptable', 'Report':[]}        
        #开始内容分析机制,        
        tAllInfo = self.getNamelistCollection()
        if tAllInfo is None or len(tAllInfo) == 0 :
            tReport['Report'].append("模型内并没有信息/n")
            tReport['status'] = 'Invalid'       
            
        for iN in tAllInfo.keys():#循环Namelist 
            #Namelist 级别
            tNMExcept = {}
            for iV in tAllInfo[iN]:
                tUrl = '%s/%s'%(iN,  iV)
                #变量本身值得验证
                tVarReport = self.validateAVariable(self.doc[tUrl])
                if tVarReport['status'] != 'Acceptable':
                    tReStr ='\n'.join(tVarReport['Report'])
                    self.logger.info("datcomModel.validate()变量%s不合规，%s"%(tUrl,tReStr ))
                    #尝试修复或者跳过
                    tNMExcept[tUrl] = tReStr
                    continue
                #附加规则验证
                #pass
            if len(tNMExcept) == 0:
                tReport['Report'].append({'Namelist':iN, 'status':'Acceptable', 'Report':[]}) 
            else:
                tReport['Report'].append({'Namelist':iN, 'status':'Intermediate', 'Report':tNMExcept})
                    
        #形成总报告
        for iN in tReport['Report']:
            if iN['status'] != 'Acceptable':
                tReport['status'] = 'Invalid'
                break   
        return tReport
    
        
    def buildDatcomInputFile(self, path):
        """
        根据当前的CASE配置创建datcom的计算文件
        过程出错将引发异常
        """
        if self.doc is None or len(self.doc ) == 0:
            raise  UserWarning('模型内并没有信息') 
        #开始内容分析机制
        tReport = self.validate()    
        if tReport['status'] != 'Acceptable':       
            #分析报告并输出
            tAllError = []
            for iR in tReport['Report']:
                if iR['status'] != 'Acceptable':
                    for iSubR in iR['Report']:
                        tAllError.append( iR['Report'][iSubR])
            tReport = '\n'.join(tAllError)
            self.logger.info(str(tReport))   
            raise  UserWarning(tReport)     
        #获得doc的全部定义
        tAllInfo = self._getNamelistCollectionInUsed()
        if tAllInfo is None or len(tAllInfo) == 0 :
            raise  UserWarning('不包含数据的空模型') 

        #将信息格式化为输出文件 要求列宽为80
        TStr = []
        tCASEDes = ''
        tCASEID =1
        for itr in tAllInfo.keys():#循环Namelist 
            #添加一个数据行，记录            
            TStr.append('')  
            theStr = " $%s "%itr
            self.Append80ColumsLimit(TStr, theStr)
            tNMlstPos = len(TStr[-1]) #记录Namelist变量的位置
            lastCheck = 1
            for itVar in tAllInfo[itr]:#循环Var []
                #获得当前变量的结论
                theStr = '%s'%itVar 
                tURl    = '%s/%s'%(itr, itVar)
                tVarStruct = self.getVariableByUrl(tURl)
                tVarValueS = tVarStruct['Value']
                #创建写入
                lastCheck +=1
                if tVarValueS is None :
                    self.logger.error("创建Datcom计算配置文件失败，不能为空值创建结果！%s/%s"%(itr,itVar ))
                    continue
                    #raise(Exception("创建Datcom计算配置文件失败，不能为空值创建结果！%s/%s"%(itr,itVar ))) 
                #跳过不在使用的变量
                if 'InUsed' in tVarStruct and tVarStruct['InUsed'] =='False' :
                    continue
                #执行分析逻辑
                tVarDf = self.dtDefine.getVariableDefineByUrl(tURl)
                if  tVarDf['TYPE'] == 'Array': #对于序列类型
                    if 'SIndex' in tVarStruct.keys() and tVarStruct['SIndex'] not in [1, '1'] :
                        theStr += '(%s)='%(tVarStruct['SIndex'])
                    else:
                        theStr += '='
                    if len(TStr[-1])> tNMlstPos: #当当前行是非空行时换行增加序列值
                        TStr.append(' '*tNMlstPos)     
                    self.Append80ColumsLimit(TStr, theStr)
                    tNewPos = len(TStr[-1]) #记录当前变量的位置   
                    for itValue in tVarValueS: #循环追加所有的数据                        
                        if itValue < int(self.Properties['numForE']) :
                            theStr = '%.3f,'%itValue
                        else:
                            theStr = '%.3E,'%itValue
                        self.Append80ColumsLimit(TStr, theStr, tNewPos)        
                    #防止个数变量出现在序列变量之后
                    if lastCheck < len(tAllInfo[itr]): 
                        TStr.append(' '*tNMlstPos) 
                elif tVarDf['TYPE'] == 'List' :#对于1.0 2.0 或者.TRUE.等字符型 
                    #具有默认值则不输出
                    if not ('Default' in tVarDf.keys() and  tVarValueS == tVarDf['Default']):  
                        theStr += '=%s,'%tVarValueS
                        self.Append80ColumsLimit(TStr, theStr, tNMlstPos)
                    else:
                        theStr = ''
                elif tVarDf['TYPE'] == 'INT' :#对于数值类型  INT  
                    theStr += '=%d.0,'%int(tVarValueS)
                    self.Append80ColumsLimit(TStr, theStr, tNMlstPos)
                elif tVarDf['TYPE'] == 'REAL' :#对于数值类型 REAL                
                    if float(tVarValueS) < float(self.Properties['numForE']):
                    #if float(tVarValueS) < self.numForE:
                        theStr += '=%.3f,'%float(tVarValueS)
                    else:
                        theStr += '=%.3E,'%float(tVarValueS)
                    self.Append80ColumsLimit(TStr, theStr, tNMlstPos)
            #写入Datcom的Namelist的结尾部分$
            TStr[-1] = TStr[-1][:-1] + '$'            
      

        #写入CASE ID
        TStr.append('CASEID %s,CASE %s'%(tCASEDes, str(tCASEID)) )       
        #写入SAVE
        TStr.append('SAVE')
        TStr.append('DUMP CASE' )      
        TStr.append('NEXT CASE' )   
        
        LastResult = '\n'.join(TStr)
        with open(path,"w") as f:
            f.write(LastResult)
            
        return LastResult
        
   
    def getVariableByUrl(self, tUrl):
        """
        获得tUrl对应的Datcom变量副本
        注意：外部修改该变量副本将影响dcmodel中值，因为返回的是一个dict
        
        """
        if tUrl is None or tUrl == "" or self.doc is None:
            return None
        if tUrl in self.doc.keys():
            return self.doc[tUrl]
            
    def getVariableCopyByUrl(self, tUrl) :
        """
        获得tUrl对应的Datcom变量的副本
        注意：外部修改该变量的值不会影响dcmodel中值，因为返回的是一个dict的拷贝        
        """
        tV =  self.getVariableByUrl(tUrl)
        if  tV is not None:
            return tV.copy()


    
    def getDiscreteVariableValueByName(self, tUrl):
        """
        函数从当前模型中获得变量的值，对应离散量 List
        tUrl变量的Url: Namelist/VarName
        """   
        return self.getVariableByUrl(tUrl)


    
    def getContinuousVariableValueByName(self, tUrl):
        """
        函数从当前模型中获得变量的值/单位/量纲，对应连续量 INT REAL
        tUrl变量的Url: Namelist/VarName
        """
        #获得对应变量的最基本的定义 {'Dimension':'', 'Value':None , 'Unit':'' }
        return self.getVariableByUrl(tUrl)
        
        
    def setDiscreteVariableValueByName(self,tUrl, tVar):
        """
        写入离散量的值，写入信息Discrete的量 对应List类型 
        varValue 直接就是对应的结果
        """        
        tVarIn = self.getDiscreteVariableValueByName(tUrl)
        if tVarIn is not None :
            if type(tVar) is str and tVar != '':
                tVarIn['Value'] = tVar
            elif type(tVar) is dict :
                if 'Value' in tVar.keys() and tVar['Value'] is None:
                    self.doc.pop(tUrl)   #当变量的值为None是，将删除该变量
                else:
                    self.doc[tUrl].update(tVar)  #当变量的值不为None是，将跟新该变量
            else:
                self.logger.error("dcModel.setDiscreteVariableValueByName，无法处理的类型信息%s"%(str(tVar))) 
        else:   #模型中本来就没有该变量
            if 'Value' in tVar.keys() and tVar['Value'] is not None:
                self.doc[tUrl] = tVar
            else :#当原来就没有时直接忽略
                pass
        

    
    def setContinuousVariableValueByName(self, tUrl, varValue):
        """
        写入连续量的值，写入信息Continuous的量 对应Int Real类型
        """
        tVarIn = self.getContinuousVariableValueByName(tUrl)
        if tVarIn is not None :
            if type(varValue) is str :
                if self.dtDefine.getVariableDefineByUrl()['TYPE'] == 'INT':
                    tVarIn['Value'] = int(float(varValue))
                elif self.dtDefine.getVariableDefineByUrl()['TYPE'] == 'REAL':
                    tVarIn['Value'] = float(varValue)
                else:
                    self.logger.error("格式异常")
            elif type(varValue) is dict :
                if 'Value' in varValue.keys() and varValue['Value'] is None:
                    self.doc.pop(tUrl)   #当变量的值为None是，将删除该变量
                else:
                    self.doc[tUrl].update(varValue)  #当变量的值不为None是，将跟新该变量
            else:
                self.logger.error("dcModel.setContinuousVariableValueByName无法处理的类型信息%s"%(str(varValue)))   
    
    def removeVariable(self, tUrl):
        """
        将从模型中移除tUrl的相关数据
        """
        if tUrl in self.doc.keys():
            self.doc.pop(tUrl)
        #self.doc.pop(tUrl)


        
    def Append80ColumsLimit(self, tStrBuffer, beAddStr,   newLineStartPos = 1, limitLen = 78):  
        """
        将输出流限制到宽度
        tStrBuffer 是一个str的List数组
        """  
        if len(beAddStr) + newLineStartPos >= limitLen:
            self.logger.error("需要追加的字段实在是太长了")
        #尝试追加数据
        if len(tStrBuffer[-1]) + len(beAddStr)  < limitLen:
            tStrBuffer[-1] += beAddStr
        else :
            tStrBuffer.append(' '*newLineStartPos + beAddStr)
            
    
    def Variable_ETElementToDict(self, elem):
        """
        将一个Variable的ET.Element节点翻译成一个dict
        elem ET.Element 是保存该变量的XML节点
        """
        if elem is None or type(elem) != ET.Element:
            return {}
        #开始翻译
        tVAttrib = elem.attrib #获得变量的属性
        tDf = self.dtDefine.getVariableDefineByUrl(tVAttrib['Url'])
        #获取值
        if elem.text != None or elem.text.strip() != '':
            if tDf['TYPE'] == 'INT':
                tVAttrib['Value'] = int(float(elem.text))
            elif tDf['TYPE'] == 'REAL':
                tVAttrib['Value'] = float(elem.text)
            elif tDf['TYPE'] == 'Array':
                tVAttrib['Value'] = eval(elem.text)
            elif tDf['TYPE'] == 'List':
                tVAttrib['Value'] = elem.text
            else:
                self.logger.error("异常类型信息：%s"%tDf['TYPE'])
        else:            
            #强制为该参数的默认值
            tVAttrib['Value'] = tDf.getVariableTemplateByUrl(tVAttrib['Url'])
        #添加到子节点中
        return tVAttrib
        
    def Variable_DictToETElement(self, tDict, tRoot = None):
        """
        将tDict转化为一个elem描述
        
        """
        tMust = ['VarName', 'Namelist', 'Url', 'Unit', 'Value']
        if tDict is None or type(tDict) != dict:
            return None
        isOk = True
        for iP in tMust:
            if iP not in tDict.keys():
                isOk = False
                break
        if not isOk:
           return None
        tValue = str(tDict['Value'])
        tElemAttrib = tDict.copy()
        tElemAttrib.pop('Value')
        
        #开始转换
        if tRoot is None :
            tNode = ET.Element('VARIABLE', tElemAttrib)
        else:
            tNode = ET.SubElement(tRoot, 'VARIABLE', tElemAttrib)
        tNode.text = tValue
        return tNode
        
    def getVariableCollection(self):
        """
        返回模型中定义的所有的变量 {url：variable}
        返回的变量组合不再影响该模型的值 self.doc.copy()
        """
        return self.doc.copy()
 

if __name__=="__main__":
    """
    """

    sPath  = r'E:\Projects\PyDatcomLab\extras\PyDatcomProjects\1\case2.xml'
    obPath = r'E:\Projects\PyDatcomLab\extras\PyDatcomProjects\1\case3.xml'
    dtPath = r'E:\Projects\PyDatcomLab\extras\PyDatcomProjects\1\case3.inp'
    try:
        aLoader = dcModel(sPath)
        aLoader.setVariable({'Url':'FLTCON/NMACH', 'Value':'20', 'Unit':''})
        #aLoader = dcModel()
        aLoader.save(obPath)
        aLoader.buildDatcomInputFile(dtPath)
    except Exception as e:
        print(repr(e))
