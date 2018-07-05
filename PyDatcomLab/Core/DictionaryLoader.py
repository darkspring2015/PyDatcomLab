#!/usr/bin/env python

"""
DTdictionary类负责加载Datcom的基本定义，包括：
1.Namelist和Varialbe的关系
2.variable的类型、取值范围、默认值、单位信息、量纲信息、变量名、显示名、帮助信息、数组长度限制等
3.variable之间的相互关系（AddtionalInforation），如分组信息，
DTdictionary类的基本行为：
1.如果path指定合适的值，从path变量指定的文件中读取Datcom配置信息；
2.如果path没有指定合适的值，则尝试从~/.Pydatcom/config/datcomDefine.xml中读取配置
3.内存中使用dict数据结构
定义Datcom各种类型选项卡加载所需要的组件
组件将XML定义文件加载到内存，并实例化相关的类
"""
import os
import logging
from xml.etree import ElementTree  as ET
from PyDatcomLab.Core.datcomDimension import Dimension
from PyDatcomLab.Core import datcomDimension as DimTools
#from PyDatcomLab.Core import datcomTools as tools

class DTdictionary():
    """
    """
    def __init__(self, path):
        """
        初始化定义文件
        """
        #创建日志
        self.logger = logging.getLogger(r'Datcomlogger') 
        #确认配置文件的正确性
        self.basePath = os.path.join(os.path.expanduser('~'), '.PyDatcomLab', 'config', 'datcomDefine.xml')
        if not os.path.exists(path):
            path = self.basePath
        self.dicPath = path
        #定义一个字典存储所有的定义信息
        self.Dictionary = {}             #按照键值对方式存储所有的信息
        self.dictAddtional = {}         #以CARD/Namelist为键，保存附加定义信息 NMACHLinkTable  RuleNumToCount RuleIndexToCombo groupDefine
        self.NamelistAttribute = {}   #存储Namelist自身的属性值信息
        self.docXML = ET.ElementTree().getroot()
        #调用加载
        self.loadDictionory(self.dicPath)
    
    def loadDictionory(self, tFile):
        """
        执行所有的定义加载逻辑
        """        
        if not os.path.isfile(tFile):
            tFile = self.basePath
        
        root = None
        try:
            root = ET.parse(tFile).getroot()
        except Exception as e:
            self.logger.error("加载Datcom定义文件过程中发生异常，%s ：%s"%(repr(e), str(e)))
            
        if root is None:return False
        #保存XML信息
        self.docXML = root
        #分析XML文件
        if not root.tag == 'DatcomVariableDefine': 
            self.logger.error("加载XML文件：%s失败，其中不包含DatcomVariableDefine节"%tFile)
            return False
        #循环获取所有配置信息  
        for nmlstNode in list(root):
            #遍历所有的根节点，认为是Namelist节点
            nmlistName = nmlstNode.tag
            self.Dictionary[nmlistName] = {}  #添加Namelist的定义
            self.NamelistAttribute[nmlistName] = nmlstNode.attrib
            for varNode  in list(nmlstNode):
                if varNode.get('dcType') == 'Infomation':
                    #如果是变量节则跳过
                    self.loadAddtionalInforation(varNode, nmlistName)
                    continue 
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
            #
        #结束加载
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
                tResDict = {}
                for iA in iT.attrib:
                    tNTable =iT.attrib[iA]
                    try:
                        tNTable = eval(iT.attrib[iA])
                    except Exception as e:
                        self.logger.warning("无法推定NMACHLinkTable属性%s：%s的实际类型"%(iA,iT.attrib[iA]))
                        
                    tResDict.update({iA:tNTable})
                     
                #写入结果
                self.dictAddtional[tNmlst]['NMACHLinkTable'] = tResDict
                 
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
            elif iT.tag == 'RuleVariableCorrelation':                
                try:
                    tAllCombo = []
                    for iR in list(iT):
                        #载入每一条规则
                        tAllComboDict = iR.attrib
                        tAllComboDict.update({'CorrelatedVariables':eval( iR.attrib['CorrelatedVariables'])})
                        tAllComboDict['HowTo'] = {}
                        for iHowto in list(iR):
                            tSets =iHowto.attrib                 
                            tSets.update({'RelationalExpr': eval(iHowto.attrib['RelationalExpr'])})
                            tAllComboDict['HowTo'].update({iHowto.attrib['key']:tSets})
                        tAllCombo.append(tAllComboDict)
                    self.dictAddtional[tNmlst]['RuleVariableCorrelation'] = tAllCombo                   
                except Exception as e:
                    self.logger.error("Datcom字典执行规则RuleVariableCorrelation解析出错")

            else:                
                self.logger.error("解析规则异常：%s"%(tNmlst + " " + iT.tag))
        
                
    def getNamelistDefineByName(self, nmlst):
        """
        返回nmlst定义的选项卡的所有变量的列表
        """
        if nmlst in self.Dictionary.keys():
            return self.Dictionary[nmlst]
        else:
            return {}
            
    def getNamelistAttributeByName(self, nmlst):
        """
        返回Namelist的自身属性，dcType， displayName等
        """
        if nmlst in self.NamelistAttribute.keys():
            return self.NamelistAttribute[nmlst]
        else:
            return {}
            
    def getCARDAddtionalInformation(self, nmlst, ruleName):
        """
        返回nmlst对应CARD的附加信息
        """
        if nmlst in self.dictAddtional.keys():
            if ruleName in self.dictAddtional[nmlst].keys():
                return self.dictAddtional[nmlst][ruleName]

        return {}
        
    def getCARDHelpDoc(self, nmlist):
        """
        获得对应nmlist的帮助文档
        nmlist str 是选项卡的名称
        """
        tHelp = self.getCARDAddtionalInformation(nmlist, 'HelpDoc')
        if tHelp is not None and 'Url' in tHelp.keys():
            return tHelp['Url']
        return "https://github.com/darkspring2015/PyDatcomLab/blob/master/"
            
        

    def gettRuleNumToCountByGroup(self, nmlst, groupNm):
        """
        通过group查询对应的控制变量
        认为控制变量
        """
        tCName = None 
        if nmlst in self.dictAddtional.keys() and 'RuleNumToCount' in self.dictAddtional[nmlst]:
            for iR in self.dictAddtional[nmlst]['RuleNumToCount']:
                if 'Num' in iR.keys() and 'Group' in iR.keys():
                    if groupNm == iR['Group']:
                        tCName = iR['Num']        
        return tCName

    def getRuleIndexToComboByGroup(self, nmlst, groupNm):
        """
        获得groupNm对应组的变量组合控制变量的名称
        """
        if nmlst in self.dictAddtional.keys() and 'RuleIndexToCombo' in self.dictAddtional[nmlst]:
            for iR in self.dictAddtional[nmlst]['RuleIndexToCombo']:
                if 'Group' in iR.keys() and 'Index' in iR.keys() and groupNm == iR['Group']:
                    return  iR       
        return None
        
    def getRuleIndexToComboByComboVariable(self, nmlst, comboVar):
        """
        获得groupNm对应组的变量组合控制变量的名称
        """
        if nmlst in self.dictAddtional.keys() and 'RuleIndexToCombo' in self.dictAddtional[nmlst]:
            for iR in self.dictAddtional[nmlst]['RuleIndexToCombo']:
                if 'Group' in iR.keys() and 'Index' in iR.keys() and comboVar == iR['Index']:
                    return  iR       
        return None        
        
    def getRuleNMACHLinkTable(self,  nmlst):
        """
        获得nmlst中的受NMACH变量影响的Group组，list形式
        """
        if nmlst in self.dictAddtional.keys() and 'NMACHLinkTable' in self.dictAddtional[nmlst]:
            return self.dictAddtional[nmlst]['NMACHLinkTable']['varList']
        return []
        
        
    def isLinkToNMACH(self, nmlst, iGroup):
        """
        判断nmlst/iGroup是否关联到NMACH
        """
        if nmlst in self.dictAddtional.keys() and 'NMACHLinkTable' in self.dictAddtional[nmlst] and\
        iGroup in self.dictAddtional[nmlst]['NMACHLinkTable']['varList']:
            return True
        else:
            return False
            
        
    def getGroupVarsByName(self, nmlst, groupNm):
        """
        获得nmlistName中对应组的所有变量定义
        """
        tVars = {}
        if nmlst in self.Dictionary.keys():
            tNmlstDf = self.Dictionary[nmlst]
            for iV in tNmlstDf.keys():
                if 'Group' in tNmlstDf[iV].keys() and groupNm  == tNmlstDf[iV]['Group']:
                    tVars[iV] = tNmlstDf[iV]
        return tVars
                    
        
    def parserArray(self, tStr):
        """
        解析Array的字符串
        """    
        tR = None
        tStr = tStr.replace('inf', "float('inf')")
        try:
            tR = eval(tStr)
        except:
            self.logger.error("无法对%s执行eval转化"%(tStr))
        
        return tR
        
    def getGroupDefine(self, tNmlst):
        """
        获得对应Namelist的组定义
        Etc：
        groupDefine ={ #组名：{'Name':'widget的名称','ShowName':'显示名称','ToolTips':'组的提示信息'}
    'ALSCHD':{'Name':'ALSCHD','ShowName':'攻角','ToolTips':'录入攻角信息'}, 
    'Speed_Atmospheric':{'Name':'Speed_Atmospheric','ShowName':'速度/大气参数','ToolTips':'录入速度/大气参数'}, 
} 
       返回的是组名的序列 ,错误的返回空序列，否则导致程序异常
        """
        tGroupList = []
        tCARD = self.getNamelistDefineByName(tNmlst)
        if tCARD is {}:return tGroupList
        
        #存在定义
        for itVar in tCARD.keys():
            if 'Group' in tCARD[itVar].keys():
                tGroupList.append(tCARD[itVar]['Group'])
        #返回结果
        return tGroupList
        
    def getGroupLimitByName(self, tNmlst, tGroup):
        """
        返回Namelist中某个组的最大数量 对应变量的Limit属性
        """
        tVars = self.getGroupVarsByName(tNmlst, tGroup)
        tL = [0, 0]
        for iV in tVars.keys():
            if 'Limit' in tVars[iV].keys() :
                tLimit = tVars[iV]['Limit']
                if tL[0] > tLimit[0]:tL[0] = tLimit[0]
                if tL[1] < tLimit[1]:tL[1] = tLimit[1]
        return tL

    def getVariableDimensionByName(self, nmlst, VarName):
        """
        获得对应变量的量纲和单位信息
        """
        tR = {'Dimension':'', 'Value':None , 'Unit':'' }
        if nmlst in self.Dictionary.keys() and VarName in self.Dictionary[nmlst].keys()\
        and 'Dimension' in self.Dictionary[nmlst][VarName].keys():
            tR['Dimension'] = self.Dictionary[nmlst][VarName]['Dimension']
            if tR['Dimension'] in Dimension.keys():
                tR['Unit'] = Dimension[tR['Dimension']][0]
            else:
                self.logger.error("获得的定义有问题，有Dimension但没有Unit！ %s-%s"%(nmlst, VarName))
        return tR

    def getPlaceholderText(self, iUrl):
        """
        返回变量iUrl的占位符
        1.如果是List 类型返回空
        2.如果是REAL, INT,返回变量范围说明,如果没有定义range，返回空
        3.如果是Array类型，返回SubType对应变量的返回
        4.如果iUrl不是合法定义，返回None
        """
        tVarDf  = self.getVariableDefineByUrl(iUrl)
        if tVarDf is None:
            return ''
        elif tVarDf['TYPE'] == 'List':
            return ''
        elif tVarDf['TYPE'] == 'REAL':
            #分析上下限
            if 'Range' in tVarDf.keys():
                tRange = tVarDf['Range']                    
                #分析占位符
                if 'Decimals' in tVarDf.keys():
                    tDec = int(tVarDf['Decimals'])
                    tFloatFormat = '%%.%df'%tDec
                    tFStr  = '%s - %s'%(tFloatFormat, tFloatFormat)
                    return tFStr%(tRange[0], tRange[1])
                else:
                    return '%f-%f'%(tRange[0], tRange[1])
            else:
                return ''
        elif tVarDf['TYPE'] == 'INT':
            #分析上下限
            if 'Range' in tVarDf.keys():
                tRange = tVarDf['Range']     
                return '%d-%d'%(tRange[0], tRange[1])
            else:
                return ''   
        elif tVarDf['TYPE'] == 'Array':
            if 'SubType' in tVarDf.keys() and tVarDf['SubType'] in ['BOOL', 'List']:
                return ''
            else:
                #分析上下限
                if 'Range' in tVarDf.keys():
                    tRange = tVarDf['Range']                    
                    #分析占位符
                    if 'Decimals' in tVarDf.keys():
                        tDec = int(tVarDf['Decimals'])
                        tFloatFormat = '%%.%df'%tDec
                        tFStr  = '%s - %s'%(tFloatFormat, tFloatFormat)
                        return tFStr%(tRange[0], tRange[1])
                    else:
                        return '%f-%f'%(tRange[0], tRange[1])
                else:
                    return ''
        return ''



    def getVariableDefineByUrl(self, tUrl):
        """
        从定义中获得对应变量的定义信息  使用getVariableDefineByName
        """
        if tUrl is None or tUrl == "":
            return None 
        nmlst   = tUrl.split('/')[-2]
        VarName = tUrl.split('/')[-1]
        return self.getVariableDefineByName(nmlst, VarName)
    

    def getVariableDefineByName(self, nmlst, VarName):
        """
        从定义中获得对应变量的定义信息，包括：
        1. 变量自身说明
        2. 涉及的相关规则
        返回值是字典
        """
        tVarDefine = {}
        if nmlst in self.Dictionary.keys() and VarName in self.Dictionary[nmlst].keys():
            tVarDefine = self.Dictionary[nmlst][VarName]
        if tVarDefine is None :return {}
        #获得附加的定义
        #RuleIndexToCombo
        tAddtionInfo = self.getCARDAddtionalInformation(nmlst, 'RuleIndexToCombo')
        if tAddtionInfo is not None :
            for iR in tAddtionInfo:
                if 'Index' in iR.keys() and VarName == iR['Index']:
                    tVarDefine['RuleIndexToCombo'] = iR
                    break        

        #RuleVariableStatus
        tAddtionInfo = self.getCARDAddtionalInformation(nmlst, 'RuleVariableStatus')
        if tAddtionInfo is not None :
            for iR in tAddtionInfo:
                if 'ControlVar' in iR.keys() and VarName == iR['ControlVar']:
                    tVarDefine['RuleVariableStatus'] = iR
                    break
     
        #RuleNumToCount
        tAddtionInfo = self.getCARDAddtionalInformation(nmlst, 'RuleNumToCount')
        if tAddtionInfo is not None :   
            tRules = []
            for iR in tAddtionInfo:
                if 'Num' in iR.keys() and VarName == iR['Num']:
                    tRules.append(iR)
            if len(tRules) >0:
                tVarDefine['RuleNumToCount'] = tRules
                
        
        return tVarDefine
        
        
    def getNamelistCollection(self):
        """
        返回Namelist的全集
        """
        return list(self.Dictionary.keys())
        
    def getBasicNamelistCollection(self):
        """
        返回基础Datcom算例的Namelist组合
        """
        return ['FLTCON', 'BODY']
        
    def getVariableTemplateByUrl(self, tUrl, isSubType = False):
        """
        返回tUrl对应的变量模板
            <VARIABLE VarName='LOOP'  Namelist='FLTCON' Url='FLTCON/LOOP' Unit='/'>['1.0']</VARIABLE>
            <VARIABLE VarName='NMACH' Namelist='FLTCON' Url='FLTCON/NMACH' Unit='/'>[3]</VARIABLE>
            <VARIABLE VarName='MACH'  Namelist='FLTCON' Url='FLTCON/MACH' Unit='/' SIndex ='1'>[0.15,0.25,0.25]</VARIABLE>
            <VARIABLE VarName='X'  Namelist='BODY' Url='BODY/X' Unit='L/m' SIndex ='1'>[0.15,0.25,0.25]</VARIABLE>
            #将保证List是由string型组成的
            每次返回的都是独立的实例
            
        """
        #获得定义
        tDf = self.checkUrl(tUrl)
        if tDf is None :return None
        #解析并构造变量的结构
        tRes = {
            'VarName':tDf['VarName'], 
            'Namelist':tDf['NameList'], 
            'Url':tUrl, 
            'Unit':'',    
            'Value':None 
        }
        #检查单位
        if 'Dimension' in  tDf.keys():
            tRes['Unit'] = DimTools.getMainUnitByDimension(tDf['Dimension'])
        #检查默认值        
        if 'Default' in tDf.keys() :
            if tDf['TYPE'] == 'Array'  :
                if  isSubType: #
                    tRes['Value'] = tDf['Default']
                else:
                    tRes['Value'] = []
            else:
                tRes['Value'] = tDf['Default']
        else:
            #进行值类型
            #INT 默认0或者range的下限
            if tDf['TYPE'] == 'INT' : 
                if 'Range' in tDf.keys() :
                    tRes['Value'] = tDf['Range'][0]  
                else:
                    tRes['Value'] = 0                    
            #REAL 默认0或者range的下限
            elif tDf['TYPE'] == 'REAL' :
                if 'Range' in tDf.keys() :
                    tRes['Value'] = float(tDf['Range'][0] ) 
                else:
                    tRes['Value'] = 0.0             
            #Array 默认[]
            elif tDf['TYPE'] == 'Array' :
                if isSubType : #如果是要求获取Array的子项的默认值
                    if 'SubType' in tDf.keys() and tDf['SubType'] in ['BOOL', 'LIST'] :  
                        #当表格输入.TRUE. .FALSE.
                        if 'Range' in tDf.keys() :
                            tRes['Value'] = str(tDf['Range'][0])  #将保证List是由string型组成的
                    else:
                        #其他没有定义'SubType'和'SubType'不是'BOOL', 'LIST'的情况
                        if 'Range' in tDf.keys() :
                            tRes['Value'] = float(tDf['Range'][0] ) 
                        else:
                            tRes['Value'] = 0.0   
                else:
                    tRes['Value'] = []    
            #List 默认            
            elif tDf['TYPE'] == 'List' :                
                if 'Range' in tDf.keys() :
                    tRes['Value'] = str(tDf['Range'][0])  #将保证List是由string型组成的
                else:
                    self.logger.error('%s对应的Define存在问题，list没有range，无法推定初始值！'%tUrl)
                
        #检查Array类型
        if tDf['TYPE'] == 'Array':
            tRes['SIndex'] = '1'
            #tRes['Value'] = []
        #设置标志位
        tRes['Edited'] = False
        
        return tRes
        
    def checkUrl(self, tUrl):
        """
        检查tUrl是否指向有效的变量
        不存在定义返回None
        存在定义返回Url对应的定义信息，包含自身定义信息和附加的规则定义信息
        """
        if tUrl is None or tUrl == "":
            return None
        
        tRes = tUrl.split('/')
        if len(tRes)< 2: 
            return None
        tNamelist = tRes[-2]
        tVarName  = tRes[-1]
        tDf = self.getVariableDefineByName( tNamelist, tVarName)
        if tDf == {}:return None        
        return tDf
        
        
        
        
        
        
#导出一个常量
defaultDatcomDefinition = DTdictionary("")


        
        
