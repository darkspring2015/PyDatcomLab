#!/usr/bin/env python

"""
定义Datcom各种类型选项卡加载所需要的组件
组件将XML定义文件加载到内存，并实例化相关的类
"""
import os
import logging
from xml.etree import ElementTree  as ET
from PyDatcomLab.Core.datcomDimension import Dimension

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
        self.basePath = os.path.join(os.path.expanduser('~'), '.PyDatcom', 'config', 'datcomDefine.xml')
        if not os.path.exists(path):
            path = self.basePath
        self.dicPath = path
        #定义一个字典存储所有的定义信息
        self.Dictionary = {}     #按照键值对方式存储所有的信息
        self.dictAddtional = {}  #以CARD/Namelist为键，保存附加定义信息 NMACHLinkTable  RuleNumToCount RuleIndexToCombo groupDefine
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
                
            else:
                self.logger.error("解析规则异常：%s"%(tNmlst + " " + iT.tag))
        
                
    def getNamelistDefineByName(self, nmlst):
        """
        返回nmlst定义的选项卡
        """
        if nmlst in self.Dictionary.keys():
            return self.Dictionary[nmlst]
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
        """
        tGroupList = []
        tCARD = self.getNamelistDefineByName(tNmlst)
        if tCARD is {}:return tGroupList
        
        #存在定义
        for itVar in tCARD.keys():
            if 'Group' in tCARD[itVar].keys():
                tGroupList.append(tCARD[itVar]['Group'])
        #返回结果
        
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
        tR = {'Dimension':'', 'Value':0 , 'Unit':'' }
        if nmlst in self.Dictionary.keys() and VarName in self.Dictionary[nmlst].keys()\
        and 'Dimension' in self.Dictionary[nmlst][VarName].keys():
            tR['Dimension'] = self.Dictionary[nmlst][VarName]['Dimension']
            if tR['Dimension'] in Dimension.keys():
                tR['Unit'] = Dimension[tR['Dimension']][0]
        return tR

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
#导出一个常量
defaultDatcomDefinition = DTdictionary("")
