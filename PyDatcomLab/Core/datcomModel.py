# -*- coding: utf-8 -*-

# datcom 计算实例的doc类
#
# @editer Linger
#
# 

from xml.etree import ElementTree  as ET
from PyDatcomLab.Core.DictionaryLoader import  defaultDatcomDefinition as DtDefine , datcomConstraint
from PyDatcomLab.Core.datcomXMLLoader import datcomXMLLoader

import  os
import logging

class dcModel(datcomXMLLoader):
    """
    dcModel 定义用于一次计算的完整配置
    """
    def __init__(self, path = None, dtDefine = DtDefine):
        """
        初始化
        """
        super(dcModel, self).__init__()
        #初始化日志系统
        self.logger = logging.getLogger(r'Datcomlogger')
        #定义各种属性
        self.dtDefine = dtDefine
        self.Properties.update({'CASEID':1,  
                                'Describe':'2', 
                                'AerocraftName':"", 
                                'Configuration':"" , })  
        
         
        #定义XML结构
        self.additionalDoc = {}              #存储datcom配置之外的信息          
        #执行path的分析
        if path is not  None and   os.path.isfile(path):
            #尝试加载文件并提示错误异常信息
            try:
                self.loadCASE(path)
            except Exception as e:
                self.logger.error("加载算例文件:%s 出现异常:%s"%(path, e.message))

        
    def __createxmldoc(self):
        """
        创建一个基本的doc，类型是ET
        """
        tRoot = ET.Element('CASE', self.Properties)
        #添加最低配置的CASE
        #FLTCON
        
        self.addNamelist('FLTCON')
        self.addNamelist('SYNTHS')
        self.addNamelist('BODY')
        self.addNamelist('WGPLNF')
        self.addNamelist('WGSCHR')
        
        
        return tRoot
        
    def loadCASE(self, tFile):
        """
        从文件path加载数据 
        path str<br />.dcXML,.xml类型<br />
        加载出现错误将引发异常
        """

        try:
            self.load(tFile)
        except Exception as e:
            self.logger.error("加载模型文件过程中发生异常，%s ：%s"%(repr(e), str(e)))
            
        if self.xmlDoc is None:return False
        #分析XML文件
        if not self.xmlDoc.tag == 'CASE': 
            self.logger.error("加载XML文件：%s失败，其中不包含CASE节"%tFile)
            return False
        #根据节点设置对象信息    
        self.Properties['modelPath'] = tFile
        #解析信息
        self.__analysisXML(self.xmlDoc)
        return True
 
    
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
#            if tNamelist not in self.dtDefine.getNamelistCollection():
#                self.logger.error("解析的Namelsit结果异常")
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
        
        for iV in variables:
            tVtp = self.dtDefine.getVariableTemplateByUrl('%s/%s'%(namelist, iV))
            if tVtp is None:
                self.logger.error("无法创建对应的变量的基本实例%s-%s"%(namelist, iV))
       
        
    def deleteNamelist(self, namelist):
        """
        从实例移除一个选项卡
        namelist str：选项卡的名称
        Notice 不承诺数据的完整性
        """
        pass
        
    def setVariable(self, variable):
        """
        设置实例的某一个变量的值
        variable是Python的dict类型，包括：{url，unit，value}
        对于不存在的namelist，将导致创建对应选项卡的操作；<br />对于存在的变量，将修改值和单位等信息<br />如果变量的值为None，将从集合中删除该变量
        """
        #进行必要的单位变换
        #写入数据到doc中
        pass
        
    def validate(self):
        """
        验证当前的配置是否是一个可以执行的配置，并返回报告信息
        返回值为当前配置的错误报告
        """
        tReport = {'status':'OK', 'Report':''}
        
        return tReport
        
    def buildDatcomInputFile(self, path):
        """
        根据当前的CASE配置创建datcom的计算文件
        过程出错将引发异常
        """
        pass
        
        
    def __analysisXML(self, tXML):
        """
        从XML ET.Element中解析出文档结构
        tXML 根节点
        Notice: 将情况自身的数据
        """
        if tXML is None: return
        #检查根节点属性是否齐全
        tNeededProperties = ['CASEID','Describe', 'AerocraftName', 'Configuration' , 'CreateTime', 'ModifyTime']
        tPNothased = []
        for iP in tNeededProperties:
            if iP not  in tXML.attrib.keys():
                tPNothased.append(iP)
        if len(tPNothased)>0:
            self.logger.error("节点属性结构异常，缺少信息%s"%(str(tPNothased)))
        #属性节点赋值
        self.Properties = tXML.attrib

        #清理自身数据
        self.doc = {}
        self.additionalDoc = {}
        
        #读取实例
        for iNode in list(tXML):
            #执行非Namelist节的解析
            if iNode.tag == 'Variable':
                tSet = iNode.attrib  #获得所有属性
                tUrl = tSet.get('Url', None)
                if tUrl is None or tUrl == '':
                    self.logger.error("信息结构异常！%s"%(str(tSet))) 
                tValue = None
                try:
                    tValue = eval(iNode.text)
                except Exception as e:
                    self.logger.error("解析值结构异常！%s"%(str(e)))  
                finally:
                    tSet['Value'] = tValue
                #写入结果
                self.doc[tUrl] = tSet
            else:
                tDoc = self.__recursiveAnalyseElement(iNode, {})
                if iNode.tag in self.additionalDoc.keys():
                    #暂时不考虑实现                    
                    self.additionalDoc[iNode.tag].append(tDoc) 
                else:
                    self.additionalDoc[iNode.tag] = [tDoc]

        #解析结束        

                
    def __recursiveAnalyseElement(self, tElem, tDoc):
        """
        递归执行解析，将xml转换为树形的dict结构
        """
        pass
        


    
    def getDiscreteVariableValueByName(self, nmlst, varName):
        """
        函数从当前模型中获得变量的值，对应离散量 List
        """        
        if nmlst in dF.reserved_NAMELISTS and nmlst  in self.doc.keys() and \
            varName in self.doc[nmlst].keys():
            return self.doc[nmlst][varName]  
        return None     
    
    def getContinuousVariableValueByName(self, nmlst, varName):
        """
        函数从当前模型中获得变量的值/单位/量纲，对应连续量 INT REAL
        """
        #获得对应变量的最基本的定义 {'Dimension':'', 'Value':None , 'Unit':'' }
        tRes = DDefine.getVariableDimensionByName(nmlst,varName )
        #判断是否存在
        if nmlst in dF.reserved_NAMELISTS and nmlst  in self.doc.keys() and \
            varName in self.doc[nmlst].keys():
            tRes['Value'] = self.doc[nmlst][varName]

        return tRes     
        
    def setDiscreteVariableValueByName(self, nmlst, varName, varValue):
        """
        写入离散量的值，写入信息Discrete的量 对应List类型
        varValue 直接就是对应的结果
        """        
        self.setNamelist(nmlst, varName, varValue)
    
    def setContinuousVariableValueByName(self, nmlst, varName, varValue):
        """
        写入连续量的值，写入信息Continuous的量 对应Int Real类型
        """
        if varValue['Value'] is None:
            self.setNamelist(nmlst, varName, None)
        #执行量纲规约
        tValue = varValue['Value']
        #写入到结果        
        self.setNamelist(nmlst, varName, tValue)


        
    def writeToDatcomInput(self, tPath):
        """
        将当前配置写入到一个输出文件中
        """
        if len(self.doc ) == 0:
            self.logger.error("模型内并没有信息")
            return 
        #开始内容分析机制,        
        #将信息格式化为输出文件 要求列宽为80

        TStr = []
        tCASEDes = ''
        tCASEID =1
        for itr in self.doc.keys():#循环Namelist
            #跳过CASEID的描述
            if itr == "CASEID":
                tCASEDes = self.doc[itr]['CASEID']
                tCASEID  = self.doc[itr]['CASE']
                continue    
            #添加一个数据行，记录            
            TStr.append('')  
            theStr = " $%s "%itr
            self.Append80ColumsLimit(TStr, theStr)
            tNMlstPos = len(TStr[-1]) #记录Namelist变量的位置
            tNmlst = self.doc[itr]
            lastCheck = 0
            for itVar in tNmlst.keys():#循环Var
                lastCheck +=1
                theStr = '%s'%itVar 
                if type(tNmlst[itVar]) == dict: #是Array
                    tVars     = tNmlst[itVar]['Value']
                    tVarIndex = tNmlst[itVar]['Index']
                    theStr += '(%s)='%tVarIndex
                    if len(TStr[-1])> tNMlstPos: #当当前行是非空行时换行增加序列值
                        TStr.append(' '*tNMlstPos) 
                    self.Append80ColumsLimit(TStr, theStr)
                    tNewPos = len(TStr[-1]) #记录当前变量的位置
                    for itValue in tVars: #循环追加所有的数据                        
                        if itValue < self.numForE :
                            theStr = '%.3f,'%itValue
                        else:
                            theStr = '%.3E,'%itValue
                        self.Append80ColumsLimit(TStr, theStr, tNewPos)
                    if lastCheck < len(tNmlst.keys()):
                        TStr.append(' '*tNMlstPos) #防止个数变量出现在序列变量之后
                else :
                    if type(tNmlst[itVar]) is str : #对于1.0 2.0 或者.TRUE.等字符型
                        theStr += '=%s,'%tNmlst[itVar]
                    else: #对于数值类型 
                        if float(tNmlst[itVar]) < self.numForE:
                            theStr += '=%.3f,'%float(tNmlst[itVar])
                        else:
                            theStr += '=%.3E,'%float(tNmlst[itVar])
                    self.Append80ColumsLimit(TStr, theStr, tNMlstPos)
            TStr[-1] = TStr[-1][:-1] + '$'            
      

        #写入CASE ID
        TStr.append('CASEID %s,CASE %s'%(tCASEDes, str(tCASEID)) )       
        #写入SAVE
        TStr.append('SAVE')
        TStr.append('DUMP CASE' )      
        TStr.append('NEXT CASE' )   
        
        LastResult = '\n'.join(TStr)
        with open(tPath,"w") as f:
            f.write(LastResult)
            
        return LastResult
        

        
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
            


            

if __name__=="__main__":
    """
    """

    sPath  = r'E:\Projects\PyDatcomLab\extras\PyDatcomProjects\1\datcomDefine2.dcxml'
    obPath = r'E:\Projects\PyDatcomLab\extras\PyDatcomProjects\1\datcomDefine3.dcxml'
    try:
        aLoader = dcModel(sPath)
        aLoader.save(obPath)
    except Exception as e:
        print(repr(e))
