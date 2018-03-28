# -*- coding: utf-8 -*-

# datcom 计算实例的doc类
#
# @editer Linger
#
# 

from xml.etree import ElementTree  as ET
from PyDatcomLab.Core.DictionaryLoader import  defaultDatcomDefinition as DtDefine , datcomConstraint
from PyDatcomLab.Core.datcomXMLLoader import datcomXMLLoader
from PyDatcomLab.Core import datcomTools as dtTools
from PyDatcomLab.Core import datcomDimension as dtDimension

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
        super(dcModel, self).__init__()  #使用空初始化以避免加载
        #初始化日志系统
        self.logger = logging.getLogger(r'Datcomlogger')
        #定义各种属性
        self.dtDefine = dtDefine
        self.Properties.update({'CASEID':'1',  
                                'Describe':'2', 
                                'AerocraftName':'AerocraftName', 
                                'Configuration':'wing-body' , 
                                'numForE':'100'
                                })  
        self.Tag      = 'CASE'   #覆盖定义         
        #self.numForE  = 100      #使用科学计数法的值
        #定义XML结构
        self.additionalDoc = {}              #存储datcom配置之外的信息          
        #执行path的分析
        if path is None :
            self.__createBasicdoc()
        elif os.path.isfile(path):
            #尝试加载文件并提示错误异常信息
            try:
                self.load(path)
            except Exception as e:
                self.logger.error("加载算例文件:%s 出现异常:%s"%(path, e.message))

        
    def __createBasicdoc(self):
        """
        创建一个基本的doc，类型是ET
        基础空白的模型为了可用默认添加了所有的变量和结果
        """
        #添加最低配置的CASE
        #FLTCON        

        for iN in self.dtDefine.getNamelistCollection():
            self.addNamelist(iN)
        #调用验证器修正错误，并给所有量赋初值
        self.validate()
    
        
        
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
                    if iNode.text is not None :
                        tValue = eval(iNode.text)
                except Exception as e:
                    self.logger.error("解析值结构异常！%s"%(str(e)))  
                finally:
                    tSet['Value'] = tValue
                #写入结果
                self.doc[tUrl] = tSet
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
            tValue = None if 'Value' not  in tVar.keys() else tVar['Value']
            tVar.pop('Value')
            if 'Edited' in tVar.keys() : tVar.pop('Edited') #不保存对应的字段
            tElem = ET.SubElement(tRoot, 'VARIABLE', tVar)
            if tValue is not None:
                tElem.text = str(tValue)
            
        return tRoot
    
 
    
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
        

        if variables is None or len(variables)  == 0:
            tNLDf = self.dtDefine.getNamelistDefineByName(namelist)
            variables = list(tNLDf.keys())
        
        for iV in variables:
            tUrl = '%s/%s'%(namelist, iV)
            tVtp = self.dtDefine.getVariableTemplateByUrl(tUrl)
            if tVtp is None:
                self.logger.error("无法创建对应的变量的基本实例%s"%(tUrl))
            self.doc[tUrl] = tVtp
       
        
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
            if type(tVar['Value']) != int: 
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
            if type(tVar['Value']) != list:
                tLog.append('变量的类型应当为Array实际是%s'%(type(tVar['Value']))) 
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
                    self.logger.error("变量%s不合规，%s"%(tUrl,tVarReport['Report'] ))
                    #尝试修复或者跳过
                    tNMExcept[tUrl] = tVarReport['Report']
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
            self.logger.error("模型内并没有信息")
            return 
        #开始内容分析机制
        tReport = self.validate()    
        if tReport['status'] != 'Acceptable':
            self.logger.info(str(tReport))
            return 
        #获得doc的全部定义
        tAllInfo = self.getNamelistCollection()
        if tAllInfo is None or len(tAllInfo) == 0 :
            return tReport
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
            lastCheck = 0
            for itVar in tAllInfo[itr]:#循环Var []
                lastCheck +=1
                theStr = '%s'%itVar 
                tURl = '%s/%s'%(itr, itVar)
                tVarStruct = self.getVariableByUrl(tURl)
                tVarValueS = tVarStruct['Value']
                if tVarValueS is None :
                    self.logger.error("创建Datcom计算配置文件失败，不能为空值创建结果！%s/%s"%(itr,itVar ))
                    continue
                    #raise(Exception("创建Datcom计算配置文件失败，不能为空值创建结果！%s/%s"%(itr,itVar ))) 
                tVarDf = self.dtDefine.getVariableDefineByUrl(tURl)
                if  tVarDf['TYPE'] == 'Array': #对于序列类型
                    if 'SIndex' in tVarStruct.keys():
                        theStr += '(%s)='%(tVarStruct['SIndex'])
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
                    if lastCheck < len(tAllInfo.keys()):
                        TStr.append(' '*tNMlstPos) #防止个数变量出现在序列变量之后
                elif tVarDf['TYPE'] == 'List' :#对于1.0 2.0 或者.TRUE.等字符型 
                    theStr += '=%s,'%tVarValueS
                elif tVarDf['TYPE'] is 'INT' :#对于数值类型  INT  
                    theStr += '=%d.0,'%int(tVarValueS)
                elif tVarDf['TYPE'] == 'REAL' :#对于数值类型 REAL                
                    if float(tVarValueS) < float(self.Properties['numForE']):
                    #if float(tVarValueS) < self.numForE:
                        theStr += '=%.3f,'%float(tVarValueS)
                    else:
                        theStr += '=%.3E,'%float(tVarValueS)
                self.Append80ColumsLimit(TStr, theStr, tNMlstPos)
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
        函数从当前模型中获得变量
        """
        if tUrl is None or tUrl == "" or self.doc is None:
            return None
        if tUrl in self.doc.keys():
            return self.doc[tUrl]
        

    
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
            if type(tVar) is str:
                tVarIn['Value'] = tVar
            elif type(tVar) is dict :
                self.doc[tUrl].update(tVar)
            else:
                self.logger.error("无法处理的类型信息%s"%(str()))            

    
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
                self.doc[tUrl].update(varValue)
            else:
                self.logger.error("无法处理的类型信息%s"%(str()))   


        
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

    sPath  = r'E:\Projects\PyDatcomLab\extras\PyDatcomProjects\1\case2.xml'
    obPath = r'E:\Projects\PyDatcomLab\extras\PyDatcomProjects\1\case3.xml'
    dtPath = r'E:\Projects\PyDatcomLab\extras\PyDatcomProjects\1\case3.inp'
    try:
        aLoader = dcModel(sPath)
        #aLoader = dcModel()
        aLoader.save(obPath)
        aLoader.buildDatcomInputFile(dtPath)
    except Exception as e:
        print(repr(e))
