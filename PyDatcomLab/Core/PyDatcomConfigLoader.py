#!/usr/bin/env python
   
        
speedRegime = ['Subsoinc', 'Transoinc', 'Supersonic', 'Hypersonic']

#定义datcom中变量应当具有的属性字段
datcomVarAttriList = {
    'VarName':'',       #Datcom变量名
    'NameList':'',      #Datcom NAMELIST
    'DisplayName':'',   #显示名
    'Tooltips':'',      #提示信息
    'TYPE':'REAL',      #变量类型  ['INT','REAL','Array','List']
    'SubType':'',       #变量子类型 对于['INT','REAL']为空,对于['Array','List']指定元素的类型 ['REAL','STR',]
    'Group':'',         #分组名   对于['INT','REAL','List']对应Group控件，对于'Array'对应table控件
    'Range':'',         #取值范围 对于['INT','REAL','Array'] 限制为 min,max，对于['List']为所有可能的值
    'DisplayRange':'' , #取值范围的显示值 对['List']有效，为所有可能显示值
    'Default':'',       #默认值   对于['INT','REAL','Array']为item的默认值，对于['List']为默认索引
    'Limit':'',         #数量     对于['INT','REAL','List']为空，对 ['Array']为min，max
    'Dimension':'',     #量纲     ['L','DEG',...] in Dimension.keys()
    'MustInput':'' ,    #是否是必须输入的量 ['MustInput','UserCheck','HasDefault'],如果为UserCheck应该创建checkbox
    'Relation':'',      #其他关联信息 rule规则暂定
              }
####################################
##
####################################

from xml.etree import ElementTree  as ET
from PyDatcomLab.Core import datcomTools as dtTool
import  os,  logging, uuid

class PyDatcomLabConfig():
    """
    datcomConfig用于读取PyDatcomLab的配置文件，xml格式
    使用说明：
    1.该类型派生自datcomXMLLoader类，需要自定义解析关系，请使用xml文档约定来实现函数ParseXmltoDoc
    
    """

    
    def __init__(self, iPath = None ):
        """
        初始化模型类，读取path指定的配置文件
        1. iPath 必须是有效的配置文件     
        2. 默认的存储路径：r'~\.PyDatcomLab\config\PyDatcomLabConfig.xml'
        """
        #初始化日志系统
        self.logger = logging.getLogger(r'Datcomlogger')    
        #使用空初始化以避免加载,调用空创建将会创建xmlDoc的根数据
        #super(PyDatcomLabConfig, self).__init__()  
        #定义各种属
        self.Tag      = 'PyDatcomLabConfig'   #覆盖定义    
        self.Properties = {                           
                           'Describe':'1',
                           'CreateTime':dtTool.getNowTime(), 
                           'ModifyTime':dtTool.getNowTime(), 
                           'modelPath':iPath, 
                           }
        self.exts = {'ALL':'*.*', 'XML':'.xml'}

        #定义XML结构
        self.configPathDefault = os.path.join(os.path.expanduser('~'), '.PyDatcomLab', 'config', 'PyDatcomLabConfig.xml')
        #决定还是去掉UUID这个选项 ，作为一个文件追踪程序，不能增加过度的附加条件，而是应该将Element类型
        self.libraryDefine = {
        'ModelLibrary':[{'MainTag':'Model', 'SubTag':['ModelName', 'path', ] , 'Path':'path', 'RootTag':'CASE'}],  #决定还是去掉UUID这个选项
        'ProjectLibrary':[{'MainTag':'Project','SubTag':['ProjectFile', 'ProjectDirectory', ] , 'Path':'ProjectFile', 'RootTag':'DatcomProject'}], 
        'ConfigurationType':[{'MainTag':'Configuration','SubTag':['CName', 'DisplayName', 'Description', 'Namelist'] }], 
        }
        #执行path的分析
        self.xmlDoc  = self.createXMLDocBasic()
        if iPath is None :
            if os.path.isfile(self.configPathDefault ):
                iPath = self.configPathDefault 
            else:
                if os.path.exists(os.path.join(os.path.expanduser('~'), '.PyDatcomLab', 'config')):
                    try:
                        self.xmlDoc  = self.createXMLDocBasic()
                        self.Properties.update({'modelPath':self.configPathDefault})
                        self.save(self.configPathDefault)
                        iPath= self.configPathDefault 
                    except Exception as e:
                        self.logger.error("加载算例文件:%s 出现异常:%s"%(iPath,repr(e)))
                else:
                    self.logger.error("没有存储路径")
        if os.path.isfile(iPath):
            #尝试加载文件并提示错误异常信息
            try:
                self.load(iPath)
                self.Properties.update({'modelPath':iPath})
            except Exception as e:
                self.logger.error("加载算例文件:%s 出现异常:%s"%(iPath,repr(e)))

    def createXMLDocBasic(self):
        """
        创建一个基本的doc，类型是ET
        基础空白的模型为了可用默认添加了所有的变量和结果
        """
        #配置subNode
        #创建唯一的序列号
        self.Properties.update({'UUID':str(uuid.uuid1())})
        #创建根节点
        tRoot = ET.Element(self.Tag, self.Properties)   
        #添加 ModelLibrary
        for iL in self.libraryDefine.keys() :
            if tRoot.find(iL) is None:
                ET.SubElement(tRoot, iL)            
        #写入一些默认值
        
#        tCE = tRoot.find('ConfigurationType')
#        if tCE is not None:
#            ET.SubElement(tCE, 'Configuration').text = '常规布局'
#            ET.SubElement(tCE, 'Configuration').text = '双垂尾布局'
#            ET.SubElement(tCE, 'Configuration').text = '高超声速布局'
            
        return tRoot

    def load(self, iPath):
        """
        加载XML文件,只提示异常不处理
        """
        root = None
        try:
            root = ET.parse(iPath).getroot()
        except Exception as e:
            #self.logger.error("加载模型文件过程中发生异常，%s ：%s"%(repr(e), str(e)))
            raise(dtTool.dtIOException('加载文件异常',"加载模型文件过程中发生异常，%s ：%s"%(repr(e), str(e))))
            
        if root is None:return 
        #根据节点设置对象信息    
        self.Properties['modelPath'] = iPath
        #解析信息
        self.xmlDoc = root

    def save(self):
        """
        按照当前文件路径保存更改
        """
        tCPath = None
        if os.path.exists( self.Properties['modelPath']):
            tCPath = self.Properties['modelPath']
        elif os.path.exists(self.configPathDefault):
            tCPath =self.configPathDefault        
        try:
            self.saveAs( tCPath)
        except Exception as e:
            self.logger.error("PyDatcomLabConfig.save 出错，无法保存到：%s"%tCPath)     
            
            
    def saveAs(self, iPath ):
        """
        将结果另存到文件
        """
        
        if self.xmlDoc  is  None :
            self.xmlDoc = self.createXMLDocBasic()
            
        root = self.xmlDoc        
        dtTool.xml_Indent(root, 0)   
        try:
            root.attrib['modelPath'] = iPath
            root.attrib['ModifyTime'] = dtTool.getNowTime()
            ET.ElementTree(root).write(iPath, encoding="UTF-8" )
        except Exception as e:
            tError = "写入XML文件：%s 失败！Message ：%s"%(iPath, repr(e))
            self.logger.error(tError)
            #raise(dtTool.dtIOException('文件写入失败',tError))       


    def getLibrary(self, iLibraryName):
        """
        获得iLibraryName对应的Library的内容，返回值为List
        """
        #如果当前没有ModelLibrary节点，则创建节点，并save
        tLibNode = self._getLibraryElement(iLibraryName)
        tModels = []  #返回值
        #获得迭代定义
        tMainTag    = self.libraryDefine[iLibraryName][0]['MainTag']
        tSubTaglist = self.libraryDefine[iLibraryName][0]['SubTag']
        #循环读取所有节点
        for iME in list(tLibNode):
            if iME.tag == tMainTag:
                #检查数据有效性,如果没有数据则跳过
                if len(tSubTaglist)  == 0:
                    tModels.append(iME.text)
                    continue
                #当有SubNode时
                tMEdict = {}
                #遍历所有子节点
                for tSE in list(iME):
                    if tSE.tag in tSubTaglist:
                        tMEdict[tSE.tag ] = tSE.text
                    else:
                        pass
                #检查读取到的子节点数
                if len(tMEdict) != len(tSubTaglist):
                    self.logger.error("getLibrary() XML文档未正确包含libraryDefine定义的所有子节点")
                    continue    
                #写入结果到
                tModels.append(tMEdict)
#                # 检查  UUID   
#                if 'Path' in self.libraryDefine[iLibraryName][0].keys():
#                    tPathKey = self.libraryDefine[iLibraryName][0]['Path']            
#                else:
#                    tPathKey = None
#                if tPathKey is not None  and 'UUID' in tSubTaglist:
#                    try:
#                        tSubME = iME.findall(tPathKey)
#                        if tSubME is not None:                            
#                            #如果存在pathkey指定的节点
#                            tRoot = ET.parse(tSubME.text).getroot()
#                            if 'UUID' in tRoot.attrib.keys():               
#                                tMEdict['UUID'] = tRoot.attrib['UUID']
#                            else:
#                                tMEdict['UUID'] = str(uuid.uuid1()) #全新生成一个UUID
#                                self.logger.warnning('datcom.getLibrary 期望输入文件的根节点有一个UUID，自动创建一个UUID给该文件')
#                    except Exception as e:
#                        self.logger.error('datcom.getLibrary 尝试读取配置文件时出错:%s!'%repr(e))
                #写入结果到
                #tModels[tMEdict['UUID'] ] = tMEdict

        #返回所有的Model
        return tModels
        
    def setLibrary(self,iLibraryName,  iCollection):
        """
        将模型数据集合iCollection写入到数据库iLibraryName
        注意，该命令将重置所有的模型库信息
        @para iCollection 
        @type dict

        """
        #清空模型库
        self.clearLibrary(iLibraryName)        
        for iM in iCollection:
            self.addItemToLibrary(iLibraryName, iM)
        #执行写入                
        self.save()
        
    def  addItemToLibrary(self,iLibraryName, iItem ):
        """
        将iItem添加到iLibraryName对应的库
        返回值决定是否 ['成功添加','不合规未添加','存在未添加']  
        """
        tResult = '成功添加'
        tLibNode     = self._getLibraryElement(iLibraryName)
        tMainTag    = self.libraryDefine[iLibraryName][0]['MainTag']
        #检查合规
        if self.checkInfo(iLibraryName, iItem) :
            #查询当前的库，如果不存在相同项，则添加
            if self._findItemInLibrary(iLibraryName, iItem ) is None:            
                tME = ET.SubElement(tLibNode, tMainTag)
                for iE in iItem.keys():
                    ET.SubElement(tME, iE).text = iItem[iE]       
                self.save()
            else:
                self.logger.warning('datcom.addItemToLibrary 待添加的项已经存在，忽略添加!')
                tResult = '存在未添加'
        else:
            tResult = '不合规未添加'
            self.logger.error('datcom.addItemToLibrary 的输入iItem不合规!')
        
        return tResult
    
    def _findItemInLibrary(self,iLibraryName, iItem  ):
        """
        在库iLibraryName中查找与iItem内容一致的SubElement ，仅返回第一个ET.SubElement对象
        没有找到返回None
        """
        tLibNode    = self._getLibraryElement(iLibraryName)
        tMainTag    = self.libraryDefine[iLibraryName][0]['MainTag']      
        #查询删除UUID一致的数据
        for iE in list(tLibNode):
            if iE.tag == tMainTag:
                tSame = True
                tSubEDict = {}
                for iSubE in list(iE):
                    tSubEDict[iSubE.tag] = iSubE.text
                for iKeys in iItem.keys():
                    if not (iKeys in tSubEDict.keys() and tSubEDict[iKeys] == iItem[iKeys]):
                        tSame = False
                #判断结果
                if tSame: 
                    return iE
        #如果没有找到返回None            
        return None
                    
        
            
    def  delItemFromLibrary(self,iLibraryName, iItem):
        """
        将iItem对应的元素从iLibraryName对应的库中删除，通过比对所有的SubElement的{tag:值}实现
        iItem @TYPE dict
        """        
        tNode = self._findItemInLibrary(iLibraryName, iItem  )
        if tNode is not None:
            tLibNode    = self._getLibraryElement(iLibraryName)
            tLibNode.remove(tNode)
            #保证操作及时的刷新到硬盘
            self.save()
 
    def  delItemFromLibraryByUUID(self,iLibraryName, iItemUUID):
        """
        将iItemUUID对应的玄素从iLibraryName对应的库中删除
        """
        tLibNode    = self._getLibraryElement(iLibraryName)
        tMainTag    = self.libraryDefine[iLibraryName][0]['MainTag']  
        tXPath       =     r".//UUID/..[@name='%s']"%(tMainTag)
        #查询删除UUID一致的数据
        for iE in tLibNode.findall(tXPath):
            if iE['UUID'].text== iItemUUID:
                tLibNode.remove(iE)
                
        #保证操作及时的刷新到硬盘
        self.save()

    def checkInfo(self,iLibraryName,  iModeldict):
        """
        检查输入的模型信息是否合规
        """
        tResult = True
        #保证所有的要求的属性都在列表中
        tSubTaglist = self.libraryDefine[iLibraryName][0]['SubTag']      
        for iK in tSubTaglist:
            if iK not in iModeldict.keys():
                return False
        #如果定义了文件路径属性，则校验相关的信息
        if 'Path' in self.libraryDefine[iLibraryName][0].keys():
            #加载对应的文件
            tPathKey = self.libraryDefine[iLibraryName][0]['Path']      
            tRoot = None            
            try:
                tRoot = ET.parse(iModeldict[tPathKey]).getroot()               
            except Exception as e:
                self.logger.error('PyDatcomLabConfig.checkInfo 尝试读取配置文件时出错:%s!'%(e))
            if tRoot is None:
                return False   
            #检查UUID属性
            if 'UUID' in iModeldict.keys():
                if 'UUID' not in tRoot.attrib.keys() or tRoot.attrib['UUID'] != iModeldict['UUID']:      
                    self.logger.error('datcom.checkInfo 比对文件UUID属性出错！')         
                    tResult = False   
  
            #检查文件的Root元素的Tag，决定是否是要加载的文件类型
            if 'RootTag'  in self.libraryDefine[iLibraryName][0].keys() :
                tRTag = self.getLibraryRootTag(iLibraryName)
                if tRoot.tag  != tRTag:      
                    self.logger.error('datcom.checkInfo 比对XML文件根节点Tag出错，期望：%s，实际：%s!'%(tRTag,  tRoot.tag))         
                    tResult = False                    
   
        #最终返回 tResult
        return tResult
        
    def clearLibrary(self, iLibraryName):
        """
        清空库iLibraryName的所有的信息，但是不会删除实体文件
        """
        tLibNode = self._getLibraryElement(iLibraryName)
        for iSubE in list(tLibNode):
            tLibNode.remove(iSubE)    
            
    def _getLibraryElement(self, iLibraryName):
        """
        返回存储iLibraryName的ET元素
        """
        tLibNode = self.xmlDoc.find(iLibraryName)
        if tLibNode is None:
            tLibNode = ET.SubElement(self.xmlDoc, iLibraryName)
        return tLibNode      
  

    def getLibraryElementTemplate(self, iLibraryName):
        """
        获得iLibraryName库对应的元素的基础模板,内容值默认为空字符串（XML兼容）
        """
        tSubTaglist = self.libraryDefine[iLibraryName][0]['SubTag']   
        tTemplate = {}
        if   tSubTaglist is not None:
            for iT in tSubTaglist:
                tTemplate[iT] = ''
        else:
            self.logger.warning("getLibraryElementTemplate()尝试查询不存在的库定义：%s"%iLibraryName)
            
        return tTemplate
                

    def getConfigurationList(self):
        """
        返回所有的Configuration的列表
        """
        return self.getLibrary('ConfigurationType')
        
    def getPathKeyByLibraryName(self, iLibraryName):
        """
        获得iLibraryName指定的库的文件路径属性的key，key保存在SubTag中
        如果定义了Path属性，则返回对应的SubTag，否则返回None        
        """
        if 'Path' in self.dtConfig[self.libraryKeyWord][0].keys():
            return self.libraryDefine[iLibraryName][0]['Path']  
        else:
            return None
            
    def getLibraryRootTag(self, iLibraryName):
        """
        获得库iLibraryName对应XML文件的的根的Tag
        不存在RootTag返回None
        """
        return self.libraryDefine[iLibraryName][0].get('RootTag', None)
        
defaultConfig = PyDatcomLabConfig(os.path.join(os.path.expanduser('~'), '.PyDatcomLab', 'config', 'PyDatcomLabConfig.xml'))

if __name__=="__main__":
    """
    测试PyDatcomLabConfig
    """
    sPath  = os.path.join(os.path.expanduser('~'), '.PyDatcomLab', 'config', 'PyDatcomLabConfig.xml')
    cPath  =  os.path.join(os.path.expanduser('~'), '.PyDatcomLab', 'extras', 'PyDatcomProjects','1','case2.xml')
    aLoader = PyDatcomLabConfig()
    aLoader.addItemToLibrary('ModelLibrary', {'ModelName':'CASE1', 'path':cPath})
    tModel = aLoader.getLibrary('ModelLibrary')
    tModel.append({'ModelName': 'CASE2', 'path': 'C:\\Users\\linger\\.PyDatcomLab\\extras\\PyDatcomProjects\\1\\case2.xml'})
    aLoader.setLibrary('ModelLibrary', tModel)
    aLoader.delItemFromLibrary('ModelLibrary', {'ModelName': 'CASE2', 'path': 'C:\\Users\\linger\\.PyDatcomLab\\extras\\PyDatcomProjects\\1\\case2.xml'})
    aLoader.save()


