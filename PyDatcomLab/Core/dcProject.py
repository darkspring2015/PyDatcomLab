# -*- coding: utf-8 -*-

"""
Module implementing dcProject.
"""

import os , time, uuid, logging
from xml.etree import ElementTree  as ET
from PyDatcomLab.Core.datcomModel import  dcModel
from PyDatcomLab.Core.datcomTools import xml_Indent as indent


class dcProject(object):
    """
    Datcom Project的类，负责承载项目信息    
    """
    
    def __init__(self, tProjectPath ):
        """ 
        tProjectPath        : 项目文件路径     
        """
        #日志系统        
        self.logger = logging.getLogger(r'Datcomlogger')
        #内部变量
        self.prjTemplete = """\
<?xml version="1.0" encoding="utf-8"?>
<datcomProject>
    <projectInfo>
        <projectName>A Datcom project</projectName>
        <projectDescribe> </projectDescribe>
        <projectUUID> </projectUUID>
        <projectDirectory/>
        <createTime/>
        <modifyTime/>
    </projectInfo>
    <casegroup GroupName="default group">
    </casegroup>
</datcomProject>
        """
        self.uuid = str(uuid.uuid1())
        self.prjName = self.uuid
        self.prjDescribe = ''   
        self.prjPath = ''
        self.doc = ET.fromstring(self.prjTemplete)     #返回的是根root元素   
        #分析项目是否存在
        if not self.loadProject(tProjectPath):
            if not self.createProject(tProjectPath):
                self.logger.error("项目既不是有效的项目文件，也无法创建对应的项目。路径：%s"%tProjectPath)

    def loadProject(self, prjFile):
        """
        从prjFile中加载项目
        prjFile : 工程文件路径 
        """        
        
        #尝试加载对应项目文件，以XML方式
        try: 
            tmpDoc = ET.parse(prjFile)  #parse是从文件，类名将是字符串
        except Exception as e:
            self.logger.warning(u'加载项目文件出错：%s！'%(e))
            return False
        #处理信息
        if not tmpDoc.getroot().tag == 'datcomProject':
            self.logger.error(u'错误的工程文件格式 %s'%tmpDoc.getroot().tag )
            return False
        #配置信息
        self.doc = tmpDoc.getroot()   
        #创建修改项目信息 
        prjInfo = self.getProjectInfo()
        self.prjName     = prjInfo['projectName']
        self.prjDescribe = prjInfo['projectDescribe'] 
        self.uuid        = prjInfo['projectUUID'] 
        self.prjPath     = prjFile
        return True

   
                         
    def createProject(self, tPrjFile, tPrjName = 'SomeDatcomPrj',tPrjDescribe = '' ):
        """
        在路径tPojectPath下创建项目结构
        """
        if tPrjFile is None:
            self.logger.error("createProject() 输入无效！%s"%tPrjFile)
            return False
        if os.path.isfile(tPrjFile):
            self.logger.error("createProject()已经存在对应的项目文件！%s"%tPrjFile)
            return False
        #分析内容
        tDir = os.path.dirname(os.path.realpath(tPrjFile))
        tFn, tExt = os.path.splitext(os.path.basename(tPrjFile))
        #创建目录
        if not os.path.exists(tDir):
            try:
                os.makedirs(tDir)
                os.mkdir(os.path.join(tDir, 'defaultGroup'))
            except Exception as e:
                self.logger.error(u'%s 过程触发异常!\n ErrorType： %s\n%s'%(
                            '创建项目目录', repr(e), str(e)))
                return False 
        
        #创建文件
        #open(tPrjFile,"w+").close()  
        tDoc = ET.fromstring(self.prjTemplete)     #返回的是根root元素           
        self.setNodeText(r'./projectInfo/projectName',     tPrjName)
        self.setNodeText(r'./projectInfo/projectDescribe', tPrjDescribe)
        self.setNodeText(r'./projectInfo/projectUUID',     str(uuid.uuid1()) )
        self.setNodeText(r'./projectInfo/createTime', 
                         time.strftime('%Y-%m-%d %H:%M:%S',time.localtime(time.time())))
        self.setNodeText(r'./projectInfo/modifyTime', 
                         time.strftime('%Y-%m-%d %H:%M:%S',time.localtime(time.time())))
        self.setNodeText(r'./projectInfo/projectDirectory',tDir)
        indent(tDoc, 0) 
        try:
            ET.ElementTree(tDoc).write(tPrjFile, encoding="UTF-8" )           
        except Exception as e:
            self.logger.error(u'%s 过程触发异常!\n ErrorType： %s\n%s'%(
                        '创建项目文件', repr(e), str(e)))
            
            
        
    def deletePrject(self):
        """
        移除项目文件和所在目录
        """
        pass
    def saveProject(self):
        """
        将项目信息刷新到项目文件中
        """
        tDoc = ET.ElementTree(self.doc).getroot()
        indent(tDoc, 0) 
        ET.ElementTree(tDoc).write(self.prjPath, encoding="UTF-8" )  
        
    def saveAsProject(self,prjFile ):
        """
        将文件保存到prjFile中
        """
        try:
            root = self.doc
            indent(root, 0)        
            ET.ElementTree(root).write(prjFile, encoding="UTF-8" )
        except IOError as e:
            self.logger.error("保存文件失败！%s,异常信息%s"%(prjFile, repr(e)+e.message))
    
    def setNodeText(self, tXPath, text):
        """
        设置doc中xpath对应节点的text
        认为是叶节点
        xpath : r'./projectInfo/projectName'
        """
        nodes = self.doc.findall(tXPath)
        
        if nodes is None or len(nodes) <= 0:
            self.logger.error("不存在对应节点：%s" % tXPath )
            return 
        if len(nodes) is not 1:
            self.logger.info(u"查询%s 过程中出错，存在%d个节点！"%(tXPath, len(nodes)))
            
        if len(nodes[0].getchildren()) is not 0 :
            self.logger.info(u"查询%s ,不是叶节点！"%tXPath)          
        nodes[0].text = text
        
    def getDoc(self):
        """
        返回对应的doc ET.Element
        """    
        return self.doc 
     
    
    
    def getProjectInfo(self):
        """
        获得项目名称和项目描述 
        dict.keys = [
                "projectName"
                "projectDescribe"
                ]
        """
        tDict = {}
        for info in self.doc.findall('projectInfo'):
            for tInfo in info:
                tDict[tInfo.tag] = tInfo.text
                
        return tDict
        
    def setProjectInfo(self, info):
        """
        设置项目信息 : 
        info = {
                "projectName":'new project Name', 
                "projectDescribe":'new project describe'
        }
        """
        for iAttr in info.keys():
            if iAttr == 'createTime':
                continue
            self.setNodeText(r'./projectInfo/'+ iAttr, info[iAttr])

 
    
    def getAerocraftInfo(self):
        """
        获得飞行器的描述信息 
        info.keys =[
        'name','configuration']
        
        """
        dict = {}
        for info in self.doc.findall('aerocraftInfo'):
            for tInfo in info:
                dict[tInfo.tag] = tInfo.text 
        
        return dict
        
         
    def getNodeTextByXPath(self, tXPath):
        """
        利用xml.etre.ElementTree 实现的xpath，当前仅支持部分功能
        假设读取最终节点的text值
        etc r'./aerocraftInfo/aerocraftName'
        本方法返回所有的Node中的最后一个text值
        """
        #转换当前doc   
        nodes = self.getNodesByXPath(tXPath)    
        if nodes:
            return nodes[-1].text
        else:
            return None


    def getNodesByXPath(self, tXPath):
        """
        返回tXPath指定的所有Nodes
        """
        nodes = self.doc.findall(tXPath)
        if nodes is None or len(nodes) <= 0:
            self.logger.error("不存在对应节点：%s" % tXPath )
            return None
            
        return nodes
        
    def addCaseModel(self, tModel,tCASEName,  tGroup = 'default group' ):
        """
        添加一个CASE/模型到项目中,只添加对应
        @param tModel dcModel类型的模型
        @type dcModel
        """
        #参数检查
        if tModel is None or type(tModel) != dcModel:
            self.logger.error(u"输入参数tModel无效：%s ！"%type(tModel))
            return
        #获得对应的Group节点
        if tGroup is None or tGroup == '':
            tGroup = 'default group' 
        tXPath = r"./casegroup[@GroupName='%s']"%tGroup
        nodes = self.getNodesByXPath(tXPath)
        if not nodes : return 
        #
        #开始复制过程
        reXML = dcModel.getDocRootElement()
        if reXML is None:
            self.logger.error(u"获得模型节点失败！")
            return 
 
        #添加
        #nodes[-1].append(reXML)
        tUUID = str(uuid.uuid1())
        if tCASEName is None or tCASEName =='':tCASEName = tUUID
        tCaseElem = ET.SubElement(nodes[-1], 'CASE', 
        {'CASEName':tCASEName,'UUID':tUUID, 'Path':tModel.modelPath})
        tCaseElem.append(reXML) 
    def addCase(self, tCasepath, tCASEGroup):    
        """
        将tCasepath添加到tCASEGroup
        """   
        
    def newCASE(self, CASEName, groupName , aerocraftName = "", aerocraftDes = ""):
        """
        新建一个CASE ,即dcModel模型一发
        """

        #导入Datcom的定义CASEName,  tGroup
        self.addCase(dcModel(aerocraftName, aerocraftDes),CASEName,  groupName )
    def reloadCASE(self, CASEName, groupName):
        """
        重新装载
        """
    def removeCASE(self, CASEName, groupName):
        """
        从项目中删除对应的CASE
        但不从磁盘删除CASE对应的文件
        """
        
    def addGroup(self, tGroupName):
        """
        创建名为 %tGroupName的新计算组
        """
        if tGroupName is None or tGroupName =='':
            tGroupName = str(uuid.uuid1())
        else:
            tXPath = "./casegroup[@GroupName='%s']"%tGroupName
            nodes = self.getNodesByXPath(tXPath)
            if nodes:
                self.logger.error("对应的分组已经存在！")
                return
        ET.SubElement(self.doc, 'casegroup', {'GroupName':tGroupName})
    
