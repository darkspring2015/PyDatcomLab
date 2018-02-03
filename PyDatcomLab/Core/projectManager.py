#
'''
项目管理器：用于创建/管理PyDatcomLab的各种项目，将项目串行化到磁盘，提供GUIs所需要的Model
主要功能包括：
1.创建项目目录结构
2.创建飞行器描述
3.创建执行算例
4.形成结果数据
主要说明：
1. 目录结构包括
'''

import os
import time  
import uuid
#from xml.dom.minidom import Document  
from PyDatcomLab.Core import tools as tl
from PyDatcomLab.Core.dcModel import  dcModel
from PyDatcomLab.Core.tools import xml_Indent as indent
from xml.etree import ElementTree  as ET

import logging


class projectManager(object):
    '''
    项目管理器被设计为后台存储所有数据的核心数据结构
    主要层级结构如下：
    projectManager  #顶级Solution prjMXML文件
        project 1  #系统运行的基本单位 dcProject承载
            目录结构
            case 1
                dcModel  #dcXML *.inp
                reslut1  #dcResXML *.out
            case 2
                dcModel
                reslut1        
        project 2
    '''
    def __init__(self, mgrFile=''):
        """
        初始化创建一个manager对象
        如果 mgrFile 存在，则从磁盘文件加载Manager配置
        """
        
        self.managerTemplate = """\
<?xml version="1.0" encoding="utf-8"?>
<datcomProjectManager>
    <managerInfo>
        <managerName>DefaultManager</managerName>    
    </managerInfo>
    <project>
        <projectName>某个项目 </projectName>
        <projectDescribe>项目样例</projectDescribe>
        <projectUUID> </projectUUID>
        <projectPath>.</projectPath>
        <canUse>False</canUse>
        <createTime/>
        <modifyTime/>
    </project>
</datcomProjectManager>
        """
        self.doc = ET.fromstring(self.managerTemplate)

        #
        if  os.path.isfile(mgrFile):
            tmpDoc = ET.parse(mgrFile)  #parse是从文件，类名将是字符串
            if tmpDoc.getroot().tag == 'datcomProjectManager':
                self.doc = tmpDoc.getroot()
                self.checkProjectExist()
                tl.logInfo(u'成功加载datcomProjectManager %s'%tmpDoc.tostring())
            else:
                tl.logError(u'加载datcomProjectManager失败 %s'%tmpDoc.tostring())
        
        self.projectSet = {}


    def checkProjectExist(self, autoRemove = False):
        """
        检查Manager中的project是否存在与磁盘
        autoRemove True自动移除不存在的实例
        """
        for prjNode in self.doc.findall('project'):
            tPath   = prjNode.find('projectPath').text
            tPjName =  prjNode.find('projectName').text
            tPrjFile = os.path.join(tPath, tPjName +'.dcprj')
            if os.path.exists(tPath) and os.path.isfile(tPrjFile):
                prjNode.find('canUse').text = r'True'
            else :
                prjNode.find('canUse').text = r'False'
                if autoRemove:
                    self.doc.remove(prjNode) #移除对应的节点
    
    
    def CreateProject(self, tParentDir , tProjectName, tAerocraftName, prjDescribe =u""):
        '''
        创建一个新工程
        '''
        #校验
        if not os.path.exists(tParentDir) or tProjectName is None :
            tl.logError(u'错误！制定参数错误 dir:%s ,name:%s'%(tParentDir, tProjectName))
            return 
        
        #开始创建项目结构的逻辑
        baseDir = os.path.join(os.path.abspath(tParentDir), tProjectName)
        try :
            os.mkdir(baseDir)
            os.makedirs(os.path.join(baseDir, r'3DModels')) #存放3D模型所需信息的目录
            os.makedirs(os.path.join(baseDir, r'Reports'))  #存放结果文件的目录
            os.makedirs(os.path.join(baseDir, r'Problems')) #存放算例的目录
            os.makedirs(os.path.join(baseDir, r'Problems', r'defaultGroup')) #第一个算例组
            
        except IOError:
            tl.logError(u'无法创建项目目录！dir:%s ,name:%s'%(tParentDir, tProjectName))
        
        #创建基础的工程文件和目录
        
        #创建项目
        prj = dcProject(prjName=tProjectName, 
                 prjDescribe = prjDescribe, 
                 tAerocraftName = tAerocraftName)
        if prj is None: 
            tl.logError(u'创建项目失败 %s - %s -%s '%(
                            tProjectName, tAerocraftName,prjDescribe ))
            return None
            
            
        # 将dom对象写入本地xml文件
        filePath = os.path.join(baseDir, tProjectName+'.dcprj')
        tXML = ET.ElementTree(prj.doc)
        #ET.register_namespace('datcom', "https://github.com/darkspring2015/PyDatcomLab/")
        tXML.write(filePath ,encoding = 'utf-8',xml_declaration=True)  
        
        # #追加到项目列表
        prjInfo = prj.getProjectInfo()
        node = ET.SubElement(self.doc, 'project')        
        #循环拷贝project info 到相关节点
        for key in prjInfo:
            ET.SubElement(node, key).text = prjInfo[key]
        ET.SubElement(node, 'projectPath').text = baseDir
        ET.SubElement(node, 'canUse').text = r'True'
        
        tl.logInfo(u'创建项目工程文件 %s - %s'%(tProjectName, tAerocraftName))
        #添加到当前的管理器
        self.projectSet[filePath] = prj
        
        return filePath

  
    def AddProject(self, tPath):
        """
        将tPath指定的项目添加到项目管理器
        """
        aPj = dcProject()
        if not aPj.loadProject(tPath):
            tl.logInfo(u'加载项目工程文件失败 %s'%tPath)
            return 
        
        self.projectSet[tPath] = aPj
        tl.logInfo(u'添加项目到管理器 %s'%tPath)
        
        
        
    def deletePrject(self, tPath):
        """
        从管理器中移除工程，并不删除磁盘文件
        """ 
        if not os.path.isfile(tPath):
            tl.logInfo('请输入合法的项目文件， %s'%tPath)
            return 
        self.projectSet.pop(tPath)
        

        
    def removeProjectFromDisk(self, tPath):
        """
        从磁盘删除一个工程
        """
        
        if not os.path.exists(tPath):
            tl.logInfo('无法删除不存在的工程： %s'%tPath)
            return 
        
        #判断删除的内容  
        rootDir = tPath
        if os.path.isfile(tPath):
            rootDir = os.path.dirname(tPath)
            
        import shutil         
        if os.path.exists(rootDir) :
            shutil.rmtree(rootDir)
            tl.logInfo('从磁盘删除：%s  工程！'%tPath)

        


        
class dcProject(object):
    """
    Datcom Project的类，负责承载项目信息    
    """
    
    def __init__(self, prjName=u'A Datcom project', prjDescribe =u''):
        """ 
        prjName        : 项目名称
        prjDescribe    : 项目描述        
        """
        #日志系统        
        self.logger = logging.getLogger(r'Datcomlogger')
        #内部变量
        self.prjName = prjName
        self.prjDescribe = prjDescribe   
        self.uuid = str(uuid.uuid1())
        #self.model = dcModel() 
        
        self.prjTemplete = """\
<?xml version="1.0" encoding="utf-8"?>
<datcomProject>
    <projectInfo>
        <projectName>A Datcom project</projectName>
        <projectDescribe> </projectDescribe>
        <projectUUID> </projectUUID>
        <createTime/>
        <modifyTime/>
    </projectInfo>
    <casegroup GroupName="default group">
    </casegroup>
</datcomProject>
        """
        
        self.doc = ET.fromstring(self.prjTemplete)     #返回的是根root元素    
        #创建项目名称 
        self.setNodeText(r'./projectInfo/projectName',     self.prjName)
        self.setNodeText(r'./projectInfo/projectDescribe', self.prjDescribe)
        self.setNodeText(r'./projectInfo/projectUUID',     self.uuid )
        self.setNodeText(r'./projectInfo/createTime', 
                         time.strftime('%Y-%m-%d %H:%M:%S',time.localtime(time.time())))
        self.setNodeText(r'./projectInfo/modifyTime', 
                         time.strftime('%Y-%m-%d %H:%M:%S',time.localtime(time.time())))


    def loadProject(self, prjFile):
        """
        从prjFile中加载项目
        prjFile : 工程文件路径 
        """
        if  not os.path.exists(prjFile ) or not os.path.isfile(prjFile):
            self.logger.error(u'输入路径错误 %s 不是有效的路径！'%prjFile)
            return
        
        tmpDoc = ET.parse(prjFile)  #parse是从文件，类名将是字符串
        if not tmpDoc.getroot().tag == 'datcomProject':
            self.logger.error(u'错误的工程文件格式 %s'%tmpDoc.tostring())
            return 
        self.doc = tmpDoc.getroot()
        
    def saveProject(self,prjFile ):
        """
        将文件保存到prjFile中
        """
        try:
            root = self.doc
            indent(root, 0)        
            ET.ElementTree(root).write(prjFile, encoding="UTF-8" )
        except IOError as e:
            self.logger.error("保存文件失败！%s,异常信息%s"%(prjFile, e))
    
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
        
        
    def addCase(self, tModel,tCASEName,  tGroup = 'default group' ):
        """
        添加一个CASE/模型到项目中,
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
        #开始复制过程
        reXML = dcModel.getDocRootElement()
        if reXML is None:
            self.logger.error(u"获得模型节点失败！")
            return 
 
        #添加
        #nodes[-1].append(reXML)
        tUUID = str(uuid.uuid1())
        if tCASEName is None or tCASEName =='':tCASEName = tUUID
        tCaseElem = ET.SubElement(nodes[-1], 'CASE', {'CASEName':tCASEName,'UUID':tUUID})
        tCaseElem.append(reXML)
        
    
    
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
        if 'projectName' in info:
            self.setNodeText(r'/datcomProject/projectInfo/projectName', info['projectName'])
        if 'projectDescribe' in info :
            self.setNodeText(r'/datcomProject/projectInfo/projectDescribe', info['projectDescribe'])
 
    
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
        
        
    def newCASE(self, CASEName, groupName , aerocraftName = "", aerocraftDes = ""):
        """
        新建一个CASE ,即dcModel模型一发
        """

        #导入Datcom的定义CASEName,  tGroup
        self.addCase(dcModel(aerocraftName, aerocraftDes),CASEName,  groupName )
    

        
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
    
    
        
    



        
        

