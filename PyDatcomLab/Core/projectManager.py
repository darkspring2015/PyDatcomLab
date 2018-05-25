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
#import time  
#import uuid
#from xml.dom.minidom import Document  
from PyDatcomLab.Core import datcomTools as tl

from PyDatcomLab.Core.datcomTools import xml_Indent as indent
from PyDatcomLab.Core.dcProject import dcProject
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
        #日志系统        
        self.logger = logging.getLogger(r'Datcomlogger')
        #内部参数定义
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
        tProjectPath = os.path.join(baseDir, '%s.dcprj'%tProjectName)
        #创建项目
        prj = dcProject(tProjectPath)
        if prj is None: 
            tl.logError(u'创建项目失败 %s - %s -%s '%(
                            tProjectName, tAerocraftName,prjDescribe ))
            return None
        #配置数据
        tPrjInfo = {     'projectName':tProjectName, 
                         'projectDescribe':prjDescribe, 
                         #'projectDirectory':tProjectPath,                         
                    }
        prj.setProjectInfo(tPrjInfo)   
        # 将dom对象写入本地xml文件
   
        #tXML = ET.ElementTree(prj.doc).getroot()
        indent(prj.doc, 0)
        ET.ElementTree(prj.doc).write(tProjectPath ,encoding = 'utf-8',xml_declaration=True)  
        
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
        self.projectSet[tProjectPath] = prj
        
        return tProjectPath

  
    def addProject(self, tPath):
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

 
