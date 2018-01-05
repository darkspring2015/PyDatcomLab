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
from xml.etree import ElementTree  as ET

from PyDatcomLab.Core import datcomDefine as dF    


class projectManager(object):
    '''
    主要设计约束：
    1.项目管理器新建目录结构
    2.CASE Group中的CASE将被创建到一个INPUT文件中
    '''
    def __init__(self, mgrFile = None):
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
        
        if  os.path.isfile(mgrFile):
            tmpDoc = ET.parse(mgrFile)  #parse是从文件，类名将是字符串
            if tmpDoc.getroot().tag == 'datcomProjectManager':
                self.doc = tmpDoc.getroot()
                self.checkProjectExist()
                tl.logInfo(u'成功加载datcomProjectManager %s'%tmpDoc.tostring())
            else:
                tl.logError(u'加载datcomProjectManager失败 %s'%tmpDoc.tostring())
        


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
    
    
    def newProject(self, tParentDir , tProjectName, tAerocraftName, prjDescribe =u""):
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

#        with open(filePath, 'w') as f:
#            tXML.write(f, addindent = r'    ' ,
#                                      newl = '\n' ,encoding = 'utf-8' )
        #日志
        
    def removeProject(self, prjDir):
        """
        从磁盘删除一个工程
        """
        tl.logInfo('尝试从磁盘移除一个工程，将无法恢复： %s'%prjDir)
        rootDir = prjDir
        if os.path.isfile(prjDir):
            rootDir = os.path.dirname(prjDir)
            
        import shutil         
        if  os.path.exists(rootDir) :
            shutil.rmtree(rootDir)
    def deletePrject(self, prjName):
        """
        从管理器中移除工程，并不删除磁盘文件
        """
        


        
class dcProject(object):
    """
    Datcom Project的类，负责承载
    
    """
    def __init__(self, prjName=u'A Datcom project', 
                 prjDescribe =u'', tAerocraftName ='A earocraft'):
        """ 
        prjName        : 项目名称
        prjDescribe    : 项目描述 
        tAerocraftName ：飞行器的名称        
        """
        
        self.prjName = prjName
        self.prjDescribe = prjDescribe    
        self.aerocraftName = tAerocraftName
        self.uuid = str(uuid.uuid1())
        
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
    <aerocraftInfo>
        <aerocraftName>A aerocraft</aerocraftName>
        <configuration>常规布局</configuration>
        <NAMELIST alias="机体" name="BODY"/>
    </aerocraftInfo>
    <casegroup name="default group">
        <CASE Index ="1"/>
    </casegroup>
</datcomProject>
        """
        
        self.doc = ET.fromstring(self.prjTemplete)
        
        #创建项目名称 
        self.setNodeText(r'./projectInfo/projectName',     self.prjName)
        self.setNodeText(r'./projectInfo/projectDescribe', self.prjDescribe)
        self.setNodeText(r'./projectInfo/projectUUID',     self.uuid )
        self.setNodeText(r'./projectInfo/createTime', 
                         time.strftime('%Y-%m-%d %H:%M:%S',time.localtime(time.time())))
        self.setNodeText(r'./projectInfo/modifyTime', 
                         time.strftime('%Y-%m-%d %H:%M:%S',time.localtime(time.time())))
        #飞行器名称
        self.setNodeText(r'./aerocraftInfo/aerocraftName', self.aerocraftName)

    def loadProject(self, prjFile):
        """
        prjFile : 工程文件路径 
        """
        if  not os.path.exists(prjFile ) or not os.path.isfile(prjFile):
            tl.logError(u'输入路径错误 %s'%prjFile)
        
        tmpDoc = ET.parse(prjFile)  #parse是从文件，类名将是字符串
        if not tmpDoc.getroot().tag == 'datcomProject':
            tl.logError(u'错误的工程文件格式 %s'%tmpDoc.tostring())
            return False
        self.doc = tmpDoc.getroot()
        
        return True
            
        

    
    def setNodeText(self, xpath, text):
        """
        设置doc中xpath对应节点的text
        认为是叶节点
        xpath : r'./projectInfo/projectName'
        """
        nodes = self.doc.findall(xpath)
        if len(nodes) is not 1:
            tl.logError(u"查询%s 过程中出错！"%xpath)
            return False
        if len(nodes[0].getchildren()) is not 0 :
            tl.logError(u"查询%s ,不是叶节点！"%xpath)
            return False
        nodes[0].text = text
        
        return True
        
        
    

    def addTextNode(self, parentNode, name, text =u''):
        """
        创建一个Node ，用name和value
        并将这个Node添加到parentNode中
        返回创建的这个Node
        """
        if name is None or parentNode is None:
            tl.logError(
                u'尝试创建错误的节点 name %s ,parentNode %s'\
                %(name, parentNode))
            return None
            
        node = self.doc.SubElement(parentNode, name) 
        if not text is '':  node.text = text
        
        return node
        
    def addNode(self,  parentNode, name):
        """
        创建一个Node ，用name
        并将这个Node添加到parentNode中
        返回创建的这个Node
        """
        if name is None or parentNode is None:
            tl.logError(
                u'尝试创建错误的节点 name %s ,parentNode %s'%(name, parentNode))
            return None
        node = self.doc.SubElement(parentNode, name)  
        
        return node
    
    def addNodeWithAttribute(self, parentNode, name, attr ):
        """
        创建一个Node ，用name
        并将这个Node添加到parentNode中
        这个Node具有属性 attr ：一个dict，key是属性名，value是属性值        
        返回创建的这个Node
        """
        
        if name is None or parentNode is None:
            tl.logError(
                u'尝试创建错误的节点 name %s ,parentNode %s'%(name, parentNode))
            return None
        
        node = self.doc.SubElement(parentNode, name)    
        #设置属性值
        for atr in attr:
            node.set(atr, attr[atr])
            
        return node
        
    
    def getProjectInfo(self):
        """
        获得项目名称和项目描述 
        dict.keys = [
                "projectName"
                "projectDescribe"
                ]
        """
        dict = {}
        for info in self.doc.findall('projectInfo'):
            for tInfo in info:
                dict[tInfo.tag] = tInfo.text
                
        return dict
        
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
        
         
    def getNodeTextByXPath(self, xpath):
        """
        利用xml.etre.ElementTree 实现的xpath，当前仅支持部分功能
        假设读取最终节点的text值
        etc r'./aerocraftInfo/aerocraftName'
        """
        #转换当前doc       

        nodes = self.doc.getroot().findall(xpath)
        if len(nodes) is 0 : 
            tl.logError("不存在对应节点：%s" % xpath )
        return nodes[0].text
        
    def newCASE(self, dict =None):
        """
        新建一个CASE
        dict 提供基本的数据定义，定义如下
        >>>from PyDatcomLab.Core import datcomDefine as dF 
        >>>dict = dF.datcomDefine.dtNmlstExmple
        
        
        """
        
        #导入Datcom的定义
        from PyDatcomLab.Core import datcomDefine as dF        
        keyLst = dF.datcomDefine.reserved_NAMELISTS  #定义了所有Namelist信息的list
        
        
        #获得CASE定义的跟节点
        casegroup = self.getNodeTextByXPath(r'./datcomProject/casegroup')
        newCASEID = casegroup.findall('CASE').count + 1
        #添加一个CASE到CASE集合
        newCase = ET.SubElement(casegroup,"CASE", {'INDEX':newCASEID})
            
        if dict is None:            
            return
        
        #若干字典信息存在，则根据字典顺序下达信息
        for nmlst in dict.keys():
            if nmlst not in keyLst:
                continue
            #如果在，则创建响应的节点
            nmlstNode = ET.SubElement(newCase, nmlst)
            if dict[nmlst] is 'dict':
                for var in dict[nmlst].keys(): #添加遍历namelist下面的遍历
                    varNode = ET.SubElement(nmlstNode, var)
                    if len(dict[nmlst][var]) == 1: #如果变量时单值的直接添加
                        ET.SubElement(varNode, dict[nmlst][var])
                        continue
                    for aVal in dict[nmlst][var]: #如果遍历是多值的，遍历变量的每一个值
                        valNode = ET.SubElement(varNode, 'value')
                        valNode.text = str(aVal)
            else:
                self.logger.info("异常的字典结构")
   
    
    def setCASEModel(self, caseID=1, dict = None):
        """
        设置CASE的Model内容
        """
        if dict is None:
            return
        
        #获得对应的CASE
        
        root = self.doc
        #导入Datcom的定义
    



        
        

