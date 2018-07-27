# -*- coding: utf-8 -*-

"""
Module implementing ModelPreview.
"""
from PyQt5 import QtWidgets #, QtCore, QtGui
from PyQt5.QtCore    import pyqtSlot, QPoint, Qt #,QModelIndex# , QFile, QIODevice
from PyQt5.QtWidgets import QWidget, QTreeWidgetItem, QFileDialog, QMessageBox, QCheckBox, QHeaderView
#from PyQt5.QtXml import QDomDocument
from PyQt5.QtGui     import QIcon

import logging, os
#import time
from xml.etree import ElementTree  as ET
from PyDatcomLab.Core.datcomModel import dcModel 
from PyDatcomLab.Core.PyDatcomConfigLoader import defaultConfig as dtConfig
#from PyDatcomLab.Core.DictionaryLoader import  defaultDatcomDefinition as DDefine
from PyDatcomLab.Core.DictionaryLoader import   DTdictionary 


from .Ui_ModelPreview import Ui_ModelPreview


class ModelPreview(QWidget, Ui_ModelPreview):
    """
    模型预览窗口，以Tree样式展示所有的Datcom模型配置信息.
    """
    def __init__(self, parent=None, iDefine = DTdictionary.defaultConfig, iConfig = dtConfig ):
        """
        Constructor
        
        @param parent reference to the parent widget
        @type QWidget
        @param iDefine reference to the DatcomDefine
        @type DTdictionary
        @param iConfig reference to the PyDatcomLabConfig
        @type PyDatcomLabConfig
        """
        super(ModelPreview, self).__init__(parent)
        self.setupUi(self)
        #日志系统        
        self.logger = logging.getLogger(r'Datcomlogger')
        #内部变量
        #加载datcom的配置文件
        if iConfig is None: 
            self.dtConfig = dtConfig
        else:
            self.dtConfig = iConfig     
        if iDefine is None: 
            self.dtDefine = DTdictionary.defaultConfig
        else:
            self.dtDefine = iDefine    
        self.libraryKeyWord = 'ProjectLibrary'
        self.rootTag  = self.dtConfig.getLibraryRootTag(self.libraryKeyWord)  #获取库文件的根节点的tag
        self.MEKeys  = list(dtConfig.getLibraryElementTemplate(self.libraryKeyWord ))
        self.configurationList = self.dtConfig.getConfigurationList()
        #配置信息
        self.ext = '.dcxml'        
        self.ModelName = "test"
        self.ModelDir = '.'
        self.Modelpath = os.path.join(self.ModelDir , self.ModelName +  self.ext)
        self.Model = dcModel()
        #配置UI
        self.treeWidget_model.setColumnCount(2)
        self.treeWidget_model.setHeaderLabels(['参数', '值'])
        self.treeWidget_model.header().setSectionResizeMode(QHeaderView.ResizeToContents )
        self.treeWidget_model.header().setStretchLastSection( True)
        self.treeWidget_model.setUniformRowHeights(True) #极大加速模型的加载过程
#        self.treeWidget_model.setItemsExpandable(True)
#        self.treeWidget_model.expandAll()
        self.isPreArray = False # True:'整理Array',False:'不整理Array'
        #self.isPreArray = True # True:'整理Array',False:'不整理Array'
        self.itemFlags = Qt.ItemIsEnabled | Qt.ItemIsSelectable | Qt.ItemIsEditable\
                        |Qt.ItemIsAutoTristate|Qt.ItemIsUserCheckable #|Qt.ItemIsUserTristate
        #绑定内容菜单
        #界面参数
        self.curPos = QPoint(0, 0)
        self.curWidget = None
        self.popMenu = None
        self._createCustomContextMenu()
        self.treeWidget_model.setContextMenuPolicy(Qt.CustomContextMenu)   

        #其他状态变量
        self.isCopy     = False             #表示本对话框是否是Copy目的
        self.isDcModel = True         #表示本对话加载的是否是Datcom模型
        

        
    @pyqtSlot(QPoint)
    def on_treeView_model_customContextMenuRequested(self, pos):
        """
        Slot 右键菜单的实现.
        
        @param pos 菜单位置
        @type QPoint
        """
        self.curPos = pos
        self.curWidget = self.treeWidget_model        
        posG = self.curWidget.mapToGlobal(pos)
        #执行一些状态检查
        self.popMenu.exec(posG)
    
    def _createCustomContextMenu(self):
        """
        创建用于树的内容菜单
        """
        self.popMenu = QtWidgets.QMenu(self.curWidget)
#        self.popMenu.addAction(self.actionNewModel)
#        self.popMenu.addAction(self.actionAddModel)
#        self.popMenu.addAction(self.actionRemoveModel)
#        self.popMenu.addAction(self.actionPreviewModel)
        
        
    def listDom(self, docElem, pItem):
        """ 
        遍历添加到docElem到pItem节点
        """
        #添加本节点和属性
        if pItem:
            tItem = QTreeWidgetItem(pItem)
        else:
            tItem = QTreeWidgetItem(self.treeWidget_model)
            tItem.setIcon(0, QIcon(":/img/images/airplaneUp.png"))
        #写入当前节点的名称
        tItem.setText(0, docElem.tag)        
        tItem.setCheckState(0, Qt.Checked)
        tItem.setFlags(self.itemFlags )
        
        #根据节点类型设置图标                    
        if 'dcType' in docElem.attrib:
           if  docElem.attrib['dcType']=='NAMELIST':
                tItem.setIcon(0, QIcon(":/img/images/content.png"))
        #判断是否是叶节点            
        if len(docElem.getchildren()) == 0:
            tItem.setText(1, docElem.text) 
            tItem.setIcon(0, QIcon(":/img/images/variable.png"))
       
        #写入节点的属性信息
        if  docElem.attrib :
            tAttrItem = QTreeWidgetItem(tItem)
            tAttrItem.setText(0, '属性')
            tAttrItem.setIcon(0, QIcon(":/img/images/type.png"))
            tAttrItem.setCheckState(0, Qt.Checked)
            tAttrItem.setFlags(self.itemFlags )
            for iAttr in docElem.attrib:
                subAttr = QTreeWidgetItem(tAttrItem)
                subAttr.setText(0, iAttr)
                subAttr.setText(1, docElem.attrib[iAttr]) 
                subAttr.setIcon(0, QIcon(":/img/images/type.png"))
                #subAttr.setFlags(self.itemFlags )
                subAttr.setCheckState(0, Qt.Checked)
            
        #判断是否有子节点
        if self.isPreArray and docElem.findall('value'):
            tValueItem = QTreeWidgetItem(tItem)
            #tValueItem.setFlags(self.itemFlags )
            tValueItem.setText(0, 'value')
            vList =[]
            for iVal in docElem.findall('value'):
                vList.append(iVal.text)
            tValueItem.setText(1, ',  '.join(vList))
            tValueItem.setIcon(0, QIcon(":/img/images/variable.png"))
            tValueItem.setCheckState(0, Qt.Checked)
            
        else:
            for iChild in docElem.getchildren():            
                self.listDom(iChild, tItem)            
    
    #END listDom
        
    def openXML(self, fileName)  :  
        """
        打开XML文件
        """
        root = ET.parse(fileName).getroot()
        if root is None:return
        #清理控件
        self.treeWidget_model.clear()
        
        #递归添加所有元素
        self.listDom(root, None)
        
        #展开控件
        self.treeWidget_model.expandToDepth(2)

        
    
    def loadModel(self, tFile):
        """
        从tFile指定的dcxml文件中加载模型
        """
        if not os.path.exists(tFile):
            self.logger.error("tFile：%s,不存在"%tFile)
            return
        #加载
        self.Model = dcModel()
        self.Model.load(tFile)
        self.lineEdit_ModelName.setText(self.Model.Properties['CName'])
        self.lineEdit_DirPath.setText(tFile)
        self.currentModelPath = tFile
        #开始界面逻辑刷新过程
        for iC in self.dtDefine.getNamelistCollection():
            tWidget = self.findChild(QCheckBox, 'checkBox_%s'%iC)
            if tWidget is None:
                self.logger.error("CARD:%s并不在Datcom的Namelist中"%iC)
                continue
            else:
                if iC in self.Model.getNamelistCollection().keys():
                    tWidget.setCheckState(Qt.Checked)
                else:
                    tWidget.setCheckState(Qt.Unchecked)
        #开始加载详细逻辑
        self.openXML( tFile) 
    
    @pyqtSlot()
    def on_pushButton_ChoiseDir_clicked(self):
        """
        选择模型文件并重新夹杂
        
        """
        #fN = QFileDialog.getExistingDirectory(self, "创建模型文件", "", QFileDialog.DontUseNativeDialog)
        tPath = self.lineEdit_DirPath.text()
        if not os.path.isfile(tPath):
            tPath =""
        fN , fType = QFileDialog.getOpenFileName(self, "模型文件路径",os.path.dirname(tPath),
                            "Datcom Model Files (*.dcxml *.xml )" )
        if not os.path.exists (fN):
            self.logger.error("目录：%s 不存在！"%fN)
            return
        #配置信息
        self.ModelDir = os.path.dirname(fN)
        self.lineEdit_DirPath.setText(fN)
        #加载模型
        self.loadModel(fN)
    
    @pyqtSlot()
    def on_pushButton_New_clicked(self):
        """
        此处保存修改过的模型信息到文件
        """
        if self.lineEdit_ModelName.text() == "":
            QMessageBox.information(self, '请指定模型名称', '模型名称不能为空')
            return
        self.Model.Properties.update({'AerocraftName':self.lineEdit_ModelName.text() })
        self.Model.Properties.update({'Configuration':self.comboBox_template.currentText()  })
        #输处目录不能为空
        if self.lineEdit_DirPath.text() == '':
            QMessageBox.information(self, '警告', '模型的目录不能为空')
            return
        #目标文件不存在
        if not os.path.exists(self.lineEdit_DirPath.text() ) : 
            fN, fType = QFileDialog.getSaveFileName(self, '选择新的文件名',self.ModelDir, 
                               "Datcom Model Files (*.dcxml *.xml )" )
            self.lineEdit_DirPath.setText(fN)
        tObPath = self.lineEdit_DirPath.text()
        self.ModelDir = os.path.dirname(tObPath) 
        if not os.path.isfile(tObPath):
            self.Modelpath = os.path.join(self.ModelDir, self.ModelName+self.ext )   
        else:
            self.Modelpath = tObPath        

        #获得配置
        for iC in self.dtDefine.getNamelistCollection():
            tWidget = self.findChild(QCheckBox, 'checkBox_%s'%iC)
            if tWidget and tWidget.checkState() == Qt.Checked:
                self.Model.addNamelist(iC)
            else:
                self.Model.deleteNamelist(iC)    
        #遍历XML文档Tree将修改进行刷新到模型
        resXMl = self.recursiveTreeToXML(self.treeWidget_model.topLevelItem(0), None)
        #判断是否需要刷新到dcModel
        if not self.isDcModel: #加载的不是dcModel对应XML则直接保存
            from PyDatcomLab.Core.tools import xml_Indent as indent
            indent(resXMl, 0)
            ET.ElementTree(resXMl).write(self.Modelpath, encoding="UTF-8" )
            QMessageBox.information(self,'修改模型文件', '已经将修改写入到模型文件：%s'%self.Modelpath)
        else:            
            self.Model.doc = {}
            self.Model.SetDocByXML(resXMl)  
            tObjpath =  self.Modelpath
            #判断是否需要另存
            if self.isCopy :
                tObjpath, fType = QFileDialog.getSaveFileName(self, '选择新的文件名',self.ModelDir,
                                                "Datcom Model Files (*.dcxml *.xml )" )
            #写入到xml文件    
            self.Model.writeToXML(tObjpath)   
            QMessageBox.information(self,'修改模型文件', '已经将修改写入到模型文件：%s'%tObjpath)
        
        
    def recursiveTreeToXML(self, item, etElem):
        """
        通过遍历的方法将QTreeWidget中的信息抽取到XML中
        @param item 待分析的Tree节点
        @type QTreeWidgetItem
        
        @param etElem 父节点XML
        @type ET.ETElement
        """
        if item is None:
            self.logger.error("输入的Item无效！")
            return None
        if etElem is None:
            etElem = ET.Element(item.text(0))
            
        #处理当前树节点的信息        
        #if item.text(0) == 'AerocraftInfo':
        if self.treeWidget_model.itemAbove(item) is None: #判断其为根节点
            #这是根节点
            for iD in range(0, item.childCount()):
                if item.child(iD).checkState(0) != Qt.Unchecked:
                    self.recursiveTreeToXML(item.child(iD), etElem)
            return etElem
            
        #进入非根节点处理过程
        #处理属性节点        
        if item.text(0) == "属性":
            for iD in range(0, item.childCount()):
                if item.child(iD).checkState(0) == Qt.Unchecked:
                    continue
                else:
                    etElem.attrib[item.child(iD).text(0)] = item.child(iD).text(1)
        #遍历所有子节点
        else:
            if item.childCount() <= 0: #没有子节点的情况 value
                #y叶节点
                if item.checkState(0) != Qt.Unchecked:
                    if item.text(0) == 'value' and self.isPreArray: #进行了压缩显示的情况
                        #该节点为值节点
                        for iVar in item.text(1).split(','):
                            aValue = ET.SubElement(etElem, 'value')
                            aValue.text = iVar.strip()
                    elif item.text(0) == 'value' and not self.isPreArray: #未进行压缩存储
                        aValue = ET.SubElement(etElem, 'value')
                        aValue.text = item.text(1).strip()
                    else : #
                        aElem = ET.SubElement(etElem, item.text(0))
                        aElem.text = '' if item.text(1) is None else item.text(1)
        
            elif item.childCount() > 0:
                #处理非属性节点
                if item.checkState(0) != Qt.Unchecked:
                    subElem = ET.SubElement(etElem,item.text(0) )
                    if item.text(1)!= "":
                        subElem.text = item.text(1).strip()
                    for iD in range(0, item.childCount()):
                        self.recursiveTreeToXML(item.child(iD), subElem)
                        
        return  etElem
    
    @pyqtSlot(int)
    def on_comboBox_template_currentIndexChanged(self, index):
        """
        Slot documentation goes here.
        
        @param index DESCRIPTION
        @type int
        """
        
        tKey = '%d.0'%(index+1)
        tList = []
        if tKey in self.configurationList.keys():
            tList = self.configurationList[index]['Namelist']
        else:
            self.logger.error('尝试的Index：%s并不存在对应的基础定义'%tKey)
            return
        #遍历所有的选项卡执行
        for iC in self.dtDefine.getNamelistCollection():
            #执行筛选逻辑
            tWidget = self.findChild(QCheckBox, 'checkBox_%s'%iC)
            if not tWidget: 
                self.logger.error('并不存在对应的CARD复选框：%s'%iC)
                continue
            else:
                #根据DcModel的内容执行选项卡的刷新逻辑
                if iC in tList:
                    if self.isDcModel:
                        #如果是Url版的dcModel
                        tCARDItemS = self.treeWidget_model.findItems(iC, Qt.MatchFixedString|Qt.MatchRecursive, 1)
                        for iI in tCARDItemS:
                            tNode = iI.parent()
                            if tNode is not None and tNode.checkState(0) != Qt.Checked:
                                tNode.setCheckState(0, Qt.Checked)                        
                    else:                        
                        tNodes = self.treeWidget_model.findItems(iC, Qt.MatchFixedString|Qt.MatchRecursive, 0)
                        if len(tNodes) == 0 :
                            tWidget.setCheckState(Qt.Unchecked)
                        else:
                            tWidget.setCheckState(Qt.Checked)
                    tWidget.setCheckState(Qt.Checked)
                else:
                    #不在列别
                    if self.isDcModel:
                        #如果是Url版的dcModel
                        tCARDItemS = self.treeWidget_model.findItems(iC, Qt.MatchFixedString|Qt.MatchRecursive, 1)
                        for iI in tCARDItemS:
                            tNode = iI.parent()
                            if tNode is not None and tNode.checkState(0) != Qt.Unchecked:
                                tNode.setCheckState(0, Qt.Unchecked)                        
                    else:                        
                        tNodes = self.treeWidget_model.findItems(iC, Qt.MatchFixedString|Qt.MatchRecursive, 0)
                        for iI in tNodes:
                            tNode.setCheckState(0, Qt.Unchecked) 
                    tWidget.setCheckState(Qt.Unchecked)

                
    def ModelNodeCheckstateChanged(self, cardName, state):
        """
        响应两侧的逻辑交换.
        @param cardName Datcom CARD的名称
        @type str
        
        @param state 复选状态 Qt::Unchecked	0	Qt::PartiallyChecked	1	Qt::Checked	2	
        @type int
        """
        #修改对应的checkbox对象
        tWidget = self.findChild(QCheckBox, 'checkBox_%s'%cardName)
        if tWidget:
            if state in [Qt.Checked, Qt.PartiallyChecked]:
                tWidget.setCheckState(Qt.Checked)
            else:
                tWidget.setCheckState(Qt.Unchecked)
        else:
            self.logger.error('不存在%s对应的Checkbox对象'%cardName)
        #修改对应的Node
        if self.isDcModel:
            #如果是Url版的dcModel
            tCARDItemS = self.treeWidget_model.findItems(cardName, Qt.MatchFixedString|Qt.MatchRecursive, 1)
            for iI in tCARDItemS:
                tNode = iI.parent()
                if tNode is not None and tNode.checkState(0) != state:
                    tNode.setCheckState(0, state)
        else:
            #如果是其他标准dcModel
            for tNode in self.treeWidget_model.findItems(cardName, Qt.MatchFixedString|Qt.MatchRecursive, 0):
                if tNode.checkState(0) != state :
                    tNode.setCheckState(0, state)
                    

    @pyqtSlot(int)
    def on_checkBox_FLTCON_stateChanged(self, p0):
        """
        Slot documentation goes here.
        
        @param p0 DESCRIPTION
        @type int
        """
        self.ModelNodeCheckstateChanged('FLTCON', p0)

    
    @pyqtSlot(int)
    def on_checkBox_OPTINS_stateChanged(self, p0):
        """
        Slot documentation goes here.
        
        @param p0 DESCRIPTION
        @type int
        """
        self.ModelNodeCheckstateChanged('OPTINS', p0)
    
    @pyqtSlot(int)
    def on_checkBox_SYNTHS_stateChanged(self, p0):
        """
        Slot documentation goes here.
        
        @param p0 DESCRIPTION
        @type int
        """
        self.ModelNodeCheckstateChanged('SYNTHS', p0)
    
    @pyqtSlot(int)
    def on_checkBox_BODY_stateChanged(self, p0):
        """
        Slot documentation goes here.
        
        @param p0 DESCRIPTION
        @type int
        """
        self.ModelNodeCheckstateChanged('BODY', p0)
    
    @pyqtSlot(int)
    def on_checkBox_WGPLNF_stateChanged(self, p0):
        """
        Slot documentation goes here.
        
        @param p0 DESCRIPTION
        @type int
        """
        self.ModelNodeCheckstateChanged('WGPLNF', p0)
    
    @pyqtSlot(int)
    def on_checkBox_WGSCHR_stateChanged(self, p0):
        """
        Slot documentation goes here.
        
        @param p0 DESCRIPTION
        @type int
        """
        self.ModelNodeCheckstateChanged('WGSCHR', p0)
    
    @pyqtSlot(int)
    def on_checkBox_HTPLNF_stateChanged(self, p0):
        """
        Slot documentation goes here.
        
        @param p0 DESCRIPTION
        @type int
        """
        self.ModelNodeCheckstateChanged('HTPLNF', p0)
    
    @pyqtSlot(int)
    def on_checkBox_HTSCHR_stateChanged(self, p0):
        """
        Slot documentation goes here.
        
        @param p0 DESCRIPTION
        @type int
        """
        self.ModelNodeCheckstateChanged('HTSCHR', p0)
    
    @pyqtSlot(int)
    def on_checkBox_VTPLNF_stateChanged(self, p0):
        """
        Slot documentation goes here.
        
        @param p0 DESCRIPTION
        @type int
        """
        self.ModelNodeCheckstateChanged('VTPLNF', p0)
    
    @pyqtSlot(int)
    def on_checkBox_VTSCHR_stateChanged(self, p0):
        """
        Slot documentation goes here.
        
        @param p0 DESCRIPTION
        @type int
        """
        self.ModelNodeCheckstateChanged('VTSCHR', p0)
    
    @pyqtSlot(int)
    def on_checkBox_VFPLNF_stateChanged(self, p0):
        """
        Slot documentation goes here.
        
        @param p0 DESCRIPTION
        @type int
        """
        self.ModelNodeCheckstateChanged('VFPLNF', p0)
    
    @pyqtSlot(int)
    def on_checkBox_VFSCHR_stateChanged(self, p0):
        """
        Slot documentation goes here.
        
        @param p0 DESCRIPTION
        @type int
        """
        self.ModelNodeCheckstateChanged('VFSCHR', p0)
    
    @pyqtSlot(int)
    def on_checkBox_EXPR_stateChanged(self, p0):
        """
        Slot documentation goes here.
        
        @param p0 DESCRIPTION
        @type int
        """
        self.ModelNodeCheckstateChanged('EXPR', p0)
    
    @pyqtSlot(int)
    def on_checkBox_PROPWR_stateChanged(self, p0):
        """
        Slot documentation goes here.
        
        @param p0 DESCRIPTION
        @type int
        """
        self.ModelNodeCheckstateChanged('PROPWR', p0)
    
    @pyqtSlot(int)
    def on_checkBox_JETPWR_stateChanged(self, p0):
        """
        Slot documentation goes here.
        
        @param p0 DESCRIPTION
        @type int
        """
        self.ModelNodeCheckstateChanged('JETPWR', p0)
    
    @pyqtSlot(int)
    def on_checkBox_GRNDEF_stateChanged(self, p0):
        """
        Slot documentation goes here.
        
        @param p0 DESCRIPTION
        @type int
        """
        self.ModelNodeCheckstateChanged('GRNDEF', p0)
    
    @pyqtSlot(int)
    def on_checkBox_TRNJET_stateChanged(self, p0):
        """
        Slot documentation goes here.
        
        @param p0 DESCRIPTION
        @type int
        """
        self.ModelNodeCheckstateChanged('TRNJET', p0)
    
    @pyqtSlot(int)
    def on_checkBox_HYPEFF_stateChanged(self, p0):
        """
        Slot documentation goes here.
        
        @param p0 DESCRIPTION
        @type int
        """
        self.ModelNodeCheckstateChanged('HYPEFF', p0)
    
    @pyqtSlot(int)
    def on_checkBox_LARWB_stateChanged(self, p0):
        """
        Slot documentation goes here.
        
        @param p0 DESCRIPTION
        @type int
        """
        self.ModelNodeCheckstateChanged('LARWB', p0)
    
    @pyqtSlot(int)
    def on_checkBox_CONTAB_stateChanged(self, p0):
        """
        Slot documentation goes here.
        
        @param p0 DESCRIPTION
        @type int
        """
        self.ModelNodeCheckstateChanged('CONTAB', p0)
    
    @pyqtSlot(int)
    def on_checkBox_SYMFLP_stateChanged(self, p0):
        """
        Slot documentation goes here.
        
        @param p0 DESCRIPTION
        @type int
        """
        self.ModelNodeCheckstateChanged('SYMFLP', p0)
    
    @pyqtSlot(int)
    def on_checkBox_ASYFLP_stateChanged(self, p0):
        """
        Slot documentation goes here.
        
        @param p0 DESCRIPTION
        @type int
        """
        self.ModelNodeCheckstateChanged('ASYFLP', p0)
    
    @pyqtSlot(int)
    def on_checkBox_TVTPAN_stateChanged(self, p0):
        """
        Slot documentation goes here.
        
        @param p0 DESCRIPTION
        @type int
        """
        self.ModelNodeCheckstateChanged('TVTPAN', p0)
    
    @pyqtSlot(QTreeWidgetItem, int)
    def on_treeWidget_model_itemChanged(self, item, column):
        """
        Slot documentation goes here.
        响应checkstate的变化，同步修改左侧的checkBox控件
        
        @param item 发生变化的QTreeWidgetItem对象
        @type QTreeWidgetItem
        @param column 发生变化的列
        @type int
        """
        cardName = item.text(0)
        if cardName in self.dtDefine.getNamelistCollection():
            self.ModelNodeCheckstateChanged(cardName, item.checkState(0))
            


