#!/usr/bin/env python
"""
本脚本从原有的定义文件中抽取Datcom定义到XML文件
"""

import logging
import os, sys
#import time
from xml.etree import ElementTree  as ET
mainPath = os.path.split(os.path.realpath(__file__))[0]
prjBase = os.path.realpath('%s\..'%mainPath)
sys.path.append(os.path.abspath(prjBase))
sys.path.append(os.path.abspath(os.path.join(prjBase, r'PyDatcomLab', 'GUIs')))
sys.path.append(os.path.abspath(os.path.join(prjBase, r'PyDatcomLab', 'GUIs', 'PlaneConfiguration')))


from PyDatcomLab.Core.dcModel import dcModel
#from PyDatcomLab.Core.datcomDefine import Dimension , groupDefine
#from PyDatcomLab.Core.datcomDefine import modelTemplate
from PyDatcomLab.Core.datcomDefine import reserved_NAMELISTS as allCard, datcomVarAttriList as dfVarAttrib
from PyDatcomLab.Core.tools import xml_Indent as indent
from PyDatcomLab.GUIs.PlaneConfiguration import *

from PyQt5.QtWidgets import QWidget, QApplication, QComboBox, QLineEdit, QTableWidget, QLabel, QCheckBox


def AnalsyeUI(tPath = '~\.PyDatcom\datcomDefine.xml'):
    """
    将所有的UI界面的信息提取到tPath定义的xml文件中
    
    """
    tlogger = logging.getLogger(r'Datcomlogger')
    tDcModel = dcModel()
    #进入系统的主循环
    app = QApplication(sys.argv)
    pWdiget = QWidget()
    root = ET.Element('DatcomVariableDefine')
    #遍历选项卡
    for iC in allCard:
        mdObj = __import__(iC)
        cardMd = getattr(mdObj,iC)
        aW = cardMd( pWdiget, tModel = tDcModel)
        #存储Group的定义信息
        tGroups = {}
        #读取信息
        tNamelistNode = ET.SubElement(root, iC,{'dcType':'Namelist'})  
        
        if not hasattr(aW,'NameList') or not hasattr(aW,'VariableList'):   
            continue   
        tNamelist = aW.NameList
        for iV in aW.VariableList.keys():
            #遍历所有的
            tAttibdict = dfVarAttrib.copy()
            tAttibdict['VarName'] = iV
            tAttibdict['NameList'] = tNamelist  
            tVar = aW.VariableList[iV]
            for iAttri in tVar.keys():
                tAttibdict[iAttri] = str(tVar[iAttri])
            #抽取DisplayName
            tLabel = aW.findChild(QLabel, 'label_%s'%iV)
            if tLabel :
                tAttibdict['DisplayName'] =  tLabel.text()
                if  tLabel.toolTip() != '':
                    tAttibdict['Tooltips'] =  tLabel.toolTip()                    
            tCheckbox = aW.findChild(QCheckBox, 'checkBox_%s'%iV)
            if tCheckbox:
                tAttibdict['DisplayName'] =  tCheckbox.text()
                if  tCheckbox.toolTip()!= '':
                    tAttibdict['Tooltips'] =  tCheckbox.toolTip()    
                tAttibdict['MustInput'] =  'UserCheck'        
            #读取帮助信息
            tWidget = None           
            
            if 'INT' == tAttibdict['TYPE'] or 'REAL' == tAttibdict['TYPE']:
                tWidget = aW.findChild(QLineEdit, 'lineEdit_%s'%iV)
            elif 'List' == tAttibdict['TYPE']:
                tWidget = aW.findChild(QComboBox, 'comboBox_%s'%iV)
                if tWidget: #tWidget有效
                    tDisRange =[]
                    for id in range(0, tWidget.count()):
                        tDisRange.append(tWidget.itemText(id))
                    tAttibdict['DisplayRange'] = str(tDisRange)
                        
            elif 'Array'  == tAttibdict['TYPE']:
                tWidget = aW.findChild(QTableWidget, 'tableWidget_%s'%tAttibdict['Group'])
                if tAttibdict['Group'] not in tGroups.keys():
                    tGroups[tAttibdict['Group']] = [tAttibdict['Group']]
                else:
                    tGroups[tAttibdict['Group']].append(iV)
            #分析提示信息
            
            if tWidget is None:
                tlogger.info("无法查询到对应的控件")
                #tTooltips = ""
            else:
                if tWidget.toolTip()!= '':
                    tAttibdict['Tooltips'] = tWidget.toolTip()
                #分析Group控件
                
            #写入结果
            tVarNode = ET.SubElement(tNamelistNode, iV,{'dcType':'Variable'})
            for iAllAt in tAttibdict.keys():
                tNode = ET.SubElement(tVarNode, iAllAt)
                tNode.text = tAttibdict[iAllAt]
        #追加一个Group定义
        tInfoNode = ET.SubElement(tNamelistNode, 'AdditionalInfo',{'dcType':'Infomation'})
        #NMACHLinkTable
        if hasattr(aW,'NMACHLinkTable'):
            tNMachLink = ET.SubElement(tInfoNode, 'NMACHLinkTable',{'dcType':'Rule'})
            tNMachLink.text = str(aW.NMACHLinkTable)
        #RuleNumToCount
        if hasattr(aW,'RuleNumToCount'):
            tRule = ET.SubElement(tInfoNode, 'RuleNumToCount',{'dcType':'Rule'})
            for iR in aW.RuleNumToCount:
                #tAtribs = {'dcType':'SubRule', 'Num':iR['Num'], 'Group':iR['Group']}
                tAtribs = {'Num':iR['Num'], 'Group':iR['Group']}                
                tSubRule0 = ET.SubElement(tRule, 'Rule',tAtribs)

        #RuleNumToCount
        if hasattr(aW,'RuleIndexToCombo'):
            tRule = ET.SubElement(tInfoNode, 'RuleIndexToCombo',{'dcType':'Rule'})
            for iR in aW.RuleIndexToCombo:
                tAtribs = {'Index':iR['Index'], 'Group':iR['Group']} 
                tSubRule0 = ET.SubElement(tRule, 'Rule',tAtribs)
                #ET.SubElement(tSubRule0, 'Index',{'dcType':'Rule'}).text = iR['Index']
                #ET.SubElement(tSubRule0, 'Group',{'dcType':'Rule'}).text = iR['Group']
                #tSubRule = ET.SubElement(tSubRule0, 'HowTo',{'dcType':'Rule'})
                for iSR in iR['HowTo'].keys():
                    #遍历新规则
                    tAttrib2 = {'key':iSR, 'value':str(iR['HowTo'][iSR])} 
                    ET.SubElement(tSubRule0, 'HowTo',tAttrib2 )
                    #tCombo = ET.SubElement(tSubRule, iSR)
                    #tCombo.text = str(iR['HowTo'][iSR])
        #附加Group的定义信息
        if len(tGroups) != 0:
            tRule = ET.SubElement(tInfoNode, 'GroupDefine',{'dcType':'Rule'})
            for iG in tGroups.keys():
                tAttrib = {'Name':iG, 'DisplayName':iG, 'ToolTips':iG}
                ET.SubElement(tRule, 'Group',tAttrib)
        
     
        #变量循环结束
    #Namelsit循环结束
    #app.exec_()
    return root

if __name__ == '__main__':
    """
    主分析脚本
    """

    tPath = os.path.expanduser('~\.PyDatcom\datcomDefine.xml')
    tXML = AnalsyeUI(tPath)
    indent(tXML, 0)
    #ET.dump(tXML)
    ET.ElementTree(tXML).write(tPath, encoding="UTF-8" )
    print("OK")
    
        
