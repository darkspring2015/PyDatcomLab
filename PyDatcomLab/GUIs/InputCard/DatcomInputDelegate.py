# -*- coding: utf-8 -*-

"""
Module implementing DatcomInputDelegate.
"""
from PyQt5 import QtCore,  QtWidgets 
from PyDatcomLab.Core.DictionaryLoader import  defaultDatcomDefinition as DDefine 
from PyDatcomLab.GUIs.InputCard.DatcomInputSingle import DatcomInputSingleNoLabel as SInput 
from PyDatcomLab.GUIs.InputCard.DatcomInputList   import DatcomInputListNoLabel as LInput 

import logging

 

class DatcomInputContinuousDelegate(QtWidgets.QStyledItemDelegate ):
    """
    定义一个用来编辑的代理控件
    """
    def __init__(self, vUrl, parent = None, iDefine = DDefine):
        """
        vUrl 指向需要代理的列的定义
        """
        super(DatcomInputContinuousDelegate, self).__init__(parent)
        #日志系统        
        self.logger = logging.getLogger(r'Datcomlogger')  
        if iDefine is None : iDefine = DDefine
        self.dtDefine = iDefine
        if vUrl is None or vUrl == "":
            self.logger.error("输入的标示是无效的：%s ！"%vUrl) 
            vUrl ='/'
        self.vUrl = vUrl
        self.Namelist  = self.vUrl.split('/')[-2]   #指向的Namelist的名称
        self.VarName   = self.vUrl.split('/')[-1]   #指向的变量名称
        self.VarDefine = self.dtDefine.getVariableDefineByName( self.Namelist, self.VarName) 
        
        
    def createEditor(self, parent, option, index):
        """  
        Reimplemented from QAbstractItemDelegate::createEditor().
        Returns the widget used to edit the item specified by index for editing. 
        The parent widget and style option are used to control how the editor widget appears.      
        """
        #printTrace('createEditor') 
        tType = 'REAL'
        if 'SubType' in self.VarDefine.keys():
            self.logger.info("%s在定义中声明的SubType为%s"%(self.vUrl, self.dtDefine['SubType']))
            tType = self.VarDefine['SubType']
        if tType in ['REAL', 'INT']:
            tTb = SInput(self.vUrl, parent=parent,  iDefinition = self.dtDefine )
            tTb.isDelegate = True 
        else:
            tTb = LInput(self.vUrl, parent=parent,  iDefinition = self.dtDefine)   
            tTb.isDelegate = True     
        return tTb
        
    def setEditorData(self,editor, index ):
        """
        Reimplemented from QAbstractItemDelegate::setEditorData().
        Sets the data to be displayed and edited by the editor from the data model item specified by the model index.
        The default implementation stores the data in the editor widget's user property.
        """
        #item_str     = index.data(QtCore.Qt.DisplayRole)
        itemUserData = index.data(QtCore.Qt.UserRole)
        editor.setDelegateData(itemUserData)


            
    def updateEditorGeometry(self, editor, option, index):
        """
        #void  updateEditorGeometry(QWidget *editor,  
         const  QStyleOptionViewItem &option,  const  QModelIndex &index) const
        Reimplemented from QAbstractItemDelegate::updateEditorGeometry().
        Updates the editor for the item specified by index according to the style option given.
        """       
        #option.parent().item(index.row(), index.column())
        editor.setGeometry(option.rect)
        #tAbsRect = option.rect
        #self.parent().mapToGlobal( option.rect.lift(), option.rect.top)
        #editor.resize(option.rect.width(),option.rect.height() )

        
    def setModelData(self, editor,  model, index):
        """
        void  setModelData(QWidget *editor, QAbstractItemModel *model,  
         const  QModelIndex &index)  const 
        """

        itemUserData = editor.getDelegateData()
        #item_str = editor.getDelegateData()  
        #回写数据  
        #model.setData(index, item_str, QtCore.Qt.EditRole)
        model.setData(index, itemUserData, QtCore.Qt.UserRole)
        model.setData(index, str(itemUserData['Value']) , QtCore.Qt.DisplayRole)
        
        
        


