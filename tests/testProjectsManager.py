#!/usr/bin/env python
#encoding: utf-8
import unittest
import sys, os, logging

#此句是单独运行调试可以通过的关键
sys.path.append(os.path.abspath(os.path.join('.', 'PyDatcomLab', 'GUIs')))

from PyDatcomLab.GUIs import  ProjectsManager as pmr
from PyQt5 import  QtWidgets, QtGui, QtCore

class parserTest(unittest.TestCase):    
    def setUp(self):
        self.tClass = pmr.ProjectsManager
    def tearDown(self):
        self.tClass = None
    def testRuning(self):

        #进入系统的主循环
        app = QtWidgets.QApplication(sys.argv)
        mainWin = self.tClass()
        mainWin.logger.info("启动了ProjectsManager")
        mainWin.show()
        
        self.model = QtGui.QStandardItemModel(3, 3)
        self.model.setHeaderData(0,QtCore.Qt.Horizontal,u"类型") 
        self.model.setHeaderData(1,QtCore.Qt.Horizontal,u"值") 
        self.model.setHeaderData(2,QtCore.Qt.Horizontal,u"说明")
        
        self.model.setItem(0, 0, QtGui.QStandardItem("计数次数"))
        self.model.setItem(1, 0, QtGui.QStandardItem("单次计数"))
        self.model.setItem(2, 0, QtGui.QStandardItem("总计数"))
        
        #检查是否能够设置
        mainWin.BindingModel(self.model)
        
        self.assertEqual(app.exec_(), 0)
        
    
        
        
if  __name__ == '__main__':
    #testDataPath =  r'./'
    nowdir = os.path.abspath(r'.')
    Guidir = os.path.join(nowdir, 'PyDatcomLab', 'GUIs')
    sys.path.append(Guidir)
    unittest.main()
