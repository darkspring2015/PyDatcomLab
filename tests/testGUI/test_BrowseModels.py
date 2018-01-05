#!/usr/bin/env python
#encoding: utf-8
import unittest
import sys, os

#此句是单独运行调试可以通过的关键
sys.path.append(os.path.abspath(os.path.join('.', 'PyDatcomLab', 'GUIs')))

from PyDatcomLab.GUIs.components import  BrowseModels
from PyQt5 import  QtWidgets
class parserTest(unittest.TestCase):    
    def setUp(self):
        self.tClass = BrowseModels.DlgBrowseModels
    def tearDown(self):
        self.tClass = None
    def testRuning(self):

        #进入系统的主循环
        app = QtWidgets.QApplication(sys.argv)
        mainWin = self.tClass()
        mainWin.logger.info("启动了ImageTips")
        mainWin.show()
        self.assertEqual(app.exec_(), 0)
    def test_setPreviewDirectory(self):

        #进入系统的主循环
        app = QtWidgets.QApplication(sys.argv)
        mainWin = self.tClass()
        mainWin.logger.info("启动了ImageTips")
        mainWin.setPreviewDirectory(os.path.abspath(r"E:\Projects\PyDatcomLab\extras\PyDatcomProjects\1"))
        mainWin.show()
        self.assertEqual(app.exec_(), 0)
        
        
if  __name__ == '__main__':
    #testDataPath =  r'./'
    nowdir = os.path.abspath(r'.')
    Guidir = os.path.join(nowdir, 'PyDatcomLab', 'GUIs')
    sys.path.append(Guidir)
    unittest.main()
