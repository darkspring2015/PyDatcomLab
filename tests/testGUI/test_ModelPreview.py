#!/usr/bin/env python
#encoding: utf-8
import unittest
import sys, os

#此句是单独运行调试可以通过的关键
sys.path.append(os.path.abspath(os.path.join('.', 'PyDatcomLab', 'GUIs')))
sys.path.append(os.path.abspath(os.path.join('.', 'PyDatcomLab', 'GUIs', 'components')))

from PyDatcomLab.GUIs.components import  ModelPreview as NM
from PyQt5 import  QtWidgets
class parserTest(unittest.TestCase):    
    def setUp(self):
        self.tClass = NM.ModelPreview
    def tearDown(self):
        self.tClass = None
    def testRuning(self):

        #进入系统的主循环
        app = QtWidgets.QApplication(sys.argv)
        mainWin = self.tClass()
        mainWin.logger.info("启动了NewModelDlg")
        mainWin.loadModel(r'E:\Projects\PyDatcomLab\extras\PyDatcomProjects\1\datcomDefine.xml')
        mainWin.show()
        self.assertEqual(app.exec_(), 0)
        
        
if  __name__ == '__main__':
    #testDataPath =  r'./'
    nowdir = os.path.abspath(r'.')
    Guidir = os.path.join(nowdir, 'PyDatcomLab', 'GUIs')
    sys.path.append(Guidir)
    unittest.main()
