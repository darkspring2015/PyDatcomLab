#!/usr/bin/env python
#encoding: utf-8
import unittest
import sys, os

#此句是单独运行调试可以通过的关键
sys.path.append(os.path.abspath(os.path.join('.', 'PyDatcomLab', 'GUIs')))
sys.path.append(os.path.abspath(os.path.join('.', 'PyDatcomLab', 'GUIs', 'PlaneConfiguration')))

from PyDatcomLab.GUIs.PlaneConfiguration import  HYPEFF as O
from PyDatcomLab.Core import datcomDefine as Df , dcModel as pm
from PyQt5 import  QtWidgets
class HYPEFF_Test(unittest.TestCase):    
    def setUp(self):
        self.tClass = O.HYPEFF
        self.tClassName = 'HYPEFF'
    def tearDown(self):
        self.tClass = None
    def testRuning(self):

        #进入系统的主循环
        app = QtWidgets.QApplication(sys.argv)
        mainWin = self.tClass()
        mainWin.logger.info("启动了%s"%self.tClassName)
        mainWin.show()
        #mainWin.close()
        self.assertEqual(app.exec_(), 0)
        
    def test_Model(self):
        #构造数据
        dM = pm.dcModel('J6', '常规布局')
        tDoc = Df.dtNmlstExmple3
        dM.setDoc(tDoc) 
        dM.setNamelist(self.tClassName, 'HNDLTA', 10)        
        dM.setNamelist(self.tClassName, 'LAMNR', '.FALSE.')
        dM.setNamelist(self.tClassName, 'HDELTA',[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]  , 1)
        dM.setNamelist(self.tClassName, 'CF',1.01)

        #进入系统的主循环
        
        app = QtWidgets.QApplication(sys.argv)
        mainWin = self.tClass(tModel = dM)
        mainWin.logger.info("启动了%s"%self.tClassName)
        mainWin.show()
        self.assertEqual(app.exec_(), 0)
        tDoc2 = mainWin.getDoc()

        self.assertTrue(tDoc2 is not None)

        

        
if  __name__ == '__main__':
    #testDataPath =  r'./'
    nowdir = os.path.abspath(r'E:\Projects\PyDatcomLab')
    Guidir = os.path.join(nowdir, 'PyDatcomLab', 'GUIs')
    sys.path.append(Guidir)
    unittest.main()