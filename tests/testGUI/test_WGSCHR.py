#!/usr/bin/env python
#encoding: utf-8
import unittest
import sys, os

#此句是单独运行调试可以通过的关键
sys.path.append(os.path.abspath(os.path.join('.', 'PyDatcomLab', 'GUIs')))
sys.path.append(os.path.abspath(os.path.join('.', 'PyDatcomLab', 'GUIs', 'PlaneConfiguration')))

from PyDatcomLab.GUIs.PlaneConfiguration import  WGSCHR as O
from PyDatcomLab.Core import datcomDefine as Df , dcModel as pm
from PyQt5 import  QtWidgets
class BODY_Test(unittest.TestCase):    
    def setUp(self):
        self.tClass = O.WGSCHR
    def tearDown(self):
        self.tClass = None
    def testRuning(self):

        #进入系统的主循环
        app = QtWidgets.QApplication(sys.argv)
        mainWin = self.tClass()
        mainWin.logger.info("启动了WGSCHR")
        mainWin.show()
        #mainWin.close()
        self.assertEqual(app.exec_(), 0)
        
    def test_Model(self):
        #构造数据
        pmObj = pm.dcModel('J6', '常规布局')
        tDoc = Df.dtNmlstExmple3
        pmObj.setDoc(tDoc) 
        pmObj2 =  pm.dcModel('J6', '常规布局')
        pmObj2.setDoc(tDoc) 
        #进入系统的主循环
        
        app = QtWidgets.QApplication(sys.argv)
        config= {'model':pmObj}
        mainWin = self.tClass(config = config)
        mainWin.logger.info("启动了WGSCHR")
        mainWin.show()
        self.assertEqual(app.exec_(), 0)
        tDoc2 = mainWin.getDoc()

        self.assertTrue(tDoc2 is not None)
        self.assertEqual(pmObj2.getNamelistVar('WGSCHR','TOVC')
                       , tDoc2.getNamelistVar('WGSCHR','TOVC'))
        

        
if  __name__ == '__main__':
    #testDataPath =  r'./'
    nowdir = os.path.abspath(r'E:\Projects\PyDatcomLab')
    Guidir = os.path.join(nowdir, 'PyDatcomLab', 'GUIs')
    sys.path.append(Guidir)
    unittest.main()
