#!/usr/bin/env python
#encoding: utf-8
import unittest
import sys, os

#此句是单独运行调试可以通过的关键
sys.path.append(os.path.abspath(os.path.join('.', 'PyDatcomLab', 'GUIs')))
sys.path.append(os.path.abspath(os.path.join('.', 'PyDatcomLab', 'GUIs', 'PlaneConfiguration')))

from PyDatcomLab.GUIs.PlaneConfiguration import  BODY as B
from PyDatcomLab.Core import datcomDefine as Df , dcModel as pm
from PyQt5 import  QtWidgets
class BODY_Test(unittest.TestCase):    
    def setUp(self):
        self.tClass = B.BODY
    def tearDown(self):
        self.tClass = None
    def testRuning(self):

        #进入系统的主循环
        app = QtWidgets.QApplication(sys.argv)
        mainWin = self.tClass()
        mainWin.logger.info("启动了PlaneConfiguration")
        mainWin.show()
        #mainWin.close()
        self.assertEqual(app.exec_(), 0)
        
    def test_Model(self):
        #构造数据
        pmObj = pm.dcModel('J6', '常规布局')
        tDoc = Df.dtNmlstExmple
        pmObj.setDoc(tDoc)      

        
        #进入系统的主循环
        
        app = QtWidgets.QApplication(sys.argv)
        mainWin = self.tClass(tModel = pmObj)
        mainWin.logger.info("启动了PlaneConfiguration")
        mainWin.show()
        self.assertEqual(app.exec_(), 0)
        tDoc2 = mainWin.getDoc()

        self.assertTrue(tDoc2 is not None)
        self.assertEqual(tDoc['BODY']['X']['Value'] 
                       , tDoc2.getNamelistVar('BODY','X')['Value'])
        

        
if  __name__ == '__main__':
    #testDataPath =  r'./'
    nowdir = os.path.abspath(r'E:\Projects\PyDatcomLab')
    Guidir = os.path.join(nowdir, 'PyDatcomLab', 'GUIs')
    sys.path.append(Guidir)
    unittest.main()
