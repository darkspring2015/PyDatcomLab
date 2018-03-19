#!/usr/bin/env python
#encoding: utf-8
import unittest
import sys, os

#此句是单独运行调试可以通过的关键
sys.path.append(os.path.abspath(os.path.join('.', 'PyDatcomLab', 'GUIs')))
sys.path.append(os.path.abspath(os.path.join('.', 'PyDatcomLab', 'GUIs', 'PlaneConfiguration')))

from PyDatcomLab.GUIs.PlaneConfiguration import  DatcomBaseUI 
from PyDatcomLab.Core.DictionaryLoader import  DTdictionary  
from PyQt5 import  QtWidgets
class parserTest(unittest.TestCase):    
    def setUp(self):
        self.tClass = DTdictionary
    def tearDown(self):
        self.tClass = None
    def testRuning(self):
        #进入系统的主循环
        app = QtWidgets.QApplication(sys.argv)
        card = QtWidgets.QWidget()
        card.NameList = 'FLTCON'        
        dfLoader = self.tClass(r'C:\Users\linger\.PyDatcom\config\datcomDefine.xml')
        card.VariableList = dfLoader.getNamelistDefineByName(card.NameList)
        card.groupDefine = dfLoader.getCARDAddtionalInformation(card.NameList, 'GroupDefine')
        ui = DatcomBaseUI.DatcomBaseUI()
        ui.setupUi(card)
        if hasattr(ui,'NALT'):
            print(ui.NALT)
        if card.findChild(QtWidgets.QLineEdit,'NALT'):
            print(card.findChild(QtWidgets.QLineEdit,'NALT'))
        card.show()
        sys.exit(app.exec_())
        
        
if  __name__ == '__main__':
    #testDataPath =  r'./'
    nowdir = os.path.abspath(r'.')
    Guidir = os.path.join(nowdir, 'PyDatcomLab', 'GUIs')
    sys.path.append(Guidir)
    unittest.main()
