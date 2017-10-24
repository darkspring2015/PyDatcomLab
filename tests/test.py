#!/usr/bin/env python
#encoding: utf-8
import unittest

#sys.path.insert(0, os.path.realpath(os.path.split( os.path.realpath( sys.argv[0] ) )[0]+r'\..'))

from PyDatcomGUI.Ui_MainWindow import  Ui_MainWindow
class mytest(unittest.TestCase):
  ##初始化工作PyDatcomGUI

  def setUp(self):    
    self.tclass = Ui_MainWindow()
    ##实例化了被测试模块中的类
  #退出清理工作
  def tearDown(self):
    pass    
  #具体的测试用例，一定要以test开头
  def test_Running(self):
    import sys
    from PyQt5 import   QtWidgets
    self.app = QtWidgets.QApplication(sys.argv)
    MainWindow = QtWidgets.QMainWindow()
    self.tclass.setupUi(MainWindow)
    #MainWindow.setModality(1)
    MainWindow.show()
    self.reCode = sys.exit(self.app.exec_())
    self.assertEqual(self.reCode[0], 0)

#创建测试套件
suite = unittest.TestSuite()
suite.addTests(unittest.defaultTestLoader.loadTestsFromTestCase(mytest))

    
if __name__ =='__main__':    
    #import sys
    #sys.path.append('C:\\Users\\lingo\\Documents\\workspace\\DatcomSolution\\PyDatcomLab')

    unittest.main()
