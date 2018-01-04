#!/usr/bin/env python
#encoding: utf-8
'''测试脚本的编写，这就是一个脚本，不需要定义Class'''
import unittest, os
os.chdir(r'..')



#将所有的测试全部租住在这里
from tests import testCore as tCore 
from tests import testParser as tP
from tests import testFrame as tFm
from tests import testDatcomRunner as tRun
from tests.testGUI import test_ImageTips as Imtips

#实例化测试套件
suite = unittest.TestSuite()  #这是eric6测试需要的关键变量
#将测试用例加载到测试套件中
#将所有的测试类放到这里：
test_cases = (tCore.Test, tP.parserTest, tFm.myFrameTet, tRun.testDatcomRunner
                Imtips.)  #每一个unittest.TestCase类的子类
for test_case in test_cases:
    tests = unittest.defaultTestLoader.loadTestsFromTestCase(test_case)
    suite.addTests(tests) 

#这是第二种编写方法
#suite.addTest(tFm.suite)


#suite.addTest(testCore.Test('test_*'))
#suite.addTest(testCore.Test('test_export'))
#suite.addTest(testCore.Test('test_parse'))
#suite.addTest(testCore.Test('test_plot'))


#suite.addTests()

#实例化TextTestRunner类
#使用run()方法运行测试套件（即运行测试套件中的所有用例）
#runner=unittest.TextTestRunner()
#runner.run(suite)

 
    




