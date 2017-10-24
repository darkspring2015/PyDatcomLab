import unittest
'''测试脚本的编写，这就是一个脚本，不需要定义Class'''

#将所有的测试全部租住在这里


#实例化测试套件
suite = unittest.TestSuite()  #这是eric6测试需要的关键变量
loader = unittest.defaultTestLoader
#将测试用例加载到测试套件中
#suite.addTest(loader.loadTestsFromModule(__import__('test.testLoadFromMoudle')))

suite.addTest(loader.loadTestsFromModule(__import__('test.testFrame')))

 
    




