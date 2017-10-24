#!/usr/bin/env python
#encoding: utf-8
import unittest, os
from PyDatcomLab.Core import  datcomRunner
class testDatcomRunner(unittest.TestCase):    
    def setUp(self):
        self.exePath = os.path.join('PyDatcomLab', 'Bin', 'DATCOM.exe')
        self.tproblemDir = os.path.join('extras','PyDatcomProjects',  'tests', 'example')
        self.tproblemFile = os.path.join('tests', 'data', 'exwin', 'EX1.INP')
        self.tClass = datcomRunner.runner(problemDir=self.tproblemDir,\
        execDatcomPath=self.exePath)
    def tearDown(self):
        self.tClass = None
    def test_runningPopen(self):
        strRes = self.tClass.runningPopen(exePath= self.exePath,problemFile=self.tproblemFile )
        self.assertEqual(strRes, "成功执行")

    
    
def getSuite():        
    tests = unittest.defaultTestLoader.loadTestsFromTestCase(testDatcomRunner)
    suite = unittest.TestSuite() 
    suite.addTests(tests)
    return suite
    
suite = getSuite()

if __name__ == '__main__':
    #os.chdir(r'..')
    unittest.main()

