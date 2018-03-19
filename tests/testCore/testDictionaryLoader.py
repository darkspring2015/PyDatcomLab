#!/usr/bin/env python

#encoding: utf-8
import unittest
from PyDatcomLab.Core.DictionaryLoader import  DTdictionary  
class test_Core_DictionaryLoader(unittest.TestCase):    
    def setUp(self):
        self.tClass = DTdictionary

   
    def tearDown(self):
        self.tClass = None

        
    def test_load(self):
        pmObj = self.tClass(r'C:\Users\linger\.PyDatcom\config\datcomDefine.xml')

        tBODY = pmObj.getNamelistDefineByName('BODY')
        print(tBODY)
        tWGSCHR = pmObj.getNamelistDefineByName('WGSCHR')
        print(tWGSCHR)

        self.assertTrue(not tBODY is  None)

    
def getSuite():        
    tests = unittest.defaultTestLoader.loadTestsFromTestCase(test_Core_DictionaryLoader)
    suite = unittest.TestSuite() 
    suite.addTests(tests)
    return suite
    
suite = getSuite()

if __name__ == '__main__':
    #os.chdir(r'..')
    unittest.main()
