#!/usr/bin/env python

#encoding: utf-8
import unittest
from PyDatcomLab.Core import  dcModel as pm
from PyDatcomLab.Core import datcomDefine as Df


class test_Core_dcModel(unittest.TestCase):    
    def setUp(self):
        self.tClass = pm.dcModel

   
    def tearDown(self):
        self.tClass = None

        
    def test_setDoc(self):
        pmObj = self.tClass('J6', '常规布局')
        tDoc = Df.dtNmlstExmple
        pmObj.setDoc(tDoc)
        tXML = pmObj.getDocXML()
        print(tXML)       
        self.assertTrue(not pmObj is  None)
        self.assertTrue(not tXML is None)
        
    def test_set_getNamlist(self):
        dM = self.tClass('J6', '常规布局')
        dM.setNamelist('BODY', 'NX', '10.0')
        dM.setNamelist('BODY', 'X', [0.0,0.258,0.589,1.260,2.260,2.590,2.930,3.590,4.570,6.260,], 1)
        
        doc = dM.getNamelist('BODY')
        self.assertTrue(type(doc) is dict)
        self.assertTrue('NX' in doc.keys())
        self.assertEqual([0.0,0.258,0.589,1.260,2.260,2.590,2.930,3.590,4.570,6.260,] 
                        , doc['X']['Value'])
        


    
    
def getSuite():        
    tests = unittest.defaultTestLoader.loadTestsFromTestCase(test_Core_dcModel)
    suite = unittest.TestSuite() 
    suite.addTests(tests)
    return suite
    
suite = getSuite()

if __name__ == '__main__':
    #os.chdir(r'..')
    unittest.main()
