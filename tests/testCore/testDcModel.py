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
        tDoc = Df.dtNmlstExmple3
        pmObj.setDoc(tDoc)
        tXML = pmObj.getDocXMLString()
        #print(tXML)   
        import tempfile
        s_2, f_2 = tempfile.mkstemp(suffix='.dcxml',prefix='Datcom', text=True)
        print(f_2)
        pmObj.writeToXML(f_2)
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
    def test_SetDocByXML(self):
        dM = self.tClass('J6', '常规布局')
        dM.setNamelist('BODY', 'NX', '10.0')
        dM.setNamelist('BODY', 'X', [0.0,0.258,0.589,1.260,2.260,2.590,2.930,3.590,4.570,6.260,], 1)

        import tempfile
        # delete默认删除，为True则关闭临时文件时候不删除，
        #f_2 = NamedTemporaryFile(delete=False)
        #tempfile.mkstemp([suffix=”[, prefix=’tmp'[, dir=None[, text=False]]]])
        s_2, f_2 = tempfile.mkstemp(suffix='.dcxml',prefix='Datcom', text=True)
        print(f_2)
        dM.writeToXML(f_2)

        
        dM2 = self.tClass('J62', '常规布局2')
        dM2.loadXML(f_2)
        
        doc = dM2.getNamelist('BODY')
        self.assertTrue(type(doc) is dict)
        self.assertTrue('NX' in doc.keys())
        self.assertEqual([0.0,0.258,0.589,1.260,2.260,2.590,2.930,3.590,4.570,6.260,] 
                                , doc['X']['Value'])
    def test_writeToDatcomInput(self):
        """测试文件写入能力"""
        import tempfile
        
        s_2, f_2 = tempfile.mkstemp(suffix='.inp',prefix='Datcom', text=True)
        print(f_2)
        
        dM = self.tClass('J6', '常规布局')
        dM.setDoc(Df.dtNmlstExmple)
        res = dM.writeToDatcomInput(f_2)
        self.assertTrue( not res  ==  "")


    
    
def getSuite():        
    tests = unittest.defaultTestLoader.loadTestsFromTestCase(test_Core_dcModel)
    suite = unittest.TestSuite() 
    suite.addTests(tests)
    return suite
    
suite = getSuite()

if __name__ == '__main__':
    #os.chdir(r'..')
    unittest.main()
