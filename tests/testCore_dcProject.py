#!/usr/bin/env python

#encoding: utf-8
import unittest
import  os
from PyDatcomLab.Core import  projectManager as pm
#import shutil

class test_Core_projectManager_dcProject(unittest.TestCase):    
    def setUp(self):
        self.tClass = pm.dcProject
        self.testDir = os.path.join(os.path.abspath(r'.'), 'extras', 'PyDatcomProjects', '1')
        self.tPName  = 'TestPojects2'
        self.tPDescribe = u'我的第一个测试项目'
        self.aerocraftName = u'U2'
        
    def tearDown(self):
        self.tClass = None
        
    def test_newprojects(self):
        pmObj = self.tClass(prjName=self.tPName, 
                 prjDescribe =self.tPDescribe, 
                 tAerocraftName =self.aerocraftName)
        
        #断言对象可以创建                          
        self.assertTrue(not pmObj is  None)
        self.assertFalse( pmObj.doc is  None)
        pminfo = pmObj.getProjectInfo()
        self.assertEqual(pminfo['projectName']     ,self.tPName )
        self.assertEqual(pminfo['projectDescribe'] ,self.tPDescribe )
        cfinfo = pmObj.getAerocraftInfo()
        self.assertFalse( cfinfo is  None)
        self.assertEqual( cfinfo['aerocraftName'] ,self.aerocraftName )
    
    def test_loadproject(self):
        pmObj = self.tClass(prjName=self.tPName, 
                 prjDescribe =self.tPDescribe, 
                 tAerocraftName =self.aerocraftName)
        tmpFile  = os.path.join(self.testDir , self.tPName+'.dcprj')
        from xml.etree import ElementTree  as ET
        xml = ET.ElementTree(pmObj.doc)
        xml.write(tmpFile)
        pmObj2 = self.tClass()
        self.assertTrue(pmObj2.loadProject(tmpFile))
        self.assertEquals(pmObj.getProjectInfo() , pmObj2.getProjectInfo())
        os.remove(tmpFile)
        
    
def getSuite():        
    tests = unittest.defaultTestLoader.loadTestsFromTestCase(test_Core_projectManager_dcProject)
    suite = unittest.TestSuite() 
    suite.addTests(tests)
    return suite
    
suite = getSuite()

if __name__ == '__main__':
    #os.chdir(r'..')
    unittest.main()
