#!/usr/bin/env python

#encoding: utf-8
import unittest, os
from PyDatcomLab.Core import  projectManager as pm
import shutil

class test_Core_projectManager(unittest.TestCase):    
    def setUp(self):
        self.tClass = pm.projectManager
        self.testDir = os.path.join(os.path.abspath(r'.'), 'extras', 'PyDatcomProjects', '1')
        self.tPName  = r'TestPojects'
        self.objxml = os.path.join(self.testDir, self.tPName, self.tPName+'.dcprj')
        
        objDir = os.path.join(self.testDir, self.tPName)
        if  os.path.exists(objDir) :
            shutil.rmtree(objDir)
   
    def tearDown(self):
        self.tClass = None
        objDir = os.path.join(self.testDir, self.tPName)
        if  os.path.exists(objDir) :
            shutil.rmtree(objDir)
        
    def test_createprojects(self):
        pmObj = self.tClass('')
        pmObj.newProject(tProjectName = self.tPName
                                  ,tParentDir =self.testDir
                                  , tAerocraftName = 'U22')
                                  
        self.assertTrue(not pmObj is  None)
        self.assertTrue(os.path.exists(self.objxml))
    def test_removePorject(self):
        pmObj = self.tClass('')
        pmObj.newProject(tProjectName = self.tPName
                                  ,tParentDir =self.testDir
                                  , tAerocraftName = 'U22')
                                  
        self.assertTrue(not pmObj is  None)
        self.assertTrue(os.path.exists(self.objxml))
        pmObj.removeProject(self.objxml)
        

    
    
def getSuite():        
    tests = unittest.defaultTestLoader.loadTestsFromTestCase(test_Core_projectManager)
    suite = unittest.TestSuite() 
    suite.addTests(tests)
    return suite
    
suite = getSuite()

if __name__ == '__main__':
    #os.chdir(r'..')
    unittest.main()
