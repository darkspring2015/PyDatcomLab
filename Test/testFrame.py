#!/usr/bin/env python
#encoding: utf-8
import unittest

class myFrameTet(unittest.TestCase):
    def setUp(self):
        pass
    def tearDown(self):
        pass
    def testHelpOpen(self):
        self.assertGreater(3, 2)
    def testHelpWeb(self):
        self.assertEqual('a', "a")
    def testHelpWeb2(self):
        self.assertCountEqual((2, 2), (2, 2))
    
    
def getSuite():        
    tests = unittest.defaultTestLoader.loadTestsFromTestCase(myFrameTet)
    suite =  unittest.TestSuite() 
    suite.addTests(tests)
    return suite
    
suite = getSuite()

if __name__ == '__main__':
    unittest.main()
