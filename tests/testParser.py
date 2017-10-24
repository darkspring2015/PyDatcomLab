#Test Parser For all
#!/usr/bin/env python
#encoding: utf-8
import unittest

class parserTest(unittest.TestCase):    
    def setUp(self):
        pass
    def tearDown(self):
        pass
    def testRuning(self):
        self.assertEqual(0, 0)
    def testStart(self):
        self.assertEqual(1, 1)  
        
        
if  __name__ == '__main__':
    testDataPath =  r'./'
    unittest.main()
    
