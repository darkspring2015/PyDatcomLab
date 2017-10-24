#!/usr/bin/env python
#encoding: utf-8
'''test loadTestsFromModule功能'''
import unittest

class myTestFromModule(unittest.TestCase):
    def setUp(self):
        pass
    def tearDown(self):
        pass
    def testHelpOpen(self):
        self.assertGreater(3, 2)
    def testHelpWeb(self):
        self.assertEqual('a', "a")
    def testHelpWeb2(self):
        self.assertCountEqual((1, 2), (2, 2))   
    


if __name__ == '__main__':
    unittest.main()
