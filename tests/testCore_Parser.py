#!/usr/bin/env python

import unittest
import os
from PyDatcomLab.Core import DatcomParser

class Test(unittest.TestCase):

    def setUp(self):
        self.tfileDir = os.path.join('tests','data', 'PyDatcom-Tests')
        self.tPrjdir = os.path.join('extras','PyDatcomProjects', 'tests')
        #pass

    def test_parse(self):
        #file_name=None, debug=0,  keep_parse_tab=False
        parser = DatcomParser(os.path.join(self.tfileDir, 'Citation.out'), 1, True)
        self.assertFalse(parser is None)
        commonDict = parser.get_common()
        self.assertFalse(commonDict is None)
        #print(commonDict)
        mycases = parser.get_cases()
        self.assertFalse(mycases is None)
        #print(mycases)
    def test_parser_UASF(self):        
        parser = DatcomParser(os.path.join('tests','data', 'exwin', 'ex1.out'))
        
        commonDict = parser.get_common()
        self.assertFalse(commonDict is None)
        
        mycases = parser.get_cases()
        self.assertFalse(mycases is None)
        #print(mycases)


        
if __name__ == '__main__':
    #加入直接运行无法工作，请直接在命令行中制定当前工作目录
    unittest.main()
