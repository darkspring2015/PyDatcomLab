#!/usr/bin/env python

import unittest
import os
from PyDatcomLab.Core import DatcomParser, DatcomExporter, DatcomPlotter


class Test(unittest.TestCase):

    def setUp(self):
        pass

    def test_parse(self):
        parser = DatcomParser(os.path.join('tests','data', 'PyDatcom-Tests', 'Citation.out'))
        self.assertFalse(parser is None)
        commonDict = parser.get_common()
        self.assertFalse(commonDict is None)
        print(commonDict)
        mycases = parser.get_cases()
        self.assertFalse(mycases is None)
        print(mycases)
    def test_parser_UASF(self):        
        parser = DatcomParser(os.path.join('tests','data', 'exwin', 'ex1.out'))
        self.assertFalse(parser is None)
        commonDict = parser.get_common()
        self.assertFalse(commonDict is None)
        print(commonDict)
        mycases = parser.get_cases()
        self.assertFalse(mycases is None)
        print(mycases)

    def test_export(self):
        parser = DatcomParser(os.path.join('tests','data', 'PyDatcom-Tests', 'Citation.out'))
        moPath = os.path.abspath(os.path.join('PyDatcomLab','Core', 'Templates', 'modelica.mo'))
        exporter = DatcomExporter(parser.get_common(), moPath)
        result = exporter.get_export()
        self.assertTrue(result != None)

    def test_plot(self):
        parser = DatcomParser(os.path.join('tests','data', 'PyDatcom-Tests', 'Citation.out'))
        plotter = DatcomPlotter(parser.get_common(), os.path.join('extras','PyDatcomProjects', 'tests', 'fig'))
        plotter.common_plots()
        self.assertTrue(plotter != None)
        
if __name__ == '__main__':
    #加入直接运行无法工作，请直接在命令行中制定当前工作目录
    unittest.main()
